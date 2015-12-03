/*
  Term encoding and decoding functions
  (c) 2000-2006 F.G. McCabe

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Contact: Francis McCabe <frankmccabe@mac.com>

  $Id: encoded.c,v 1.6 2004/07/06 16:51:43 fmccabe Exp $
  $Log: encoded.c,v $
*/

#include "config.h"		/* pick up standard configuration header */
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>

#include "go.h"
#include "eval.h"
#include "process.h"
#include "dict.h"
#include "symbols.h"
#include "signature.h"
#include "str.h"
#include "term.h"
#include "char.h"
#include "hash.h"
#include "encoded.h"             /* pick up the term encoding definitions */
#include "global.h"

/* Decode a term encoded message ... from a file stream */

typedef struct _encoding_support_ {
  ptrPo lbls;				/* The table of labels */
  long maxlbl;				/* How big is our table */
  ptrPo vars;				/* The table of variables */
  long maxvar;				/* How big is the variable table */
  uniChar *errorMsg;			/* Place to put error messages */
  long msgSize;				/* How big is that buffer */
  heapPo R;				/* Where should the roots go? */
} EncodeSupport, *encodePo;

#define TRM_VAL_MASK 0x0f
#define TRM_TAG_MASK 0xf0

/* 
 * Decode a structure from an input stream
 */
static retCode decode(ioPo in,encodePo S,heapPo H,long long lbl,ptrPo tgt);
static retCode decInt(ioPo in,encodePo S,integer *ii,char tag); 
static retCode codedEstimate(ioPo in,encodePo S,integer *amnt,integer *perm);

retCode decodeTerm(ioPo in,heapPo H,heapPo R,ptrPo tgt,uniChar *errorMsg,long msgSize)
{
  EncodeSupport support = { NULL, 0, NULL, 0, errorMsg, msgSize,R};
  uniChar ch;

 again:
  switch(inChar(in,&ch)){
  case Eof:
    return Eof;
  default:
    strMsg(errorMsg,msgSize,"stream prematurely ended");
    return Error;
  case Interrupt:
    goto again;
  case Ok:{
    if((ch&TRM_TAG_MASK)!=trmString){
      strMsg(errorMsg,msgSize,"invalid lead-in in code sequence");
      return Error;
    }
    else{
      integer len;
      retCode res = decInt(in,&support,&len,ch);
        
      switch(res){
      case Eof:
	strMsg(errorMsg,msgSize,"stream prematurely ended: %U",errorMsg);
	return Error;
      default:
	strMsg(errorMsg,msgSize,"io error in decoding");
	return Error;
      case Ok:{
	byte buff[MAX_MSG_LEN*10];
	byte *buffer = buff;
	long blen;
	    
	if(len>NumberOf(buff))
	  buffer = malloc(sizeof(byte)*len);
            
	res=inBytes(in,buffer,len,&blen); /* read in a block of bytes */
	  
	switch(res){
	case Ok:{
	  ioPo str = openInByteStr(buffer,len,rawEncoding);
	  integer amnt,perm = 0;
              
	  res = codedEstimate(str,&support,&amnt,&perm);

	  //logMsg(logFile,"Estimate of space requirements: %d heap, %d permanent",perm);
	  
	  if(perm>0 || H==&globalHeap)
	    lockGlobal();		/* lock access to the global heap */
	  if(res==Ok){
	    if(H!=&globalHeap){
	      res = reserveSpace(&globalHeap,perm);
	      if(res==Ok)
		res = reserveSpace(H,amnt);
	    }
	    else
	      res = reserveSpace(&globalHeap,perm+amnt);
	  }
	  if(res==Ok){
	    rewindByteStr(O_BYTE(str)); /* re-read from string buffer */

	    res = decode(str,&support,H,-1,tgt);

	    closeFile(str);

	    if(buffer!=buff)
	      free(buffer);
	    if(support.lbls!=NULL)
	      free(support.lbls);
	    if(support.vars!=NULL)
	      free(support.vars);
	  }
	  if(perm>0 || H==&globalHeap)
	    unlockGlobal();		/* release access to the global heap */
	  return res;
	}
	default:
	  strMsg(errorMsg,msgSize,"error in decoding");
	  return Error;
	}
      }
      }
    }
  }
  }
}

retCode decInt(ioPo in,encodePo S,integer *ii,char tag)
{
  integer len = tag&TRM_VAL_MASK;
  integer result = 0;
  int i;

  if(len==0)			/* recursive length */
    decInt(in,S,&len,inCh(in));

  for(i=0;i<len;i++){
    uniChar ch;

  again:
    switch(inChar(in,&ch)){
    case Eof:
      return Eof;
    case Ok:
      if(i==0)
	result = (integer)(signed char)ch;
      else
	result = (result<<8)|ch;
      continue;
    case Interrupt:
      if(checkForPause(getProcessOfThread()))
	goto again;
      else
	return Interrupt;
    default:
      strMsg(S->errorMsg,S->msgSize,"stream prematurely ended");
      return Error;
    }
  }

  if(ii!=NULL)
    *ii = result;
  return Ok;
}

static retCode decodeCode(ioPo str,encodePo S,unsigned long signature,ptrPo tgt);
static void updateLbls(encodePo S,ptrPo dst,ptrPo orig);
static void setLabel(encodePo S,long lbl,ptrI tgt);
static retCode decodeName(ioPo str,encodePo S,uniChar *buffer,long bufferLen);

static inline long mx(long a,long b)
{
  if(a>b)
    return a;
  else
    return b;
}

/*
  Warning: caller assumes responsibility for ensuring that tgt is a valid root
*/
retCode decode(ioPo in,encodePo S,heapPo H,long long lbl,ptrPo tgt)
{
  uniChar ch = inCh(in);
  retCode res;

  if(ch==uniEOF)
    return Eof;

  switch(ch & TRM_TAG_MASK){
  case trmVar:{				    /* an unbound variable */
    integer vno;

    if((res=decInt(in,S,&vno,ch))!=Ok)
      return res;

    if(vno>=0){
      if(vno>=S->maxvar){
	if(S->vars==NULL){		/* First variable in table */
	  S->maxvar = mx(128,vno);
	  S->vars = (ptrPo)malloc(sizeof(ptrI)*S->maxvar);
	  memset(S->vars,0,sizeof(ptrI)*S->maxvar); /* clear variables */
	}
	else{
	  long newmax = mx(S->maxvar+(S->maxvar>>1),vno);   /* 50% growth */
	  ptrPo newvars = (ptrPo)realloc(S->vars,sizeof(ptrI)*newmax);

	  if(newvars==NULL){
	    syserr("no space");
	    return Error;
	  }
	  else{
	    long i = S->maxvar;
	    for(i=S->maxvar;i<newmax;i++)
	      newvars[i]=0;		    /* clear the new entry */
	    
	    S->vars = newvars;
	    S->maxvar = newmax;
	  }
	}
      }

      if(S->vars[vno]==0){
	if(inHeap(H,(objPo)tgt))
	  S->vars[vno]=unBind(tgt);	 /* initialize the variable */
	else
	  S->vars[vno]=*tgt=allocateVar(H);
      }
      else
	*tgt = S->vars[vno];	    /* copy in the variable binding */
    }
    else
      unBind(tgt);			    /* A void variable? */

    return Ok;
  }
  case trmInt:{
    integer i;
    if((res=decInt(in,S,&i,ch))!=Ok)
      return res;
    *tgt=allocateInteger(H,i);
    return Ok;
  }
  case trmFlt:{
    integer exp;
    uniChar buff[16];
    double f = 0.0;
    int len = ch&TRM_VAL_MASK;
    int i;

    if((res=decInt(in,S,&exp,inCh(in)))!=Ok)
      return res;

    for(i=0;i<len;i++)
      if((buff[i]=inCh(in))==uniEOF)
        return Eof;

    while(len--)
      f = (f + (buff[len]&0xff))/256;

    f = ldexp(f,exp);
    *tgt = allocateFloat(H,f);
    return Ok;
  }

  case trmNegFlt:{
    integer exp;
    uniChar buff[16];
    double f = 0.0;
    int len = ch&TRM_VAL_MASK;
    int i;

    if((res=decInt(in,S,&exp,inCh(in)))!=Ok)
      return res;

    for(i=0;i<len;i++)
      if((buff[i]=inCh(in))==uniEOF)
        return Eof;

    while(len--)
      f = (f + (buff[len]&0xff))/256;

    f = ldexp(f,exp);
    *tgt = allocateFloat(H,-f);
    return Ok;
  }

  case trmSym:{
    integer sLen;
    
    if((res=decInt(in,S,&sLen,ch))!=Ok)
      return res;
    else{
      uniChar buff[sLen+1];
      integer i;

      for(i=0;res==Ok && i<sLen;i++){
        uniChar ch;
	
	res = inChar(in,&ch);
        
	buff[i]=ch;
      };

      buff[i]='\0';

      *tgt = newUniSymbol(buff);

      return res;
    }
  }

  case trmChar:{
    integer c;
    if((res=decInt(in,S,&c,ch))!=Ok)
      return res;
    else{
      *tgt = newChar((uniChar)c);
      return Ok;
    }
  }

  case trmString:{                      /* A literal string */
    integer len;                        /* The length of the encoded string */
    
    if((res=decInt(in,S,&len,ch))!=Ok)
      return res;
    else{
      byte bBuff[2048];
      byte *bData = (len<NumberOf(bBuff)?bBuff:(byte*)(malloc(len*sizeof(byte)))); 
      long blen;
      uniChar buff[256];
      uniChar *uData = len<NumberOf(buff)?buff:malloc(sizeof(uniChar)*len);      /* This is the # of bytes in the block */
      integer i;
            
      res = inBytes(in,bData,len,&blen);

      if(len>=2 && bData[0]==uniBOMhi && bData[1]==uniBOMlo){
        integer j;
        for(i=2,j=0;i<len;i+=2,j++)
          uData[j]=((bData[i]&0xff)<<8)|(bData[i+1]&0xff);
        len = j;
      }
      else if(len>=2 && bData[1]==uniBOMhi && bData[0]==uniBOMlo){
        integer j;
        for(i=2,j=0;i<len;i+=2,j++)
          uData[j]=((bData[i+1]&0xff)<<8)|(bData[i]&0xff);
        len=j;
      }
      else{
        for(i=0;i<len;i++)
          uData[i] = bData[i]&0xff;
      }

      if(res==Ok)
        *tgt = allocateString(H,uData,len);
      if(uData!=&buff[0])
        free(uData);
      if(bData!=bBuff)
        free(bData);
      return res;
    }
  }

  case trmCode:{
    integer cLen;

    if((res=decInt(in,S,&cLen,ch))!=Ok)
      return res;
    else{
      byte buffBlock[2048];
      byte *buff = (cLen<NumberOf(buffBlock)?buffBlock:(byte*)malloc(sizeof(byte)*cLen));
      long len;

      res = inBytes(in,buff,cLen,&len);

      if(res==Ok){
        ioPo sub = openInByteStr(buff,cLen,rawEncoding);

	unsigned long signature = (inCh(sub)&0xff)<<24|
	  (inCh(sub)&0xff)<<16|(inCh(sub)&0xff)<<8|(inCh(sub)&0xff);

	if(signature==SIGNATURE || signature==SIGNBSWAP ||
	   signature==SIGNWSWAP || signature==SIGNBWSWP){
	  ptrI Tgt = kvoid;
	  rootPo subRoot = gcAddRoot(S->R,&Tgt);
	  res = decodeCode(sub,S,signature,&Tgt);
	  gcRemoveRoot(S->R,subRoot);
	  *tgt = Tgt;
	}
	else{
	  strMsg(S->errorMsg,S->msgSize,"invalid structure in object stream");
	  res = Error;
	}

        if(lbl>=0)
	  setLabel(S,lbl,*tgt);	/* Update the label table */
        
        closeFile(sub);
      }
      if(buff!=buffBlock)
        free(buff);
      return res;
    }
  }

  case trmClass:{		      /* We have a class definition structure */
    integer arity;

    if((res=decInt(in,S,&arity,ch))!=Ok) /* How many arguments in the class */
      return res;

    if(res==Ok)
      res = inChar(in,&ch);

    if(res==Ok && (ch&TRM_TAG_MASK)!=trmSym)
      res = Error;

    if(res==Ok){
      integer sLen;

      if((res=decInt(in,S,&sLen,ch))==Ok){
	uniChar buff[sLen+1];
	integer i;
	
	for(i=0;res==Ok && i<sLen;i++){
	  uniChar ch;
	  
	  res = inChar(in,&ch);
	  
	  buff[i]=ch;
	};
	
	buff[i]='\0';
	
	*tgt = newClassDef(buff,arity);
      }
    }

    return res;
  }

  case trmProgram:{		      /* We have a program label */
    integer arity;

    if((res=decInt(in,S,&arity,ch))!=Ok) /* How many arguments in the class */
      return res;

    if(res==Ok)
      res = inChar(in,&ch);

    if(res==Ok && (ch&TRM_TAG_MASK)!=trmSym)
      res = Error;

    if(res==Ok){
      integer sLen;

      if((res=decInt(in,S,&sLen,ch))==Ok){
	uniChar buff[sLen+1];
	integer i;
	
	for(i=0;res==Ok && i<sLen;i++){
	  uniChar ch;
	  
	  res = inChar(in,&ch);
	  
	  buff[i]=ch;
	};
	
	buff[i]='\0';
	
	*tgt = newProgramLbl(buff,arity);
      }
    }

    return res;
  }

  case trmObject:{
    ptrI class = kvoid;
    rootPo root = gcAddRoot(S->R,&class);
    integer arity;

    res=decInt(in,S,&arity,ch);		/* pick up the actual number of arg terms */

    uniChar className[MAX_SYMB_LEN];
    if(res==Ok){
      res = decodeName(in,S,className,NumberOf(className));

      if(res==Ok)
	class = newClassDef(className,arity);
    }

    gcAddRoot(S->R,&class);		/* Keep the pointer to the class */

    if(res==Ok){
      objPo obj = allocate(H,arity+1);

      obj->class = class;

      *tgt = objP(obj);

      ptrI el = kvoid;
      integer i;
    
      gcAddRoot(S->R,&el);

      if(lbl>=0)
        setLabel(S,lbl,class);	        /* Update the label table */

      for(i=0;i<arity;i++){
        if((res=decode(in,S,H,-1,&el))!=Ok) /* read each element of term */
	  break;			/* we might need to skip out early */
        else{
	  ptrPo arg = nthArg(obj,i);

	  updateLbls(S,arg,&el);	/* do this carefully */
	  *arg = el;			/* stuff in the new element */
        }
      }
    }

    gcRemoveRoot(S->R,root);		/* clear the GC root */
    return res;
  }

  case trmTag:{				/* A tagged structure */
    assert(lbl==-1);

    if((res=decInt(in,S,&lbl,ch))!=Ok)
      return res;

    if(lbl>=S->maxlbl){
      if(S->lbls==NULL){
	long max = mx(128,lbl);
	S->lbls = (ptrPo)malloc(sizeof(ptrI)*max);
	S->maxlbl = max;
      }
      else{
	long newmax = lbl+(S->maxlbl>>1); /* 50% growth */
	long i;
	ptrPo newlabels = (ptrPo)realloc(S->lbls,sizeof(ptrI)*newmax);

	for(i=S->maxlbl;i<newmax;i++)
	  newlabels[i]=0;		/* clear off new section of the table */

	S->lbls = newlabels;
	S->maxlbl = newmax;
      }
    }

    return decode(in,S,H,lbl,tgt);	/* decode the defined structure */
  }

  case trmRef:{
    long long lbl;

    if((res=decInt(in,S,&lbl,ch))!=Ok)
      return res;

    assert(lbl<S->maxlbl);
    *tgt = S->lbls[lbl];
    return Ok;
  }
  
  default:{
    strMsg(S->errorMsg,S->msgSize,"invalid encoding");
    return Error;
  }
  }
}

static retCode decodeName(ioPo in,encodePo S,uniChar *buffer,long bufferLen)
{
  integer sLen;
  uniChar ch;
  retCode ret = inChar(in,&ch);
    
  if(ret!=Ok || (ret=decInt(in,S,&sLen,ch))!=Ok)
    return ret;
  else if(sLen<bufferLen){
    integer i;
    
    for(i=0;ret==Ok && i<sLen;i++){
      uniChar ch;
      
      ret = inChar(in,&ch);
      
      buffer[i]=ch;
    };
    
    buffer[i]='\0';
    
    return ret;
  }
  else
    return Error;
}

static void updateLbls(encodePo S,ptrPo dst,ptrPo orig)
{
  int i;
  for(i=0;i<S->maxlbl;i++)
    if(S->lbls[i]==(ptrI)orig){
      S->lbls[i]=(ptrI)dst;	/* update the label table */
      *orig = unBind(dst);
      break;
    }
}

static void setLabel(encodePo S,long lbl,ptrI tgt)
{
  if(lbl>=S->maxlbl){
    if(S->lbls==NULL){
      long max = mx(128,lbl);
      S->lbls = (ptrPo)malloc(sizeof(ptrI)*max);
      S->maxlbl = max;
    }
    else{
      long newmax = mx(lbl,S->maxlbl+(S->maxlbl>>1)); /* 50% growth */
      long i;
      ptrPo newlabels = (ptrPo)realloc(S->lbls,sizeof(ptrI)*newmax);

      for(i=S->maxlbl;i<newmax;i++)
	newlabels[i]=0;		/* clear off new section of the table */

      S->lbls = newlabels;
      S->maxlbl = newmax;
    }
  }
  S->lbls[lbl]=tgt;
}


/* swap bytes in the little endian game */
static inline void SwapBytes(unsigned long *x)
{
  *x=((*x&0xff)<<8)|((*x>>8)&0xff)|((*x&0x00ff0000L)<<8)|((*x&0xff000000L)>>8);
}

static inline void SwapWords(unsigned long *x)
{
  *x = (*x&0x0000ffffL)<<16 | (*x&0xffff0000L)>>16;
}

static retCode decodeCode(ioPo in,encodePo S,unsigned long signature,ptrPo tgt)
{
  integer size;
  retCode res;
  integer arity;
  heapPo H = &globalHeap;
  integer litCount;

  if((res=decInt(in,S,&size,inCh(in)))!=Ok)
    return res;

  if((res=decInt(in,S,&arity,inCh(in)))!=Ok)
    return res;

  if((res=decInt(in,S,&litCount,inCh(in)))!=Ok)
    return res;

  ptrI pc=*tgt=permCode(size,litCount);
  long i;
  insPo cd = FirstInstruction(pc);
  ptrI el=kvoid;
  rootPo root = gcAddRoot(S->R,&pc);	/* in case of GC ... */

  gcAddRoot(S->R,&el);			/* we need a temporary pointer */

  /* get the instructions */
  for(i=0;i<size;i++)
    cd[i]=(inCh(in)&0xff)<<24| (inCh(in)&0xff)<<16|
      (inCh(in)&0xff)<<8| (inCh(in)&0xff);

  /* Now convert the main code to handle little endians etc */
  if(signature==SIGNATURE){}	/* endian load same as endian save */
  else if(signature==SIGNBSWAP){ /* swap bytes keep words */
    unsigned long *xx=(unsigned long*)FirstInstruction(pc);
    long cnt = size;
    for(;cnt--;xx++) 
      SwapBytes(xx);
  }
  else if(signature==SIGNWSWAP){ /* swap words keep bytes */
    unsigned long *xx=(unsigned long*)FirstInstruction(pc);
    long cnt = size;
    for(;cnt--;xx++) 
      SwapWords(xx);
  }
  else if(signature==SIGNBWSWP){ /* swap words and bytes */
    unsigned long *xx=(unsigned long*)FirstInstruction(pc);
    long cnt = size;
    for(;cnt--;xx++){
      SwapWords(xx);
      SwapBytes(xx);
    }
  }

  codeV(pc)->arity = arity;           /* set the arity of the program */

  // Pick up the literal table
  for(i=0;i<litCount;i++){
    res=decode(in,S,H,-1,&el);
    updateCodeLit(codeV(pc),i,el);
  }

  gcRemoveRoot(S->R,root);		/* we're all done!! */
  if(res==Ok && enableVerify)		/* Except we have to verify ... */
    res = verifyCode(pc);
  return res;
}

/*
  Estimate amount of heap space needed
*/
static retCode estimateCode(ioPo in,encodePo S,integer *amnt,integer *perm);
static retCode estimate(ioPo in,encodePo S,integer *amnt,integer *perm);

static retCode codedEstimate(ioPo in,encodePo S,integer *amnt,integer *perm)
{
  *amnt = 0;
  *perm = 0;
  
  return estimate(in,S,amnt,perm);
}

static retCode estimate(ioPo in,encodePo S,integer *amnt,integer *perm)
{
  uniChar ch = inCh(in);
  retCode res;

  if(ch==uniEOF)
    return Eof;

  switch(ch & TRM_TAG_MASK){
  case trmVar:{			/* an unbound variable */
    integer vno;

    if((res=decInt(in,S,&vno,ch))!=Ok)
      return res;

    (*amnt)+=VariableCellCount;
    return Ok;
  }
  case trmInt:{
    integer i;
    if((res=decInt(in,S,&i,ch))!=Ok)
      return res;
    (*amnt)+=CellCount(sizeof(integerRec));
    return Ok;
  }
  case trmNegFlt:
  case trmFlt:{
    integer exp;
    int len = ch&TRM_VAL_MASK;
    int i;

    if((res=decInt(in,S,&exp,inCh(in)))!=Ok)
      return res;

    for(i=0;i<len;i++)
      if(inCh(in)==uniEOF)
        return Eof;
        
    (*amnt)+=CellCount(sizeof(floatRec));
    return Ok;
  }

  case trmSym:{
    integer sLen;
    
    if((res=decInt(in,S,&sLen,ch))!=Ok)
      return res;
    else{
      integer i=0;
      uniChar ch;

      for(i=0;res==Ok && i<sLen;i++){
	res = inChar(in,&ch);
      };
      (*perm)+=CellCount(sizeof(symbolRec)+(sLen+1)*sizeof(uniChar));
      return res;
    }
  }

  case trmChar:{
    integer c;
    if((res=decInt(in,S,&c,ch))!=Ok)
      return res;
    else{
      (*perm)+=CellCount(sizeof(charRec));
      return Ok;
    }
  }

  case trmString:{                  /* A literal string */
    integer len;                    /* The length of the utf8 encoded string */
    
    if((res=decInt(in,S,&len,ch))!=Ok)
      return res;
    else{
      integer i;
      
      for(i=0;i<len;i++){
        uniChar ch=inCh(in);            /* We are ignoring the actual char */
        
        if(ch==uniEOF)
          return Eof;
      }

      if(res==Ok)
        (*amnt)+=len*(CellCount(sizeof(charRec))+classArity((clssPo)objV(listClass)));

      return res;
    }
  }

  case trmCode:{
    integer cLen;

    if((res=decInt(in,S,&cLen,ch))!=Ok)
      return res;
    else{
      byte buffBlock[2048];
      byte *buff = (cLen<NumberOf(buffBlock)?buffBlock:(byte*)malloc(sizeof(byte)*cLen));
      long len;

      res = inBytes(in,buff,cLen,&len);

      if(res==Ok){
        ioPo sub = openInByteStr(buff,cLen,rawEncoding);

	unsigned long signature = (inCh(sub)&0xff)<<24|
	  (inCh(sub)&0xff)<<16|(inCh(sub)&0xff)<<8|(inCh(sub)&0xff);

	if(signature==SIGNATURE || signature==SIGNBSWAP ||
	   signature==SIGNWSWAP || signature==SIGNBWSWP)
	  res = estimateCode(sub,S,amnt,perm);
	else
	  res = Error;

        closeFile(sub);
      }
      if(buff!=buffBlock)
        free(buff);
      return res;
    }
  }

  case trmClass:{			/* A constructor structure definition */
    integer arity;

    if((res=decInt(in,S,&arity,ch))!=Ok)
      return res;

    return estimate(in,S,amnt,perm);	/* pick up the constructor name */
  }
      
  case trmProgram:{			/* A program label definition */
    integer arity;

    if((res=decInt(in,S,&arity,ch))!=Ok)
      return res;

    return estimate(in,S,amnt,perm);	/* pick up the program name */
  }
      
  case trmObject:{			/* A constructor object */
    integer arity;

    if((res=decInt(in,S,&arity,ch))!=Ok)
      return res;
      
    res = estimate(in,S,amnt,perm);       /* pick up the class constructor */

    if(res==Ok){
      integer i;
    
      for(i=0;res==Ok&&i<arity;i++)
        res=estimate(in,S,amnt,perm);     /* read each constructor element */
    }

    (*amnt)+=arity+1;
    return res;
  }

  case trmTag:{
    integer lbl;
    if((res=decInt(in,S,&lbl,ch))!=Ok)
      return res;

    return estimate(in,S,amnt,perm);      /* estimate the defined structure */
  }

  case trmRef:{
    integer lbl;

    if((res=decInt(in,S,&lbl,ch))!=Ok)
      return res;

    return Ok;
  }
  
  case trmShort:{                       /* This probably isnt going to happen all that often */
    integer lbl;

    res=decInt(in,S,&lbl,ch);
    
    if(res==Ok)
      res = estimate(in,S,amnt,perm);
      
    if(res==Ok)
      res = estimate(in,S,amnt,perm);
      
    return res;
  }
  
  default:
    return Error;
  }
}

static retCode estimateCode(ioPo in,encodePo S,integer *amnt,integer *perm)
{
  integer size,arity,litCnt;
  retCode res=decInt(in,S,&size,inCh(in));

  if(res==Ok)
    res = decInt(in,S,&arity,inCh(in));

  if(res==Ok)
    res = decInt(in,S,&litCnt,inCh(in));

  if(res!=Ok)
    return res;
  else{
    long i,l;
    byte buff[4];

    /* get the instructions */
    for(i=0;res==Ok && i<size;i++){
      res = inBytes(in,buff,NumberOf(buff),&l);
    }

    /* estimate the tuple of literals */
    for(i=0;res==Ok&&i<litCnt;i++)
      res=estimate(in,S,perm,perm);

    (*perm)+=CodeCellCount(size,litCnt);
    return res;
  }
}

static retCode display(ioPo in,encodePo S,ioPo out);
static retCode displayCode(ioPo in,encodePo S,ioPo out);

retCode displayEncoded(ioPo out,byte *buffer,long len)
{
  ioPo str = openInByteStr(buffer,len,rawEncoding);
  uniChar errorMsg[1024];
  long msgSize = NumberOf(errorMsg);
  EncodeSupport support = { NULL, 0, NULL, 0, errorMsg, msgSize};
  retCode ret = display(str,&support,out);
              
  flushFile(out);

  closeFile(str);
  return ret;
}

retCode sE(byte *buffer,long len)
{
  return displayEncoded(logFile,buffer,len);
}

static retCode display(ioPo in,encodePo S,ioPo out)
{
  uniChar ch = inCh(in);
  retCode res;

  if(ch==uniEOF)
    return Eof;

  switch(ch & TRM_TAG_MASK){
  case trmVar:{				    /* an unbound variable */
    integer vno;

    if((res=decInt(in,S,&vno,ch))!=Ok)
      return res;

    return outMsg(out,"_%ld",vno);
  }
  case trmInt:{
    integer i;
    if((res=decInt(in,S,&i,ch))!=Ok)
      return res;
    return outMsg(out,"%ld",i);
  }
  case trmFlt:{
    integer exp;
    uniChar buff[16];
    double f = 0.0;
    int len = ch&TRM_VAL_MASK;
    int i;

    if((res=decInt(in,S,&exp,inCh(in)))!=Ok)
      return res;

    for(i=0;i<len;i++)
      if((buff[i]=inCh(in))==uniEOF)
        return Eof;

    while(len--)
      f = (f + (buff[len]&0xff))/256;

    f = ldexp(f,exp);

    return outMsg(out,"%f",f);
  }

  case trmNegFlt:{
    integer exp;
    uniChar buff[16];
    double f = 0.0;
    int len = ch&TRM_VAL_MASK;
    int i;

    if((res=decInt(in,S,&exp,inCh(in)))!=Ok)
      return res;

    for(i=0;i<len;i++)
      if((buff[i]=inCh(in))==uniEOF)
        return Eof;

    while(len--)
      f = (f + (buff[len]&0xff))/256;

    f = ldexp(f,exp);
    return outMsg(out,"%f",-f);
  }

  case trmSym:{
    integer sLen;
    
    if((res=decInt(in,S,&sLen,ch))!=Ok)
      return res;
    else{
      uniChar buff[sLen+1];
      integer i;

      for(i=0;i<sLen;i++){
        uniChar ch=inCh(in);
        
        if(ch==uniEOF)
          return Eof;
        else
          buff[i]=ch;
      };

      buff[i]='\0';

      if(buff[0]=='\'')
        return outMsg(out,"%U'",buff);
      else
        return outMsg(out,"%U",buff);
    }
  }

  case trmChar:{
    integer c;
    if((res=decInt(in,S,&c,ch))!=Ok)
      return res;
    else{
      return outMsg(out,"`%c",(uniChar)c);
    }
  }

  case trmString:{                      /* A literal string */
    integer len;                        /* The length of the encoded string */
    
    if((res=decInt(in,S,&len,ch))!=Ok)
      return res;
    else{
      byte bBuffBlock[2048];
      byte *bData = len<NumberOf(bBuffBlock)?bBuffBlock:(byte*)malloc(sizeof(byte)*len);
      uniChar uBuffBlock[256];
      uniChar *uData = len<NumberOf(uBuffBlock)?uBuffBlock:
        (uniChar*)malloc(sizeof(uniChar)*len);      /* This is the # of bytes in the block */
      integer i;
      long slen;
            
      res = inBytes(in,bData,len,&slen);
      
      if(len>=2 && bData[0]==uniBOMhi && bData[1]==uniBOMlo){
        integer j;
        for(i=2,j=0;i<len;i+=2,j++)
          uData[j]=((bData[i]&0xff)<<8)|(bData[i+1]&0xff);
        len = j;
      }
      else if(len>=2 && uData[1]==uniBOMhi && uData[0]==uniBOMlo){
        integer j;
        for(i=2,j=0;i<len;i+=2,j++)
          uData[j]=((bData[i+1]&0xff)<<8)|(bData[i]&0xff);
        len=j;
      }
      else{
        for(i=0;i<len;i++)
          uData[i]=bData[i];
      }

      if(res==Ok){
        res = outStr(out,"\"");

        if(res==Ok)
          res = outText(out,uData,len);

        if(res==Ok)
          res = outStr(out,"\"");
        return res;
      }
      if(uData!=&uBuffBlock[0])
        free(uData);
      if(bData!=&bBuffBlock[0])
        free(bData);
      return res;
    }
  }

  case trmCode:{
    integer cLen;

    if((res=decInt(in,S,&cLen,ch))!=Ok)
      return res;
    else{
      byte buffBlock[2048];
      byte *buff = (cLen<NumberOf(buffBlock)?buffBlock:(byte*)malloc(sizeof(byte)*cLen));
      long len;

      res = inBytes(in,buff,cLen,&len);

      if(res==Ok){
        ioPo sub = openInByteStr(buff,cLen,rawEncoding);

	unsigned long signature = (inCh(sub)&0xff)<<24|
	  (inCh(sub)&0xff)<<16|(inCh(sub)&0xff)<<8|(inCh(sub)&0xff);

	if(signature==SIGNATURE || signature==SIGNBSWAP ||
	   signature==SIGNWSWAP || signature==SIGNBWSWP)
	  res = displayCode(sub,S,out);
	else
          res = outMsg(out,"<opaque %x %ld bytes>",signature,cLen);

        closeFile(sub);
      }
      if(buff!=buffBlock)
        free(buff);
      return res;
    }
  }

  case trmClass:{ 			/* A class literal */
    integer arity;

    if((res=decInt(in,S,&arity,ch))!=Ok)
      return res;

    res = inChar(in,&ch);

    if(res==Ok && (ch&TRM_TAG_MASK)!=trmSym)
      res = Error;

    if(res==Ok){
      integer sLen;

      if((res=decInt(in,S,&sLen,ch))==Ok){
	uniChar buff[sLen+1];
	integer i;
	
	for(i=0;res==Ok && i<sLen;i++){
	  uniChar ch;
	  
	  res = inChar(in,&ch);
	  
	  buff[i]=ch;
	};
	
	buff[i]='\0';
	
        return outMsg(out,"%U/%d",buff,arity);
      }
    }

    return res;
  }

  case trmProgram:{
    integer arity;

    if((res=decInt(in,S,&arity,ch))!=Ok)
      return res;

    res = inChar(in,&ch);

    if(res==Ok && (ch&TRM_TAG_MASK)!=trmSym)
      res = Error;

    if(res==Ok){
      integer sLen;

      if((res=decInt(in,S,&sLen,ch))==Ok){
	uniChar buff[sLen+1];
	integer i;
	
	for(i=0;res==Ok && i<sLen;i++){
	  uniChar ch;
	  
	  res = inChar(in,&ch);
	  
	  buff[i]=ch;
	};
	
	buff[i]='\0';
	
        return outMsg(out,"%U/%d",buff,arity);
      }
    }

    return res;
  }
  
  case trmObject:{			/* An object constructor */
    integer arity;

    res = decInt(in,S,&arity,ch);

    if(res==Ok)
      res = display(in,S,out);

    if(res==Ok)
      res = outMsg(out,"/%d",arity);

    if(arity>0){
      long ix;
      char *sep = "";

      res = outStr(out,"(");

      for(ix=0;res==Ok && ix<arity;ix++){
	res = outStr(out,sep);
	sep = ", ";
	res = display(in,S,out);
      }

      if(res==Ok)
	res = outStr(out,")");
    }
    return res;
  }

  case trmTag:{
    integer lbl;
    if((res=decInt(in,S,&lbl,ch))!=Ok)
      return res;

    res = outMsg(out,"T:%ld",lbl);

    if(res==Ok)
      res = display(in,S,out);       /* display the defined structure */
    return res;
  }

  case trmRef:{
    integer lbl;
    if((res=decInt(in,S,&lbl,ch))!=Ok)
      return res;

    return outMsg(out,"R:%ld",lbl);
  }
  
  case trmShort:{               /* This probably isnt going to happen all that often */
    integer lbl;
    if((res=decInt(in,S,&lbl,ch))!=Ok)
      return res;

    res = outMsg(out,"S:%ld",lbl);

    if(res==Ok)
      res = display(in,S,out);       /* display the defined structure */
    return res;
  }

  default:
    syserr("invalid term");
    return Error;
  }
}

static retCode displayCode(ioPo in,encodePo S,ioPo out)
{
  integer i,litCount,arity,size;
  retCode res;

  if((res=decInt(in,S,&size,inCh(in)))!=Ok)
    return res;

  if((res=decInt(in,S,&arity,inCh(in)))!=Ok)
    return res;

  if((res=decInt(in,S,&litCount,inCh(in)))!=Ok)
    return res;

  res = outMsg(out,"<< arity:%ld %ld ins, ",arity,size);

  /* get the instructions */
  for(i=0;res==Ok && i<size;i++){
    byte ins[4];
    long ilen;
    res = inBytes(in,ins,NumberOf(ins),&ilen);
  }

  /* display the tuple of literals */
  for(i=0;res==Ok&&i<litCount;i++)
    res = display(in,S,out);

  if(res==Ok)
    res = outStr(out,">>");
  return res;
}

static retCode skipTrm(ioPo in,encodePo S)
{
  uniChar ch = inCh(in);
  retCode res = Ok;

  if(ch==uniEOF)
    return Eof;

  switch(ch & TRM_TAG_MASK){
  case trmVar:{				    /* an unbound variable */
    integer vno;

    return decInt(in,S,&vno,ch);
  }
  case trmInt:{
    integer i;
    return decInt(in,S,&i,ch);
  }
  case trmFlt:{
    integer exp;
    int len = ch&TRM_VAL_MASK;
    uniChar ch;
    long i;

    if((res=decInt(in,S,&exp,inCh(in)))!=Ok)
      return res;

    for(i=0;res==Ok && i<len;i++)
      res = inChar(in,&ch);

    return res;
  }

  case trmNegFlt:{
    integer exp;
    int len = ch&TRM_VAL_MASK;
    uniChar ch;
    int i;

    if((res=decInt(in,S,&exp,inCh(in)))!=Ok)
      return res;

    for(i=0;res==Ok && i<len;i++)
      res = inChar(in,&ch);

    return res;
  }

  case trmSym:{
    integer sLen;
    
    if((res=decInt(in,S,&sLen,ch))!=Ok)
      return res;
    else{
      integer i;
      uniChar ch;

      for(i=0;res==Ok && i<sLen;i++)
	res = inChar(in,&ch);

      return res;
    }
  }

  case trmChar:{
    integer c;
    return decInt(in,S,&c,ch);
  }

  case trmString:{                      /* A literal string */
    integer len;                        /* The length of the encoded string */
    
    if((res=decInt(in,S,&len,ch))!=Ok)
      return res;
    else{
      integer i;
      byte dummy;

      for(i=0;res==Ok && i<len;i++)
	res = inByte(in,&dummy);
            
      return res;
    }
  }

  case trmCode:{
    integer cLen;

    if((res=decInt(in,S,&cLen,ch))!=Ok)
      return res;
    else{
      byte dummy;
      long i;

      for(i=0;res==Ok && i<cLen;i++)
	res = inByte(in,&dummy);

      return res;
    }
  }

  case trmClass:{ 			/* A class literal */
    integer arity;

    if((res=decInt(in,S,&arity,ch))!=Ok)
      return res;

    res = inChar(in,&ch);

    if(res==Ok && (ch&TRM_TAG_MASK)!=trmSym)
      res = Error;

    if(res==Ok){
      integer sLen;

      if((res=decInt(in,S,&sLen,ch))==Ok){
	integer i;
	
	for(i=0;res==Ok && i<sLen;i++){
	  uniChar ch;
	  
	  res = inChar(in,&ch);
	};
      }
    }
    return res;
  }

  case trmObject:{			/* An object constructor */
    integer arity;

    res = decInt(in,S,&arity,ch);

    if(res==Ok)
      res = skipTrm(in,S);

    if(arity>0){
      long ix;

      for(ix=0;res==Ok && ix<arity;ix++){
	res = skipTrm(in,S);
      }
    }
    return res;
  }

  case trmTag:{
    integer lbl;
    if((res=decInt(in,S,&lbl,ch))!=Ok)
      return res;

    return skipTrm(in,S);
  }

  case trmRef:{
    integer lbl;

    return decInt(in,S,&lbl,ch);
  }
  
  case trmShort:{
    integer lbl;
    if((res=decInt(in,S,&lbl,ch))!=Ok)
      return res;

    if(res==Ok)
      return skipTrm(in,S);
    return res;
  }
  
  default:
    return Error;
  }
}

retCode skipEncoded(ioPo in,uniChar *errorMsg, long msgLen)
{
  EncodeSupport support = { NULL, 0, NULL, 0, errorMsg, msgLen,NULL};

  return skipTrm(in,&support);
}
