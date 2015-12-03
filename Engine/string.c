/*
  String handling functions for the Go! engine
  (c) 1994-2000 Imperial College & F.G. McCabe

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

  $Id: string.c,v 1.4 2004/07/16 15:30:06 fmccabe Exp $
  $Log: string.c,v $
  Revision 1.4  2004/07/16 15:30:06  fmccabe
  Adjusted the compiler some

  Revision 1.3  2004/06/30 04:28:12  fmccabe
  Some bug fixes, new grammar operator and new term operator

  Revision 1.2  2004/04/29 16:24:28  fmccabe
  Completely new type system


*/

#include "config.h"		/* pick up standard configuration header */
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <time.h>
#include <limits.h>
#include "go.h"
#include "symbols.h"
#include "process.h"
#include "dict.h"
#include "term.h"
#include "fileio.h"
#include "char.h"


ptrI allocateCString(heapPo H,const char *m)
{
  ptrI str = emptyList;
  ptrI el = kvoid;
  long pos;
  long len = strlen(m);
  rootPo root = gcAddRoot(H,&str);

  gcAddRoot(H,&el);

  for(pos=len-1;pos>=0;pos--){    
    el = newChar(m[pos]);
    str = consLsPair(H,el,str);
  }

  gcRemoveRoot(H,root);
  return str;
}

ptrI allocateString(heapPo H,uniChar *m,long len)
{
  ptrI str = emptyList;
  ptrI el = kvoid;
  long pos;
  rootPo root = gcAddRoot(H,&str);

  gcAddRoot(H,&el);

  for(pos=len-1;pos>=0;pos--){    
    el = newChar(m[pos]);
    str = consLsPair(H,el,str);
  }

  gcRemoveRoot(H,root);

  return str;
}

ptrI allocateByteList(heapPo H,byte *m,long len)
{
  ptrI str = emptyList;
  ptrI el = kvoid;
  ptrI pair = kvoid;
  long pos;
  rootPo root = gcAddRoot(H,&str);
    
  gcAddRoot(H,&el);
  gcAddRoot(H,&pair);

  for(pos=len-1;pos>=0;pos--){    
    el = allocateInteger(H,m[pos]);
    str = consLsPair(H,el,str);
  }

  gcRemoveRoot(H,root);
  return str;
}

long StringLen(ptrPo t)
{
  long counter = 0;
  ptrI px = deRefI(t);

  while(IsList(px)){
    counter++;
    px = deRefI(listTail(objV(px)));
  }

  return counter;
}

/* Ok == ground string, Fail = not properly terminated, Error means variable or non-char */
retCode isGroundString(ptrPo s)
{
  ptrI x = deRefI(s);

  while(IsList(x)){
    ptrPo h = listHead(objV(x));

    ptrI hx = deRefI(h);

    switch(ptg(hx)){
    case varTg:
      return Fail;
    case objTg:
      if(!IsChar(hx))
        return Fail;
      x = deRefI(h+1);
      continue;
    default:
      return Error;
    }
  }

  if(identical(x,emptyList))
    return Ok;
  else if(isvar(x))
    return Fail;
  else
    return Error;
}

/* Copy a string into a unicode buffer */

retCode String2Uni(ptrPo s,uniChar *buff,long len)
{
  ptrI x = deRefI(s);
  long pos = 0;

  while(IsList(x)){
    ptrPo h = listHead(objV(x));
    ptrI H = deRefI(h);

    if(isobj(H) && IsChar(H)){
      if(pos<len){
        buff[pos++]=CharVal(charV(H));
        x = deRefI(h+1);
      }
      else
        return Fail;
    }
    else
      return Error;
  }

  if(x==emptyList){
    if(pos<len){
      buff[pos]=0;
      return Ok;
    }
    else
      return Fail;
  }
  else
    return Error;
}


retCode String2Utf8(ptrPo s,char *buff,long len)
{
  ptrI x = deRefI(s);
  long pos = 0;

  while(IsList(x)){
    ptrPo h = listHead(objV(x));
    ptrI H = deRefI(h);

    if(!isvar(H) && IsChar(H)){
      uniChar cc[2] = {CharVal(charV(H)),0};
      byte uc[10];
      long clen = uni_utf8(cc,1,uc,NumberOf(uc));
        
      if(pos+clen<len){
        int i;
          
        for(i=0;i<clen;i++)
          buff[pos++]=uc[i];
      }
      else
        return Fail;
    }
    x = deRefI(h+1);
  }

  if(identical(x,emptyList)){
    if(pos<len){
      buff[pos]=0;
      return Ok;
    }
    else
      return Fail;
  }
  else
    return Error;
}

/* Support for writing string values */
retCode writeString(ioPo f,void *data,long depth,long prec,logical alt) 
{
  ptrPo x = (ptrPo)data;
  
  if(x!=NULL){
    ptrI vx = *(x=deRef(x));
    
    if(isGroundString(&vx)!=Ok)
      return Error;
    else if(prec>=0 || ListLen(vx)<=3){
      retCode r = Ok;
      
      while(r==Ok && IsList(vx)){
	ptrPo ll = listHead(objV(vx));
	uniChar ch = CharVal(charV(deRefI(ll)));

	r = outChar(f,ch);
	      
	vx = deRefI(ll+1);
      }
      return r;
    }
    else{
      retCode r = Ok;
      
      while(r==Ok && prec-->0 && IsList(vx)){
	ptrPo ll = listHead(objV(vx));
	uniChar ch = CharVal(charV(deRefI(ll)));

	r = outChar(f,ch);
	      
	vx = deRefI(ll+1);
      }
      return r;
    }
  }
  else
    return outStr(f,"(NULL)");
}

retCode g_stringOf(processPo P,ptrPo a)
{
  ptrI Data = deRefI(&a[1]);
  ptrI Width = deRefI(&a[2]);
  ptrI Prec = deRefI(&a[3]);
  heapPo H = &P->proc.heap;
  
  if(isvar(Width)||!isInteger(objV(Width)))
    return liberror(P,"__stringOf",eINTNEEDD);
  else if(isvar(Prec) || !isInteger(objV(Prec)))
    return liberror(P,"__stringOf",eINTNEEDD);
  else{
    long width = integerVal(intV(Width));
    long prec = integerVal(intV(Prec));
    uniChar *buffer = (prec<0 ? 
		       (uniChar*)malloc(sizeof(uniChar)*(-prec+1))
		       :NULL);
    ioPo str = (prec<0?
		openBufferStr(buffer,-prec+1,utf16Encoding):
		openOutStr(utf16Encoding));
    retCode ret = outCell(str,&Data,prec==0?INT_MAX/4:abs(prec),0,False);
    
    if(ret!=Ok){
      if(buffer!=NULL)
	free(buffer);
      return liberror(P,"__stringOf",eINVAL);
    }
    else{
      long len;
      uniChar *txt = getStrText(O_STRING(str),&len);
      
      if(len<0)
	return liberror(P,"__stringOf",eINVAL);
      else if(width!=0){
      	long sLen = abs(width)+1;
      	uniChar text[sLen];
      	
      	if(width>0){                    /* right padded */
      	  uniChar *p;
      	  long w = width-len;
      	  
      	  uniNCpy(text,sLen,txt,len);
      	  p = text+len;
      	  while(w-->0)
      	    *p++=' ';                   /* pad out with spaces */
      	  *p=0;
      	}
      	else{
      	  uniChar *p = text;
      	  long w = -width-len;
      	  while(w-->0)
      	    *p++=' ';                   /* left pad with spaces */
      	  if(abs(width)>len)
      	    uniNCpy(p,sLen-(p-text),txt,len);
      	  else
      	    uniNCpy(p,sLen-(p-text),txt+len+width,-width);
      	}
      	
	{
	  ptrI txtList = allocateString(H,text,abs(width));

	  closeFile(str);	/* close the file down */
	  if(buffer!=NULL)
	    free(buffer);
          return funResult(P,a,4,txtList);
        }
      }
      else{
        ptrI txtList = kvoid;
        rootPo root = gcAddRoot(&P->proc.heap,&txtList);
        retCode ret = closeOutString(str,&P->proc.heap,&txtList);
        
        gcRemoveRoot(&P->proc.heap,root);
	if(buffer!=NULL)
	  free(buffer);
        
        if(ret==Ok)
	  return equal(P,&a[4],&txtList);
	else
          return ret;
      }
    }
  }
}

/*
 Trim a string to be a particular width
 negative width = right justified 
*/
retCode g_trim(processPo P,ptrPo a)
{
  ptrI Data = deRefI(&a[1]);
  ptrI Width = deRefI(&a[2]);
  
  if(isvar(Width)||!isInteger(objV(Width)))
    return liberror(P,"__trim",eINTNEEDD);
  else if(isvar(Data))
    return liberror(P,"__trim",eINSUFARG);
  else if(isGroundString(&Data)!=Ok)
    return liberror(P,"__trim",eINVAL);
  else{
    long width = integerVal(intV(Width));
    long len = StringLen(&Data);
    long awidth = abs(width);
    heapPo H = &P->proc.heap;

    if(width==0||awidth==len)
      return equal(P,&a[1],&a[3]);      /* just return the string itself */
    else{
      uniChar buff[MAX_SYMB_LEN];
      uniChar *buffer = (width>NumberOf(buff)?(uniChar*)malloc(sizeof(uniChar)*awidth):buff);

      if(width<0){                      /* right justified */
        int cnt = len-awidth;           /* how much we have to step into the string */
        ptrI l = Data;

        while(cnt-->0 && IsList(l)){
          l = deRefI(listTail(objV(l)));
        }
        cnt = awidth-len;               /* the number of pad characters */
        while(cnt>0)
          buffer[--cnt]=' ';
        cnt = awidth-len;
        if(cnt<0)
          cnt = 0;
        String2Uni(&l,&buffer[cnt],awidth-cnt); /* plop in the string contents */
      }
      else{
        String2Uni(&Data,buffer,awidth); /* we ignore any excess */

        if(len<width){
          int i;
          for(i=len;i<width;i++)
            buffer[i]=' ';
        }
      }

      {
        ptrI txtList = allocateString(H,buffer,awidth);

        if(buffer!=buff)
          free(buffer);

        return funResult(P,a,3,txtList);
      }
    }
  }
}

// Prepare a string to be formatted in a differently sized field

static retCode strPrepare(uniChar *tgt,long tLen,uniChar *src,long sLen,
		   uniChar pad, logical left, long width)
{
  long i,j;

  if(width==0)
    width = sLen;

  if(width>=tLen)
    return Error;			/* requested width too large */

  long gaps = width-sLen;		/* How many gap fillers */

  if(left){				/* left aligned */
    if(gaps<0){				/* We have to trim, lose trailing */
      for(i=0;i<width;i++)
	tgt[i]=src[i];
      tgt[i]='\0';			/* terminate */
    }
    else{
      for(i=0;i<sLen;i++)
	tgt[i]=src[i];
      while(i<width)
	tgt[i++]=pad;
      tgt[i]='\0';
    }
  }
  else{					/* right aligned */
    if(gaps<0){
      for(j=0,i=sLen+gaps;i<sLen;i++,j++) /* lose the left part of the source */
	tgt[j]=src[i];
      tgt[j]='\0';
    }
    else{				/* extra pad on the left */
      for(j=0,i=gaps;i>0;j++,i--)
	tgt[j]=pad;
      for(i=0;i<sLen;j++,i++)
	tgt[j]=src[i];
      tgt[j]='\0';
    }	
  }
  return Ok;
}

retCode g_num2str(processPo P,ptrPo a)
{
  ptrI a1 = deRefI(&a[1]);
  ptrI a2 = deRefI(&a[2]);
  ptrI a3 = deRefI(&a[3]);
  ptrI a4 = deRefI(&a[4]);
  ptrI a5 = deRefI(&a[5]);
  
  if(isvar(a1) || isvar(a2) || isvar(a3) || isvar(a4) || isvar(a5))
    return liberror(P,"num2str",eINSUFARG);
  else{
    integer width = isFloat(objV(a2))?floatVal(floatV(a2)):integerVal(intV(a2));
    integer prec = isFloat(objV(a3))?floatVal(floatV(a3)):integerVal(intV(a3));
    logical left = width>0;
    uniChar buffer[128];
    ioPo out = openBufferStr(buffer,NumberOf(buffer),utf16Encoding);
    retCode res = outDouble(out,NumberVal(objV(a1)),
			    identical(a5,trueClass)?'g':'f',
			    abs(width),prec,' ',left,"",identical(a4,trueClass));
    
    if(res==Ok){
      long len;
      uniChar *text = getStrText(O_STRING(out),&len);
      ptrI rslt = allocateString(&P->proc.heap,text,len);
      closeFile(out);

      return funResult(P,a,6,rslt);
    }
    else
      return liberror(P,"num2str",eIOERROR);
  }
}

retCode g_int2str(processPo P,ptrPo a)
{
  ptrI a1 = deRefI(&a[1]);
  ptrI a2 = deRefI(&a[2]);
  ptrI a3 = deRefI(&a[3]);
  ptrI a4 = deRefI(&a[4]);
  
  if(isvar(a1) || isvar(a2) || isvar(a3) || isvar(a4))
    return liberror(P,"int2str",eINTNEEDD);
  else{
    integer val = isFloat(objV(a1))?floatVal(floatV(a1)):integerVal(intV(a1));
    integer base = isFloat(objV(a2))?floatVal(floatV(a2)):integerVal(intV(a2));
    logical sign = base<0;
    integer width = isFloat(objV(a3))?floatVal(floatV(a3)):integerVal(intV(a3));
    uniChar pad = CharVal(charV(a4));
    logical left = width<0;
    uniChar buffer[128];
    uniChar result[128];

    retCode ret = int2Uni(val,abs(base),sign,buffer,NumberOf(buffer));
    if(ret==Ok)
      ret = strPrepare(result,NumberOf(result),buffer,uniStrLen(buffer),
		       pad,left,abs(width));

    if(ret==Ok){
      ptrI rslt = allocateString(&P->proc.heap,result,uniStrLen(result));

      return funResult(P,a,5,rslt);
    }
    else
      return liberror(P,"int2str",eIOERROR);
  }
}

retCode g_explode(processPo P,ptrPo a)
{
  ptrI Sym = deRefI(&a[1]);
  
  if(isvar(Sym))
    return liberror(P,"explode",eINSUFARG);
  else if(!IsSymb(Sym))
    return liberror(P,"explode",eINVAL);
  else{
    symbPo sym = symbV(Sym);            // We copy the symbol's text out in case of GC during string creation
    long sLen = SymLen(sym);
    uniChar text[sLen+1];
    
    uniCpy(text,sLen+1,SymVal(sym));

    return funResult(P,a,2,allocateString(&P->proc.heap,text,uniStrLen(text)));
  }
}

retCode g_implode(processPo P,ptrPo a)
{
  ptrI Str = deRefI(&a[1]);
  
  if(isvar(Str) || isGroundString(&Str)==Error)
    return liberror(P,"implode",eINSUFARG);
  else if(identical(Str,emptyList))
    return funResult(P,a,2,emptyList);
  else{
    long sLen = StringLen(&Str)+2;
    uniChar text[sLen];
    
    strMsg(text,sLen,"%L",&Str);
    switchProcessState(P,in_exclusion);
    ptrI sym = newUniSymbol(text);
    setProcessRunnable(P);
    return funResult(P,a,2,sym);
  }
}

retCode g_gensym(processPo P,ptrPo a)
{
  ptrI Str = deRefI(&a[1]);
  
  if(isvar(Str) || isGroundString(&Str)==Error)
    return liberror(P,"gensym",eINSUFARG);
  else{
    long sLen = StringLen(&Str)+32;
    uniChar text[sLen];
                     
    strMsg(text,sLen,"%L%ld",&Str,random()); /* gensym generates a randomized symbol  */
    
    
    switchProcessState(P,in_exclusion);
    ptrI nSym = newUniSymbol(text);
    setProcessRunnable(P);
    return equal(P,&a[2],&nSym);
  }
}

retCode closeOutString(ioPo f,heapPo H,ptrPo tgt)
{
  long len;
  uniChar *buff = getStrText(O_STRING(f),&len);
  ptrI str = allocateString(H,buff,len);
  
  *deRef(tgt) = str;
  
  return closeFile(f);
}

// Map a string into a list of UTF8 bytes

ptrI allocateBytes(heapPo H,unsigned char *m,long len)
{
  ptrI str = emptyList;
  ptrI el = kvoid;
  long pos;
  rootPo root = gcAddRoot(H,&str);
    
  gcAddRoot(H,&el);

  for(pos=len-1;pos>=0;pos--){    
    el = allocateInteger(H,m[pos]);
    str = consLsPair(H,el,str);
  }

  gcRemoveRoot(H,root);
  return str;
}

retCode g_str2utf8(processPo P,ptrPo a)
{
  ptrI Str = deRefI(&a[1]);
  
  if(isvar(Str) || isGroundString(&Str)==Error)
    return liberror(P,"__str2utf8",eINSUFARG);
  else{
    long sLen = StringLen(&Str);
    long uLen = 4*sLen;                 /* Worst case scenario */
    static uniChar buffer[MAX_MSG_LEN];
    uniChar *text = buffer;
    static byte ubuffer[MAX_MSG_LEN];
    byte *utext = ubuffer;

    if(sLen>=NumberOf(buffer))
      text = (uniChar*)malloc(sizeof(uniChar)*(sLen+1));
    if(uLen>=NumberOf(ubuffer))
      utext = (byte*)malloc(sizeof(byte)*(uLen+1));
    
    if(String2Uni(&Str,text,sLen+1)==Ok){
      long aLen = uni_utf8(text,sLen,utext,uLen);
      ptrI R = allocateBytes(&P->proc.heap,utext,aLen);

      if(text!=buffer)
	free(text);

      if(utext!=ubuffer)
	free(utext);

      return equal(P,&R,&a[2]);
    }
    else{
      if(text!=buffer)
        free(text);
      if(utext!=ubuffer)
	free(utext);
      
      return liberror(P,"__str2utf8",eINVAL);
    }
  }
}

retCode g_utf82str(processPo P,ptrPo a)
{
  ptrI Str = deRefI(&a[1]);
  
  if(!isGroundTerm(&Str))
    return liberror(P,"__utf82str",eINSUFARG);
  else{
    long sLen = ListLen(Str)+1;
    static uniChar buffer[MAX_MSG_LEN];
    uniChar *text = buffer;
    static byte ubuffer[MAX_MSG_LEN];
    byte *utext = ubuffer;
    long aLen;

    if(sLen>NumberOf(buffer))
      text = (uniChar*)malloc(sizeof(uniChar)*sLen);
    if(sLen>NumberOf(ubuffer))
      utext = (byte*)malloc(sizeof(byte)*sLen);

    {
      ptrI sP = Str;
      byte *uP = utext;

      while(IsList(sP)){
        ptrPo el = listHead(objV(sP));
        ptrI pr = deRefI(el);

        if(IsInt(pr)){
          integer vl = IntVal(pr);

          if(vl>=0 && vl<=255)
            *uP++=(byte)vl;
          else
            return liberror(P,"__utf82str",eINVAL);
        }
        sP = deRefI(el+1);
      }
      aLen = uP-utext;
    }

    sLen = utf8_uni(utext,aLen,text,sLen);

    { 
      ptrI R = allocateString(&P->proc.heap,text,sLen);

      if(text!=buffer)
        free(text);
      if(utext!=ubuffer)
        free(utext);

      return equal(P,&R,&a[2]);
    }
  }
}
