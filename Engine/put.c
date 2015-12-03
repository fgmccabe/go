/* 
  Convert a term into a unit clause
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
*/
#include "config.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>		/* we need variable arguments */
#include "go.h"			/* main header file */
#include "opcodes.h"		/* The definitions of the opcodes */
#include "dict.h"		/* Dictionary handling stuff */
#include "process.h"		/* Process handling */
#include "symbols.h"
#include "pool.h"		/* the memory pool functions */

typedef struct _instruction_ *iPo;
typedef struct _literal_ *lPo;
typedef struct _variable_ *vPo;
typedef struct _typevar_ *tvPo;

typedef struct _instruction_ {
  insWord code;		        /* The instruction bitmap itself */
  opAndSpec pA;
  iPo add;			/* Alternate address target */
  iPo next;			/* next instruction */
} InsRec;

typedef struct _variable_ {
  ptrI var;
  int reg;
  int count;				/* How many references */
  vPo next;				/* next variable in the chain */
} VarRec;

typedef struct _literal_ {
  ptrI lit;
  int off;				/* which literal is it? */
  lPo next;				/* next literal in the chain */
} LitRec;

typedef struct {
  int minAreg;			  /* all A registers before this are reserved */
  logical yMap[256];                    /* map of the Y registers in use */
  logical aMap[GO_REGS];                /* map of the A registers in use */
} map;

typedef struct {
  heapPo heap;				/* What heap are we using? */
  int minYreg;				/* How many locals in use */
  vPo topVar;				/* The variables that we have */
  lPo topLit;				/* Stack of literals in the program */
  int litNo;				/* How many literals so far */
} CodeGenRec, *genPo;

static poolPo iPool = NULL;
static poolPo vPool = NULL;
static poolPo lPool = NULL;
static pthread_once_t once = PTHREAD_ONCE_INIT;

static iPo appendIns(iPo a,iPo b);

static void initCodeGen(void)
{
  if(iPool==NULL){
    iPool = newPool(sizeof(InsRec),128);
    vPool = newPool(sizeof(VarRec),128);
    lPool = newPool(sizeof(LitRec),128);
  }
}

/* Estimate how much heap will be required for a term */
static integer estimateHeap(ptrPo p,logical top)
{
  if(isGroundTerm(p))
    return 0;
  else{
    ptrI xx = deRefI(p);

    switch(ptg(xx)){
    case varTg:
      if(top)
	return VariableCellCount;
      else
	return 0;			/* in-place variable */
    case objTg:{
      objPo o = objV(xx);

      long size = objectSize(o);
      long arity = objectArity(o);
      long ix;
      ptrPo a = objectArgs(o);

      for(ix=0;ix<arity;ix++,a++)
	size+=estimateHeap(a,False);

      return size;
    }
    default:
      syserr("unexpected garbage");
      return -1;
    }
  }
}

static iPo putIns(genPo S,opCode op,...);

static map pickReg(map aRegs, int *pick)
{
  unsigned int i;

  for(i=2;i<NumberOf(aRegs.aMap);i++){  // Registers 0,1 are permanent
    if(aRegs.aMap[i]){
      *pick = i;
      aRegs.aMap[i]=False;
      return aRegs;
    }
  }

  for(i=0;i<NumberOf(aRegs.yMap);i++){
    if(aRegs.yMap[i]){
      *pick = -(i+1);
      aRegs.yMap[i]=False;
      return aRegs;
    }
  }
  
  syserr("too complicated");
  return aRegs;
}

static map addReg(map regs, int reg)
{
  if(reg<0){
    assert(-reg<(signed)NumberOf(regs.yMap));
    regs.yMap[-(reg-1)]=True;
  }
  else if(reg>regs.minAreg){
    assert(reg<(signed)NumberOf(regs.aMap));
    regs.aMap[reg]=True;
  }
  return regs;
};

static iPo gcMap(genPo S,iPo ins,map regs)
{
  /* We need to decide what kind of gcmap code to generate */
  int i;
  int yCount=0;
  iPo mapIns = NULL;
  int aCount = 0;

  for(i=NumberOf(regs.yMap)-1;yCount==0 && i>=0;i--){
    if(!regs.yMap[i])
      yCount = i;
  }

  for(i=0;i<yCount;i++){
    if(!regs.yMap[i])
      mapIns = appendIns(mapIns,putIns(S,clY,i+1));
  }

  for(i=NumberOf(regs.aMap)-1;aCount==0 && i>0;i--){ /* find the last used register */
    if(!regs.aMap[i])
      aCount = i;
  }

  for(i=regs.minAreg;i<aCount;i++){
    if(!regs.aMap[i])
      mapIns = appendIns(mapIns,putIns(S,vdA,i));
  }

  mapIns = appendIns(mapIns,ins);
  mapIns = appendIns(mapIns,putIns(S,gcmap,aCount,yCount));
  return mapIns;
}

#ifdef EXECTRACE
static void showMap(map regs)
{
  unsigned int i;
  char *sep = "[";

  for(i=0;i<NumberOf(regs.aMap);i++)
    if(regs.aMap[i]){
      unsigned int j = i;
      outMsg(logFile,"%s%d",sep,i);

      while(j<NumberOf(regs.aMap) && regs.aMap[j])
	j++;
      if(j!=i){
	outMsg(logFile,"-%d",j-1);
	i = j-1;
      }
      sep = ",";
    }

  outMsg(logFile,"]");
  sep = "/[";
  for(i=0;i<NumberOf(regs.yMap);i++)
    if(regs.yMap[i]){
      unsigned int j = i;
      outMsg(logFile,"%s%d",sep,i+1);
      while(j<NumberOf(regs.yMap) && regs.yMap[j])
	j++;
      if(j!=i){
	outMsg(logFile,"-%d",j);
	i = j-1;
      }
      sep = ",";
    }
  if(*sep!='/')
    outMsg(logFile,"]");
}
#endif

static int newRegOperand(genPo S,map aRegs,map *nRegs)
{
  int reg = 0;

  *nRegs = pickReg(aRegs,&reg);

  if(reg<S->minYreg)
    S->minYreg=reg;

  return reg;
}

static map iniMap(unsigned int iniReg,unsigned int resReg)
{
  map init;
  unsigned int i;
  for(i=0;i<NumberOf(init.yMap);i++)
    init.yMap[i]=True;
  for(i=0;i<NumberOf(init.aMap);i++)
    init.aMap[i]=True;
  for(i=1;i<=iniReg;i++)
    init.aMap[i]=False;
  init.minAreg = resReg;
  
  return init;
}

static int findReg(genPo S,ptrI vr,logical *first,long *count,map aRegs,map *nRegs)
{
  vPo v = S->topVar;

  while(v!=NULL){
    if(v->var==vr){
      if(v->reg==0){
	v->reg = newRegOperand(S,aRegs,nRegs);
	*first = True;
      }
      else
	*first = False;

      *count = v->count;
      return v->reg;
    }
    v = v->next;
  }

  assert(False);			/* Should not get here */
  return 0;
}

static void findVar(genPo S,ptrI vr)
{
  vPo v = S->topVar;

  while(v!=NULL){
    if(v->var==vr){
      v->count++;
      return;
    }
    v = v->next;
  }

  v = (vPo)allocPool(vPool);

  v->var = vr;
  v->reg = 0;
  v->next = S->topVar;
  v->count = 1;
  S->topVar = v;
}

static int findLit(genPo S,ptrI lt)
{
  lPo v = S->topLit;

  while(v!=NULL){
    if(identical(v->lit,lt))
      return v->off;

    v = v->next;
  }

  v = (lPo)allocPool(lPool);

  v->lit = lt;                          /* this will be frozen in the cleanup phase */
  v->next = S->topLit;
  v->off = S->litNo++;
  S->topLit = v;
  gcAddRoot(S->heap,&v->lit);	/* make sure we're GC safe */

  return v->off;
}

#ifdef EXECTRACE

static ptrPo whichLit(genPo S,int i)
{
  lPo lits = S->topLit;

  assert(i<=S->litNo);

  while(lits!=NULL){
    if(lits->off==i)
      return &lits->lit;
    lits=lits->next;
  }
  return 0;
}
   
static void showLits(genPo S)
{
  lPo lits = S->topLit;

  outMsg(logFile,"Literals:\n");

  while(lits!=NULL){
    outMsg(logFile,"%d: %w\n",lits->off,&lits->lit);
    lits=lits->next;
  }
}

#endif /* EXECTRACE */

static void cleanLits(genPo S)
{
  lPo lits = S->topLit;

  while(lits!=NULL){
    lPo next = lits->next;
    freePool(lPool,lits);
    lits = next;
  }
}

static logical isComplexTerm(ptrPo p)
{
  if(isGroundTerm(p))
    return False;
  else{
    ptrI xx = deRefI(p);

    switch(ptg(xx)){
    case varTg:
      return False;
    case objTg:{
      objPo t = objV(xx);
      long arity = objectArity(t);
      if(arity>0)
	return True;
      else
	return False;
    }
    default:
      return True;
    }
  }
}

static iPo appendIns(iPo a,iPo b)
{
  if(a==NULL)
    return b;
  else{
    iPo x = a;

    while(a->next!=NULL)
      a = a->next;

    a->next = b;
    return x;
  }
}

static long codeCount(iPo i)
{
  long count = 0;

  while(i!=NULL){
    count++;
    i = i->next;
  }

  return count;
}

static void locateVars(genPo S,ptrPo p)
{
  ptrI xx = deRefI(p);

  switch(ptg(xx)){
  case varTg:{
    findVar(S,xx);
    return;
  }
  case objTg:{
    objPo t = objV(xx);
    if(isObjct(t)){
      ptrPo a = objectArgs(t);
      long arity = objectArity(t);
      long ix;

      for(ix=0;ix<arity;ix++,a++)
	locateVars(S,a);
    }
    return;
  }
  default:
    syserr("problem in put");
  }
}

static iPo uniSequence(genPo S,ptrPo p,int count,iPo *post,map regs,map *nregs);

/* Compile unification against a term */
static iPo uniTerm(genPo S,ptrPo p,int tgt,iPo *post,map regs,map *nregs)
{
  *nregs = regs;

  if(isGroundTerm(p)){
    if(tgt!=0)
      regs = addReg(regs,tgt);

    if(tgt==0)
      return putIns(S,uSlit,p);
    else if(tgt>0)
      return putIns(S,uAlit,tgt,p);
    else
      return appendIns(putIns(S,mAY,0,tgt),putIns(S,uAlit,0,p));
  }
  else{
    ptrI xx = deRefI(p);

    switch(ptg(xx)){
    case varTg:{
      logical First=False;
      long vCount=0;
      int var = findReg(S,xx,&First,&vCount,regs,nregs);

      if(First){
	if(tgt==0){
	  if(vCount==1)
	    return putIns(S,clS);	/* We ignore the variable */
	  else if(var>0)
	    return putIns(S,mAS,var);
	  else
	    return putIns(S,mYS,var);
	}
	else if(tgt>0){
	  if(vCount==1)
	    return putIns(S,clA,tgt);
	  else if(var>0)
	    return putIns(S,clAA,var,tgt);
	  else
	    return putIns(S,clAY,tgt,var);
	}
	else{
	  if(vCount==1)
	    return putIns(S,clY,tgt);
	  else if(var>0)
	    return putIns(S,clAY,var,tgt);
	  else
	    return appendIns(putIns(S,clAY,0,tgt),putIns(S,mYA,var,0));
	}
      }
      else if(tgt==0){
	if(var>0)
	  return putIns(S,uAS,var);
	else
	  return putIns(S,uYS,var);
      }
      else if(tgt>0){
	if(var>0)
	  return putIns(S,uAA,tgt,var);
	else
	  return putIns(S,uAY,tgt,var);
      }
      else if(var>0)
	return putIns(S,uAY,var,tgt);
      else
	return appendIns(putIns(S,mAY,0,var),putIns(S,uAY,0,tgt));
    }
    case objTg:{
      objPo t  = objV(xx);
      ptrPo class = &t->class;

      iPo ins = tgt==0?putIns(S,uScns,class):
	tgt>0?putIns(S,uAcns,tgt,class):
	appendIns(putIns(S,mAY,0,tgt),putIns(S,uAcns,class));

      long arity = objectArity(t);

      if(arity>0){
	iPo argCode = uniSequence(S,objectArgs(t),arity,post,regs,nregs);
        
	return appendIns(ins,argCode);
      }
      else
	return ins;
    }
    default:
      syserr("problem in put");
      return NULL;
    }
  }
}

static iPo uniSequence(genPo S,ptrPo p,int count,
		       iPo *post,map regs,map *nregs)
{
  if(count>1){
    if(isComplexTerm(p)){
      int a = newRegOperand(S,regs,&regs);

      iPo ins = appendIns(putIns(S,a>0?mAS:mYS,a),
			  uniSequence(S,p+1,count-1,post,regs,&regs));

      iPo eCode = uniTerm(S,p,a,post,regs,&regs);

      *nregs = addReg(regs,a);              // Free the target register

      return appendIns(ins,eCode);
    }
    else{
      iPo elIns = uniTerm(S,p,0,post,regs,&regs);
      
      return appendIns(elIns,uniSequence(S,p+1,count-1,post,regs,nregs));
    }
  }
  else if(count==1)
    return uniTerm(S,p,0,post,regs,nregs);
  else
    return NULL;
}


static iPo bldSequence(genPo S,ptrPo p,int count,iPo *pre,map regs,map *nregs);

/*
 * Compile the construction of a term
 */

static iPo bldTerm(genPo S,ptrPo p,int tgt,iPo *pre,map regs,map *nregs)
{
  *nregs = regs;

  if(isGroundTerm(p)){
    if(tgt!=0)
      regs = addReg(regs,tgt);

    if(tgt==0)
      return putIns(S,mSlit,p);
    else if(tgt>0)
      return putIns(S,mAlit,tgt,p);
    else
      return appendIns(putIns(S,mAlit,0,p),putIns(S,mYA,tgt,0));
  }
  else{
    ptrI xx = deRefI(p);

    switch(ptg(xx)){
    case varTg:{
      logical First=False;
      long vCount=0;
      int var = findReg(S,xx,&First,&vCount,regs,nregs);

      if(First){
	if(tgt==0){
	  if(vCount==1)
	    return putIns(S,clS);	/* A single occurrence */
	  else if(var>0)
	    return putIns(S,clSA,var);
	  else
	    return putIns(S,clSY,var);
	}
	else if(tgt>0){
	  if(vCount==1)
	    return putIns(S,clA,tgt);
	  else if(var>0)
	    return putIns(S,clAA,tgt,var);
	  else
	    return putIns(S,clAY,tgt,var);
	}
	else{
	  if(vCount==1)
	    return putIns(S,clY,tgt);
	  else if(var>0)
	    return putIns(S,clAY,var,tgt);
	  else
	    return appendIns(putIns(S,clAY,0,tgt),putIns(S,mYA,var,0));
	}
      }
      else if(tgt==0){
	if(var>0)
	  return putIns(S,mSA,var);
	else
	  return putIns(S,mSY,var);
      }
      else if(tgt>0){
	if(var>0)
	  return putIns(S,mAA,tgt,var);
	else
	  return putIns(S,mAY,tgt,var);
      }
      else if(var>0)
	return putIns(S,mYA,tgt,var);
      else
	return appendIns(putIns(S,mAY,0,var),putIns(S,mYA,tgt,0));
    }
    case objTg:{
      objPo t  = objV(xx);
      ptrPo class = &t->class;

      iPo ins = tgt==0?putIns(S,mScns,class):
	tgt>0?putIns(S,mAcns,tgt,class):
	appendIns(putIns(S,mAcns,class),putIns(S,mAY,tgt,0));

      long arity = objectArity(t);

      if(arity>0){
	iPo argCode = bldSequence(S,objectArgs(t),arity,pre,regs,nregs);
	
	if(tgt==0){
	  *pre = appendIns(*pre,argCode);
	  return ins;
	}
	else
	  return appendIns(argCode,ins);
      }
      else
	return ins;
    }
    default:
      syserr("problem in put");
      return NULL;
    }
  }
}

static iPo bldSequence(genPo S,ptrPo p,int count,iPo *pre,map regs,map *nregs)
{
  if(count>1){

    if(isComplexTerm(p)){
      iPo remIns = bldSequence(S,p+1,count-1,pre,regs,&regs);
      int a = newRegOperand(S,regs,&regs);
      
      iPo eCode = bldTerm(S,p,a,pre,regs,nregs);
      
      iPo ins = appendIns(putIns(S,a>0?mSA:mSY,a),remIns);

      //*nregs = addReg(regs,a);              // Free the target register
      *pre = appendIns(eCode,*pre);

      return ins;
    }
    else{
      iPo remIns = bldSequence(S,p+1,count-1,pre,regs,&regs);
      iPo elIns = bldTerm(S,p,0,pre,regs,nregs);
      
      return appendIns(elIns,remIns);
    }
  }
  else if(count==1)
    return bldTerm(S,p,0,pre,regs,nregs);
  else
    return NULL;
}

static void genOpAnd(genPo S,iPo ins,opAndSpec A,va_list *args)
{
  switch(A){
  case nOp:                             // No operand
    return;
  case iAh:                             // input argument register in upper slot (0..255)
  case oAh:                            // theta variable in argument register in upper slot (0..255)
  case iLh:				/* input local var, in upper slot */
  case oLh:{				/* output local var, in upper slot */
    int reg = (int)va_arg(*args,int) ;

    ins->code |= (reg&0xff)<<24;
    return;
  }
  case iAm:                             // input argument register in middle slot (0..255)
  case oAm:                             // theta variable in argument register in middle slot (0..255)
  case iLm:				/* input local var, in middle slot */
  case oLm:{				/* output local var, in middle slot */
    int reg = (int)va_arg(*args,int) ;

    ins->code |= (reg&0xff)<<16;
    return;
  }
  case iAl:                             // input argument register in lower slot (0..255)
  case oAl:                             // output argument register in lower slot (0..255)
  case iLl:				/* input local var, in lower slot */
  case oLl:{				/* output local var, in lower slot */
    int reg = (int)va_arg(*args,int) ;

    ins->code |= (reg&0xff)<<8;
    return;
  }
  case iLc:                            // input local variable offset (0..65535)
  case oLc:{                         // output local variable offset  (0..65535)
    int reg = -(int)va_arg(*args,int) ;

    assert(reg>0);

    ins->code |= (reg&0xffff)<<8;
    return;
  }
  case iSt:                             // input at current structure pointer
  case oSt:                             // output to current structure pointer
    return;
  case uAr:                             // Arity in upper slot
  case uLt:{                             // small literal in upper slot (-128..127)
    int reg = (int)va_arg(*args,int) ;

    ins->code |= (reg&0xff)<<24;
    return;
  }
  case Lt:                              // 16bit literal (-32768..32767)
  case vSz:                             // Size of local variable vector
  case lSz:                             // Size of local variable vector
  case cSz:              		// Structure size
  case Es:{                             // escape code (0..65535)
    int reg = (int)va_arg(*args,int) ;

    ins->code |= (reg&0xffff)<<8;
    return;
  }
  case pcr:                             // program counter relative offset (-32768..32767)
  case pcl:                             // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
    ins->add = (iPo)va_arg(*args,iPo);
    ins->pA = A;                        // record the operand type
    return;
  case ltl:{                            // literal number (0..65535)
    int lit = findLit(S,deRefI((ptrPo)va_arg(*args,ptrPo)));

    ins->code |= (lit&0xffff)<<8;

    return ;
  }
  default:
      syserr("operand mode not handled");
  }
}

#define instruction(Op,Cd,A1,A2,Cmt) \
  case Op:{                      /* Cmt */\
    ins->code = Cd;\
    ins->pA = nOp;\
    genOpAnd(S,ins,A1,&args);			\
    genOpAnd(S,ins,A2,&args);			\
    break;\
  }


static iPo putIns(genPo S,opCode mnem,...)
{
  iPo ins = (iPo)allocPool(iPool);
  va_list args;			/* access the generic arguments */

  va_start(args,mnem);

  ins->add = NULL;
  ins->next = NULL;

  switch(mnem){
#include "instructions.h"
  default:
    syserr("problem in putIns");
  }

  va_end(args);
  return ins;
}

static void stitchAdds(iPo code)
{
  iPo cde = code;
  
  while(cde!=NULL){
    if(cde->add!=NULL){
      iPo x = code;
      long pc = 0;
      
      while(x!=NULL && x!=cde->add){
        x=x->next;
        pc++;
      };
      assert(x!=NULL);
      
      if(cde->pA==pcr)
        cde->code|=(pc&0xffff)<<8;
      else
        cde->code|=pc<<8;
    }
    cde = cde->next;
  }
}
      
static void cleanIns(iPo ins)
{
  while(ins!=NULL){
    iPo next = ins->next;
    freePool(iPool,ins);
    ins = next;
  }
}

#ifdef EXECTRACE
static char *dumpOpand(genPo S,ioPo out,iPo ins,char *sep,opAndSpec A)
{
  switch(A){
  case nOp:                             // No operand
    return sep;
  case iAh:                             // input argument register in upper slot (0..255)
    outMsg(out,"%sA[%d]",sep,op_h_val(ins->code));
    return ",";
  case oAh:                             // output argument register in upper slot (0..255)
    outMsg(out,"%s*A[%d]",sep,op_h_val(ins->code));
    return ",";
  case iAm:                             // input argument register in middle slot (0..255)
    outMsg(out,"%sA[%d]",sep,op_m_val(ins->code));
    return ",";
  case oAm:                             // output argument register in middle slot (0..255)
    outMsg(out,"%s*A[%d]",sep,op_m_val(ins->code));
    return ",";
  case iAl:                             // input argument register in lower slot (0..255)
    outMsg(out,"%sA[%d]",sep,op_l_val(ins->code));
    return ",";
  case oAl:                             // output argument register in lower slot (0..255)
    outMsg(out,"%s*A[%d]",sep,op_l_val(ins->code));
    return ",";
  case iLh:                             // input local variable offset (0..255)
  case oLh:                             // output local variable offset (0..255)
    outMsg(out,"%sY[%d]",sep,op_h_val(ins->code));
    return ",";
  case iLm:                             // input local variable offset (0..255)
  case oLm:                             // output local variable offset (0..255)
    outMsg(out,"%sY[%d]",sep,op_m_val(ins->code));
    return ",";
  case iLl:                             // input local variable offset (0..255)
  case oLl:                             // output local variable offset (0..255)
    outMsg(out,"%sY[%d]",sep,op_l_val(ins->code));
    return ",";
  case iLc:                             // input local variable offset (0..65535)
    outMsg(out,"%sY[%d]",sep,op_o_val(ins->code));
    return ",";
  case oLc:                             // output local variable offset  (0..65535)
    outMsg(out,"%s*[%d]",sep,op_o_val(ins->code));
    return ",";
  case iSt:                             // input at current structure pointer
    outMsg(out,"%sS++",sep);
    return ",";
  case oSt:                             // output to current structure pointer
    outMsg(out,"%s*S++",sep);
    return ",";
  case uAr:                             // Arity in upper slot
    outMsg(out,"%s#%d",sep,op_h_val(ins->code));
    return ",";
  case uLt:                             // small literal in upper slot (-128..127)
    outMsg(out,"%s%d",sep,op_h_val(ins->code));
    return ",";
  case Lt:                              // 16bit literal (-32768..32767)
    outMsg(out,"%s#%d",sep,op_so_val(ins->code));
    return ",";
  case vSz:                             // Size of local variable vector
  case lSz:                             // Size of local variable vector
  case cSz:             		// Structure size
    outMsg(out,"%s#%d",sep,op_o_val(ins->code));
    return ",";
  case Es:                              // escape code (0..65535)
    outMsg(out,"%s%s",sep,escapeName(op_o_val(ins->code)));
    return ",";
  case pcr:                             // program counter relative offset (-32768..32767)
  case pcl:                             // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
    outMsg(out,"%s0x%x%d",sep,ins->add);
    return ",";
  case ltl:                             // literal number (0..65535)
    outMsg(out,"%s%w",sep,whichLit(S,op_o_val(ins->code)));
    return ",";
  default:
    syserr("Problem in generating showing type");
    return sep;
  }
}

#undef instruction

#define instruction(Op,Cd,A1,A2,Cmt) \
  case Op:{                     /* Cmt */\
    char *sep = "";\
    outMsg(logFile,"0x%x: %s ",pc,#Op);\
    sep = dumpOpand(S,logFile,ins,sep,A1);	\
    sep = dumpOpand(S,logFile,ins,sep,A2);	\
    outMsg(logFile,"\n");\
    break;\
  }

static void dumpIns(genPo S,iPo ins)
{
  long pc = 0;

  while(ins!=NULL){
    switch(op_cde(ins->code)){
#include "instructions.h"
    default:
      syserr("unknown instruction");
    }
    ins = ins->next;
    pc++;
  }
  
  flushFile(logFile);
}
#endif

/* Construct a executable copy of a term
   It assigns a zero-argument function to a symbol that, when evaluated, generates a copy 
   of the term
*/
retCode g_term(processPo P,ptrPo a)
{
  ptrPo t1 = deRef(&a[1]);		/* term itself */
  CodeGenRec G = {&P->proc.heap,0,NULL,NULL,0};
  genPo S = &G;
  ptrI tn = kvoid;
  heapPo H = &P->proc.heap;
  rootPo root = gcAddRoot(H,&tn);

  if(!isvar(deRefI(&a[2])))
    return liberror(P,"__term",eVARNEEDD);

  switchProcessState(P,in_exclusion);

  pthread_once(&once,initCodeGen);

  uniChar name[MAX_SYMB_LEN];

  /* generate a randomized symbol  */
  strMsg(name,NumberOf(name),"@%ld",random()); 

  tn = newProgramLbl(name,1);

  long hsize = estimateHeap(t1,True);
  map regs = iniMap(1,1);
  iPo post = NULL;

  locateVars(S,t1);

  iPo ins = gcMap(S,putIns(S,gc,1,hsize),regs);
  
  ins = appendIns(ins,uniTerm(S,t1,1,&post,regs,&regs));
		      
  if(post!=NULL)
    ins = appendIns(ins,post);
    
  if(S->minYreg<0)
    ins = appendIns(appendIns(putIns(S,alloc,1,-S->minYreg),
			      putIns(S,gcmap,1,0)),
		    appendIns(ins,putIns(S,dealloc)));

  ins = appendIns(ins,putIns(S,succ)); /* terminating succ */

#ifdef EXECTRACE
  if(tracePut){
    outMsg(logFile,"Generated code for %w (%w) is:\n",t1,&tn);
    dumpIns(S,ins);
    showLits(S);
    showMap(regs);
    outMsg(logFile,"\n");
    flushFile(logFile);
  }
#endif

  long litCount = S->litNo;
  lPo lt;
  long size = codeCount(ins); /* how many instructions do we have? */
  ptrI code = permCode(size,litCount);
  ptrI xx = kvoid;
  retCode ret = Ok;
  uniChar eMsg[MAX_SYMB_LEN];
  
  gcAddRoot(H,&xx);
  gcAddRoot(H,&code);

  for(lt=S->topLit;ret==Ok && lt!=NULL;lt=lt->next){
    if((ret=freezeTerm(H,&xx,lt->lit,eMsg,NumberOf(eMsg)))!=Ok){
      gcRemoveRoot(H,root);
      setProcessRunnable(P);
      
      if(ret==Error)
	return liberror(P,"__term",eINSUFARG);
      else
	return liberror(P,"__term",eSPACE);
    }
    updateCodeLit(codeV(code),lt->off,xx);
  }

  gcAddRoot(H,&code);

  insPo pc = FirstInstruction(code);	/* program counter */
  iPo apc=ins;				/* abstract program counter */
	
  stitchAdds(apc);		       /* make sure that code pointers are OK */

  while(apc!=NULL){		 /* copy the generated code into the code seg */
    *pc++=apc->code;
    apc=apc->next;
  }
  

  defineProg(tn,code);			/* store the code in the symbol */
  
  cleanLits(S);				/* clean the pool of literals */
  cleanIns(ins);

  setProcessRunnable(P);
  gcRemoveRoot(H,root);

  return equal(P,&tn,&a[2]);		/* return the constructed symbol */
}

// This forcibly clears a program associated with a symbolic name
retCode g_remove(processPo P,ptrPo a)
{
  ptrI pr = deRefI(&a[1]);		/* term itself */

  if(isvar(pr))
    return liberror(P,"__remove",eINSUFARG);
  else if(!IsProgLbl(pr))
    return liberror(P,"__remove",eINVAL);
  else{
    defineProg(pr,kvoid);		/* clear the code portion */
    return Ok;
  }
}

/* 
 * Assert a term as the code associated with a symbol
 */
retCode g_assert(processPo P,ptrPo a)
{
  ptrI  pr  = deRefI(&a[1]);		/* The name of the program */
  ptrPo t1 = deRef(&a[2]);		/* term itself */
  CodeGenRec G = {&P->proc.heap,0,NULL,NULL,0};
  genPo S = &G;
  ptrI tn = kvoid;
  heapPo H = &P->proc.heap;
  rootPo root = gcAddRoot(H,&tn);

  if(isvar(pr))
    return liberror(P,"__assert",eINSUFARG);
  else if(!IsSymb(pr))
    return liberror(P,"__assert",eINVAL);

  gcAddRoot(H,&pr);

  switchProcessState(P,in_exclusion);

  pthread_once(&once,initCodeGen);

  tn = newProgramLbl(SymVal(symbV(pr)),1);

  long hsize = estimateHeap(t1,True);
  map regs = iniMap(1,1);
  iPo post = NULL;

  locateVars(S,t1);

  iPo ins = gcMap(S,putIns(S,gc,1,hsize),regs);
  
  ins = appendIns(ins,uniTerm(S,t1,1,&post,regs,&regs));
		      
  if(post!=NULL)
    ins = appendIns(ins,post);
    
  if(S->minYreg<0)
    ins = appendIns(appendIns(putIns(S,alloc,1,-S->minYreg),
			      putIns(S,gcmap,1,0)),
		    appendIns(ins,putIns(S,dealloc)));

  ins = appendIns(ins,putIns(S,succ)); /* terminating succ */

#ifdef EXECTRACE
  if(tracePut){
    outMsg(logFile,"Generated code for %w (%w) is:\n",t1,&tn);
    dumpIns(S,ins);
    showLits(S);
    showMap(regs);
    outMsg(logFile,"\n");
    flushFile(logFile);
  }
#endif

  long litCount = S->litNo;
  lPo lt;
  long size = codeCount(ins); /* how many instructions do we have? */
  ptrI code = permCode(size,litCount);
  ptrI xx = kvoid;
  retCode ret = Ok;
  uniChar eMsg[MAX_SYMB_LEN];
  
  gcAddRoot(H,&xx);
  gcAddRoot(H,&code);

  for(lt=S->topLit;ret==Ok && lt!=NULL;lt=lt->next){
    if((ret=freezeTerm(H,&xx,lt->lit,eMsg,NumberOf(eMsg)))!=Ok){
      gcRemoveRoot(H,root);
      setProcessRunnable(P);
      
      if(ret==Error)
	return liberror(P,"__term",eINSUFARG);
      else
	return liberror(P,"__term",eSPACE);
    }
    updateCodeLit(codeV(code),lt->off,xx);
  }

  gcAddRoot(H,&code);

  insPo pc = FirstInstruction(code);	/* program counter */
  iPo apc=ins;				/* abstract program counter */
	
  stitchAdds(apc);		       /* make sure that code pointers are OK */

  while(apc!=NULL){		 /* copy the generated code into the code seg */
    *pc++=apc->code;
    apc=apc->next;
  }
  
  defineProg(tn,code);			/* store the code */
  
  cleanLits(S);				/* clean the pool of literals */
  cleanIns(ins);

  setProcessRunnable(P);
  gcRemoveRoot(H,root);

  return Ok;
}

retCode g_retract(processPo P,ptrPo a)
{
  ptrI  pr  = deRefI(&a[1]);		/* The name of the program */

  if(isvar(pr))
    return liberror(P,"__retract",eINSUFARG);
  else if(!IsProgLbl(pr))	      /* Only allowed to retract dynamic code */
    return liberror(P,"__retract",eINVAL);

  defineProg(pr,kvoid);			/* clear the code portion */

  return Ok;
}

/*
 * construct a new class, and implement it as though it were defined by:
 * newObject(T) => valof{
 *   Tf = consOf(T);
 *   O = genSym(Tf);
 *   _assert(O(G,O,this) :- Tf(G,T,this))
 *   valis O
 * }
 */
retCode g_newObject(processPo P,ptrPo a)
{
  ptrI T = deRefI(&a[1]);

  if(isvar(T))
    return liberror(P,"__newObject",eINSUFARG);
  else{
    switchProcessState(P,in_exclusion);

    ptrI O = goObject(&P->proc.heap,T);

    setProcessRunnable(P);
  
    return equal(P,&O,&a[2]);
  }
}

/*
 * construct a new object, and implement it as though it were defined by:
 * newObject(T) => valof{
 *   Tf = consOf(T);
 *   O = genSym(Tf);
 *   _assert(O(G,O,this) :- Tf(G,T,this))
 *   valis O
 * }
 */
ptrI goObject(heapPo H,ptrI T)
{
  assert(isobj(T));

  ptrI Tf = T;				/* The initial constructor */
  CodeGenRec G = {H,0,NULL,NULL,0};
  genPo S = &G;
  rootPo root = gcAddRoot(H,&Tf);
  ptrI O = dynamicObject(H);

  gcAddRoot(H,&T);
  gcAddRoot(H,&O);

  locateVars(S,&T);

  Tf = objV(T)->class;
  Tf = programOfClass(objV(Tf));

  /* We have to build the program */
  // O(gVar,_,tVar) :- Tf(gVar,T,tVar)

  pthread_once(&once,initCodeGen);

  findLit(S,O);			       /* Put the object as the first literal */

  {
    long hsize = estimateHeap(&T,True);
    map regs = iniMap(3,3);
    
    iPo ins = gcMap(S,putIns(S,gc,3,hsize),regs);
    iPo pre = NULL;
    iPo post = bldTerm(S,&T,2,&pre,regs,&regs);

    post = appendIns(pre,post);		/* stitch in the prefix instructions */

    if(S->minYreg<0)
      post = appendIns(post,putIns(S,dlkawl,3,&Tf));
    else
      post = appendIns(post,putIns(S,lkawl,3,&Tf));
    
    ins = appendIns(ins,post);
    
    if(S->minYreg<0)
      ins = appendIns(putIns(S,alloc,3,-S->minYreg),
		      appendIns(putIns(S,gcmap,3,0),ins));
    
#ifdef EXECTRACE
    if(tracePut){
      outMsg(logFile,"Generated code for %w is:\n",&T);
      dumpIns(S,ins);
      showLits(S);
      showMap(regs);
      outMsg(logFile,"\n");
      flushFile(logFile);
    }
#endif
    
    {
      long litCount = S->litNo;
      lPo lt;
      long size = codeCount(ins); /* how many instructions do we have? */
      ptrI code = permCode(size,litCount);
      ptrI xx = kvoid;
      retCode ret = Ok;
      uniChar eMsg[MAX_SYMB_LEN];
      
      gcAddRoot(H,&code);
      gcAddRoot(H,&xx);
      
      for(lt=S->topLit;ret==Ok && lt!=NULL;lt=lt->next){
	switch((ret=freezeTerm(H,&xx,lt->lit,eMsg,NumberOf(eMsg)))){
	case Ok: break;
	case Space:
	  syserr("ran out of heap space in newObject");
	default:
	  syserr("problem in newObject"); /* should never happen */
	}
	updateCodeLit(codeV(code),lt->off,xx);
      }
      
      {
	insPo pc = FirstInstruction(code); /* program counter */
	iPo apc=ins;		/* abstract program counter */
        
	stitchAdds(apc);                /* make sure that code pointers are OK */
        
	while(apc!=NULL){	/* copy the generated code into the code seg */
	  *pc++=apc->code;
	  apc=apc->next;
	}
      }

      setDynamicCode(O,code);		/* set the code */
    }
      
    cleanLits(S);		/* clean the pool of literals */
    cleanIns(ins);
    
    gcRemoveRoot(H,root);
  }

  return O;
}



#ifdef EXECTRACE
void dCode(genPo S,iPo ins)
{
  dumpIns(S,ins);
  flushFile(logFile);
}
#endif
