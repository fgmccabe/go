/*
  Memory allocation for the heap
  (c) 1994-2006 Imperial College & F.G. McCabe

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

#include "config.h"		/* pick up standard configuration header */
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <string.h>
#include "go.h"
#include "symbols.h"
#include "process.h"
#include "symbols.h"
#include "opcodes.h"
#include "hash.h"
#include "char.h"
#include "hashtable.h"
#include "errors.h"

/*
 * initHeap creates a global heap for use in sharing between processes
 */

void setupHeap(heapPo H,processPo owner,long size)
{
  H->base = (objPo)malloc(sizeof(ptrI)*size);
  H->end = &H->base[size];
  H->create = H->base;
  H->roots = &H->rts[0];
  H->topRoot = 0;
  H->maxRoot = NumberOf(H->rts);
  H->owner = owner;
}

retCode reserveSpace(heapPo P,size_t size)
{
  if(P->create+size>P->end)
    gcCollect(P,size);		/* this aborts if there is no memory */

  if(P->create+size<=P->end)
    return Ok;
  else
    return Space;
}

// Root pointer management
void growRoots(heapPo H)
{
  long nmax = H->maxRoot+(H->maxRoot>>2); /* 25% growth */
  
  if(H->roots!=H->rts)
    H->roots = realloc(H->roots,sizeof(objPo*)*nmax);
  else{
    long i;
    H->roots = malloc(sizeof(objPo*)*nmax);
    for(i=0;i<H->topRoot;i++)
      H->roots[i]=H->rts[i];
  }
  H->maxRoot = nmax;
}

rootPo gcAddRoot(heapPo H,ptrPo ptr)
{
  if(H->topRoot>=H->maxRoot)
    growRoots(H);
  H->roots[H->topRoot]=ptr;
  assert(H->topRoot<H->maxRoot);
  return H->topRoot++;
}

/*
 * Structure copy function
 * Copy a term into a specific heap area
 */

typedef struct {
  ptrI orig;			/* original variable */
  ptrI new;			/* new variable location */
} VarRec, *varCopyPo;

typedef struct {
  heapPo H;				/* The heap associated with the copy */
  logical deep;
  varCopyPo vTable;			/* The table of variable entries */
  long vTop;				/* current top of the table */
  long vSize;				/* current size of the table */
} VarTableRec,*varTablePo;

static ptrI getRef(varTablePo vT,ptrI orig)
{
  long i;

  for(i=vT->vTop-1;i>=0;i--){ /* The most recent one is the most likely one that we need */
    if(vT->vTable[i].orig==orig){
      return vT->vTable[i].new;
    }
  }

  return 0;
}

static void setRef(varTablePo vT,ptrI new,ptrI orig)
{
  assert(vT->vTop<vT->vSize);

  {
    int vTop = vT->vTop++;

    vT->vTable[vTop].orig = orig;
    vT->vTable[vTop].new = new;
    gcAddRoot(vT->H,&vT->vTable[vTop].orig);
    gcAddRoot(vT->H,&vT->vTable[vTop].new);
  }
}

/* Compute the size of a term prior to copying it */
static long termSize(ptrI src,long *vCount,heapPo H,logical deep)
{
  switch(ptg(src)){
  case varTg:
    (*vCount)++;			/* increment count of variables */
    return VariableCellCount;           /* We may need to allocate a variable */
  case objTg:{
    objPo p = objV(src);

    if(!deep && inHeap(H,p))		/* already in the heap */
      return 0;
    else if(isObjct(p)){
      long arity = objectArity(p);
      long size = objectSize(p);
      ptrPo a = objectArgs(p);

      long ix;
      for(ix=0;ix<arity;ix++,a++)
	size+=termSize(deRefI(a),vCount,H,deep);
      return size;
    }
    else{
      assert(isSpecialObject(p));

      specialClassPo sClass = sClassOf(p);
      return sClass->sizeFun(sClass,p);
    }
  }
  default:
    syserr("invalid object found in copying");
    return 0;
  }
}

long spaceNeeded(ptrI src,long *vCount,heapPo H,logical deep)
{
  long size = termSize(src,vCount,H,deep);
  return size;
}

static retCode copyTermToHeap(ptrPo dst,ptrPo src,varTablePo vT)
{
  ptrI S = deRefI(src);

  switch(ptg(S)){
  case varTg:{
    ptrI xx = *dst = getRef(vT,S);
      
    if(xx!=0)
      return Ok;
    else if(!vT->deep && inHeap(vT->H,(objPo)dst))
      xx = unBind(dst);
    else
      xx = *dst = allocateVar(vT->H);
    setRef(vT,xx,S);
    return Ok;
  }
  case objTg:{
    if(vT->deep || !inHeap(vT->H,(objPo)dst)){
      objPo p = objV(S);                  // We already know its not a variable
      if(isObjct(p)){
	ptrPo sA = objectArgs(p);
	long arity = objectArity(p);
	retCode res = Ok;

	objPo xx = allocate(vT->H,objectSize(p));
	*dst = objP(xx);
	
	int ix;
	ptrPo dA = objectArgs(xx);
	
	for(ix=0;res==Ok && ix<arity;ix++)
	  res = copyTermToHeap(dA++,sA++,vT);
	return res;
      }
      else if(isSpecialObject(p)){
	specialClassPo sClass = sClassOf(p);

	objPo new = allocateSpecial(&globalHeap,p->class);

	*dst = objP(new);
	
	sClass->copyFun(sClass,new,p);
	return Ok;
      }
      else
	return Error;
    }
    else{
      *dst = *src;
      return Ok;
    }
  }
  default:
    return Error;
  }
}

retCode localCopy(ptrPo dst,heapPo H,ptrPo src)
{
  long vCount = 0;

  if(reserveSpace(H,spaceNeeded(*src,&vCount,H,True))==Ok){
    VarTableRec vTable = { H, True, vCount>0?(varCopyPo)malloc(sizeof(VarRec)*vCount):NULL, 0, vCount};

    {
      rootPo root = gcCurrRoot(H);
      retCode ret = copyTermToHeap(dst,src,&vTable);
      gcRemoveRoot(H,root);

      if(vTable.vTable!=NULL)
	free(vTable.vTable);		/* release the allocated table */
      return ret;
    }
  }
  else return Error;
}

static retCode freezeT(ptrPo dst,ptrPo src,heapPo H,uniChar *eMsg,long len)
{
  ptrI S = deRefI(src);

  switch(ptg(S)){
  case varTg:
    strMsg(eMsg,len,"variable in value");
    return Error;
  case objTg:{
    objPo p = objV(S);                  // We already know its not a variable

    if(inGlobalHeap(p)){
      *dst = S;
      return Ok;
    }
    else if(isObjct(p)){
      ptrPo a = objectArgs(p);
      long arity = objectArity(p);
      retCode ret = Ok;
      long ix;
      objPo new = allocSpace(&globalHeap,objectSize(p));

      *dst = objP(new);

      new->class = p->class;
      ptrPo n = objectArgs(new);
      for(ix=0;ret==Ok && ix<arity;ix++,a++,n++)
	ret = freezeT(n,a,H,eMsg,len);

      return ret;
    }
    else if(isSpecialObject(p) && inHeap(H,p)){
      objPo new = allocateSpecial(&globalHeap,p->class);

      *dst = objP(new);

      specialClassPo sClass = sClassOf(p);
      sClass->copyFun(sClass,new,p);

      return Ok;
    }
    else{
      strMsg(eMsg,len,"invalid value %0,3w in value",src);
      return Error;
    }
  }
  default:
    strMsg(eMsg,len,"invalid cell %0,3w in value",src);
    return Error;
  }
}

retCode freezeTerm(heapPo H,ptrPo dst,ptrI src,uniChar *eMsg,long len)
{
  long vCount = 0;
  rootPo root = gcAddRoot(H,&src);
  ptrI xx = kvoid;
  long size = spaceNeeded(src,&vCount,&globalHeap,True);
  retCode ret;

  lockGlobal();

  if(vCount>0){
    ret = Error;
    strMsg(eMsg,len,"%d unbound variables in value",vCount);
  }
  else if(reserveGlobalSpace(size)==Ok){
    ret = freezeT(&xx,&src,H,eMsg,len);
    gcRemoveRoot(H,root);
    *dst = xx;
  }
  else{
    ret = Space;
    strMsg(eMsg,len,"out of heap space");
  }

  unlockGlobal();
  return ret;
}

/* Determine if a term is ground or not */
static logical isGround(ptrPo t)
{
  ptrI xx = deRefI(t);

  switch(ptg(xx)){
  case varTg:
    return False;
  case objTg:{
    objPo p = objV(xx);
    if(isObjct(p)){
      ptrPo a = objectArgs(p);
      long arity = objectArity(p);
      long ix;

      for(ix=0;ix<arity;ix++,a++)
	if(!isGround(a))
	  return False;
    }
    return True;
  }
  default:
    return True;
  }
}

logical isGroundTerm(ptrPo p)
{
  return isGround(p);
}

logical IsBinOp(ptrPo p,ptrI key,ptrPo a1,ptrPo a2)
{
  ptrI T = deRefI(p);
  objPo t = objV(T);
  
  if(!isvar(T) && hasClass(t,key)){
    assert(objectArity(t)==2);

    ptrPo a = objectArgs(t);
    *a1 = *a++;
    *a2 = *a++;
    return True;
  }
  else
    return False;
}


#ifdef MEMTRACE
/*
 * Verify integrity of process/heap structure 
 */

static void verifyPtr(ptrPo ptr,heapPo P);

static retCode helpVerify(ptrPo arg,void *c)
{
  heapPo H = (heapPo)c;
  
  verifyPtr(arg,H);
  return Ok;
}

static objPo verifyObj(objPo ob,heapPo H)
{
  if(isObjBase((ptrPo)ob)){
    ptrPo arg = objectArgs(ob);
    long arity = objectArity(ob);
    
    long ix;
    for(ix=0;ix<arity;ix++,arg++)
      verifyPtr(arg,H);
    return ob+objectSize(ob);
  }
  else{
    assert(isSpecialObject(ob));
    specialClassPo sClass = sClassOf(ob);
    sClass->scanFun(sClass,helpVerify,H,ob);
    return ob+sClass->sizeFun(sClass,ob);
  }
}

static void verifyPtr(ptrPo ptr,heapPo P)
{
  if(ptr!=NULL){
    ptrI vx = *ptr;

    switch(ptg(vx)){
    case varTg:{
      ptrPo ref = (ptrPo)objV(vx);

      assert(ref>=(ptrPo)P->base && ref<(ptrPo)P->create);
      return;
    }
    case objTg:{
      objPo ref = objV(vx);

      if(P->owner==NULL)
	assert(inGlobalHeap(ref));
      else
	assert(inHeap(P,ref) || inGlobalHeap(ref));
      return;
    }
    default:
      syserr("problem in verifyPtr");
    }
  }
}

void verifyVar(ptrPo ptr,processPo P)
{
  if(ptr!=NULL){
    ptrI vx = *ptr;

    switch(ptg(vx)){
    case varTg:{                        /* A variable reference can in the stack or in a heap */
      objPo ref = objV(vx);

      assert(inHeap(&P->proc.heap,ref)||
	     ((ptrPo)ref>=P->proc.sBase && (ptrPo)ref<P->proc.sTop)||
	     inGlobalHeap(ref));
      return;
    }
    case objTg:{                         /* An object mst also be in the heap */
      objPo ref = objV(vx);

      assert(isChr(ref) || inHeap(&P->proc.heap,ref)|| inGlobalHeap(ref));
      return;
    }
    default:
      syserr("invalid tagged pointer in variable");
    }
  }
}

void verifyHeap(heapPo P)
{
  objPo scan = P->base;

  assert(P->base<=P->create && P->create<=P->end);
  while(scan<P->create){
    assert(scan<P->end);
    scan = verifyObj(scan,P);
  }
  assert(scan==P->create);
}


static void verifyTerm(ptrPo ptr,heapPo P);

void verifyTrm(objPo ob,heapPo P)
{
  assert((ob>=P->base && ob<P->create) ||
	 (ob>=globalHeap.base && ob<globalHeap.create));

  ptrPo args = objectArgs(ob);
  long arity = objectArity(ob);

  long ix;
  for(ix=0;ix<arity;ix++,args++)
    verifyTerm(args,P);
}

static void verifyTerm(ptrPo ptr,heapPo P)
{
  if(ptr!=NULL){
    ptrI vx = *ptr;

    switch(ptg(vx)){
    case varTg:{
      ptrPo ref = (ptrPo)objV(vx);

      assert((ref>=(ptrPo)P->base && ref<(ptrPo)P->create)||
	     (ref>=(ptrPo)globalHeap.base && ref<(ptrPo)globalHeap.create));
      if(ref!=ptr)
	verifyTerm(ref,P);
      return;
    }
    case objTg:{
      objPo ref = objV(vx);

      if(P->owner==NULL)
	assert(inGlobalHeap(ref));
      else
	assert(isChr(ref) || inHeap(P,ref)|| inGlobalHeap(ref));
      verifyTrm(ref,P);
      return;
    }
    default:
      syserr("problem in verifyTerm");
    }
  }
}

/* We check the argument registers to see if they represent valid data */
static logical validPtr(processPo P,ptrPo x)
{
  if(x==NULL)
    return False;
  else if(inHeap(&P->proc.heap,(objPo)x) || inGlobalHeap((objPo)x))
    return True;
  else{
    register callPo C = P->proc.C;
    register choicePo B = P->proc.B;
    register int len = envSize(P->proc.cPC);

    while(B<(choicePo)P->proc.sTop || C<(callPo)P->proc.sTop){
      assert(B<=(choicePo)P->proc.sTop && B>=(choicePo)P->proc.sBase &&
	     C<=(callPo)P->proc.sTop && C>=(callPo)P->proc.sBase);
      if((ptrPo)B<(ptrPo)C){
	register ptrPo A = (ptrPo)(B+1);
        
        if(x>=A&&x<&A[B->AX])
          return True;                          /* We wouldnt normally expect this */

	if(B->C<C && B->C<(callPo)P->proc.sTop){ 
	  C = B->C;		/* the choice point's call back is newer */
	  len=envSize(B->cPC);	/* do this before the next step */
	}
	B = B->B;		/*look at the previous choice point */
      }
      else{			/* mark a call environment */
	register ptrPo Y = (ptrPo)C;

        if(x>=Y-len&&x<Y)
          return True;

	len=envSize(C->cPC);	/* do this before the next step */
	C = C->cC;
      }
    }

    assert((ptrPo)C==P->proc.sTop && (ptrPo)B==P->proc.sTop);
    return False;                               /* The pointer is a wild one */
  }
}

/* Used in verification */
void verifyProc(processPo p)
{
  register ptrPo oBase = p->proc.sBase;
  register ptrPo oTop = p->proc.sTop;
  heapPo heap = &p->proc.heap;
  ptrPo heapMark = (ptrPo)p->proc.heap.create; /* check for strictly descending H vars */

  assert(oBase<oTop);

  if(p->proc.state!=dead){		/* only process live processes */
    register callPo C = p->proc.C;
    register choicePo B = p->proc.B;
    register choicePo T = p->proc.T;
    register int len = envSize(p->proc.cPC);
    register int i;

    while(B<(choicePo)p->proc.sTop || C<(callPo)p->proc.sTop){
      assert(B<=(choicePo)oTop && B>=(choicePo)oBase &&
	     C<=(callPo)oTop && C>=(callPo)oBase);

      if((ptrPo)B<(ptrPo)C){
	register ptrPo A = (ptrPo)(B+1);

	assert((ptrPo)B<=(ptrPo)T && (ptrPo)B<(ptrPo)C);
	
	verifyVar(&B->PROG,p);
	verifyVar(&B->cPROG,p);
          
	for(i=0;i<B->AX;i++,A++)
	  verifyVar(A,p);
        
	assert(B->cSB<=(choicePo)p->proc.sTop);
	assert(B->B<=(choicePo)p->proc.sTop);
	assert(B->T<=(choicePo)p->proc.sTop);
	assert(B->C<=(callPo)p->proc.sTop);
        
	assert(B->trail>=(trailPo)p->proc.sBase &&
	       B->trail<=p->proc.trail);
	assert(op_cde(*B->cPC)==gcmap);
	assert(B->H>=(ptrPo)p->proc.heap.base && B->H<=(ptrPo)p->proc.heap.create);
	assert(B->H<=heapMark);
	assert((ptrPo)T>=(ptrPo)B);
	heapMark = B->H;

	if(B==T)
	  T = T->T;
          
	if(B->C<C && B->C<(callPo)p->proc.sTop){ 
	  C = B->C;		/* the choice point's call back is newer */
	  len=envSize(B->cPC);	/* do this before the next step */
	}
	B = B->B;		/*look at the previous choice point */
      }
      else{			/* verify a call environment */
	register ptrPo Y = (ptrPo)C;
        
        assert((ptrPo)C<(ptrPo)B && (ptrPo)C<(ptrPo)T);
	for(i=0;--Y,i<len;i++)
	  verifyVar(Y,p);

	verifyVar(&C->cPROG,p);

	assert(C->cSB<=(choicePo)p->proc.sTop);
	assert(C->cC<=(callPo)p->proc.sTop);
	assert(op_cde(*C->cPC)==gcmap);
	assert(C->cPC-FirstInstruction(C->cPROG)>=0);

	len=envSize(C->cPC);	/* do this before the next step */
	C = C->cC;
      }
    }

    assert((ptrPo)C==p->proc.sTop && (ptrPo)B==p->proc.sTop);

    for(i=0;i<argArity(p->proc.PC);i++)
      if(validPtr(p,(ptrPo)objV(p->proc.A[i])))
	verifyVar(&p->proc.A[i],p);
      
    verifyVar(&p->proc.PROG,p);
    verifyVar(&p->proc.cPROG,p);
    verifyVar(&p->proc.trigger,p);

    {
      register trailPo tr = p->proc.trail;

      while(tr-->(trailPo)oBase){
	assert((tr->var>=(ptrPo)p->proc.heap.base&&tr->var<(ptrPo)p->proc.heap.create)||
	       (tr->var>=p->proc.sBase&&tr->var<p->proc.sTop));

	verifyVar(tr->var,p);
	verifyVar(&tr->val,p);
      }
    }

    verifyVar(&p->proc.thread,p);

    verifyHeap(heap);
  }
}

static retCode vP(processPo p,void *c)
{
  verifyProc(p);
  return Ok;
}

void verifyAllProcesses(void)
{
  processProcesses(vP,NULL);
}


#endif /* MEMTRACE */
