/*
  A concurrent garbage collector for the Go! engine
  (c) 2006-2007 F.G. McCabe

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.
  
  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA  02111-1307, USA.
  
  Contact: frankmccabe@mac.com
*/

#include "config.h"		/* pick up standard configuration header */
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <string.h>
#include "go.h"
#include "hashtable.h"
#include "global.h"

/*
  At any one time, we are either building the heap in the lower space
  or the upper space. In the former case, the heap looks like:


  || old objects | current heap | free  | ***** not in use ***** ||
                 ^ base         ^create ^ end

  in the latter case, the overall heap looks like:

  || old objects | **** not in use **** | current heap | free    ||
                                        ^ base         ^create   ^end

  in the latter state, during gc the old object space will grow:

  || old objects + newly old | current      |  ** not in use **  ||
                             ^ new base     ^ new end

  In effect, an object is promoted to the old generation if it survives 
  at least one or two garbage collections.

  The bitmap grey contains a bit for every cell in the heap. For every old
  object that has been updated the corresponding bit will be set.

  At some point a full compaction will be required to clear long lived
  objects. In the event that even this fails, then a new, larger, heap
  is built.

  This strategy is adapted from the strategy from the April system.
*/

#ifdef MEMTRACE
static void verifyGlobalHeap(void);
static void verifyOldGeneration(void);
#endif


// We protect write access to the global memory with a lock
static pthread_mutex_t globalMutex;

void lockGlobal(void)
{
  pthread_mutex_lock(&globalMutex);
}

void unlockGlobal(void)
{
  pthread_mutex_unlock(&globalMutex);
}

objPo globalSpace;			/* This is the raw memory block */
static long globalSize;			/* And how big it is */
static cardMap *grey;			/* A mark table for adding grey markers */
static long greySize;			/* Size of the grey table in cardMaps */

HeapRec globalHeap;			/* This is the global heap */

objPo leftBase;				/* The left hand side base */
objPo rightBase;			/* The right hand side base */

#ifdef MEMTRACE
static long globalCount = 0;
#endif

void initGlobal(long size)
{
  initRecursiveMutex(&globalMutex);

  size = ALIGNPTR(size,CARDWIDTH);	/* make sure that size is lined up with cards */

  globalSize = size*2;
  leftBase = globalSpace = (objPo)malloc(sizeof(ptrI)*globalSize);
  rightBase = &leftBase[size];
  greySize = globalSize>>CARDSHIFT;
  grey = (cardMap*)calloc(greySize,sizeof(cardMap));

  globalHeap.base = globalHeap.create = leftBase;
  globalHeap.end = rightBase;
  globalHeap.owner = NULL;

  globalHeap.roots = &globalHeap.rts[0];
  globalHeap.topRoot = 0;
  globalHeap.maxRoot = NumberOf(globalHeap.rts);

  initMasks();
}

void markGrey(objPo p)
{
  if(p>=globalSpace && p<leftBase){
    unsigned long add = p-globalSpace;
    unsigned long index = add>>CARDSHIFT;
    unsigned long off = add&CARDMASK;

    lockGlobal();
    grey[index]|=masks[off];
    unlockGlobal();
  }
}

retCode reserveGlobalSpace(long size)
{
  if(globalHeap.create+size>globalHeap.end)
    globalGC(size);

  if(globalHeap.create+size>globalHeap.end)
    return Space;
  return Ok;
}

objPo permAllocate(long size)
{
  pthread_mutex_lock(&globalMutex);

  if(globalHeap.create+size>globalHeap.end){
    globalGC(size);

    if(globalHeap.create+size>globalHeap.end)
      syserr("ran out of global heap");
  }

  {
    register objPo new = globalHeap.create;
    globalHeap.create+=size;

    pthread_mutex_unlock(&globalMutex);

    return new;
  }
}

objPo permObject(heapPo H,ptrI cls)
{
  rootPo root = gcAddRoot(H,&cls);

  clssPo class = (clssPo)objV(cls);
  long arity = classArity(class);

  objPo o = permAllocate(class->arity+1);
  o->class = cls;

  ptrPo p = objectArgs(o);
  long ix;

  for(ix=0;ix<arity;ix++)
    p[ix] = kvoid;

  gcRemoveRoot(H,root);
  return o;
}

objPo permSpecial(heapPo H,ptrI cls,long size)
{
  rootPo root = gcAddRoot(H,&cls);

  specialClassPo spClass = (specialClassPo)objV(cls);
  size += spClass->sizeFun(spClass,NULL);

  objPo o = permAllocate(size);
  o->class = cls;

  gcRemoveRoot(H,root);
  return o;
}

static logical gIsTerm(objPo o);
static logical gIsSpecial(objPo o);

static long gTermArity(objPo p);
static clssPo gClassOf(objPo p);

static specialClassPo specialClassOf(objPo o);

static logical gFwdCmp(ptrI a,ptrI b);
static logical gHasClass(objPo o,ptrI cl);

static void scanAllProcesses(globalGcPo G);
static void scanProcess(processPo p,globalGcPo G);
static void resetAllProcesses(heapPo tH);

// These are versions of the standard functions that know about 
// objects being moved (in particular class structures themselves)
static logical gIsTerm(objPo o)
{
  if(isfwd(*(ptrPo)o))
    return True;			/* only base of term is forward marked*/
  else{					/* Ok, let us take a closer look */
    ptrI Cl = o->class;			/* The class word */
    ptrPo pCl = (ptrPo)objV(Cl);

    if(isfwd(*pCl))			/* Has the class been moved? */
      pCl = (ptrPo)objV(*pCl);		/* go to where the class is now */

    objPo oCl = (objPo)pCl;
    
    return oCl->class == classClass;	/* classes have #class as their class */
  }
}


// We have to be careful because things can move during garbage collection
static long gTermArity(objPo o)
{
  ptrI oCl = o->class;

  if(isfwd(oCl))
    oCl = *((ptrPo)objV(oCl));

  return ((clssPo)objV(oCl))->arity;
}

static clssPo gClassOf(objPo p)
{
  clssPo cl = (clssPo)objV(p->class);

  if(isfwd(cl->class))			/* this class has been moved */
    cl = (clssPo)objV(cl->class);

  return cl;
}

static logical gFwdCmp(ptrI a,ptrI b)
{
  if(isfwd(a))
    a = *((ptrPo)objV(a));

  if(isfwd(b))
    b = *((ptrPo)objV(b));

  return a==b;
}

static logical gHasClass(objPo o,ptrI cl)
{
  return gFwdCmp(o->class,cl);
}

static logical gIsSpecial(objPo o)
{
  ptrI Cl = o->class;			/* The class word */
  ptrPo pCl = (ptrPo)objV(Cl);
  
  if(isfwd(*pCl))			/* Has the class been moved? */
    pCl = (ptrPo)objV(*pCl);		/* go to where the class is now */

  objPo oCl = (objPo)pCl;
    
  return oCl->class == specialClass; /* special classes have #specialclass as their class */
}

static specialClassPo specialClassOf(objPo o)
{
  ptrI Cl = o->class;			/* The class word */
  ptrPo pCl = (ptrPo)objV(Cl);
  
  if(isfwd(*pCl))			/* Has the class been moved? */
    pCl = (ptrPo)objV(*pCl);		/* go to where the class is now */

  objPo oCl = (objPo)pCl;

  assert(oCl->class==specialClass);

  return (specialClassPo)oCl;
}

//
// scanPtr looks at an individual pointer cell and copies the referenced
// structure if necessary
//
ptrI scanPtr(globalGcPo G,ptrI orig)
{
  objPo o = objV(orig);			/* not nec. valid at this stage */
  heapPo sH = G->fH;
  heapPo tH = G->tH;

  switch(ptg(orig)){
  case varTg:{
    if(inHeap(sH,o)){
      ptrPo po = (ptrPo)o;
      long offset = 0;			/* look for the object base */
      
      while(po>=(ptrPo)sH->base){
	if(isfwd(*po))
	  return varP(po+offset);	/* Already moved */
	else if(isobj(*po) && gIsTerm((objPo)po)){
	  objPo new = tH->create;
	  long size = gTermArity((objPo)po);
	  memmove(new,po,size*sizeof(ptrI));
	  *po = fwdP(new);		/* plant the forward pointer */
	  tH->create+=size;
	  return varP(new+offset);
	}
	else{
	  po--;
	  offset++;
	}
      }
    }
    else
      return orig;
  }
  case objTg:{
    if(isfwd(o->class))
      return objP(objV(o->class));
    else if(inHeap(sH,o)){
      objPo new = tH->create;

      assert(new>=tH->base);

      if(gIsTerm(o)){
	long size = gTermArity((objPo)o)+1;
	memmove(new,o,size*sizeof(ptrI));
	o->class = fwdP(new);		/* plant the forward pointer */
	tH->create+=size;
	return objP(new);
      }
      else{
	if(gIsSpecial(o)){
	  specialClassPo sClass = specialClassOf(o);
	  long size = sClass->sizeFun(sClass,o);

	  memmove(new,o,size*sizeof(ptrI));
	  o->class = fwdP(new);
	  tH->create+=size;
	  
	  if(gHasClass(new,symbolClass))
	    installSymbol((symbPo)new);
	  else if(gHasClass(new,classClass))
	    installClass((clssPo)new);
	  else if(gHasClass(new,programClass))
	    installProgram(new);
	  return objP(new);
	}
	else{
	  syserr("Unexpected entity in global heap");
	}
      }
    }
    else return orig;
  }
  default:
    syserr("illegal pointer data");
    return orig;			/* Should not happen normally */
  }
}

static retCode scanSpecial(ptrPo arg,void *c)
{
  globalGcPo G = (globalGcPo)c;
  *arg = scanPtr(G,*arg);
  return Ok;
}

// Look at an copied object scanning each element
static objPo scanObj(globalGcPo G,objPo scan)
{
  assert(G->tH->create<G->tH->end);

  if(gIsTerm(scan)){
    clssPo class = gClassOf(scan);
    long arity = class->arity;
    ptrPo args = objectArgs(scan);
    long ix;

    scan->class = scanPtr(G,scan->class);
    for(ix=0;ix<arity;ix++,args++)
      *args = scanPtr(G,*args);
    return (objPo)args;
  }
  else if(gIsSpecial(scan)){
    specialClassPo sClass = specialClassOf(scan);
    sClass->scanFun(sClass,scanSpecial,G,scan);

    return scan+sClass->sizeFun(sClass,scan);
  }
  else{
    syserr("Unexpected entity in global heap");
    return NULL;
  }
}

static void scanGreys(globalGcPo G)
{
  long i,j;
  long max = ALIGNPTR(leftBase-globalSpace,CARDWIDTH)/CARDWIDTH;
  for(i=0;i<max;i++){
    if(grey[i]!=0){
      for(j=0;j<CARDWIDTH;j++){
	if((grey[i]&masks[j])!=0){
	  objPo o = globalSpace+(i<<CARDSHIFT)+j;
	  scanObj(G,o);		/* examine the dirtied object */
	}
      }
    }
  }
}

static void clearGreys()
{
  long i;
  for(i=0;i<greySize;i++)
    grey[i]=0;				/* clear the grey table */
}

static void collectPhase(globalGcPo G)
{
  restartDictionary(G);	    /* tell the dictionary to start rebuilding itself */

  scanAllProcesses(G);

  int i;

  for(i=0;i<G->fH->topRoot;i++)
    *G->fH->roots[i] = scanPtr(G,*G->fH->roots[i]);

  scanGreys(G);

  objPo scan = G->tH->base;

  while(scan<(objPo)G->tH->create)
    scan = scanObj(G,scan);

  // We should have no more than we started with
  assert(G->tH->create-G->tH->base<=G->fH->create-G->fH->base);

  resetAllProcesses(G->tH);
}

void globalGC(long request)		/* how many cells were requested? */
{
  HeapRec toHeap;			/* The current to space */
  pthread_t self = pthread_self();	/* This thread will not be paused ... */

  pauseAllThreads(self);		/* pause all threads, other than this one */

#ifdef MEMTRACE
  if(traceMemory){
    outMsg(logFile,"Starting global garbage collection\n%_");
    verifyGlobalHeap();
    verifyOldGeneration();

    verifyAllProcesses();
  }

  globalCount++;
#endif

  if(globalHeap.base==leftBase){
    toHeap.base = toHeap.create = rightBase;
    toHeap.end = &globalSpace[globalSize];
  }
  else{
    toHeap.base = toHeap.create = leftBase;
    toHeap.end = globalHeap.base;
  }

  globalGcRec G =
    { globalHeap.base>toHeap.base, &globalHeap, &toHeap };

  collectPhase(&G);

  // Recalculate where the global heap etc should go

  if(globalHeap.base==leftBase){
    globalHeap.base = rightBase;
    globalHeap.create = toHeap.create;
    globalHeap.end = toHeap.end;
  }
  else{
    globalHeap.base = globalHeap.create = leftBase = toHeap.create;
    globalHeap.end = rightBase = leftBase+(&globalSpace[globalSize]-leftBase)/2;
    clearGreys();	    /* only clear the dirty flags when old generation is grown */
  }

  if(spaceLeft(&globalHeap)*2<request ||
     (totalHeapSize(&globalHeap)*2/3>spaceLeft(&globalHeap))){
    long size = globalSize+request;
    globalSize = size*2;

#ifdef MEMTRACE
    if(traceMemory)
      outMsg(logFile,"growing global heap to %d cells\n%_",globalSize);
#endif

    objPo newBase = (objPo)malloc(sizeof(ptrI)*globalSize);
    objPo midPoint = &newBase[size];
    greySize = globalSize>>CARDSHIFT;
    free(grey);
    grey = (cardMap*)calloc(greySize,sizeof(cardMap));
    HeapRec nHeap = { newBase, midPoint, newBase}; /* set up a heap to go to */

    globalGcRec NG = { False, &globalHeap, &nHeap};

    collectPhase(&NG);			/* copy to the new heap */

    free(globalSpace);
    globalSpace = leftBase = globalHeap.base = newBase;
    rightBase = globalHeap.end = midPoint;
    globalHeap.create = nHeap.create;
  }

#ifdef MEMTRACE
  if(traceMemory){
    outMsg(logFile,"Ending global garbage collection\n%_");
    verifyGlobalHeap();
    verifyAllProcesses();
  }
#endif

  resumeAllThreads(self);
}

// Scan a process for roots into the global heap space

static insPo relativePC(ptrI PROG,insPo orig);

static void scanProcess(processPo p,globalGcPo G)
{
  register callPo C = p->proc.C;
  register choicePo B = p->proc.B;
  register int len = envSize(p->proc.cPC);
  register int i,arity;

  for(i=0;i<p->proc.heap.topRoot;i++)
    *p->proc.heap.roots[i] = scanPtr(G,*p->proc.heap.roots[i]);

  arity = argArity(p->proc.PC);

  p->proc.PC = relativePC(p->proc.PROG,p->proc.PC);
  p->proc.PROG = scanPtr(G,p->proc.PROG);

  p->proc.cPC = relativePC(p->proc.cPROG,p->proc.cPC);
  p->proc.cPROG = scanPtr(G,p->proc.cPROG);

  for(i=0;i<=arity;i++)
    p->proc.A[i]=scanPtr(G,p->proc.A[i]);

  p->proc.trigger = scanPtr(G,p->proc.trigger);

  {
    trailPo tr = p->proc.trail;

    while((ptrPo)tr-->p->proc.sBase){
      tr->var = (ptrPo)scanPtr(G,(ptrI)tr->var);
      tr->val = scanPtr(G,tr->val);
    }
  }

  p->proc.thread = scanPtr(G,p->proc.thread);
  p->proc.errorCode = scanPtr(G,p->proc.errorCode);

  {
    objPo scan = p->proc.heap.base;

    while(scan<p->proc.heap.create)
      scan = scanObj(G,scan);
  }

  while(B<(choicePo)p->proc.sTop || C<(callPo)p->proc.sTop){
    if((ptrPo)B<(ptrPo)C){	/* mark a choice point */
      ptrPo A = (ptrPo)(B+1);
      long bLen = envSize(B->cPC);

      B->PC = relativePC(B->PROG,B->PC);
      B->PROG = scanPtr(G,B->PROG);
      B->cPC = relativePC(B->cPROG,B->cPC);
      B->cPROG = scanPtr(G,B->cPROG);
        
      for(i=0;i<B->AX;i++,A++){
	*A=scanPtr(G,*A);
      }
        
      if(B->C<C && B->C<(callPo)p->proc.sTop){
	C = B->C;		/* the choice point's call back is newer */
	len=bLen;
      }
      B = B->B;		/*look at the previous choice point */
    }
    else{			/* mark a call environment */
      register ptrPo Y = (ptrPo)C;

      for(i=0;--Y,i<len;i++){
	*Y = scanPtr(G,*Y);
      }
      
      len=envSize(C->cPC);	/* do this before the next step */
      C->cPC = relativePC(C->cPROG,C->cPC);
      C->cPROG = scanPtr(G,C->cPROG);

      C = C->cC;
    }
  }

  assert((ptrPo)C==p->proc.sTop && (ptrPo)B==p->proc.sTop);
}

static insPo relativePC(ptrI PROG,insPo orig)
{
  codePo code = (codePo)objV(PROG);
  insPo base = code->data;
  long offset = orig-base;	/* the offset is relative to original address */
  codePo aCode = code;			/* but we need the new location */

  if(isfwd(aCode->class))
    aCode = (codePo)objV(aCode->class);

  assert(offset>=0 && offset<aCode->size);
  return (insPo)offset;
}

static retCode scanP(processPo P,void *c)
{
  scanProcess(P,(globalGcPo)c);
  return Ok;
}

static void scanAllProcesses(globalGcPo G)
{
  processProcesses(scanP,G);
}


// This phase moves all relative program counter back to absolute


static insPo absolutePC(ptrI PROG,insPo orig);

static void resetProcess(processPo p,heapPo tH)
{
  register callPo C = p->proc.C;
  register choicePo B = p->proc.B;

  p->proc.PC = absolutePC(p->proc.PROG,p->proc.PC);
  p->proc.cPC = absolutePC(p->proc.cPROG,p->proc.cPC);

  while(B<(choicePo)p->proc.sTop || C<(callPo)p->proc.sTop){
    if((ptrPo)B<(ptrPo)C){	/* mark a choice point */
      B->PC = absolutePC(B->PROG,B->PC);
      B->cPC = absolutePC(B->cPROG,B->cPC);
        
      if(B->C<C && B->C<(callPo)p->proc.sTop){
	C = B->C;		/* the choice point's call back is newer */
      }
      B = B->B;				/* look at previous choice point */
    }
    else{				/* adjust call environment */
      C->cPC = absolutePC(C->cPROG,C->cPC);

      C = C->cC;
    }
  }
  assert((ptrPo)C==p->proc.sTop && (ptrPo)B==p->proc.sTop);
}

static insPo absolutePC(ptrI PROG,insPo orig)
{
  objPo p = objV(PROG);
  long offset = (long)orig;

  if(isfwd(p->class))
    p = objV(p->class);

  codePo code = (codePo)p;

  assert(offset>=0 && offset<code->size);

  return code->data+offset;
}

static retCode resetP(processPo P,void *c)
{
  resetProcess(P,(heapPo)c);
  return Ok;
}

static void resetAllProcesses(heapPo tH)
{
  processProcesses(resetP,tH);
}

#ifdef MEMTRACE

static retCode verifyGlobalPtr(ptrPo p)
{
  switch(ptg(*p)){
  case varTg:{
    syserr("found a variable in the global heap");
    return Error;
  }
  case objTg:{
    objPo ref = objV(*p);

    assert(inGlobalHeap(ref));
    return Ok;
  }
  case fwdTg:
    syserr("found a live forward cell in globalheap");
    return Error;
  default:
    syserr("found an invalid cell in globalheap");
    return Error;
  }
}

static retCode verifySpecial(ptrPo arg,void *c)
{
  return verifyGlobalPtr(arg);
}

static void verifyGlobalHeap(void)
{
  objPo ob = globalHeap.base;
  objPo create = globalHeap.create;
  objPo end = globalHeap.end;

  assert(globalHeap.base<=create && create<=end);

  while(ob<create){
    assert(ob>=globalHeap.base && ob<end);

    clssPo class = gClassOf(ob);

    if(gFwdCmp(class->class,classClass)){
      long arity = class->arity;
      ptrPo args = objectArgs(ob);
      long ix;
      
      assert(inGlobalHeap(ob));
      
      for(ix=0;ix<arity;ix++,args++)
	verifyGlobalPtr(args);
      ob+=arity+1;
    }
    else if(gFwdCmp(class->class,specialClass)){
      specialClassPo sClass = (specialClassPo)class;
      
      if(gHasClass(ob,symbolClass))
	assert(symbolPresent(SymVal((symbPo)ob))==objP(ob)); /* check its in the dictionary */
      else if(gHasClass(ob,classClass))
	assert(classPresent(className((clssPo)ob))==objP(ob));
      
      sClass->scanFun(sClass,verifySpecial,NULL,ob);
      
      ob+=sClass->sizeFun(sClass,ob);
    }
    else{
      syserr("Unexpected entity in global heap");
    }
  }

  assert(ob==create);
}

static void verifyGrey(ptrI P,objPo base)
{
  if(isobj(P)){
    objPo o = objV(P);
    if(o>=globalSpace && o<leftBase)
      return;
    else if(base>=globalSpace && base<leftBase && 
	    o>=leftBase && o<globalSpace+globalSize){
      unsigned long add = base-globalSpace;
      unsigned long index = add>>CARDSHIFT;
      unsigned long off = add&CARDMASK;
      
      assert(grey[index]&masks[off]);
    }
  }
}

static retCode specialGrey(ptrPo arg,void *c)
{
  verifyGrey(*arg,(objPo)c);
  return Ok;
}

static void verifyOldGeneration(void)
{
  objPo ob = globalSpace;
  objPo end = leftBase;

  while(ob<end){
    assert(ob>=globalHeap.base && ob<end);

    clssPo class = gClassOf(ob);

    if(gFwdCmp(class->class,classClass)){
      long arity = class->arity;
      ptrPo args = objectArgs(ob);
      long ix;
      
      assert(inGlobalHeap(ob));
      
      for(ix=0;ix<arity;ix++,args++)
	verifyGrey(*args,ob);

      ob+=arity+1;
    }
    else if(gFwdCmp(class->class,specialClass)){
      specialClassPo sClass = (specialClassPo)class;
      
      if(gHasClass(ob,symbolClass))
	assert(symbolPresent(SymVal((symbPo)ob))==objP(ob)); /* check its in the dictionary */
      else if(gHasClass(ob,classClass))
	assert(classPresent(className((clssPo)ob))==objP(ob));
      
      sClass->scanFun(sClass,specialGrey,(void*)ob,ob);
      
      ob+=sClass->sizeFun(sClass,ob);
    }
    else{
      syserr("Cannot verify integrity of global heap");
    }
  }
}

void sG(void)
{
  outMsg(logFile,"GS=%x, LB=%x, RB=%x, End=%x\n",globalSpace,leftBase,rightBase,&globalSpace[globalSize]);
  outMsg(logFile,"H=%x-%x-%x\n%_",globalHeap.base,globalHeap.create,globalHeap.end);
}

#endif
