/*
  Garbage collection program
  (c) 1994-2007 F.G. McCabe

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

cardMap masks[CARDWIDTH];

#ifdef MEMTRACE
logical traceMemory = False;
logical stressMemory = False;

long gcCount = 0;                       /* Number of times GC is invoked */
#endif


/* Manage the marks table */

typedef struct {		// Entry in the mark stack
  ptrPo base;
  int count;
} MarkStackRec;

typedef struct {
  ptrPo start;
  ptrPo final;
} breakEntry, *breakPo;

typedef struct _gc_support_ {
  heapPo H;
  cardMap mrks[MAX_TABLE];
  cardMap *marks;
  long mrkSize;
  long nmarks;
  ptrPo oldCreate;

  MarkStackRec mrkStack[MAX_TABLE];
  MarkStackRec *markStack;
  long markStackSize;
  long markStackTop;

  breakEntry Breaks[MAX_TABLE];
  breakPo Brk;
  breakPo endBrk;
  long BrkSize;
  ptrPo hpBase;
  ptrPo hpLimit;
  long oCnt;
} GCSupport;

static void markProcess(processPo P,gcSupportPo G);
static objPo markScanObj(gcSupportPo G,objPo x);
static void markPtr(gcSupportPo G,ptrPo x);
static void adjustProcess(processPo p,gcSupportPo G);

/* The standard garbage collector invoked when a process runs out of 
   heap is a compacting garbage collector, O(n) in time and space
   although the space overhead can often be shared */


/* The first phase marks all the accessible objects in the process heap */

void initMasks(void)
{
  int i;

  for(i=0;i<CARDWIDTH;i++)
    masks[i] = 1<<i;		/* compute 2**i for i=0 to i=31 */
}

static void initMarkTable(long count,heapPo H,gcSupportPo G)
{  
  long i;
  long size = (count+CARDWIDTH-1)/CARDWIDTH;

  G->H = H;

  if(size>NumberOf(G->mrks)){
    G->marks = (cardMap*)malloc(size*sizeof(cardMap));
    G->mrkSize = size;
  }
  else{
    G->marks = &G->mrks[0];
    G->mrkSize = NumberOf(G->mrks);
  }
  
  for(i=0;i<G->mrkSize;i++)
    G->marks[i] = 0;
  G->nmarks = size;

  G->markStack = &G->mrkStack[0];
  G->markStackSize = NumberOf(G->mrkStack);
  G->markStackTop = 0;

  G->oldCreate = (ptrPo)H->create;
  G->oCnt = 0;
}

static void clearGcSupport(gcSupportPo G)
{
  if(G->marks!=&G->mrks[0])
    free(G->marks);

  if(G->Brk!=&G->Breaks[0])
    free(G->Brk);

  if(G->markStack!=&G->mrkStack[0])
    free(G->markStack);
}

static inline logical setMarkBit(gcSupportPo G,ptrPo x)
{   
  heapPo H = G->H;
  long add = x-(ptrPo)H->base;		/* the bit number to set */

  assert(inHeap(H,(objPo)x) && (add>>CARDSHIFT)<G->nmarks);

  if((G->marks[add>>CARDSHIFT]&masks[add&CARDMASK])==0){
    G->marks[add>>CARDSHIFT] |= masks[add&CARDMASK];
    return True;
  }
  else
    return False;
}

/* Find the previous marked object */
static ptrPo nextMarked(gcSupportPo G,ptrPo x)
{
  long add = x-(ptrPo)G->H->base;		/* the bit number to get */
  long limit = G->oldCreate-(ptrPo)G->H->base;

  assert((add>>CARDSHIFT)<=G->nmarks);
  
  while(add<limit && (G->marks[add>>CARDSHIFT]&masks[add&CARDMASK])==0)
    add++;
  return (ptrPo)G->H->base+add;
}

static void initBreakTable(gcSupportPo G,long size)
{  
  if(size>NumberOf(G->Breaks))
    G->Brk = G->endBrk = (breakPo)malloc(sizeof(breakEntry)*size);
  else
    G->Brk = G->endBrk = &G->Breaks[0];
  G->BrkSize = size;
}

static breakPo nextBrk(gcSupportPo G,ptrPo orig,ptrPo final)
{
  if(G->endBrk>=&G->Brk[G->BrkSize]){
    long count=G->endBrk-G->Brk;
    
    if(G->Brk==&G->Breaks[0]){
      long i;
      long nsize = NumberOf(G->Breaks)+(NumberOf(G->Breaks)>>1);
      breakPo nTbl = (breakPo)malloc(sizeof(breakEntry)*nsize);
      
      for(i=0;i<count;i++)
        nTbl[i]=G->Breaks[i];
      G->BrkSize = nsize;
      G->Brk = nTbl;
      G->endBrk = &G->Brk[count];
    }
    else{
      long nsize = G->BrkSize = G->BrkSize+(G->BrkSize>>1);
      breakPo nTbl = (breakPo)realloc(G->Brk,sizeof(breakEntry)*nsize);
      
      G->endBrk = &nTbl[count];
      G->Brk = nTbl;
      assert(nTbl!=NULL);
    }
  }
  
  G->endBrk->start = orig;
  G->endBrk->final = final;
  
  return G->endBrk++;
}

static ptrPo searchBreak(breakPo Brk,breakPo endBrk,ptrPo p)
{
  while(endBrk>=Brk){
    breakPo middle = &Brk[(endBrk-Brk)/2];

    if(middle->start>p){
      if(middle!=Brk)
	endBrk=middle;
      else
	return NULL;		/* couldnt find it in the table */
    }
    else if(middle->start<p){
      if((middle+1)->start>p)	/* pointer into a block */
	return middle->final+(p-middle->start);
      else if(middle!=Brk)
	Brk=middle;
      else
	return NULL;
    }
    else
      return middle->final;
  }
  return NULL;
}

static void shuffleHeap(gcSupportPo G,heapPo H);

void gcCollect(heapPo H,long amount)
{
  if(H==&globalHeap)
    globalGC(amount);
  else{
    long i;
    GCSupport GCSRec;
    gcSupportPo G = &GCSRec;

    initMarkTable(H->create-H->base,H,G);
    
#ifdef MEMTRACE
    if(traceMemory)
      verifyProc(H->owner);

    gcCount++;
#endif

    for(i=0;i<H->topRoot;i++)		/* mark the external roots */
      markPtr(G,H->roots[i]);

    markProcess(H->owner,G);
  
#ifdef MEMTRACE
    if(traceMemory){
      outMsg(logFile,"%d objects found in mark phase of %w\n",
	     G->oCnt,&H->owner->proc.thread);
      flushFile(logFile);
    }
#endif

    initBreakTable(G,G->oCnt+2);
    shuffleHeap(G,H);		/* compact heap structure, build break table */

    adjustProcess(H->owner,G);

    for(i=0;i<H->topRoot;i++)
      *H->roots[i] = adjustPtr(G,*H->roots[i]);
  
#ifdef MEMTRACE
    if(traceMemory)
      verifyProc(H->owner);
#endif

    if(H->end-H->create<=amount){
      if(extendStack(H->owner,2,3,amount)!=Ok)
	syserr("Unable to grow process heap");
    }

    clearGcSupport(G);
  }
}

/* Mark an object as being in use... */
static void markStruct(gcSupportPo G,objPo x)
{  
  if(setMarkBit(G,(ptrPo)x)){
    G->oCnt++;

    markScanObj(G,x);
  }
}

/* mark a cell -- by setting the appropriate bit in the card table */
static void clearMarkTable(gcSupportPo G)
{    
  heapPo H = G->H;

  while(G->markStackTop>0){
    ptrPo base = G->markStack[--G->markStackTop].base;
    long count = G->markStack[G->markStackTop].count;
      
    while(count-->0){
      ptrI vx = *base++;

      switch(ptg(vx)){
      case varTg:{
        objPo pp = objV(vx);

        if(inHeap(H,pp)){
	  if(isSuspVar((ptrPo)pp))
	    markStruct(G,(objPo)(pp-1));
	  else{
	    while(pp>H->base && !isObjBase((ptrPo)pp))
	      pp--;
	    markStruct(G,(objPo)pp);
          }
        }
        continue;
      }
      case objTg:{
        objPo pp = objV(vx);
        if(inHeap(H,pp))
          markStruct(G,pp);
        continue;
      }
      case fwdTg:			/* shouldn't happen */
        ;
      }
    }
  }
}

static void markPtr(gcSupportPo G,ptrPo x)
{
  if(x!=NULL){
    G->markStack[G->markStackTop].base = x;
    G->markStack[G->markStackTop++].count=1;

    clearMarkTable(G);
  }
}

void pushPtr(gcSupportPo G,ptrPo ptr,long count)
{
  if(G->markStackTop>=G->markStackSize){
    long nsize = G->markStackSize+(G->markStackSize>>2);
    
    if(G->markStack!=G->mrkStack)
      G->markStack = (MarkStackRec*)realloc(G->markStack,sizeof(MarkStackRec)*nsize);
    else{
      long i;
      G->markStack = (MarkStackRec*)malloc(sizeof(MarkStackRec)*nsize);
      
      for(i=0;i<G->markStackTop;i++)
        G->markStack[i]=G->mrkStack[i];
    }
    
    G->markStackSize = nsize;
  }
  
  G->markStack[G->markStackTop].base = ptr;
  G->markStack[G->markStackTop++].count=count;
}

static retCode markScanHelper(ptrPo arg,void *c)
{
  pushPtr((gcSupportPo)c,arg,1);
  return Ok;
}

static objPo markScanObj(gcSupportPo G,objPo x)
{
  if(IsSpecialClass(x->class)){
    specialClassPo sClass = (specialClassPo)classOf(x);
    sClass->scanFun(sClass,markScanHelper,G,x);
    return (objPo)(((ptrPo)x)+sClass->sizeFun(sClass,x));
  }
  else{
    assert(isTermClass((objPo)classOf(x)));

    ptrPo ptrData = objectArgs(x);
    long arity = objectArity(x);

    if(arity>0)
      pushPtr(G,ptrData,arity);

    return (objPo)(((ptrPo)x)+objectSize(x));
  }
}

static void markProcess(processPo P,gcSupportPo G)
{
  register callPo C = P->proc.C;
  register choicePo B = P->proc.B;
  register int len = envSize(P->proc.cPC);
  register int i;

  assert(G->H->owner==P);

#ifdef MEMTRACE
  if(traceMemory)
    outMsg(logFile,"Mark process %.3w\n",&P->proc.thread);
#endif

  for(i=1;i<=argArity(P->proc.PC);i++)
    markPtr(G,&P->proc.A[i]);

  while(B<(choicePo)P->proc.sTop || C<(callPo)P->proc.sTop){
    if((ptrPo)B<(ptrPo)C){	/* mark a choice point */
      register ptrPo A = (ptrPo)(B+1);
      register insPo cStart = FirstInstruction(B->cPROG);
      register insPo Start = FirstInstruction(B->PROG);

      for(i=0;i<B->AX;i++,A++){
	markPtr(G,A);
      }

      if(B->C<C && B->C<(callPo)P->proc.sTop){ 
	C = B->C;		/* the choice point's call back is newer */
	len=envSize(B->cPC);	/* do this before the next step */
      }

      assert(B->PC-Start>=0);
      assert(B->cPC-cStart>=0);
      B->PC = (insPo)(B->PC-Start);
      B->cPC = (insPo)(B->cPC-cStart);
        
      markPtr(G,&B->PROG);
      markPtr(G,&B->cPROG);
        
      B = B->B;			/*look at the previous choice point */
    }
    else{			/* mark a call environment */
      register ptrPo Y = (ptrPo)C;

      for(i=0;--Y,i<len;i++)
	markPtr(G,Y);
      
      len=envSize(C->cPC);	/* do this before the next step */

      assert(C->cPC-FirstInstruction(C->cPROG)>=0);

      C->cPC = (insPo)(C->cPC-FirstInstruction(C->cPROG));
      markPtr(G,&C->cPROG);

      C = C->cC;
    }
  }

  assert((ptrPo)C==P->proc.sTop && (ptrPo)B==P->proc.sTop);

  P->proc.PC = (insPo)(P->proc.PC-FirstInstruction(P->proc.PROG));
  P->proc.cPC = (insPo)(P->proc.cPC-FirstInstruction(P->proc.cPROG));
  markPtr(G,&P->proc.PROG);
  markPtr(G,&P->proc.cPROG);

  {
    register trailPo tr = P->proc.trail;

    while((ptrPo)tr-->P->proc.sBase){    /* mark all the trail entries */
      markPtr(G,(ptrPo)&tr->var);
      markPtr(G,&tr->val);
    }
  }

  markPtr(G,&P->proc.errorCode);

  markPtr(G,&P->proc.thread);
  markPtr(G,&P->proc.trigger);
}

/* 
   Second phase of the garbage collector -- copy the referenced block down,
   and build a break table showing where objects have moved to 
*/

static retCode helpAdjust(ptrPo arg,void *c);

static void shuffleHeap(gcSupportPo G,heapPo H)
{
  ptrPo next = (ptrPo)H->base;
  objPo scan = H->base;
  ptrPo heap = (ptrPo)H->base;
  ptrPo mark = (ptrPo)H->base;

  long i=0;

#ifdef MEMTRACE
  if(traceMemory)
    outMsg(logFile,"compacting process heap\n");
#endif

  for(i=0;i<G->nmarks;i++){
    if(G->marks[i]!=0){
      int j;

      for(j=0;j<CARDWIDTH;j++){
	if((G->marks[i]&masks[j])!=0){
	  ptrPo orig = heap+(i<<CARDSHIFT)+j;
	  
          if(orig>=mark){	   /* We only consider marks that are current */
            nextBrk(G,orig,next);
            assert(orig>=(ptrPo)H->base&&orig<(ptrPo)H->create);

            switch(ptg(*orig)){
            case varTg:{
              *next++=*orig++;
              mark = orig;              /* record the next entry to consider */
              break;		      /* we copy down a single tagged pointer */
	    }
	    case fwdTg:			/* should not happen */
	      syserr("forward cell in local heap");
	      break;

            case objTg:{
	      if(isObjBase(orig)){
		long size = objectSize((objPo)orig);
		int ix;
		for(ix=0;ix<size;ix++)
		  *next++ = *orig++;
		mark = orig;
	      }
	      else{
		assert(isSpecialObject((objPo)orig));

		specialClassPo sClass = sClassOf((objPo)orig);
		long size = sClass->sizeFun(sClass,(objPo)orig);
		long ix;

		for(ix=0;ix<size;ix++)
		  *next++ = *orig++;

		mark = orig;		/* record the next entry to consider */
	      }
	      break;
	    }
	    }
	  }
        }
      }
    }
  }

  nextBrk(G,(ptrPo)H->create,next);	/* sentinel marker just past the heap */
  nextBrk(G,(ptrPo)H->create,next);	/* double the sentinel marker */
  G->hpBase = (ptrPo)H->base;
  G->hpLimit = (ptrPo)H->create;
  G->endBrk--;

  /* Now that we have built the break table, we can adjust pointers */

  while(scan<(objPo)next){
    if(isObjBase((ptrPo)scan)){
      ptrPo scanner = objectArgs(scan);
      long arity = objectArity(scan);

      int ix;
      for(ix=0;ix<arity;ix++,scanner++)
	*scanner = adjustPtr(G,*scanner);

      scan = scan+objectSize(scan);
    }
    else{
      assert(isSpecialObject(scan));
      specialClassPo sClass = sClassOf(scan);
      sClass->scanFun(sClass,helpAdjust,G,scan);
      scan = scan+sClass->sizeFun(sClass,scan);
    }
  }
  H->create = (objPo)next;		/* new top of the heap */
}

static retCode helpAdjust(ptrPo arg,void *c)
{
  gcSupportPo G = (gcSupportPo)c;
  
  *arg = adjustPtr(G,*arg);
  return Ok;
}

ptrI adjustPtr(gcSupportPo G,ptrI cell)
{
  switch(ptg(cell)){
  case objTg:{
    ptrPo orig = (ptrPo)objV(cell);
    if(orig>=G->hpBase && orig<G->hpLimit){
      ptrPo final = searchBreak(G->Brk,G->endBrk,orig);
      if(final!=NULL)
        return objP(final);
    }
    return cell;		/* leave other pointers alone */
  }
  case varTg:{
    ptrPo orig = (ptrPo)objV(cell);

    if(orig>=G->hpBase && orig<G->hpLimit){
      ptrPo final = searchBreak(G->Brk,G->endBrk,orig);

      if(final!=NULL)
        return varP(final);
    }
    return cell;		/* leave other pointers alone */
  }
  default:
    syserr("Illegal pointer cell found");
    return cell;
  }
}


/* Third phase of garbage collection -- adjust all pointers */
static void adjustProcess(processPo p,gcSupportPo G)
{
  register heapPo H = G->H;
  register callPo C = p->proc.C;
  register choicePo B = p->proc.B;
  register int len;
  register int i;

  p->proc.PROG = adjustPtr(G,p->proc.PROG);
  p->proc.cPROG = adjustPtr(G,p->proc.cPROG);

  p->proc.PC = (insPo)((ptrI)p->proc.PC+FirstInstruction(p->proc.PROG));
  p->proc.cPC = (insPo)((ptrI)p->proc.cPC+FirstInstruction(p->proc.cPROG));

  len = envSize(p->proc.cPC);

  for(i=0;i<=argArity(p->proc.PC);i++)
    p->proc.A[i]=adjustPtr(G,p->proc.A[i]);

  p->proc.trigger = adjustPtr(G,p->proc.trigger);

  {
    trailPo tr = p->proc.trail;

    while((ptrPo)tr-->p->proc.sBase){
      tr->var = (ptrPo)adjustPtr(G,(ptrI)tr->var);
      tr->val = adjustPtr(G,tr->val);
    }
  }

  p->proc.thread = adjustPtr(G,p->proc.thread);
  p->proc.errorCode = adjustPtr(G,p->proc.errorCode);

  while(B<(choicePo)p->proc.sTop || C<(callPo)p->proc.sTop){
    if((ptrPo)B<(ptrPo)C){	/* mark a choice point */
      register ptrPo A = (ptrPo)(B+1);
      register insPo Start;

      B->PROG = adjustPtr(G,B->PROG);
      B->cPROG = adjustPtr(G,B->cPROG);
      
      /* We need to be a little careful when adjusting the H pointer */
      if(H->owner==p){
	ptrPo finalH = searchBreak(G->Brk,G->endBrk,(B->H>=(ptrPo)H->base && B->H<(ptrPo)H->end?nextMarked(G,B->H):B->H));
          
	if(finalH!=NULL)
	  B->H = finalH;
      }
        
      Start = FirstInstruction(B->PROG);
        
      for(i=0;i<B->AX;i++,A++){
	*A=adjustPtr(G,*A);
      }
        
      B->PC = (insPo)((ptrI)B->PC+Start);
      B->cPC = (insPo)((ptrI)B->cPC+FirstInstruction(B->cPROG));
        
      if(B->C<C && B->C<(callPo)p->proc.sTop){
	C = B->C;		     /* the choice point's call back is newer */
	len=envSize(B->cPC);		/* do this before the next step */
      }
      B = B->B;				/*look at the previous choice point */
    }
    else{			/* mark a call environment */
      register ptrPo Y = (ptrPo)C;

      for(i=0;--Y,i<len;i++){
	*Y = adjustPtr(G,*Y);
      }
      
      C->cPROG = adjustPtr(G,C->cPROG);
      C->cPC = (insPo)((ptrI)C->cPC+FirstInstruction(C->cPROG));
      len=envSize(C->cPC);	/* do this before the next step */

      //      cPC = C->cPC;
      C = C->cC;
    }
  }

  assert((ptrPo)C==p->proc.sTop && (ptrPo)B==p->proc.sTop);
}

