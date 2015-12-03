/*
 * Header file for the heap management of Go! engine
   (c) 2001-2007 F.G.McCabe

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

#ifndef _HEAP_H_
#define _HEAP_H_

#ifndef MAXROOT
#define MAXROOT 128
#endif

typedef long rootPo;

typedef struct _heap_rec_ {
  objPo base;				/* base of the heap */
  objPo end;				/* end of the heap */
  objPo create;			   /* Where to create the next heap structure */
  processPo owner;			/* This is NULL for the global heap */
  ptrPo rts[MAXROOT];
  ptrPo *roots;
  long topRoot;
  long maxRoot;
} HeapRec;

extern HeapRec globalHeap;

typedef struct _gc_support_ *gcSupportPo;
typedef struct _global_gc_support_ *globalGcPo;

#ifndef CARDSHIFT
#define CARDSHIFT 5		/* 32 bits in a long */
#define CARDWIDTH  (1<<CARDSHIFT)
#define CARDMASK  (CARDWIDTH-1)
#endif

typedef unsigned long cardMap;

extern cardMap masks[CARDWIDTH];
extern void initMasks(void);

extern long initStackHeapSize;

extern void setupHeap(heapPo H,processPo owner,long size);

void gcCollect(heapPo P,long amount);

void pushPtr(gcSupportPo G,ptrPo x,long count);
ptrI adjustPtr(gcSupportPo G,ptrI cell);

static inline logical inHeap(heapPo P,const objPo x)
{
  return x>=P->base && x<P->create;
}

/* Root management */
extern void growRoots(heapPo H);

extern inline rootPo gcAddRoot(heapPo H,ptrPo ptr)
{
  assert(H!=&globalHeap);
  if(H->topRoot==H->maxRoot)
    growRoots(H);

  int R = H->topRoot++;

  H->roots[R] = ptr;

  return R;
}

static inline rootPo gcCurrRoot(heapPo H)
{
  return H->topRoot;
}

static inline void gcRemoveRoot(heapPo H,rootPo mk)
{
  H->topRoot=mk;
}

static inline objPo allocSpace(heapPo P,size_t size)
{
  objPo new;

  if(P->create+size>P->end)
    return NULL;		/* allow caller to invoke GC */

  new = (objPo)P->create;
  P->create+=size;

  return new;
}

extern retCode reserveSpace(heapPo P,size_t size);

static inline objPo allocate(heapPo H,size_t size)
{

#ifdef MEMTRACE
  if(stressMemory)
    gcCollect(H,size);		/* gc on every allocation */
#endif

  if(H->create+size>H->end)
    gcCollect(H,size);		/* this aborts if there is no memory */

  {
    register objPo new = H->create;
    H->create+=size;

    return new;
  }
}

static inline objPo allocateObject(heapPo H,ptrI class)
{
  assert(isClass(class));

  objPo o = allocate(H,((clssPo)objV(class))->arity+1);
  o->class = class;
  return o;
}

static inline objPo allocateSpecial(heapPo H,ptrI class)
{
  assert(IsSpecialClass(class));

  specialClassPo sClass = (specialClassPo)objV(class);

  objPo o = allocate(H,sClass->sizeFun(sClass,NULL));
  o->class = class;
  return o;
}

static inline long spaceLeft(heapPo H)
{
  return H->end-H->create;
}

static inline long totalHeapSize(heapPo H)
{
  return H->end-H->base;
}

retCode localCopy(ptrPo dst,heapPo H,ptrPo src);
retCode freezeTerm(heapPo H,ptrPo dst,ptrI src,uniChar *eMsg,long len);
logical isGroundTerm(ptrPo p);
void markStandardClasses(globalGcPo G);

#ifdef MEMTRACE
void verifyAllProcesses(void);
void verifyHeap(heapPo P);
void verifyProc(processPo p);
#endif

#ifdef EXECTRACE
void verifyVar(ptrPo ptr,processPo P);
#endif

#endif
