/*
   Global memory management of Go! engine
   (c) 2006 F.G.McCabe

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

#ifndef _GLOBAL_H_
#define _GLOBAL_H_

#include "heap.h"

extern HeapRec globalHeap;

typedef struct _global_gc_support_ {
  logical left;				/* Are we moving up or down? */
  heapPo fH;
  heapPo tH;
} globalGcRec;

static inline logical oldGeneration(objPo p)
{
  extern objPo globalSpace;
  extern objPo leftBase;
  return globalSpace<=p && p<leftBase;
}

static inline logical inGlobalHeap(const objPo p)
{
  return inHeap(&globalHeap,p) || oldGeneration(p);
}

extern void initGlobal(long size);
extern void globalGC(long request);

extern retCode reserveGlobalSpace(long size);
extern void markGrey(objPo p);
extern objPo permAllocate(long size);
extern objPo permObject(heapPo H,ptrI cls);
extern ptrI realPtr(ptrI X);

extern volatile logical globalGcRequest;
extern ptrI scanPtr(globalGcPo G,ptrI orig);

extern void lockGlobal(void);
extern void unlockGlobal(void);

extern rootPo globalRoot(ptrPo p,rootPo r);
extern void resetGlobalRoot(rootPo r);

#endif
