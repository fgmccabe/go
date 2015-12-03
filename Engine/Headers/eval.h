/* 
   Main memory layout definitions for the Go! engine
   (c) 2000 F.G.McCabe

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
#ifndef _ENGINE_EVAL_H_
#define _ENGINE_EVAL_H_

#include "logical.h"		/* import a definition of true and false */
#include "integer.h"
#include "retcode.h"
#include <math.h>
#include <assert.h>
#include "process.h"
#include "opcodes.h"

#ifdef EXECTRACE
#include "stats.h"
#endif

retCode equal(processPo P,ptrPo T1,ptrPo T2);
retCode unifyType(processPo P,ptrPo T1,ptrPo T2);
logical identical(ptrI T1,ptrI T2);
comparison compTerm(ptrPo T1,ptrPo T2);
retCode match(processPo P,ptrPo T1,ptrPo T2);
retCode testmatch(ptrPo T1,ptrPo T2);

typedef enum {readMode,writeMode,dummyMode} rwmode;

extern void chainSuspension(processPo P,ptrPo var);

static inline void bindVar(processPo P,ptrPo ptr,ptrI val)
{
  assert((ptr>=(ptrPo)P->proc.heap.base && ptr<(ptrPo)P->proc.heap.create) ||
	 (ptr>=(ptrPo)P->proc.sBase && ptr<(ptrPo)P->proc.sTop));

  if(((void*)P->proc.B<(void*)P->proc.T?
      (ptr<(ptrPo)P->proc.B->H):
      ptr<P->proc.T->H)
     ||ptr>(ptrPo)P->proc.B){
    P->proc.trail->var=ptr;
    P->proc.trail->val=*ptr;
    P->proc.trail++;
  }
  if(isSuspVar(ptr))
    chainSuspension(P,ptr);

  *ptr=val;
}

static inline void bndVar(processPo P,ptrPo ptr,ptrI val)
{
  assert((ptr>=(ptrPo)P->proc.heap.base && ptr<(ptrPo)P->proc.heap.create) ||
	 (ptr>=(ptrPo)P->proc.sBase && ptr<(ptrPo)P->proc.sTop));

  if(((void*)P->proc.B<(void*)P->proc.T?
      (ptr<(ptrPo)P->proc.B->H):
      ptr<P->proc.T->H)
     ||ptr>(ptrPo)P->proc.B){
    P->proc.trail->var=ptr;
    P->proc.trail->val=*ptr;
    P->proc.trail++;
  }

  *ptr=val;
}

static inline ptrI unBind(ptrPo x)
{
  return *x = (ptrI)x;
}

static inline short int envSize(insPo pc)
{
  assert(op_cde(*pc)==gcmap);
  return op_o_val(*pc);
}

static inline short int carefulEnv(insPo pc)
{
  switch(op_cde(*pc)){
  case escape:
  case kawl:
  case kawlO:
    return envSize(pc+1);
  default:
    return envSize(pc);
  }
}
    

static inline short int argArity(insPo pc)
{
  assert(op_cde(*pc)==gcmap||op_cde(*pc)==escape||op_cde(*pc)==kawl||op_cde(*pc)==kawlO||op_cde(*pc)==trycl||op_cde(*pc)==tryme || op_cde(*pc)==gc || op_cde(*pc)==alloc);
  return op_h_val(*pc);
}

retCode raiseError(processPo P,uniChar *name,ptrI code);
retCode raiseException(processPo P,ptrI exc);
void recoverFromException(processPo P);

void init_args(char **argv, int argc, int start);

static inline retCode funResult(processPo P,ptrPo args,int which,ptrI value)
{
  rootPo root = gcAddRoot(&P->proc.heap,&value);
  retCode ret = equal(P,&value,&args[which]);
  gcRemoveRoot(&P->proc.heap,root);
  return ret;
}

// Define the event flags
#ifndef SUSP_ACTIVE
#define SUSP_ACTIVE (1)                 /* activated suspension */
#define INT_ACTIVE (2)                  /* interrupt request */
#define GC_ACTIVE (4)                   /* gc request */
#endif

#endif


