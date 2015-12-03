/* 
   Variable related definitions for the Go! engine
   (c) 2000-2007 F.G.McCabe

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

#ifndef _ENGINE_VARS_H_
#define _ENGINE_VARS_H_

#include "word.h"

/* Dynamic variable, not often used because most variables are in the ptr */
typedef struct _variable_record_ {
  ptrI class;			/* == varClass */
  ptrI val;			/* value to which variable is bound */
} variableRec, *variablePo;

extern ptrI varClass;

static inline ptrPo deRef(register ptrPo p)
{
  do{
    register ptrI q = *p;

    if(!isvar(q) || (ptrPo)q==p)
      return p;
    p = (ptrPo)q;
  } while(True);
}

static inline ptrI deRefI(register ptrPo p)
{
  do{
    register ptrI q = *p;

    if(!isvar(q))
      return q;
    else if((ptrPo)q==p)
      return q;
    p = (ptrPo)q;
  } while(True);
}

//#define deRefI(p) (!isvar(*p) ? *p : dRfI(p))

static inline ptrI mkvar(ptrPo p)
{
  return (ptrI)p;
}

static inline logical IsFrozenVar(ptrI x)
{
  return isobj(x) && HasClass(x,varClass);
}

extern logical notRecentVar(ptrPo vx);

#define VariableCellCount CellCount(sizeof(variableRec))

static inline ptrI allocateVar(heapPo H)
{
  variablePo new = (variablePo)allocate(H,VariableCellCount);
  ptrPo var = &new->val;

  new->class = varClass;
  new->val = (ptrI)var;		/* make new variable unbound */

  return (ptrI)var;
}

// Suspension record

#define SuspensionCellCount CellCount(sizeof(suspensionRec))
#define SuspensionMark objectMark(suspensionKey,SuspensionCellCount)

typedef struct _susp_record_ {
  ptrI class;                            /* = suspClass */
  ptrI var;                             /* variable that is suspended */
  ptrI key;				/* A copy of the sentinel for a suspension record */
  ptrI goal;                            /* goal structure to activate on suspension */
} suspensionRec, *suspensionPo;

extern ptrI suspClass;

static inline logical IsSusp(ptrI x)
{
  return HasClass(x,suspClass);
}

static inline logical isSuspVar(ptrPo x)
{
  return x[1]==suspClass && x[-1]==suspClass;
}


static inline ptrI allocateSusp(heapPo H,ptrI goal)
{
  rootPo root = gcAddRoot(H,&goal);
  suspensionPo new = (suspensionPo)allocate(H,SuspensionCellCount);
  ptrPo var = &new->var;
  ptrI susp = objP(new);

  new->class = new->key = suspClass;
  new->var = (ptrI)var;		/* make new suspension's variable unbound */
  new->goal = emptyList;

  gcAddRoot(H,&susp);

  new->goal = consLsPair(H,goal,emptyList);

  gcRemoveRoot(H,root);
  return (ptrI)var;			/* looks like a variable ... until you try to bind it */
}

#endif
