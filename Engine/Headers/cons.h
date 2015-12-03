/* 
   Constructor related definitions for the Go! engine
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


#ifndef _ENGINE_CONS_H_
#define _ENGINE_CONS_H_

#include "word.h"

/*
 * Constructor term
 */

typedef struct _cons_record_ {
  ptrI sign;			/* == class specific Mark */
  ptrI data[ZEROARRAYSIZE];
} consRec, *consPo;

static inline consPo consV(ptrI x)
{
  return (consPo)(x&PTR_MASK);
}

static inline ptrPo ConsEl(consPo p,long n)
{
  assert(objectArity((objPo)p)>n);

  return &p->data[n];
}

static inline void updateCons(consPo cns,int el,ptrI val)
{
  assert(objectArity((objPo)cns)>el);

  cns->data[el]=val;
}

static inline ptrPo FirstConsEl(consPo cons)
{
  return cons->data;
}

extern ptrI commaClass;

#endif /* _ENGINE_CONS_H_ */

