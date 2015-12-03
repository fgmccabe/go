/* 
   Number related definitions for the Go! engine
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


#ifndef _ENGINE_FLOAT_H_
#define _ENGINE_FLOAT_H_

#include "word.h"
#include <string.h>			/* access template for memcpy */

#ifndef PI
// Define PI to 50 decimal places ....
#define	PI	3.14159265358979323846264338327950288419716939937510
#endif

typedef struct _integer_record_ {
  ptrI class;			/* == integerClass */
  integer i;
} integerRec, *integerPo;

extern ptrI integerClass;		/* integerClass is a specialClass */

static inline ptrI allocateInteger(heapPo P,integer i)
{
  integerPo new = (integerPo)allocateSpecial(P,integerClass);

  memcpy(&new->i,&i,sizeof(integer));
  return objP(new);
}

static inline logical IsInt(ptrI p)
{
  return HasClass(p,integerClass);
}

static inline logical isInteger(objPo p)
{
  return hasClass(p,integerClass);
}

static inline integerPo intV(ptrI x)
{
  assert(IsInt(x));
  return (integerPo)objV(x);
}

static inline integer integerVal(integerPo p)
{
#ifdef DOUBLE_ALIGNMENT
  integer i;

  memcpy(&i,&p->i,sizeof(integer));
  return i;
#else
  return p->i;
#endif
}

static inline integer IntVal(ptrI p)
{
  assert(IsInt(p));
  return integerVal(intV(p));
}

extern ptrI permInteger(integer i);

typedef struct _float_record_ {
  ptrI class;				/* == floatClass */
  number f;
} floatRec, *floatPo;

extern ptrI floatClass;

static inline logical isFloat(objPo p)
{
  return hasClass(p,floatClass);
}

static inline floatPo floatV(ptrI x)
{
  assert(HasClass(x,floatClass));
  return (floatPo)objV(x);
}

static inline number floatVal(floatPo p)
{
  assert(isFloat((objPo)p));

#ifdef DOUBLE_ALIGNMENT
  number f;
  memcpy(&f,&p->f,sizeof(number));
  return f;
#else
  return p->f;
#endif
}

static inline logical IsNumber(objPo p)
{
  return isFloat(p)||isInteger(p);
}

static inline number NumberVal(objPo p)
{
  if(isInteger(p))
    return (number)integerVal((integerPo)p);
  else 
    return floatVal((floatPo)p);
}

static inline number roundNumber(register number f)
{
  if(f>=0.0)
    return floor(f);
  else
    return ceil(f);
}

static inline ptrI allocateFloat(heapPo P,double f)
{
  floatPo new;

  new = (floatPo)allocateSpecial(P,floatClass);

  memcpy(&new->f,&f,sizeof(double));
  return objP(new);
}

extern ptrI permFloat(double f);

static inline ptrI allocateNumber(heapPo P,double f)
{
  integer n = (integer)f;
  
  if((double)n==f)
    return allocateInteger(P,n);
  else
    return allocateFloat(P,f);
}

void initArithClasses(void);
#endif
