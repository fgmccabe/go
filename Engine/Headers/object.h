/* 
   Go Object related definitions for the Go! engine
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

#ifndef _GO_OBJECT_H_
#define _GO_OBJECT_H_

#include "lock.h"

/*
 * A dynamically created object
 */
typedef struct _object_record_ {
  ptrI sign;                            /* == symbolClass */
  ptrI props;                           /* The properties of the symbol */
  ptrI code;
  lockPo lock;				/* The mutex lock on the symbol */
  uinteger hash;			/* A hash for uniqueness */
} dynRec, *dynPo;

extern ptrI dynamicClass;		/* A dynamically created entity */

extern ptrI goObject(heapPo H,ptrI T);	/* create a Go! object */

static inline logical isGoObject(objPo p)
{
  return p->sign == dynamicClass;
}

static inline logical IsGoObject(ptrI x)
{
  return isGoObject(objV(x));
}

static inline dynPo goObjV(ptrI x)
{
  assert(IsGoObject(x));

  return (dynPo)objV(x);
}

#endif
