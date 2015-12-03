/* 
   List related definitions for the Go! engine
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


#ifndef _ENGINE_LIST_H_
#define _ENGINE_LIST_H_

#include "word.h"

extern ptrI emptyList;			/* The class constructor "go.stdlib#[]" */
extern ptrI listClass,nilClass;

static inline logical isList(objPo p)
{
  return hasClass(p,listClass);
}

static inline logical IsList(ptrI x)
{
  return isobj(x) && isList(objV(x));
}

static inline ptrPo listHead(objPo l)
{
  assert(isList(l));

  return objectArgs(l);
}

static inline ptrPo listTail(objPo l)
{
  assert(isList(l));

  return objectArgs(l)+1;
}

extern long ListLen(ptrI l);

extern ptrI permLsPair(heapPo H,ptrI head,ptrI tail);

/*
 * Alternate implementation of list pairs, based on a different pointer tag
 */

static inline ptrI consLsPair(heapPo P,ptrI head,ptrI tail)
{
  rootPo root = gcAddRoot(P,&head);

  gcAddRoot(P,&tail);

  {
    objPo new = allocateObject(P,listClass);

    ptrPo data = objectArgs(new);

    data[0] = head;
    data[1] = tail;

    gcRemoveRoot(P,root);

    return objP(new);
  }
}

#endif

