/*
  List handling functions for Go!
  (c) 1994-2000 Imperial College & F.G. McCabe

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

  $Id: list.c,v 1.2 2004/04/29 16:24:27 fmccabe Exp $
  $Log: list.c,v $
  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system

*/

#include "config.h"
#include <string.h>		/* Access string defs */
#include <stdlib.h>		/* Memory allocation etc. */
#include <assert.h>		/* Run-time predicate verification */

#include "go.h"
#include "heap.h"
#include "list.h"

ptrI permLsPair(heapPo H,ptrI head,ptrI tail)
{
  rootPo root = gcAddRoot(H,&head);

  gcAddRoot(H,&tail);

  objPo new = permObject(H,listClass);

  new->args[0] = head;
  new->args[1] = tail;

  gcRemoveRoot(H,root);

  return objP(new);
}

long ListLen(ptrI xx)
{
  long count = 0;

  while(IsList(xx)){
    xx = deRefI(listTail(objV(xx)));
    count++;
  }

  if(identical(xx,emptyList))
    return count;
  else
    return -count;
}

