/*
  Number handling functions for Go!
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

  $Id: float.c,v 1.2 2004/04/29 16:24:27 fmccabe Exp $
  $Log: float.c,v $
  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system

*/

#include "config.h"
#include <string.h>		/* Access string defs */
#include <stdlib.h>		/* Memory allocation etc. */
#include <assert.h>		/* Run-time predicate verification */

#include "go.h"
#include "heap.h"
#include "floats.h"

ptrI permInteger(integer i)
{
  return allocateInteger(&globalHeap,i);
}

ptrI permFloat(double f)
{
  return allocateFloat(&globalHeap,f);
}

