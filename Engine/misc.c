/*
  Miscellaneous functions for the Go! engine
  (c) 2000 F.G. McCabe

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

  $Id: misc.c,v 1.3 2004/05/26 18:39:24 fmccabe Exp $
  $Log: misc.c,v $
  Revision 1.3  2004/05/26 18:39:24  fmccabe
  new ground test

  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system

*/

#include "config.h"		/* pick up standard configuration header */
#include <math.h>
#include <stdlib.h>
#include <limits.h>
#include <errno.h>		/* system error numbers */
#include <ctype.h>
#include <string.h>
#include "go.h"
#include "process.h"

/* General off-line unification */
retCode g_equal(processPo P,ptrPo a)
{
  return equal(P,deRef(&a[1]),deRef(&a[2]));
}

retCode g_match(processPo P,ptrPo a)
{
  return match(P,deRef(&a[1]),deRef(&a[2]));
}

retCode g_ident(processPo P,ptrPo a)
{
  if(identical(deRefI(&a[1]),deRefI(&a[2])))
    return Ok;
  else
    return Fail;
}

retCode g_var(processPo P,ptrPo a)
{
  ptrI xx = deRefI(&a[1]);

  if(isvar(xx) || IsFrozenVar(xx))
    return Ok;
  else
    return Fail;
}

retCode g_nonvar(processPo P,ptrPo a)
{
  ptrI xx = deRefI(&a[1]);

  if(isvar(xx))
    return Fail;
  else
    return Ok;
}

retCode g_ground(processPo P,ptrPo a)
{
  ptrI xx = deRefI(&a[1]);

  if(isGroundTerm(&xx))
    return Ok;
  else
    return Fail;
}

