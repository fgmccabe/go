/* 
  Interface for dynamically loaded code
  (c) 1994-2003 Imperial College and F.G.McCabe

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

#ifndef _DLL_H_
#define _DLL_H_

#include "go.h"

typedef struct {
  void *handle;			/* The dl handle */
  uniChar *sig;			/* The signature key of the module */
  long sigLen;			/* The length of the signature */
  long modLen;			/* How many functions are exported? */
  struct {
    funpo fun;			/* The escape function itself */
    int ar;			/* Its arity */
  } funs[];			/* The array of escape functions being */
} SignatureRec, *SignaturePo;

#endif
