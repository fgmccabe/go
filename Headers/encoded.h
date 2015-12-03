/*
  Term encoding format definitions
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

*/

#ifndef _TERM_ENCODED_H_
#define _TERM_ENCODED_H_

typedef enum {trmVar=0x00, trmInt=0x10, trmFlt=0x20, trmNegFlt=0x30,
              trmSym=0x40, trmChar=0x50, trmString=0x60, trmCode=0x70,
	      trmClass=0x80, trmObject=0x90, trmProgram=0xa0,
	      trmTag=0xb0, trmRef=0xc0, trmShort=0xd0} trmElTag;

#endif
