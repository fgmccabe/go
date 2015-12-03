/*
   Standard constant definitions for the Go Engine
   (c) 2000-2004 F.G.McCabe

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

#ifndef _GO_CONSTANTS_H_
#define _GO_CONSTANTS_H_

/* This is used for many internal fixed char buffers */
#ifndef MAX_SYMB_LEN
#define MAX_SYMB_LEN 1024
#endif

/* This is used for some internal stack buffers */
#ifndef MAX_DEPTH
#define MAX_DEPTH 512
#endif

/* This is used for some internal tables */
#ifndef MAX_TABLE
#define MAX_TABLE 2048
#endif

/* This is used for message buffers */
#ifndef MAX_MSG_LEN
#define MAX_MSG_LEN 1024
#endif

#endif
