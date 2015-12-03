/* 
   Statistics collection interface
   (c) 2000 F.G.McCabe

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
#ifndef _STATS_COLLECT_H_
#define _STATS_COLLECT_H_

#include "logical.h"		/* import a definition of true and false */
#include "integer.h"
#include "retcode.h"
#include "opcodes.h"		/* The definitions of the opcodes */

extern logical traceCount;	/* Are we counting instructions? */
extern long pcCount;		/* number of instructions executed */

extern void countEscape(insWord PCX);
void dumpInsCount(void);


#endif /* _STATS_COLLECT_H_ */


