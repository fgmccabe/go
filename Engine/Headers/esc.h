/* 
   Interface to the escape management functions
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


#ifndef _ENGINE_ESC_H_
#define _ENGINE_ESC_H_

#include "word.h"

void install_escapes(void);
funpo escapeCode(unsigned int code);
char *escapeName(int code);
int escapeOpcode(char *name);
funpo getescape(symbPo name);
logical validEscape(unsigned int code,int arity);
void showEscape(processPo P,long code,ptrPo args,long arity);
logical validEscape(unsigned int code,int arity);
void ScanEscapes(void);

void showCall(processPo P,ptrI prog,ptrPo args,long arity);
void showOCall(processPo P,ptrPo obj,ptrPo call,ptrPo this);

#endif
