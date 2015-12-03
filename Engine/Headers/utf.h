/*
   Interface for the UTF/unicode conversion functions 
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

#ifndef _UTF_8_H_
#define _UTF_8_H_

#include "config.h"
#include "go.h"

uniChar nChar(ioPo f);
retCode pChar(ioPo f,uniChar c);
retCode outUni(ioPo f,uniChar *s);
retCode uniMsg(ioPo f,void *data,int width,int precision,logical alt);
int utf2uni(const char *str,uniChar *buff,long len);
char *uni2utf(uniChar *s,char *buff,long len);
int uniStrLen(uniChar *s);
uniChar *uniCat(uniChar *dest,long len,const uniChar *src);
uniChar *uniCpy(uniChar *dest,long len,const uniChar *src);
uniChar *uniTack(uniChar *dest,long len,const char *src);
int uniCmp(uniChar *s1,uniChar *s2,long l);
int uniNCmp(uniChar *s1,uniChar *s2,long l);

#endif
