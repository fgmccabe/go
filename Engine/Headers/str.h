/*
 * Header file for  string management in the Go! engine
   (c) 2001-2007 F.G.McCabe

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

#ifndef _ENGINE_STR_H_
#define _ENGINE_STR_H_

extern retCode isGroundString(ptrPo s);
extern long StringLen(ptrPo a);
extern ptrI allocateString(heapPo H,uniChar *buff,long len);
extern ptrI allocateCString(heapPo H,const char *text);
extern retCode String2Uni(ptrPo P,uniChar *buff,long len);
extern retCode String2Utf8(ptrPo s,char *buff,long len);
extern retCode writeString(ioPo f,void* x,long depth,long prec,logical alt);
extern ptrI allocateByteList(heapPo P,byte *m,long len);

retCode closeOutString(ioPo f,heapPo P,ptrPo tgt);

#endif
