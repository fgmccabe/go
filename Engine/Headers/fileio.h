/* 
   File I/O part of the I/O library
   (c) 1994-2000 Imperial College and F.G. McCabe

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

#ifndef _FILEIO_H_
#define _FILEIO_H_

void initFileIo(void);
void initFiles(void);
void setUpAsyncIO(int fd);

extern uniChar goSysPath[];		/* Go! installation point */

ioPo openSocketFile(char *name,int sock);
retCode filePerms(char *file,unsigned long *mode);
logical executableFile(char *file);
ioEncoding pickEncoding(ptrI k);

ioPo filePtr(ptrI p);
ptrI allocFilePtr(ioPo file);

retCode load_code_file(uniChar *name,ptrPo tgt);

typedef enum { input, output } ioMode;
 
retCode attachProcessToFile(ioPo f,processPo p,ioMode mode);
void detachProcessFromFile(ioPo f,processPo p);
void detachProcessFromIo(processPo p);

#include <sys/time.h>
int set_in_fdset(fd_set *set);
int set_out_fdset(fd_set *set);
void trigger_io(fd_set *inset,fd_set *outset,int max);

#endif
