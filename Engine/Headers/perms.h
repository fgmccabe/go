/*
  Permissions handling header for the Go! run-time system
  (c) 1994-1998 Imperial College and F.G. McCabe

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
#ifndef _PERMS_H_
#define _PERMS_H_

#include "config.h"		/* pick up standard configuration header */
#include "go.h"			/* Main header file */

logical inPermissions(ptrI perms,ptrI perm);
ptrI permitedAmount(ptrI perms,ptrI perm);
ptrI defaultPerms(ptrI baseURI);

ptrI generateKey(char *prefix);
ptrI generateUniKey(uniChar *prefix);

extern ptrI
  pFORK,			// Permission to fork
  pIO,				// Permission to perform IO
  pSENDMSG,			// Permission to send messages
  pRECVMSG,			// Permission to receive messages
  pFILE,			// Permission to perform file operations
  pROOTURL,			// Permission to access root URL
  pIOKEY;			// Key for accessing the file system

#endif
