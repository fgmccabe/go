/* 
  Spinlock interface
  (c) 1994-2002 Imperial College and F.G.McCabe

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

#ifndef _LOCK_H_
#define _LOCK_H_

#include "word.h"

typedef struct {
  long count;				/* The lock recursion count */
  pthread_t owner;			/* The current owner of the lock */
  pthread_mutex_t mutex;		/* The mutex itself */
  pthread_cond_t cond;			/* Condition variable */
} GoLock, *lockPo;

lockPo newLock(void);

retCode acquireLock(lockPo l,number tmOut);
retCode releaseLock(lockPo l);
retCode waitLock(lockPo l,number tmOut);

#endif
