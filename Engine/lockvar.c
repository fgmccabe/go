/* 
   Lock synchronization functions
   (c) 2002-2006 F.G. McCabe

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

  $Id: lockvar.c,v 1.2 2004/04/29 16:24:27 fmccabe Exp $
  $Log: lockvar.c,v $
  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system

*/ 

#include "config.h"		/* pick up standard configuration header */
#include <string.h>
#include <stdlib.h>
 
#include <assert.h>
#include <limits.h>
#include <errno.h>

#include "go.h"
#include "process.h"
#include "dict.h"
#include "symbols.h"
#include "str.h"
#include "lock.h"
#include "clock.h"
#include "term.h"
#include "timer.h"

#define NANO (1000000000)

// We are having to do this somewhat clumsily because not all systems 
// support pthread_mutex_timedlock (posix 1.d) including mac os x :(
// So we use a condition variable to signal that a lock is available

retCode acquireLock(lockPo l,number tmOut)
{
#ifdef LOCKTRACE
  if(traceLock)
    outMsg(logFile,RED_ESC_ON "acquire lock (%f) [%d] on 0x%x"RED_ESC_OFF"\n%_",tmOut,l->count,l);
#endif

  if(pthread_mutex_lock(&l->mutex))	/* This will be transitory */
    syserr("problem in acquiring lock mutex");

 again:
  if(l->count==0){
    l->owner = pthread_self();
    l->count = 1;
    if(pthread_mutex_unlock(&l->mutex))
      syserr("problem in releasing lock mutex");
    return Ok;
  }
  else if(pthread_equal(l->owner,pthread_self())){
    l->count++;

    if(pthread_mutex_unlock(&l->mutex))
      syserr("problem in releasing lock mutex");
    return Ok;
  }
  else if(tmOut==0.0){			/* treat as a no-timeout lock */
#ifdef LOCKTRACE
    if(traceLock)
      outMsg(logFile,RED_ESC_ON "getLock cond_wait on 0x%x"RED_ESC_OFF"\n%_",l);
#endif
    
    if(pthread_cond_wait(&l->cond,&l->mutex))
      syserr("problem in waiting");
    goto again;				/* try to lock again */
  }
  else{
    struct timespec tm;
    double seconds;
    double fraction = modf(tmOut,&seconds);

    tm.tv_sec=(long)seconds;
    tm.tv_nsec=(long)(fraction*NANO);	// Convert microseconds to nanoseconds
    switch(pthread_cond_timedwait(&l->cond,&l->mutex,&tm)){
    case 0:
      goto again;
    case ETIMEDOUT:
      pthread_mutex_unlock(&l->mutex);
      return Fail;
    default:
      pthread_mutex_unlock(&l->mutex);
      return Error;
    }
  }
}

retCode releaseLock(lockPo l)
{
  if(pthread_mutex_lock(&l->mutex))	/* This will be transitory */
    syserr("problem in acquiring lock mutex");
  retCode ret = Ok;

#ifdef LOCKTRACE
  if(traceLock)
    outMsg(logFile,RED_ESC_ON "releaseLock [%d] on 0x%x"RED_ESC_OFF"\n%_",l->count,l);
#endif

  if(pthread_equal(l->owner,pthread_self())){
    if(l->count>0){
      l->count--;

      if(l->count<=0){
	l->count = 0;
	l->owner = NULL;
	pthread_cond_broadcast(&l->cond);
      }
    }
  }
  else if(l->count!=0){
#ifdef LOCKTRACE
    if(traceLock)
      outMsg(logFile,RED_ESC_ON "tried to release non-owned lock 0x%x"RED_ESC_OFF"\n%_",l);
#endif

    ret = Fail;
  }

  if(pthread_mutex_unlock(&l->mutex))
    syserr("problem in releasing lock");
  return ret;
}

// waitLock releases a lock and puts the client on the lock's wait queue
retCode waitLock(lockPo l,number tmOut)
{
#ifdef LOCKTRACE
  if(traceLock){
    outMsg(logFile,RED_ESC_ON "waitLock on 0x%x"RED_ESC_OFF"\n%_",l);
  }
#endif
  retCode ret = Ok;

  if(pthread_mutex_lock(&l->mutex))	/* This will be transitory */
    syserr("problem in acquiring lock mutex");

  if((pthread_equal(l->owner,pthread_self()) && l->count==1) ||
     l->count==0){
    l->count = 0;
    l->owner = NULL;		/* the equivalent of unlocking */

    if(tmOut==0.0){			/* treat as a no-timeout wait */
#ifdef LOCKTRACE
      if(traceLock){
	outMsg(logFile,RED_ESC_ON "waiting on 0x%x"RED_ESC_OFF"\n%_",l);
      }
#endif

      if(pthread_cond_broadcast(&l->cond))
	syserr("problem in broadcast");
	
      if(pthread_cond_wait(&l->cond,&l->mutex))
	syserr("problem in condwait");
      ret = Ok;
    }
    else{
      struct timespec tm;
      double seconds;
      double fraction = modf(tmOut,&seconds);

#ifdef LOCKTRACE
      if(traceLock)
	outMsg(logFile,RED_ESC_ON "waiting for %f seconds"RED_ESC_OFF"\n%_",seconds);
#endif
      tm.tv_sec=(long)seconds;
      tm.tv_nsec=(long)(fraction*NANO);	/* convert fractions to nanoseconds */

      int unixRet = 0;
      switch(unixRet=pthread_cond_timedwait(&l->cond,&l->mutex,&tm)){
      case 0:
	ret = Ok;
	break;			      /* somewhere else we will try to relock */
      case ETIMEDOUT:
	ret = Fail;
	break;
      default:
#ifdef LOCKTRACE
	if(traceLock)
	  outMsg(logFile,RED_ESC_ON "error on wait %d"RED_ESC_OFF"\n%_",unixRet);
#endif

	ret = Error;
	break;
      }
    }
  }
  else{
#ifdef LOCKTRACE
    if(traceLock)
      outMsg(logFile,RED_ESC_ON "0x%x locked more than once"RED_ESC_OFF"\n%_",l);
#endif

    ret = Error; /* not allowed to wait on a lock that is locked more than once */
  }

  pthread_mutex_unlock(&l->mutex);
  return ret;
}

lockPo newLock(void)
{
  lockPo l = (lockPo)malloc(sizeof(GoLock));

  l->owner = NULL;
  l->count = 0;

  pthread_mutexattr_t attr;

  pthread_mutexattr_init(&attr);
#ifdef TRACELOCK
  pthread_mutexattr_settype(&attr,PTHREAD_MUTEX_ERRORCHECK);
#else
  pthread_mutexattr_settype(&attr,PTHREAD_MUTEX_NORMAL);
#endif

 mtxInit:
  switch(pthread_mutex_init(&l->mutex,&attr)){
  case 0:
    break;
  case EINVAL:
    syserr("cannot init");
  case ENOMEM:
    syserr("no memory");
  case EAGAIN:
    goto mtxInit;
  default:
    syserr("!!mutex init");
  }

  pthread_mutexattr_destroy(&attr);

 again:
  switch(pthread_cond_init(&l->cond,NULL)){
  case 0:
    return l;
  case EAGAIN:
    goto again;
  default:
    syserr("cannot init lock");
    return l;
  }
}

retCode g_acquireLock(processPo P,ptrPo a)
{
  ptrI A1 = deRefI(&a[1]);
  ptrI A2 = deRefI(&a[2]);              // Second argument is a timeout value
  objPo o2 = objV(A2);
  
  if(isvar(A2))
    return liberror(P,"__acquireLock",eINSUFARG);
  else if(!IsNumber(o2))
    return liberror(P,"__acquireLock",eNUMNEEDD);
  else{
    number tmOut = NumberVal(o2);

    if(isvar(A1))
      return liberror(P,"__acquireLock",eINSUFARG);

    rootPo root = gcAddRoot(&P->proc.heap,&A1);
    gcAddRoot(&P->proc.heap,&A2);

    dynPo lk = goObjV(A1);		/* pick up the lock-carrying object */

    assert(lk->lock!=NULL);

    switchProcessState(P,wait_lock);

#ifdef LOCKTRACE
    if(traceLock)
      outMsg(logFile,RED_ESC_ON "%w: getting lock 0x%x on %w"RED_ESC_OFF"\n%_",&P->proc.thread,lk->lock,&a[1]);
#endif
    retCode ret = acquireLock(lk->lock,tmOut);
    setProcessRunnable(P);

    gcRemoveRoot(&P->proc.heap,root);

    switch(ret){
    case Ok:
#ifdef LOCKTRACE
      if(traceLock)
	outMsg(logFile,RED_ESC_ON "%w: Locked %w"RED_ESC_OFF"\n%_",&P->proc.thread,&a[1]);
#endif
      return Ok;
    case Fail:    // Could not acquire the lock and timeout is not established
#ifdef LOCKTRACE
      if(traceLock)
	outMsg(logFile,RED_ESC_ON "%w: Lock on %w failed"RED_ESC_OFF"\n%_",&P->proc.thread,&a[1]);
#endif
      return Fail;
    case Error:
    default:
#ifdef LOCKTRACE
      if(traceLock)
	outMsg(logFile,RED_ESC_ON "%w: problem with waitlock on %w"RED_ESC_OFF"\n%_",&P->proc.thread,&a[1]);
#endif
      return liberror(P,"__acquireLock",eDEAD);
    }
  }
}
        
retCode g_waitLock(processPo P,ptrPo a)
{
  ptrI A1 = deRefI(&a[1]);
  ptrI A2 = deRefI(&a[2]);              // Second argument is a timeout value
  objPo o2 = objV(A2);

  if(isvar(A1))
    return liberror(P,"__waitLock",eINSUFARG);

  if(isvar(A2))
    return liberror(P,"__waitLock",eINSUFARG);
  else if(!IsNumber(o2))
    return liberror(P,"__waitLock",eNUMNEEDD);
  else{
    number tmOut = NumberVal(o2);

    rootPo root = gcAddRoot(&P->proc.heap,&A1);
    gcAddRoot(&P->proc.heap,&A2);
  
    dynPo lk = goObjV(A1);		/* pick up the lock-carrying object */

    assert(lk->lock!=NULL);		/* must already have the lock */

    switchProcessState(P,wait_lock);

#ifdef LOCKTRACE
    if(traceLock)
      outMsg(logFile,RED_ESC_ON "%w: waiting on lock 0x%x on %w"RED_ESC_OFF"\n%_",&P->proc.thread,lk->lock,&a[1]);
#endif

    retCode ret = waitLock(lk->lock,tmOut);
    setProcessRunnable(P);

    gcRemoveRoot(&P->proc.heap,root);

    switch(ret){
    case Ok:
#ifdef LOCKTRACE
      if(traceLock)
	outMsg(logFile,RED_ESC_ON "%w: come back on %w "RED_ESC_OFF"\n%_",&P->proc.thread,&a[1]);
#endif
      return Ok;
    case Fail:
#ifdef LOCKTRACE
      if(traceLock)
	outMsg(logFile,RED_ESC_ON "%w: Timed out, waiting on %w "RED_ESC_OFF"\n%_",&P->proc.thread,&a[1]);
#endif
      return Fail;
    case Error:
#ifdef LOCKTRACE
      if(traceLock)
	outMsg(logFile,RED_ESC_ON "%w: problem with waitlock on %w"RED_ESC_OFF"\n%_",&P->proc.thread,&a[1]);
#endif
      return liberror(P,"__waitLock",eDEAD);
    default:
      return liberror(P,"__waitLock",eINVAL);
    }
  }
}
        
retCode g_releaseLock(processPo P,ptrPo a)
{
  ptrI A1 = deRefI(&a[1]);
  
  if(isvar(A1))
    return liberror(P,"__releaseLock",eINSUFARG);

  dynPo lk = goObjV(A1);

  if(lk->lock!=NULL){
#ifdef LOCKTRACE
    if(traceLock)
      outMsg(logFile,RED_ESC_ON "%w: releasing lock 0x%x on %w"RED_ESC_OFF"\n%_",&P->proc.thread,lk->lock,&a[1]);
#endif

    retCode ret = releaseLock(lk->lock);

    switch(ret){
    case Ok:
#ifdef LOCKTRACE
      if(traceLock)
	outMsg(logFile,RED_ESC_ON "%w: Lock released on %w"RED_ESC_OFF"\n%_",&P->proc.thread,&a[1]);
#endif
      
      return Ok;
    case Fail:
#ifdef LOCKTRACE
      if(traceLock)
	outMsg(logFile,RED_ESC_ON "%w: Could not release %w "RED_ESC_OFF"\n%_",&P->proc.thread,&a[1]);
#endif
      return Fail;
    case Error:
#ifdef LOCKTRACE
      if(traceLock)
	outMsg(logFile,RED_ESC_ON "%w: did not own %w "RED_ESC_OFF"\n%_",&P->proc.thread,&a[1]);
#endif
      return liberror(P,"__releaseLock",eFAIL);
    default:
      return liberror(P,"__releaseLock",eINVAL);
    }
  }
  else
    return Ok;
}

