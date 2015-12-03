/*
  Support a multi-wait for global GC
  (c) 2006 F.G. McCabe

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

#include "config.h"		/* pick up standard configuration header */
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <string.h>
#include "go.h"
#include "global.h"
#include "process.h"

#include <pthread.h>
#include <sched.h>
#include <signal.h>

/*
 * When a thread has been suspended for G/C, it will be woken up again by
 * the G/C thread when that has completed
 */

/* This is called by a process when it has been asked to suspend,
   and it is now safe to do so.
   Major constraint is that no request to allocate permanent memory permitted
   between the initial request and this response
*/

logical checkForPause(processPo P)
{
  lock(O_OBJECT(P));
  if(P->proc.pauseRequest){   /* pauseRequest may only be set by this process */
    process_state currState = P->proc.state;

    P->proc.state = wait_rendezvous;
    pthread_cond_broadcast(&P->proc.cond); /* We are ready to pause */

    while(P->proc.pauseRequest)
      pthread_cond_wait(&P->proc.cond,&P->object.mutex);
    P->proc.state = currState;		/* restart ... */
    unlock(O_OBJECT(P));
    return True;
  }
  unlock(O_OBJECT(P));
  return False;
}

void setProcessRunnable(processPo P)
{
  lock(O_OBJECT(P));
  if(P->proc.pauseRequest){
    P->proc.state = wait_rendezvous;
    pthread_cond_broadcast(&P->proc.cond); /* We are ready to pause */

    while(P->proc.pauseRequest)
      pthread_cond_wait(&P->proc.cond,&P->object.mutex);
  }
  P->proc.state = runnable;
  unlock(O_OBJECT(P));
}

void switchProcessState(processPo P,process_state state)
{
  lock(O_OBJECT(P));
  if(P->proc.pauseRequest){
    P->proc.state = wait_rendezvous;
    pthread_cond_broadcast(&P->proc.cond); /* We are ready to pause */

    while(P->proc.pauseRequest)
      pthread_cond_wait(&P->proc.cond,&P->object.mutex);
  }
  P->proc.state = state;
  unlock(O_OBJECT(P));
}



// This is called by the GC active thread to pause a given thread

static retCode pauseThread(processPo P,void *c)
{
  pthread_t thrID = ps_threadID(P);
  pthread_t self = (pthread_t)c;

  if(!pthread_equal(thrID,self)){
    lock(O_OBJECT(P));
    P->proc.pauseRequest = True;

    if(ps_state(P)==runnable){
      while(ps_state(P)==runnable)	/* Wait for the synchronization */
	pthread_cond_wait(&P->proc.cond,&P->object.mutex);
    }
    unlock(O_OBJECT(P));
  }
  return Ok;
}

static pthread_mutex_t suspMutex = PTHREAD_MUTEX_INITIALIZER;

void pauseAllThreads(pthread_t self)
{
  // assert(pthread_self()==gcThread);	/* Only the GC thread should call this */

  pthread_mutex_lock(&suspMutex);	/* We should never need this */
  processProcesses(pauseThread,(void*)self);
  pthread_mutex_unlock(&suspMutex);
}

static retCode resumeThread(processPo P,void *c)
{
  pthread_t self = (pthread_t)c;

  lock(O_OBJECT(P));
  P->proc.pauseRequest=False;
  
  if(!pthread_equal(ps_threadID(P),self) && ps_state(P)==wait_rendezvous)
    pthread_cond_signal(&P->proc.cond);

  unlock(O_OBJECT(P));
  return Ok;
}

void resumeAllThreads(pthread_t self)
{
  // assert(pthread_self()==gcThread);	/* Only the GC thread should call this */

  pthread_mutex_lock(&suspMutex);	/* We should never need this */
  processProcesses(resumeThread,(void*)self);
  pthread_mutex_unlock(&suspMutex);
}

