/* 
   Go! process record structure
   (c) 2000-2006 F.G.McCabe

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

#ifndef _PROCESS_H_
#define _PROCESS_H_

#include "heap.h"                       /* access heap mgt interface */
#include "objectP.h"                    /* access the object framework */
#include "managedP.h"                   /* processes are managed objects */
#include "lock.h"
#include <pthread.h>

typedef enum {
  quiescent,				/* process has yet to execute */
  runnable,			    /* process is running or available to run */
  wait_io,				/* process is waiting for I/O */
  wait_term,		/* process is waiting for another thread to terminate */
  wait_timer,				/* waiting for an interval times */
  wait_lock,				// Waiting for a lock to be released
  wait_child,                           // Wait for a child process
  wait_rendezvous,		       /* Waiting for a rendezvous to release */
  in_exclusion,				/* In an exclusion zone */
  dead                                  /* process has died */
} process_state;

/* A call record */

typedef struct _trail_rec_ *trailPo;
typedef struct _choice_rec_ *choicePo;
typedef struct _call_rec_ *callPo;

typedef struct _call_rec_ {
  insPo cPC;				/* parent program counter */
  ptrI cPROG;                           /* continuation program  */
  callPo cC;                            /* parent environment */
  choicePo cSB;                         /* where to cut to in parent */
} CallRec;

/* definition of a choice point record */
 
typedef struct _trail_rec_ {
  ptrPo var;                            /* variable that was bound */
  ptrI val;                             /* last value held by the variable */
} TrailRec;

typedef struct _choice_rec_ {
  unsigned int AX;                      /* no. of arguments saved */
  insPo PC;                             /* next clause to try */
  insPo cPC;                            /* back up continuation pointer */
  choicePo B;                           /* previous choice point */
  choicePo cSB;                         /* continuation cut point */
  callPo C;                             /* back up call record */
  ptrI cPROG;                           /* continuation program  */
  ptrI PROG;                            /* current program  */
  trailPo trail;                        /* current trail level */
  choicePo T;				/* error recovery trap */
  ptrPo H;                         /* value of the heap stack at choice point */
} ChoiceRec;

/* Object and class interface */

typedef struct{
  long liveProcesses;			/* Number of live processes */
  pthread_key_t processKey;		/* Special key for thread specific */
} ProcessClassPartRec;

typedef struct _process_class_ {
  ObjectClassRec objectPart;            /* The object part of a process */
  ManagedClassPartRec managedPart;   /* The managed part of the process class */
  ProcessClassPartRec processPart;
} ProcessClassRec;

extern ProcessClassRec ProcessClass;

typedef struct process_{
  ptrPo sBase;                          /* Base of on-board stack */
  ptrPo sTop;                           /* Limit of on-board stack */
  HeapRec heap;                         /* Stack heap record */
  ptrI A[GO_REGS];                      /* Copies of the argument registers */
  choicePo B;                           /* Current choice point */
  choicePo SB;                          /* Where to cut to */
  choicePo cSB;                         /* Continuation slashback point */
  choicePo T;				/* Trap recovery */
  trailPo trail;                        /* Current trail point */
  callPo C;                             /* Current call frame */
  callPo cC;                            /* Continuation call frame */
  insPo PC;                             /* Current program counter */
  insPo cPC;                            /* Continuation pointer */
  ptrI cPROG;                           /* continuation program  */
  ptrI PROG;                            /* current program  */
  ptrI trigger;                         /* triggered variables */
  unsigned short F;			/* Trigger flag */

  uniChar errorMsg[1024];		// Current error message
  ptrI errorCode;			/* Error code of this process */

  pthread_t threadID;			/* What is the posix thread ID? */
  pthread_cond_t cond;			/* Condition variable attached  */
  ptrI thread;                          /* The thread structure of this process */
  void *cl;                             /* Client specific data */
  process_state state;                 /* What is the status of this process? */
  logical pauseRequest;	       /* Has a pause of this process been requested? */
  process_state savedState;		/* Saved state of this process? */
} ProcessRec;

typedef struct _process_object_ {
  ObjectRec object;                     /* The object part of the Process object */
  ManagedRec managed;                   /* The managed part of the process object */
  ProcessRec proc;                      /* The proccess part of the Process object */
} ProcessObject;
  
extern long LiveProcesses;              /* Number of executing processes */

typedef retCode (*procProc)(processPo p,void *cl);

extern ptrI commandLine(heapPo H);      /* command line arguments */
extern ptrI cmdLineOptions(heapPo H);

retCode processProcesses(procProc p,void *cl);
processPo getProcessOfThread(void);

pthread_t ps_threadID(processPo p);
processPo ps_suspend(processPo p,process_state reason);
processPo ps_resume(register processPo p, register logical fr); /* resume process */
void ps_kill(processPo p);		  /* kill process */

void switchProcessState(processPo P,process_state state);
void setProcessRunnable(processPo P);

void pauseAllThreads(pthread_t except);
void resumeAllThreads(pthread_t except);
logical checkForPause(processPo P);

void *ps_client(processPo p);	/* Get the process's client information */
void *ps_set_client(processPo p,void *cl);

process_state ps_state(processPo p);

processPo runerr(processPo); /* non-fatal error */

processPo rootProcess(ptrI thread,ptrI boot,uniChar *classPath);
void bootstrap(uniChar *entry,logical debug,uniChar *bootfile,uniChar *cwd);
	       
void displayProcesses(void);
void displayProcess(processPo p);
void stackTrace(processPo p);

retCode extendStack(processPo p,int sfactor,int hfactor,int hmin);

ptrI buildDieEnv(void);
ptrI buildRootEnv(void);
ptrI buildExitEnv(void);
ptrI buildLoaderEnv(void);

void verifyProc(processPo p);
void initThreadClass(void);

#endif
