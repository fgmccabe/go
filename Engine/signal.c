/*
  Unix Signal handling for the Go! run-time engine
  (c) 1994-2003 Imperial College and F.G. McCabe

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

  $Id: signal.c,v 1.2 2004/04/29 16:24:28 fmccabe Exp $
  $Log: signal.c,v $
  Revision 1.2  2004/04/29 16:24:28  fmccabe
  Completely new type system

*/
#include "config.h"		/* pick up standard configuration header */
#include <signal.h>
#include "go.h"			/* Main header file */
#include "symbols.h"
#include "process.h"
#include "clock.h"
#include "signals.h"

/* 
 * Signal handling functions
 */

static void sig_fatal(int sig)
{
  outMsg(logFile,"bus error or segmentation fault\n");
  go_exit(EXIT_FAIL);
}

/* Handle the suspension of Go reasonably ... */
static void sig_suspend(int sig)
{
  if(!interactive){
    reset_stdin();		/* Reset the standard input channel */
    raise(SIGSTOP);             /* Actually suspend */
    setup_stdin();              /* Put it back */
  }
  else
    raise(SIGSTOP);             /* Actually suspend */
}

static void interruptMe(int ignored);

void setupSignals(void)
{
  signal(SIGBUS, sig_fatal);
  signal(SIGTSTP, sig_suspend);	/* When the user suspends an Go program */
  signal(SIGPIPE, SIG_IGN);	/* We not want to be interrupted by this */
  signal(SIGSEGV, sig_fatal);
  signal(SIGFPE, sig_fatal);
  signal(SIGQUIT,interruptMe);
  signal(SIGINT,interruptMe);
}

sigset_t stopInterrupts(void)	/* stop control-C from generating a signal */
{
  sigset_t mask;
  sigset_t current;

  sigemptyset(&mask);

  sigaddset(&mask,SIGQUIT);	/* The quit signal */
  sigaddset(&mask,SIGVTALRM);	/* The virtual timer signal */
  sigaddset(&mask,SIGALRM);	/* The real timer signal */

  sigprocmask(SIG_BLOCK,&mask,&current);
  return current;
}

void startInterrupts(sigset_t blocked)	/* enable interrupts again */
{
  sigprocmask(SIG_SETMASK,&blocked,NULL);
}

/* 
 * Interrupt handling -- on a control^C we send a message to the monitor process
 */

/* Warning --- important that SIGINT is blocked during this handler */

static void interruptMe(int sig) /* This one is invoked when user presses ^C */
{
  go_exit(EXIT_FAIL);		/* We just abort everything */
}


void go_exit(int code)
{
  if(code!=0)
    outMsg(logFile,"Terminating with code %d\n",code);

#ifdef EXECTRACE
  if(traceCount)
    dumpInsCount();
#endif

  exit(code);
}

