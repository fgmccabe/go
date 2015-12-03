/*
 * Signal management functions
 */
#ifndef _G_SIGNAL_H_
#define _G_SIGNAL_H_

#include <signal.h>

void setupSignals(void);
void startInterrupts(sigset_t blocked);	/* enable control-C interrupts */
sigset_t stopInterrupts(void);	/* stop control-C interruptes */
void go_exit(int);		/* When we want to stop */
void initSuspend(void);
#endif
