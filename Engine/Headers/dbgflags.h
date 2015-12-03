#ifndef _FLAGS_H_
#define _FLAGS_H_

#include "logical.h"

extern logical debugging;	/* instruction tracing option */
extern logical interactive;	/* Whether it should be interactive */
extern logical enableVerify;    /* true if we wish to enable code verification */
extern logical SymbolDebug;	/* symbolic debugging generation */
extern logical traceEscapes;	/* tracing of escape calls  */
extern logical traceVerify;	/* Tracing of code verification */
extern logical traceMessage;	/* message tracing  */
extern logical traceTCP;	/* tracing of TCP messages */
extern logical traceResource;	/* tracing of the resource manager */
extern logical tracePut;	/* tracing of term freeze function */
extern logical traceLock;	/* tracing of synchronization */

#ifdef ALLTRACE			/* are we enabling all tracing? */
#ifndef PROCTRACE
#define PROCTRACE
#endif
#ifndef MEMTRACE
#define MEMTRACE
#endif
#ifndef EXECTRACE
#define EXECTRACE
#endif
#ifndef MSGTRACE
#define MSGTRACE
#endif
#ifndef CLOCKTRACE
#define CLOCKTRACE
#endif
#ifndef VERIFYTRACE
#define VERIFYTRACE
#endif
#ifndef RESOURCETRACE
#define RESOURCETRACE
#endif
#ifndef STATSTRACE
#define STATSTRACE
#endif
#ifndef MSGTRACE
#define MSGTRACE
#endif
#ifndef LOCKTRACE
#define LOCKTRACE
#endif

#ifndef XTRACE
#define XTRACE
#endif

#endif

#ifdef MEMTRACE
extern logical traceMemory;	/* tracing of stack memory access */
extern logical stressMemory;	/* stress test GC */
#endif

#endif



