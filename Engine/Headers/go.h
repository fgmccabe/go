/* 
 Public header file for accessing April's abstract syntax
*/
#ifndef _GO_H
#define _GO_H

#include <stdlib.h>
#include "dbgflags.h"		/* standard debugging flags */
#include "local.h"		/* localization */
#include "ooio.h"

typedef struct _process_object_ *processPo;
extern classPo processClass;

#ifdef VERIFY_OBJECT
objectPo checkCast(void *c,classPo class);

#define O_PROCESS(c) ((processPo)(checkCast((c),processClass)))
#else
#define O_PROCESS(c) ((processPo)(c))
#endif

#ifndef GO_REGS			/* do we know how many registers? */
#define GO_REGS 64		/* Should'nt be more than #bits in an integer*/
#endif

#include "word.h"		/* Standard definition of a cell & access fns */
#include "heap.h"
#include "global.h"
#include "symbols.h"		/* standard symbols available to the engine */
#include "errors.h"		/* standard error codes */
#include "eval.h"
#include "str.h"                /* String manipulation */

#ifndef NULL
#define NULL            ((void*)0) /* The NULL pointer */
#endif

#undef NumberOf
#define NumberOf(a) (sizeof(a)/sizeof(a[0]))

#undef ElementSize
#define ElementSize(a) (sizeof(a[0]))

typedef retCode (*funpo)(processPo p,ptrPo args); /* Escape function pointer */

void runGo(register processPo P);

#include "dict.h"
#include "signals.h"

#define EXIT_SUCCEED 0		/* Normal exit */
#define EXIT_FAIL 1		/* Failing exit */

#endif
