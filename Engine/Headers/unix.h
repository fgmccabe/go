#ifndef _UNIX_COMP_H_
#define _UNIX_COMP_H_

#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#if defined(_QNX__) || defined(ibm)	/* QNX and ibm need sys/select */
#include <sys/select.h>
#endif

#ifdef _WIN32			/* Windows only */
#include <process.h>
#include <windows.h>
#else
#include <dirent.h>
#include <unistd.h>
#endif

#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <signal.h>
#include <string.h>
#include <ctype.h>

/* 
 * Sometimes this is not defined ...
 */

int setenv(const char *name,const char *val,int overwrite);

#endif
