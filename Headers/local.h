#ifndef _ALOCAL_H_
#define _ALOCAL_H_

/* Localization for specific operating systems */

#ifdef _WIN32				/* Windows95 version */
#define ACCESS_MODE _A_NORMAL
#define DIR_SEP "\\"			/* directory separator in file names */
#define DIR_CHR DIR_SEP[0]
#define PTH_SEP ","			/* Path separator string */
#define PTH_CHR ','			/* Path separator character */
#endif

/* define escape sequences for bold highlighting */
#ifdef __QNX__
#define BOLD_ESC_ON "\033<"
#define BOLD_ESC_OFF "\033>"
#define ULINE_ESC_ON "\033<"
#define ULINE_ESC_OFF "\033>"
#endif

/* Code warrior specific definitions */
#ifdef __MWERKS__
#define GODIR ""			/* This should be changed */
#define INCLUDEPATH ""			/* default list of , separated paths */
#define DIR_SEP ":"			/* directory separator in file names */
#define DIR_CHR ':'			/* Same in character formar */
#define PTH_SEP ","			/* Path separator string */
#define PTH_CHR ','			/* Path separator character */
#define ACCESS_MODE 0			/* dummy for Macintosh */
#define BOLD_ESC_ON 			/* No bold character string */
#define BOLD_ESC_OFF
#define ULINE_ESC_ON		        /* No underline character string */
#define ULINE_ESC_OFF
#define NEW_LINE '\r'			/* Mac has a different idea of new line */
#define EOF_CHAR '\z'

int access(const char *filename,int mode);		/* We must provide this function */
char *GetEnv(const char *key);

#define getenv(a) GetEnv(a)		/* Use our environment variable handler */

#endif

/* Defaults */
#ifndef BOLD_ESC_ON
#define BOLD_ESC_ON "\033[5m"
#define BOLD_ESC_OFF "\033[0m"
#endif

#ifndef RED_ESC_ON
#define RED_ESC_ON "\033[31m"
#define RED_ESC_OFF "\033[0m"
#endif

#ifndef GREEN_ESC_ON
#define GREEN_ESC_ON "\033[32m"
#define GREEN_ESC_OFF "\033[0m"
#endif

#ifndef ULINE_ESC_ON
#define ULINE_ESC_ON "\033[7m"
#define ULINE_ESC_OFF "\033[0m"
#endif

#ifndef ACCESS_MODE
#define ACCESS_MODE F_OK|R_OK
#endif

/* Configuration option to turn off colours */

#ifdef NOCOLOURS
#undef BOLD_ESC_ON
#define BOLD_ESC_ON ""
#undef BOLD_ESC_OFF
#define BOLD_ESC_OFF ""

#undef RED_ESC_ON
#define RED_ESC_ON ""
#undef RED_ESC_OFF
#define RED_ESC_OFF ""

#undef ULINE_ESC_ON
#define ULINE_ESC_ON ""
#undef ULINE_ESC_OFF
#define ULINE_ESC_OFF ""

#undef GREEN_ESC_ON
#define GREEN_ESC_ON ""
#undef GREEN_ESC_OFF
#define GREEN_ESC_OFF ""
#endif /* NOCOLOURS */

#ifndef DIR_SEP
#define DIR_SEP "/"
#define DIR_CHR '/'
#define PTH_SEP ":"			/* Path separator string */
#define PTH_CHR ':'			/* Path separator character */
#endif

#ifndef MAX_APRIL_INT
#define MAX_APRIL_INT   ((integer)(((unsigned long long)((-1)<<1))>>1)) 	/* largest april integer */
#define MIN_APRIL_INT   -MAX_APRIL_INT 	/* smallest april integer */
#endif

#ifndef INCLUDEPATH
#define INCLUDEPATH "."			/* default list of PTH_CHR separated paths */
#endif

#ifndef NEW_LINE
#define NEW_LINE '\n'
#endif

#ifndef EOF_CHAR
#define EOF_CHAR '\04'                  /* control-D */
#endif

#ifndef NULL
#define NULL ((void*)0)			/* The NULL pointer */
#endif

#ifdef LOCALDIR
#include <libintl.h>
#define _(String) gettext(String)
#else
#define _(String) (String)
#define N_(String) (String)
#define textdomain(Domain)
#define bindtextdomain(Package, Directory)
#endif

#endif
