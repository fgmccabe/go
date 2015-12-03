/* 
  Main program for the Go! run-time system
  (c) 2000-2007 F.G.McCabe

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

  $Id: main.c,v 1.2 2004/04/29 16:24:27 fmccabe Exp $
  $Log: main.c,v $
  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system

*/

#include "config.h"
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <locale.h>
#include <limits.h>
#include "go.h"		/* Main header file */
#include "fileio.h"
#include "process.h"
#include "eval.h"
#include "clock.h"
#include "lock.h"
#include "hashtable.h"
#include "debug.h"

logical debugging = False;	/* instruction tracing option */
logical interactive = False;	/* Whether it should be interactive */
logical enableVerify = True;    /* true if we wish to enable code verification */
logical SymbolDebug = False;	/* symbolic debugging generation */
logical traceVerify = False;	/* true if tracing code verification */
logical traceMessage = False;	/* true if tracing message passing */
logical tracePut = False;	/* true if tracing term freeze */
logical traceLock = False;	/* true if tracing locks */
#ifdef XTRACE
#ifdef GOXLIB
logical traceX = False;			/* True if tracing X windows stuff */
#endif
#endif

uniChar goSysPath[MAX_MSG_LEN] = {0};      // Pointer to Go!'s installation point
static uniChar goCWD[MAX_MSG_LEN] = {0};
static uniChar classPath[MAX_MSG_LEN] = {0}; /* Go class path string */
static uniChar entryPoint[MAX_MSG_LEN] = {0}; /* Go entry point class */
static uniChar debugPkg[MAX_MSG_LEN] = {0};   /* Standard debug package */

static struct{
  uniChar option;                       /* The name of the option */
  uniChar value[MAX_MSG_LEN];           /* the value of the option */
} Options[32];                          /* An array of them */
static int optCount = 0;                /* How many do we have? */

#include "version.h"		/* Version ID for the evaluator */
char copyRight[]="(c) 2000-2006 F.G.McCabe\nGNU LGPL licence";

static long initHeapSize = 200*1024;
long initStackHeapSize = 1024;

static void splitFirstArg(int argc, char **argv,int *newArgc, char ***newArgv)
{
  /* 
     Splits the first command-line argument into multiple
     arguments if it starts with "-%". The
     delimiter is the first character following the percent sign.
     For example: "-%%-pdir1%-pdir2" will be split into
     two arguments "-pdir1", "-pdir2".

     This helps to work around the limitation that #! scripts
     can pass a maximum of one argument to the interpreter
     under certain operating systems (eg. Linux).
  */

  *newArgc = argc;
  *newArgv = argv;

  if (argc<2)
    return;

  if(strncmp(argv[1],"-%",2)==0){
    char delimiter = argv[1][2];
    int extra = 0, arg = 1;
    char *p;

    /* Count number of & in passed arguments */
    p = argv[1] + 3;
    do {
      char *q = strchr(p, delimiter);
      if (q == NULL) 
	break;

      p = q + 1;
      extra++;
    } while (*p != '\0');


    /* We didn't find any delimiters */
    if (extra == 0)
      return;

    /* Make the extra arguments */
    *newArgc = argc + extra;
    *newArgv = (char **) malloc(*newArgc * sizeof(char *));
    (*newArgv)[0] = argv[0];

    p = argv[1] + 3;
    do {
      char *q = strchr(p, delimiter);
      if (q == NULL) {
	(*newArgv)[arg++] = p;
	break;
      }
      else {
	int len = q - p;
	char *data = (char *) malloc(len + 1);
	
	strncpy(data, p, len);
	data[len] = '\0';
	(*newArgv)[arg++] = data;
	p = q + 1;
      }
    } while (True);
  }
}

int getOptions(int argc, char **argv)
{
  int opt;
  extern char *optarg;
  extern int optind;
  
  splitFirstArg(argc,argv,&argc,&argv);

  for(;optCount<NumberOf(Options)&&
	(opt=getopt(argc,argv,GNU_GETOPT_NOPERMUTE "n:m:M:D:P:d:gG:x:vVh:s:L:R:"))>=0;optCount++){
    Options[optCount].option = opt;     /* store the option */

    if(optarg!=NULL){
      _uni((unsigned char*)optarg,Options[optCount].value,NumberOf(Options[optCount].value)); /* copy the value of the option */
    }
    else
      Options[optCount].value[0]='\0';

    switch(opt){
    case 'D':{			/* turn on various debugging options */
      char *c = optarg;

      while(*c){
	switch(*c++){
	case 'e':		/* Escape call tracing */
#ifdef EXECTRACE
	  traceEscapes = True;
	  continue;
#else
	  logMsg(logFile,"Escape tracing not enabled\n");
	  return -1;
#endif

	case 'd':		/* single step instruction tracing */
#ifdef EXECTRACE
	  debugging = True;
	  continue;
#else
	  logMsg(logFile,"Instruction-level debugging not enabled\n");
	  return -1;
#endif

	case 'v':		/* turn on verify tracing */
#ifdef VERIFYTRACE
	  traceVerify=True;
	  continue;
#else
	  logMsg(logFile,"code verification not enabled\n");
	  return -1;
#endif

	case 'm':		/* trace memory allocations  */
#ifdef MEMTRACE
	  if(traceMemory)
	    stressMemory=True;
	  else
	    traceMemory = True;
	  continue;
#else
	  logMsg(logFile,"memory tracing not enabled");
	  return -1;
#endif 
	  break;

	case 'l':		/* trace synch locks */
#ifdef LOCKTRACE
          traceLock = True;
	  continue;
#else
	  logMsg(logFile,"sync tracing not enabled");
	  return -1;
#endif
	  break;

	case 'p':		/* trace put-style operations */
#ifdef EXECTRACE
	  tracePut = True;
	  continue;
#else
	  logMsg(logFile,"put tracing not enabled");
	  return -1;
#endif

	case 'G':		/* Internal symbolic tracing */
#ifdef EXECTRACE
	  SymbolDebug = True;
	  interactive = False;
	  continue;
#else
	  logMsg(logFile,"tracing not enabled");
	  return -1;
#endif

	case 'g':		/* Internal symbolic debugging */
	  SymbolDebug = True;
	  interactive = True;
	  continue;

	case 'I':
#ifdef STATSTRACE
	  traceCount = True;
	  atexit(dumpInsCount);
	  break;
#else
	  logMsg(logFile,"instruction counting not enabled");
	  return -1;
#endif	  

#if 0
	case 'x':		/* turn on tracing of X windows */
#ifdef XTRACE
#ifdef GOXLIB
	  traceX=True;
#else
	  logMsg(logFile,"X not enabled\n");
	  return -1;
#endif
	  continue;
#else
	  logMsg(logFile,"X tracing not enabled\n");
	  return -1;
#endif
#endif

	case '*':		/* trace everything */
#ifdef ALLTRACE
	  traceEscapes = True;
	  debugging = True;
	  interactive = True;
	  traceVerify=True;
	  traceCount=True;
	  traceMessage = True;
	  if(traceMemory)
	    stressMemory=True;
	  else
	    traceMemory = True;
	  tracePut = True;              /* term freeze */
#else
	  logMsg(logFile,"debugging not enabled\n");
	  return -1;
#endif
	}
      }
      break;
    }

    case 'g':{
      SymbolDebug = True;	/* turn on symbolic debugging */
      interactive = True;       // Initially its also interactive
      break;
    }

    case 'G':{				/* non-default debugging package */
      strMsg(debugPkg,NumberOf(debugPkg),"%s",optarg);
      break;
    }

    case 'P':{
      uniChar buff[MAX_MSG_LEN];

      strMsg(buff,NumberOf(buff),"%s:%U",optarg,classPath);
      uniCpy(classPath,NumberOf(classPath),buff);

      break;
    }

    case 'm':{                          /* modify the entry point */
      strMsg(entryPoint,NumberOf(entryPoint),"go.boot@%s",optarg);
      break;
    }

    case 'd':{                      /* non-standard initial working directory */
      strMsg(goCWD,NumberOf(goCWD),"%s",optarg);
      break;
    }

    case 'R':{                          /* fix the random seed */
      srand(atoi(optarg));
      break;
    }

    case 'L':{
      uniChar fn[MAX_MSG_LEN];

      _uni((unsigned char*)optarg,fn,NumberOf(fn));
      if(initLogfile(fn)!=Ok){
	logMsg(logFile,"log file %s not found",optarg);
	return -1;
      }
      break;
    }

    case 'v':                           /* Display version ID */
      outMsg(logFile,"%s",version);
      break;
      
    case 'V':                      /* Turn on (will be off) code verification */
      enableVerify = !enableVerify;
      break;

    case 'h':                           /* set up heap size */
      initHeapSize = atoi(optarg)*1024;
      break;

    case 's':                           /* set up initial size of a thread */
      initStackHeapSize = atoi(optarg)*1024;
      break;

    default:
      break;                            /* ignore options we dont understand */
    }
  }
  return optind;
}

/*
 * Go evaluator main program
 */
int main(int argc, char **argv)
{
  int narg;

#ifdef HAVE_LOCALECONV
  setlocale(LC_ALL,"");		/* set up locale */
#endif

#ifdef LOCALEDIR
  bindtextdomain(PACKAGE,LOCALEDIR);
  textdomain(PACKAGE);
#endif

  {
    uniChar fn[]={'-',0};
    initLogfile(fn);
  }

  {
    char *dir = getenv("GO_DIR"); /* pick up the installation directory */
    char cbuff[MAXPATHLEN];
    char *cwd = getcwd(cbuff,NumberOf(cbuff)); /* compute current starting directory */

    if(dir==NULL)
      dir = GODIR "/";                  /* Default installation path */

    strMsg(goSysPath,NumberOf(goSysPath),"%s",dir);

    if(cwd==NULL) 
      syserr("cant determine current directory");
    else{
      strMsg(goCWD,NumberOf(goCWD),"%s/",cwd);
      strMsg(classPath,NumberOf(classPath),"%s:%s/",dir,cwd);
    }
  }

  strMsg(entryPoint,NumberOf(entryPoint),"go.boot@__main"); /* standard entry point */

  if((narg=getOptions(argc,argv))<0){
    outMsg(logFile,_("usage: %s [-v] [-m mainclass] [-L log] [-g host:port] [-V] [-P classpath]"
#ifdef ALLTRACE
	   " [-D debugopts]"
#endif
	   " [-h sizeK] [-s sizeK] [-d rootdir] args ...\n"),argv[0]);
    exit(1);
  }

  /* IMPORTANT -- Keep the order of these set up calls */
  initFileIo();				/* Set up special file handling */
  initGlobal(initHeapSize);		/* start up the global space */
  initClass();				/* Initialize the class handlers */
  initPrograms();			/* Initialize program handling */
  initDict();				/* Start up the dictionaries */
  install_escapes();			/* Initialize the escape table */
  initFiles();				/* initialize file tables */
  init_args(argv,argc,narg);		/* Initialize the argument list */
  init_time();				/* Initialize time stuff */

  setupSignals();

  bootstrap(entryPoint,SymbolDebug,classPath,goCWD);

  return EXIT_SUCCEED;	        /* exit the go system cleanly */
}

/*
 * This is here for convenience ...
 */
ptrI cmdLineOptions(heapPo H)
{
  ptrI options = emptyList;
  rootPo root = gcAddRoot(H,&options);
  ptrI pair = kvoid;
  ptrI tmp = kvoid;
  int i;
    
  gcAddRoot(H,&pair);
  gcAddRoot(H,&tmp);

  for(i=0;i<optCount;i++){
    pair = objP(allocateObject(H,commaClass));	/* a new option pair */

    tmp = newChar(Options[i].option);
    updateArg(objV(pair),0,tmp);

    tmp = newUniSymbol(Options[i].value);
    updateArg(objV(pair),1,tmp);

    options = consLsPair(H,pair,options);
  }

  gcRemoveRoot(H,root);

  return options;
}
