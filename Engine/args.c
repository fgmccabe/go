/* 
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

  A special escape to pick up on arguments to a program

  $Id: args.c,v 1.2 2004/04/29 16:24:27 fmccabe Exp $
  $Log: args.c,v $
  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system


*/

#include "config.h"		/* pick up standard configuration header */
#include <string.h>
#include <stdlib.h>		/* Standard C library */
#include "go.h"
#include "process.h"
#include "dict.h"

static char **argsv=NULL;	/* Store the command line list */
static int argcnt=0;

void init_args(char **argv, int argc, int start)
{
  argsv = &argv[start];
  argcnt = argc-start;
}

ptrI commandLine(heapPo H)
{
  int i;
  ptrI alist=emptyList;
  ptrI ag = kvoid;
  rootPo root = gcAddRoot(H,&alist);

  gcAddRoot(H,&ag);

  for(i=argcnt-1;i>=0;i--){
    ag = allocateCString(H,argsv[i]);
    alist = consLsPair(H,ag,alist);       /* construct in reverse order */
  }

  gcRemoveRoot(H,root);                   /* clear off additional roots */
  return alist;
}

retCode g_command_line(processPo P,ptrPo args)
{
  ptrI alist = commandLine(&P->proc.heap);
  rootPo root = gcAddRoot(&P->proc.heap,&alist);
  retCode ret = equal(P,&args[1],&alist);

  gcRemoveRoot(&P->proc.heap,root);
  return ret;
}

retCode g_commandOptions(processPo P,ptrPo args)
{
  ptrI alist = cmdLineOptions(&P->proc.heap);
  rootPo root = gcAddRoot(&P->proc.heap,&alist);
  retCode ret = equal(P,&args[1],&alist);

  gcRemoveRoot(&P->proc.heap,root);
  return ret;
}
