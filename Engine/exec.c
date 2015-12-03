/*
  System interface escapes
  (c) 2000 F.G.McCabe

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

  $Id: exec.c,v 1.2 2004/04/29 16:24:27 fmccabe Exp $
  $Log: exec.c,v $
  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system

 */
#include "config.h"		/* pick up standard configuration header */
#include <string.h>		/* String functions */
#include <ctype.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>		/* system error numbers */
#include <fcntl.h>
#include <sys/time.h>
#include <sys/resource.h>
#include "go.h"
#include "fileio.h"

retCode g_exit(processPo P,ptrPo a)
{
  exit((long)NumberVal(objV(deRefI(&a[1]))));
}


/* This is used to attach a shell to a process, so that when the child terminates
   becomes available, the right process is kicked off
*/

retCode g_shell(processPo P,ptrPo a)
{
  ptrI pth = deRefI(&a[1]);
  ptrI ags = deRefI(&a[2]);
  ptrI env = deRefI(&a[3]);

  long aLen = ListLen(ags);
  long eLen = ListLen(env);

  if(isvar(pth)||isGroundString(&pth)!=Ok)
    return liberror(P,"__shell",eSTRNEEDD);
  else if(isvar(ags) || aLen<0 || isvar(env) || eLen<0)
    return liberror(P,"__shell",eINSUFARG);
  else{
    long len = StringLen(&pth)+1;
    char cmd[3*len];
    
    String2Utf8(&pth,cmd,3*len);
          
    switchProcessState(P,wait_io);
    
    if(access(cmd,F_OK|R_OK|X_OK)!=0){
      setProcessRunnable(P);
      return liberror(P,"__shell",eNOTFND);
    }
    else if(!executableFile(cmd)){
      setProcessRunnable(P);
      return liberror(P,"__shell",eNOPERM);
    }
    else{
      char **argv = (char **)calloc(aLen + 2, sizeof(char *));
      char **envp = (char **)calloc(eLen + 1, sizeof(char *));
      int pid;
      long i;

      argv[0] = cmd;
      
      for(i=1;IsList(ags);i++,ags = deRefI(listTail(objV(ags)))){
	ptrPo l = listHead(objV(ags));
	long al = StringLen(l);
        
	if(al<0)
	  return liberror(P,"__shell",eINSUFARG);
	else{
	  char astr[al*3];
          
	  String2Utf8(l,astr,al*3);
	  argv[i]=strdup(astr);
	}
      }
      
      argv[i]=NULL;
      
      for(i=0;IsList(env);i++,env = deRefI(listTail(objV(env)))){
	ptrPo l = listHead(objV(env));
	ptrI El = deRefI(l);
	ptrI var,val;
	long al;
        
	if(!IsBinOp(&El,commaClass,&var,&val)||(al=StringLen(&val))<0||isvar(var))
	  return liberror(P,"__shell",eINSUFARG);
	else{
	  long bSize = al*3+uniStrLen(SymVal(symbV(var)))+10;
	  uniChar str[bSize];
	  char estr[bSize];
          
	  strMsg(str,bSize,"%U = %L",SymVal(symbV(var)),&val);
	  _utf(str,(unsigned char*)estr,bSize);

	  envp[i]=strdup(estr);
	}
      }
      
      envp[i]=NULL;

      switchProcessState(P,wait_child);	/* We are now waiting for a child */
              
      if ((pid=fork()) == 0){
	// child process, terminating after execve
	execve(cmd, argv, envp);
	// abnormal termination -- should never get here
	_exit(127);
      }
      else{
	// parent process (agent)
	for(i=1;argv[i]!=NULL;i++)	// argv[0] is a local string
	  free(argv[i]);
          
	for(i=0;envp[i]!=NULL;i++)
	  free(envp[i]);

	free(argv);
	free(envp);

	do{
	  int childStatus;
	  int res = waitpid(pid,&childStatus,0);

	  setProcessRunnable(P);	/* now we can run */

	  if(res<0){
	    switch(errno){
	    case ECHILD:
	      return liberror(P,"__shell",eNOTFND);
	    case EFAULT:
	      return liberror(P,"__shell",eINVAL);
	    case EINTR:
	      continue;
	    }
	  }
	  else if(WIFEXITED(childStatus)){ /* exited normally */
	    ptrI r = allocateInteger(&P->proc.heap, 
				     WEXITSTATUS(childStatus)); 
	    
	    return equal(P,&a[4],&r);
	  }
	  else if(WIFSIGNALED(childStatus))
	    return liberror(P,"__shell",eINTRUPT);
	} while(True);
      }
    }
  }
}

    
