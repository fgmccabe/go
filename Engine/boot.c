/*
  Bootstrap the Go! run-time
  (c) 2000-2007 F.G. McCabe

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

  $Id: boot.c,v 1.3 2004/06/30 04:28:12 fmccabe Exp $
  $Log: boot.c,v $
  Revision 1.3  2004/06/30 04:28:12  fmccabe
  Some bug fixes, new grammar operator and new term operator

  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system

*/

#include "config.h"		/* pick up standard configuration header */
#include "go.h"			/* Main Go! header file */
#include "process.h"
#include "symbols.h"
#include "opcodes.h"
#include "debug.h"
#include "fileio.h"
#include "perms.h"
#include "esc.h"

#include <string.h>


/* An initial bootstrap sequence */

static ptrI buildCode(insPo cd,short arity,long cdlen,long litCnt)
{
  ptrI code =  permCode(cdlen,litCnt);
  codePo pc = codeV(code);

  memcpy(pc->data,cd,cdlen*sizeof(insWord));
  pc->arity = arity;
  return code;
}

/* This code is associated with all symbols which have no definition */
static void defineDieProg(void)
{
  static insWord proc_exit_seq[] = {
    instrhb(gcmap,0,0),
    instr(die)                  /* Then we expire */
  };

  dieProg=buildCode(proc_exit_seq,0,NumberOf(proc_exit_seq),0);
}

/* This code is executed when a process terminates */

static void defineExitProg(void)
{
  static insWord proc_exit_seq[] = {
    instrhb(gcmap,0,0),
    instr(die)                  /* Then we expire */
  };

  exitProg=buildCode(proc_exit_seq,0,NumberOf(proc_exit_seq),0);
}

static void defineTrapProg(void)
{
  insWord proc_seq[] = {
    instrhb(alloc,3,2),
    instrhb(gcmap,3,0),
    instrhb(clAY,1,1),                  /* pick up the error trap value */
    instrhb(escape,1,escapeOpcode("__trapval")),
    instrhb(gcmap,1,1),
    instrhb(mAlit,2,0),         /* 0 */
    instrhb(mAlit,3,0),         /* 0 */
    instrhb(clAY,4,2),          /* $s */
    instrhb(escape,4,escapeOpcode("__stringOf")),      /* __stringOf(xx,0,0,$s) */
    instrhb(gcmap,4,2),
    instrhb(mAY,1,2),           /* $s */
    instrhb(escape,1,escapeOpcode("__logmsg")),      /* __logMsg($s) */
    instrhb(gcmap,1,0),
    instr(die)                  /* Finally we expire */
  };
  ptrI trap = buildCode(proc_seq,0,NumberOf(proc_seq),1);

  updateCodeLit(codeV(trap),0,zero);

  trapProg=trap;
}

static void defineObjectProg(ptrI sym)
{
  insWord proc_seq[] = {                /* (gVar,_,tVar) :- _getProp(tVar,"$label",T), 
                                           ocall(gVar,T,tVar)*/
    instrhb(alloc,3,3),
    instrhb(gcmap,3,0),
    instrhb(mYA,1,1),                   /* gVar */
    instrhb(mYA,3,2),                   /* tVar */
    instrhm(mAA,1,3),                   /* tVar */
    instrhb(mAlit,2,0),                 /* $label */
    instrhb(clAY,3,3),
    instrhb(escape,3,escapeOpcode("__getProp")),
    instrhb(gcmap,3,3),
    instrhb(mAY,1,1),                   /* gVar */
    instrhb(mAY,2,3),                   /* T */
    instrhb(mAY,3,2),                   /* tVar */
    instrhm(dlkawlO,3,2),
    instrhb(gcmap,3,0)
  };
  ptrI obj = buildCode(proc_seq,3,NumberOf(proc_seq),1);

  updateCodeLit(codeV(obj),0,klabel);

  defineProg(sym,obj);
}

/*
 * This program is used to map special classes like integer into
 * Go! programs
 * This program is only safe before the first GC
 */
ptrI defineSpecialProg(const char *name)
{
#ifdef MEMTRACE
  extern long gcCount;
  long gCount = gcCount;
#endif

  insWord proc_seq[] = {	     // (gVar,_,tVar) :- name%3(gVar,name,tVar) 
    instrhb(mAlit,2,0),			/* name */
    instrhb(lkawl,3,1),			/* name%3 */
  };
  ptrI obj = buildCode(proc_seq,3,NumberOf(proc_seq),2);
  ptrI nameEnum = newEnumSym(name);

  updateCodeLit(codeV(obj),0,nameEnum);	/* name */

  ptrI nameProg = newProgLbl(name,3);	/* name%3 */

  defineProg(nameProg,obj);
  updateCodeLit(codeV(obj),1,nameProg);

#ifdef MEMTRACE
  assert(gcCount==gCount);
#endif
  return obj;
}



static void defineResumeProgs(void)
{
  int ar;

  for(ar=0;ar<NumberOf(doResume);ar++){
    insWord seq[] = {	       /* Y[arity] = interrupted code, Y[i] arguments */
      instrhb(gcmap,0,ar),
      instrh(resume,ar),		/* this resumes the execution */
      instr(succ)
    };
    ptrI cde = buildCode(seq,1,NumberOf(seq),0);

    doResume[ar] = cde;
  }
}


/* Top-level bootstrap sequence, load the initial boot program and enter with standard entry point */

void bootstrap(uniChar *bootEntry,logical debug,uniChar *classPath,uniChar *cwd)
{
  ptrI loaded = emptyList;
  uniChar errorMsg[1024];

  defineExitProg();
  defineDieProg();
  defineTrapProg();
  defineObjectProg(kmain);
  defineResumeProgs();                  /* define the resume entry points */

  ptrI mainThread = goObject(&globalHeap,objP(permObject(&globalHeap,threadClass)));

  processPo root = rootProcess(mainThread,kmain,classPath);

  rootPo gcRoot = gcAddRoot(&globalHeap,&loaded);
  
  switch(classLoader(&root->proc.heap,classPath,bootProg,emptySymbol,&loaded,
		     errorMsg,NumberOf(errorMsg))){
  default:
    logMsg(logFile,"corrupt or no boot file found in %U: %U",classPath,errorMsg);
    go_exit(EXIT_FAIL);
  case Eof:
    logMsg(logFile,"no boot file found in path %U",classPath);
    go_exit(EXIT_FAIL);
  case Ok:{
    gcRemoveRoot(&globalHeap,gcRoot);

    ptrI prog = newProgramLbl(bootEntry,3);

    if(!IsProgram(prog)){
      logMsg(logFile,"%U entry point not found",bootEntry);
      go_exit(EXIT_FAIL);
    }
    else{
      root->proc.PROG = ProgramOf(prog); /* this is where we establish the program */
      root->proc.PC = FirstInstruction(root->proc.PROG);
      root->proc.thread = kmainThread;

      runGo(root);
    }
  }
  }
}
