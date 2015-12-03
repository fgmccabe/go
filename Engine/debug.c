/* 
  Instruction-level debugging of Go! code
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

  $Id: debug.c,v 1.2 2004/04/29 16:24:27 fmccabe Exp $
  $Log: debug.c,v $
  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system

*/
#include "config.h"
#include "go.h"
#include "opcodes.h"
#include "dict.h"
#include "symbols.h"
#include "process.h"
#include "debug.h"
#include "dbgflags.h"

logical tracing = True;		/* do we show each step */

retCode g_ins_debug(processPo P,ptrPo a)
{
  debugging=interactive=True;
  return Ok;
}

/* Parse a command line to display a register value */
void showReg(ptrPo a,char *name,integer reg)
{
  ptrPo ptr = &a[reg];

  outMsg(logFile,"%s[%d] = ",name,(int)reg);
  
  if(*ptr==0)
    outMsg(logFile,"<<NULL>>\n");
  else{
    while(isvar(*ptr)){
      if((ptrPo)*ptr==ptr){
	if(isSuspVar(ptr))
	  outMsg(logFile,"_*%x",ptr);
	else
	  outMsg(logFile,"_%x",ptr);
        return;
      }
      else{
        outMsg(logFile,"_%x->",ptr);
        ptr = (ptrPo)*ptr;
      }
    }

    outMsg(logFile,"%.40w\n",ptr);
  }
}

DebugWaitFor waitingFor = nextIns; /* waiting for next instruction */
long cmdCounter = 0;

#ifdef EXECTRACE

static long cmdCount(uniChar *cmdLine)
{
  long count = (long)parseInteger(cmdLine,uniStrLen(cmdLine));
  if(count==0)
    return 1;			/* never return 0 */
  else
    return count;
}

static processPo focus = NULL; /* non-null implies only interested in this */
static insPo dissass(uniChar *pref,codePo code,insPo pc,ptrPo a,ptrPo y,ptrPo S, rwmode mode,choicePo B,ptrPo hBase,ptrPo hLimit);

static pthread_mutex_t debugMutex = PTHREAD_MUTEX_INITIALIZER;

retCode debug_stop(processPo p,ptrI prog,insPo pc,ptrI cprog,insPo cpc,ptrPo a,ptrPo y,
		   ptrPo S,long Svalid,rwmode mode,
                   choicePo B,choicePo SB,choicePo T,
		   ptrPo hBase,ptrPo H,trailPo trail,
		   ptrI prefix)
{
  int ch;
  static uniChar cmdLine[256] = {'n',0};
  codePo code = codeV(prog);
  codePo ccode = codeV(cprog);
  extern HeapRec globalHeap;

  pthread_mutex_lock(&debugMutex);
  
  if(focus==NULL || focus==p){
    switch(waitingFor){
    case nextIns:
      cmdCounter--;
      break;
    case nextSucc:
      switch(op_cde(*pc)){
      case succ:
	cmdCounter--;
	break;
      case kawlO: case kawl: 
	cmdCounter++;
	break;
      default:
	;
      }
    case nextBreak:		/* nothing to do here */
    case nextFail:
      break;
    }

    if(tracing||cmdCounter<=0){
      uniChar pref[MAX_SYMB_LEN];
      ptrPo Lits = codeLits(code);

      strMsg(pref,NumberOf(pref),"%w "RED_ESC_ON "[%d]" RED_ESC_OFF " %w",
	     &prefix,pcCount,&Lits[0]);
      dissass(pref,code,pc,a,y,S,mode,B,hBase,H);

      outMsg(logFile,"\ncPC=%w[%d],B=%x,SB=%x,T=%x,Y=%x[%d],",
	     &codeLits(ccode)[0],cpc-codeIns(ccode),B,SB,T,y,envSize(cpc));
      if(Svalid>0)
        outMsg(logFile,"S=%x(%d),",S,Svalid);
      else if(mode==writeMode)
	outMsg(logFile,"S=%x,",H);
      outMsg(logFile,"trail=%x",trail);
      if(!identical(p->proc.trigger,emptyList))
	outMsg(logFile,",%d triggered",ListLen(deRefI(&p->proc.trigger)));
      outMsg(logFile,"\nH=%x[%d], ",H,(ptrPo)p->proc.heap.end-H);
      outMsg(logFile,"global=%x[%d]\n",globalHeap.create, globalHeap.end-globalHeap.create);
      flushFile(logFile);
    }

    if(cmdCounter<=0){		/* do we need to stop? */
      while(debugging&&cmdCounter<=0){	/* prompt the user */
	uniChar *ln = cmdLine;
	outMsg(logFile," => ");
	flushOut();

	if((ch=inCh(stdIn))!='\n' && ch!=uniEOF){
	  do{
	    *ln++=ch;
	    ch = inCh(stdIn);
	  } while(ch!='\n' && ch!=uniEOF);
	  *ln++='\0';
	}
    
	switch(cmdLine[0]){
	case ' ':
	  cmdCounter = cmdCount(cmdLine+1);
	  waitingFor = nextIns;
	  tracing=True;
	  break;
	case 'n':
	  cmdCounter = cmdCount(cmdLine+1);
	  waitingFor = nextIns;
	  tracing=False;
	  break;
	case 'N':
	  cmdCounter = cmdCount(cmdLine+1);
	  switch(op_cde(*pc)){
	  case kawlO: case kawl: 
	  case lkawlO: case lkawl:
	  case dlkawlO: case dlkawl:
	    waitingFor = nextSucc;
	    break;
	  default:
	    waitingFor = nextIns;
	  }
	  tracing=False;
	  break;

	case uniEOF:
	case '\n':
	  cmdCounter = 1;
	  waitingFor = nextIns;
	  break;
	  
	case 'x':		/* wait for a success */
	  cmdCounter = cmdCount(cmdLine+1);
	  waitingFor = nextSucc;
	  break;

	case 'f':
	  focus = p;
	  outMsg(logFile,"Focussing on program %w\n",&p->proc.thread);
	  uniLit(cmdLine,NumberOf(cmdLine),"n\n"); /* default to next instruction */
	  break;

	case 'F':
	  pthread_mutex_unlock(&debugMutex);
	  return Fail;

	case 'u':
	  focus = NULL;
	  uniLit(cmdLine,NumberOf(cmdLine),"n\n"); /* default to next instruction */
	  break;

	case 'q':
	  outMsg(logFile,"terminating go session");
	  go_exit(0);
	  
	case 'c':
	  cmdCounter = cmdCount(cmdLine+1);
	  waitingFor = nextBreak;
	  tracing = False;
	  break;

	case 't':
	  waitingFor = nextBreak;
	  tracing = True;
	  cmdCounter = 1;
	  break;

	case 'S':
	  SymbolDebug = True;
	  debugging = False;
	  interactive = True;
	  uniLit(cmdLine,NumberOf(cmdLine),"n\n"); /* default to next instruction */
	  break;

	case 'a':{		/* dump an argument register */
	  showReg(a,"A",parseInteger(&cmdLine[1],uniStrLen(&cmdLine[1])));
	  uniLit(cmdLine,NumberOf(cmdLine),"n\n"); /* default to next instruction */
	  continue;
	}

	case 'y':{		/* dump a local variable */
	  integer off=parseInteger(&cmdLine[1],uniStrLen(&cmdLine[1]));

	  outMsg(logFile,"Y[%ld] = %w\n",off,&y[-off]);
	  uniLit(cmdLine,NumberOf(cmdLine),"n\n"); /* default to next instruction */
	  continue;
	}

	case 'r':{		/* show all registers */
	  unsigned int i;
	  int Ylen = envSize(cpc);

	  for(i=1;i<=B->AX;i++)
	    outMsg(logFile,"A[%d]=%w\n",i,&a[i]);

	  for(i=1;i<=Ylen;i++)
	    outMsg(logFile,"%Y[%d]=%w\n",i,&y[-i]);

	  uniLit(cmdLine,NumberOf(cmdLine),"n\n"); /* default to next instruction */
	  continue;
	}

	case 'P':{		/* Display all processes */
	  displayProcesses();
	  uniLit(cmdLine,NumberOf(cmdLine),"n\n"); /* default to next instruction */
	  continue;
	}
	case 's':		/* Show a stack trace of this process */
	  p->proc.B = B;
	  p->proc.C = (callPo)y;
	  p->proc.cPC = cpc;
	  stackTrace(p);
	  uniLit(cmdLine,NumberOf(cmdLine),"n\n"); /* default to next instruction */
	  continue;

	case '0': case '1': case '2': case '3': case '4': case '5': 
	case '6': case '7': case '8': case '9': {
	  cmdCounter = cmdCount(cmdLine);
	  waitingFor = nextIns;
	  continue;
	}
        
        case 'i':{              /* Show the following instructions */
          long count = cmdCount(cmdLine+1);
          insPo tmpPc = pc;
          insPo limit = &code->data[code->size];

          while(count-->0 && tmpPc<limit){
            tmpPc = dissass(NULL,codeV(prog),tmpPc,a,y,S,dummyMode,B,NULL,NULL);
            outMsg(logFile,"\n");
          }
	  uniLit(cmdLine,NumberOf(cmdLine),"n\n"); /* default to next instruction */
	  continue;
        }
                
	default:
	  outMsg(logFile,"'n' = step, 'N' = step over, 'c' = continue, 't' = trace mode, 'q' = stop\n");
	  outMsg(logFile,"'x' = step until success, 'F' = force backtrack\n");
	  outMsg(logFile,"'<n>' = step <n>\n");
	  outMsg(logFile,"'i <count> = list <count> instructions\n");
	  outMsg(logFile,"'S' = symbolic mode\n");
	  outMsg(logFile,"'r' = show registers, 'a <n>' = show A[n], 'y <n>' = show Y[n]\n");
	  outMsg(logFile,"'s' = show stack trace\n");
	  outMsg(logFile,"'P' = show all processes\n");
	  outMsg(logFile,"'f' = focus on this process\n");
	  outMsg(logFile,"'u' = unfocus \n");

	  continue;
	}
      }
      pthread_mutex_unlock(&debugMutex);
      return Ok;
    }
  }
  pthread_mutex_unlock(&debugMutex);
  return Ok;
}
#endif /* EXECTRACE */

static inline long min(long a,long b)
{
  if(a<b)
    return a;
  else
    return b;
}

#ifdef EXECTRACE		/* only compiled if debugging */

#undef instruction
#define instruction(Nm,Cd,A1,A2,Cmt) \
  case Nm:                      /* Cmt */\
  return outIns(logFile,#Nm,A1,A2,code,pc,pcx,a,y,S,mode,B,hBase,hLimit);

static insPo outIns(ioPo out,char *Nm,opAndSpec A1,opAndSpec A2,
		    codePo code,insPo pc,insWord pcx,
		    ptrPo a,ptrPo y,ptrPo S,
		    rwmode mode,choicePo B,ptrPo hBase,ptrPo hLimit);


/* disassemble instruction at pc */
static insPo dissass(uniChar *pref,codePo code,insPo pc,ptrPo a,ptrPo y,ptrPo S,rwmode mode,choicePo B,ptrPo hBase,ptrPo hLimit)
{
  register insWord pcx = *pc;

  if(pref!=NULL)
    outMsg(logFile,"%U",pref);

  switch(pcx&op_mask){
#include "instructions.h"
  default:			/* illegal instruction */
    outMsg(logFile,"unknown[%x]",pcx);
    return pc+1;
  }
}

#ifdef EXECTRACE
void showInstructions(codePo code,insPo pc,int count)
{
   insPo tmpPc = pc;
          
    while(count-->0){
      tmpPc = dissass(NULL,code,tmpPc,NULL,NULL,NULL,dummyMode,NULL,NULL,NULL);
      outMsg(logFile,"\n");
   }
   flushFile(logFile);
}
#endif

static char *showOpAnd(ioPo out,char *sep,opAndSpec A,
		       insWord pcx,
		       rwmode mode,ptrPo a,ptrPo y,ptrPo S,ptrPo Lits,
		       ptrPo hBase,ptrPo hLimit)
{
  switch(A){
  case nOp:                             // No operand
    return sep;
  case iAh:                             // input argument register in upper slot (0..255)
    if(mode==dummyMode)
      outMsg(out,"%sA[%d]",sep,op_h_val(pcx));
    else
      outMsg(out,"%sA[%d]=%,3w",sep,op_h_val(pcx),&a[op_h_val(pcx)]);
    return ",";
  case oAh:                             // output argument register in upper slot (0..255)
    outMsg(out,"%sA[%d]",sep,op_h_val(pcx));
    return ",";
  case iAm:                             // input argument register in middle slot (0..255)
    if(mode==dummyMode)
      outMsg(out,"%sA[%d]",sep,op_m_val(pcx));
    else
      outMsg(out,"%sA[%d]=%,3w",sep,op_m_val(pcx),&a[op_m_val(pcx)]);
    return ",";
  case oAm:                             // output argument register in middle slot (0..255)
    outMsg(out,"%sA[%d]",sep,op_m_val(pcx));
    return ",";
  case iAl:                             // input argument register in lower slot (0..255)
    if(mode==dummyMode)
      outMsg(out,"%sA[%d]",sep,op_l_val(pcx));
    else
      outMsg(out,"%sA[%d]=%,3w",sep,op_l_val(pcx),&a[op_l_val(pcx)]);
    return ",";
  case oAl:                             // output argument register in lower slot (0..255)
    outMsg(out,"%sA[%d]",sep,op_l_val(pcx));
    return ",";

  case iLh:                             // input local variable offset (0..255)
    if(mode==dummyMode)
      outMsg(out,"%sY[%d]",sep,op_h_val(pcx));
    else
      outMsg(out,"%sY[%d]=%,3w",sep,op_h_val(pcx),&y[-op_h_val(pcx)]);
    return ",";
  case iLm:                             // input local variable offset (0..255)
    if(mode==dummyMode)
      outMsg(out,"%sY[%d]",sep,op_m_val(pcx));
    else
      outMsg(out,"%sY[%d]=%,3w",sep,op_m_val(pcx),&y[-op_m_val(pcx)]);
    return ",";
  case iLl:                             // input local variable offset (0..255)
    if(mode==dummyMode)
      outMsg(out,"%sY[%d]",sep,op_l_val(pcx));
    else
      outMsg(out,"%sY[%d]=%,3w",sep,op_l_val(pcx),&y[-op_l_val(pcx)]);
    return ",";
  case iLc:                             // input local variable offset (0..65535)
    if(mode==dummyMode)
      outMsg(out,"%sY[%d]",sep,op_o_val(pcx));
    else
      outMsg(out,"%sY[%d]=%,3w",sep,op_o_val(pcx),&y[-op_o_val(pcx)]);
    return ",";
  case oLh:                             // output local variable offset (0..255)
    outMsg(out,"%sY[%d]",sep,op_h_val(pcx));
    return ",";
  case oLm:                             // output local variable offset (0..255)
    outMsg(out,"%sY[%d]",sep,op_m_val(pcx));
    return ",";
  case oLl:                             // output local variable offset (0..255)
    outMsg(out,"%sY[%d]",sep,op_l_val(pcx));
    return ",";
  case oLc:                             // output local variable offset  (0..65535)
    outMsg(out,"%sY[%d]",sep,op_o_val(pcx));
    return ",";
  case iSt:                             // input at current structure pointer
    if(mode==readMode)
      outMsg(out,"%sS++=%,3w",sep,S);
    else
      outMsg(out,"%sS++",sep);
    return ",";
  case oSt:                             // output to current structure pointer
    outMsg(out,"%sS++",sep);
    return ",";
  case oAr:                             // Output arity in upper slot
  case uAr:                             // Arity in upper slot
    outMsg(out,"%s#%d",sep,op_sh_val(pcx));
    return ",";
  case uLt:                             // small literal in upper slot (-128..127)
    outMsg(out,"%s%d",sep,op_sh_val(pcx));
    return ",";
  case Lt:                              // 16bit literal (-32768..32767)
  case vSz:                             // Size of local variable vector
  case lSz:                             // Size of local variable vector
  case cSz:             		// Structure size
    outMsg(out,"%s%d",sep,op_o_val(pcx));
    return ",";
  case Es:                              // escape code (0..65535)
    outMsg(out,"%s%s",sep,escapeName(op_o_val(pcx)));
    return ",";
  case pcr:                             // program counter relative offset (-32768..32767)
    outMsg(out,"%spc%+d",sep,op_so_val(pcx));
    return ",";
  case pcl:                             // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
    outMsg(out,"%spc%+d",sep,op_ll_val(pcx));
    return ",";
  case ltl:                             // literal number (0..65535)
    outMsg(out,"%s%,3w",sep,&Lits[op_o_val(pcx)]);
    return ",";
  default:
    syserr("Problem in generating showing type");
    return NULL;
  }
}
		 

static insPo outIns(ioPo out,char *Nm,opAndSpec A1,opAndSpec A2,
		    codePo code,insPo pc,insWord pcx,
		    ptrPo a,ptrPo y,ptrPo S,
		    rwmode mode,choicePo B,ptrPo hBase,ptrPo hLimit)
{
  char *sep = " ";
  ptrPo Lits = codeLits(code);

  outMsg(out,"[%d]: %s",pc-code->data,Nm);
  
  sep = showOpAnd(out,sep,A1,pcx,mode,a,y,S,Lits,hBase,hLimit);
  sep = showOpAnd(out,sep,A2,pcx,mode,a,y,S,Lits,hBase,hLimit);

  return pc+1;
}
#endif /* EXECTRACE */

