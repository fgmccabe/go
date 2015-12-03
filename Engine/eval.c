/* 
  The run-time engine for Go!
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

  $Id: eval.c,v 1.9 2004/07/06 16:51:43 fmccabe Exp $
  $Log: eval.c,v $
  Revision 1.9  2004/07/06 16:51:43  fmccabe
  Fixed issue in estimating variables during load

  Revision 1.8  2004/05/06 21:14:41  fmccabe
  Tinkered with the debugging output

  Revision 1.7  2004/04/29 16:24:27  fmccabe
  Completely new type system

*/
#include "config.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include "go.h"			/* main header file */
#include "opcodes.h"		/* The definitions of the opcodes */
#include "dict.h"		/* Dictionary handling stuff */
#include "process.h"		/* Process handling */
#include "global.h"
#include "symbols.h"
#include "clock.h"
#include "hash.h"		/* we need access to the hash functions */
#include "errors.h"             /* Access error definitions */
#include "debug.h"		/* Debugger access functions */
#include "term.h"
#include "esc.h"

#ifndef TRAIL_FUDGE
#define TRAIL_FUDGE GO_REGS	/* How much room to leave for trail entries */
#endif

#ifdef EXECTRACE
#define testA(O) { assert(!identical(A[O],kvoid));}
#define testY(O) { assert(!identical(Y[-O],kvoid));}
#else
#define testA(O)
#define testY(O)
#endif

#define Areg(off) (&A[off])
#define Yreg(off) (&((ptrPo)C)[off])
#define Ereg(off) (&E[off])

static retCode uni(processPo P,choicePo B,ptrPo H,ptrPo T1,ptrPo T2);
static retCode mtch(processPo P,choicePo B,ptrPo H,ptrPo T1,ptrPo T2);

#ifdef DO_OCCURS_CHECK
static logical occCheck(ptrPo x,ptrPo v);
#else
#define occCheck(x,v) (False)
#endif

#define localVar(p)\
  ((p)>=(ptrPo)P->proc.sBase && (p)<(ptrPo)P->proc.sTop)

#define bindVr(d,v)					\
  do{							\
    assert(((d)>=(ptrPo)P->proc.heap.base && (d)<H)||			\
	   ((d)>=(ptrPo)P->proc.sBase && (d)<(ptrPo)P->proc.sTop));	\
									\
    if((d)<(B)->H || (d)>(ptrPo)B){					\
      P->proc.trail->var=d;						\
      P->proc.trail->val=*d;						\
      P->proc.trail++;							\
    }									\
    if(isSuspVar((d)) && (d)+1<H && (d)>=(ptrPo)P->proc.heap.base){	\
      suspensionPo susp = (suspensionPo)(d-1);				\
									\
      if(isvar(v) && isSuspVar((ptrPo)v)){				\
	suspensionPo other = (suspensionPo)(((ptrPo)v)-1);		\
	ptrPo tail = &other->goal;					\
									\
	while(IsList(*tail))						\
	  tail = listTail(objV(*tail));				\
									\
	bndVr(tail,susp->goal);					\
      }									\
      else{								\
	ptrPo tail = &susp->goal;					\
									\
	while(IsList(*tail))						\
	  tail = listTail(objV(*tail));				\
									\
	P->proc.F = SUSP_ACTIVE;					\
	bndVr(tail,P->proc.trigger);					\
	P->proc.trigger = susp->goal;					\
      }									\
    }									\
    *d = v;								\
  }while(0)

#define bndVr(d,v)					\
  do{							\
    assert(((d)>=(ptrPo)P->proc.heap.base && (d)<H)||			\
	   ((d)>=(ptrPo)P->proc.sBase && (d)<(ptrPo)P->proc.sTop));	\
									\
    if((d)<(B)->H || (d)>(ptrPo)(B)){					\
      P->proc.trail->var=d;						\
      P->proc.trail->val=*d;						\
      P->proc.trail++;							\
    }									\
    *d = v;								\
  }while(0)

#define saveRegs(pc)				\
  {						\
    P->proc.PC = pc;				\
    P->proc.cPC = cPC;				\
    P->proc.B = B;				\
    P->proc.SB = SB;				\
    P->proc.cSB = cSB;				\
    P->proc.T = T;				\
    P->proc.C = C;				\
    P->proc.cC = cC;				\
    P->proc.PROG = PROG;			\
    P->proc.cPROG = cPROG;			\
    P->proc.heap.create = (objPo)H;		\
  }

#define restRegs()				\
  {						\
    cPC = P->proc.cPC;				\
    B = P->proc.B;				\
    SB = P->proc.SB;				\
    cSB = P->proc.cSB;				\
    T = P->proc.T;				\
    C = P->proc.C;				\
    Y = (ptrPo)C;				\
    cC = P->proc.cC;				\
    PROG = P->proc.PROG;			\
    cPROG = P->proc.cPROG;			\
    PC = P->proc.PC;				\
    {						\
      codePo pc = codeV(PROG);			\
      Lits = codeLits(pc);			\
    }                                           \
    H = (ptrPo)P->proc.heap.create;		\
    A = &P->proc.A[0];				\
    assert(P->proc.state==runnable);		\
  }

#define backTrack()				\
  do{						\
        PC = B->PC;				\
	PROG = B->PROG;				\
	Lits = codeLits(codeV(PROG));		\
  }while(False)

/* Specialized allocation functions -- for use when allocating in
   the evaluator
*/

#ifdef EXECTRACE
#define enoughSpace(X) { assert(H+X<=P->proc.sBase); }
#else
#define enoughSpace(X)
#endif

/* Manage the activation of suspended variables */
void chainSuspension(processPo P,ptrPo p)
{
  suspensionPo susp = (suspensionPo)(p-1);
  P->proc.F = SUSP_ACTIVE;
  ptrPo tail = &susp->goal;

  while(IsList(*tail))
    tail = listTail(objV(*tail));

  bndVar(P,tail,P->proc.trigger);
  P->proc.trigger = susp->goal;
}

#ifdef EXECTRACE
#define isSvalid(C) {assert(mode==readMode?((S==NULL||inGlobalHeap((objPo)S) || (S>=(ptrPo)P->proc.heap.base && S<H)) && (Svalid-=C)>=0):True);}
#define validateS(N) {Svalid=N;}
#define lastValid() { assert(Svalid==0); }
#else
#define isSvalid(C)
#define validateS(N)
#define lastValid() 
#endif

#define allocframe(pc,cpc,len,ln) {					\
      register callPo nC;                                               \
                                                                        \
      if((ptrPo)C<(ptrPo)B)						\
          nC = (callPo)((ptrPo)(C-1)-len);                              \
      else								\
        nC = ((callPo)B)-1;                                             \
                                                                        \
      if(((trailPo)(((ptrPo)nC)-ln))-TRAIL_FUDGE<=P->proc.trail){       \
	saveRegs(PC);                                                   \
	if(extendStack(P,2,2,0)!=Ok)	/* grow the stack */            \
	  syserr("Unable to grow process stack");                       \
	restRegs();                                                     \
                                                                        \
        if((ptrPo)C<(ptrPo)B)						\
            nC = (callPo)((ptrPo)(C-1)-len);                            \
        else if((ptrPo)B<(ptrPo)T)                                      \
          nC = ((callPo)B)-1;                                           \
      }                                                                 \
                                                                        \
      cC = C;                                                           \
      C = nC;                                                           \
                                                                        \
      C->cPC = cPC;		/* where to return to */                \
      C->cSB = cSB;                                                     \
      C->cPROG = cPROG;                                                 \
      C->cC = cC;                                                       \
                                                                        \
      Y = (ptrPo)C;		/* negative values used for local variables */ \
  }

void runGo(register processPo P)
{
  register insWord PCX=0;		/* Current instruction register  */
  register insPo PC;		        /* program counter */
  insPo cPC;				/* continuation program counter */

  register ptrPo A;			/* Argument registers */

  ptrPo H;			        /* heap stack creation point */
  ptrPo trH;				/* heap trail marker */
  ptrPo S=NULL;				/* structure pointer */
  rwmode mode = readMode;
#ifdef EXECTRACE
  long Svalid = 0;			/* how long S is valid for */
#endif

  ptrI PROG;				/* Current program being executed */
  ptrI cPROG;				/* Continuation program */
  ptrPo Y;			        /* Current global variables */
  ptrPo Lits;				/* Pointer to the literals vector */

  callPo C;				/* Current call frame */
  callPo cC;				/* continuation call frame */

  choicePo B;				/* last choice point */
  choicePo SB;				/* where to cut */
  choicePo cSB;				/* continuation cut */			
  choicePo T;				/* Trap recovery point */

  uniChar errorMsg[MAX_MSG_LEN];

  restRegs();

  for(;;){			/* Loop forever, until execution terminates */
    assert(P->proc.heap.topRoot==0);
    assert(op_cde(*cPC)==gcmap);

#ifdef EXECTRACE
    pcCount++;
    extern int insCount[];
    insCount[op_cde(*PC)]++;

    if(debugging){
      switch(debug_stop(P,PROG,PC,cPROG,cPC,A,(ptrPo)C,S,Svalid,mode,B,SB,T,
			(ptrPo)P->proc.heap.base,H,
			P->proc.trail,P->proc.thread)){
      case Ok:
	break;
      case Fail:
	backTrack();                   
      default:
	;
      }
    }
#endif

    PCX=*PC++;			/* pick up the next instruction */
    switch(op_cde(PCX)){

    case halt:				/* stop execution */
      go_exit(0);			/* exit the program */
      return;

    case die:				/* kill this process */
#ifdef LOCKTRACE
      if(traceLock)
	outMsg(logFile,"%w: terminated\n",&P->proc.thread);
#endif
      pthread_exit(NULL);		/* Abort the process */

    case succ:                          /* succeed clause */
      if(P->proc.F){			/* have we triggered anything? */
	SB = B;
	P->proc.F = 0;			/* reset the flag */

        A[1] = P->proc.trigger;  /* and pick up the trigger list */
	P->proc.trigger = emptyList;
        PROG = ProgramOf(kdelay);	/* The standard delay handler */

        {
          codePo code = codeV(PROG);
          PC = codeIns(code);
          Lits = codeLits(code);
	  SB = B;
          continue;
        }
      }
      else{
	SB = cSB;			/* copy back the cut point */
	PC = cPC;			/* continue from parent call */
	PROG = cPROG;			/* and the environment itself */
	Lits = CodeLits(PROG);
	PC++;				/* skip over the gcmap instruction */
	continue;
      }

    case kawl:{                         /* call Ar,prog call program */
      if(P->proc.F){			/* We have been interrupted */
	int arity;

	// Compute the arity of the call, so we can save the right registers

	{
	  ptrI prog = Lits[op_o_val(PCX)];

	  if(IsDefined(prog))
	    arity = codeArity(codeV(ProgramOf(prog)));
	  else
	    arity = GO_REGS;
	}

	cPC = PC;			/* emulate a kawl, return back to this instruction */
	cPROG = PROG;
	cSB = SB;
	SB = B;

	allocframe(PC,cPC,envSize(PC),arity+1); /* allocate a frame to store the current arguments */

	P->proc.F = 0;			/* reset the flag */

        {
          int i;			/* store the current argument registers */

          for(i=1;i<=arity;i++)
            Y[-i] = A[i];
        }

	/* emulate a kawl to the delay handler */
        cPROG = doResume[arity];	/* pick the right code to return via */
        cPC = FirstInstruction(cPROG);

        A[1] = P->proc.trigger;		/* and pick up the trigger list */
	P->proc.trigger = emptyList;
        PROG = ProgramOf(kdelay);	/* The standard delay handler */

	codePo code = codeV(PROG);
	PC = codeIns(code);
	Lits = codeLits(code);
	SB = B;
	continue;
      }
      else{
	ptrI prog = Lits[op_o_val(PCX)];

	if(IsDefined(prog)){
	  cPC = PC;                       /* We will be returning here */
	  cPROG = PROG;
	  cSB = SB;
	  SB = B;
        
	  PROG = ProgramOf(prog);	/* We have a new environment */
	  codePo code = codeV(PROG);
	  PC = codeIns(code);
	  Lits = codeLits(code);

#ifdef EXECTRACE
	if(traceEscapes)
	  showCall(P,prog,&A[1],codeArity(code));
#endif

	  continue;
	}
	else{
	  strMsg(errorMsg,NumberOf(errorMsg),"%w undefined",&prog);
	  saveRegs(PC);
	  raiseError(P,errorMsg,eCODE);
	  restRegs();
	  continue;
	}
      }
    }
      
    case lkawl:{                  /* Depth,Lit tail recursive call to program */
      ptrI prog = Lits[op_o_val(PCX)];

      if(IsProgLbl(prog)){
	SB = B;
	  
	PROG = ProgramOf(prog);		/* We have a new program to execute */

	codePo code = codeV(PROG);
	PC = codeIns(code);
	Lits = codeLits(code);

#ifdef EXECTRACE
	if(traceEscapes)
	  showCall(P,prog,&A[1],codeArity(code));
#endif

	continue;
      }
      else{
	strMsg(errorMsg,NumberOf(errorMsg),"%w not legal or not defined",&prog);
	saveRegs(PC);
	raiseError(P,errorMsg,eCODE);
	restRegs();
	continue;
      }
    }

    case dlkawl:{                       /* deallocating last call */
      ptrI prog = Lits[op_o_val(PCX)];

      if(IsProgLbl(prog)){
	PROG = ProgramOf(prog);		/* We have a new environment */
	  
	SB = B;
	  
	codePo code = codeV(PROG);
	PC = codeIns(code);
	Lits = codeLits(code);
	  
	cPC = C->cPC;
	cSB = C->cSB;
	cPROG = C->cPROG;
	
	C = C->cC;
	Y = (ptrPo)C;

#ifdef EXECTRACE
	if(traceEscapes)
	  showCall(P,prog,&A[1],codeArity(code));
#endif

	continue;
      }
      else{
	strMsg(errorMsg,NumberOf(errorMsg),"%w not defined or not legal",&prog);
	saveRegs(PC);
	raiseError(P,errorMsg,eCODE);
	restRegs();
	continue;
      }
    }


    case kawlO:{                        /* call O(Gl,Ob,Th) call program */
      if(P->proc.F){
	cPC = PC;	   /* emulate a kawl, return back to this instruction */
	cPROG = PROG;
	cSB = SB;
	SB = B;

	allocframe(PC,cPC,envSize(PC),OBJECT_ARITY+1); /* allocate a frame to store the current arguments */

	P->proc.F = 0;			/* reset the flag */

        {
          int i;			/* store the current argument registers */

          for(i=1;i<=OBJECT_ARITY;i++)
            Y[-i] = A[i];
        }

	/* emulate a kawl to the delay handler */
        cPROG = doResume[OBJECT_ARITY];	/* pick the right code to return via */
        cPC = FirstInstruction(cPROG);

        A[1] = P->proc.trigger;  /* and pick up the trigger list */
	P->proc.trigger = emptyList;
        PROG = ProgramOf(kdelay);	/* The standard delay handler */

        {
          codePo code = codeV(PROG);
          PC = codeIns(code);
          Lits = codeLits(code);
	  SB = B;
          continue;
        }
      }
      else{
	ptrI prog = deRefI(&A[op_m_val(PCX)]); /* note that we deref through suspensions */

#ifdef EXECTRACE
	if(traceEscapes)
	  showOCall(P,&prog,&A[1],&A[3]);
#endif

	switch(ptg(prog)){
	case varTg:
	  A[op_m_val(PCX)]=thingClass;
	  prog = ProgramOf(thingProg);

	  break;                          /* This should eventually raise an error */
	case fwdTg:
	  strMsg(errorMsg,NumberOf(errorMsg),"%w invalid program label",&A[op_m_val(PCX)]);
	  saveRegs(PC);
	  raiseError(P,errorMsg,eINVAL);
	  restRegs();
	  continue;
	default:
	case objTg:{
	  objPo p = objV(prog);

	  if(isGoObject(p))
	    prog = objectCode(goObjV(prog));
	  else if(isObjct(p))
	    prog = ProgramOf(programOfClass(objV(p->class)));
	  else{
	    assert(isSpecialObject(p));
	    prog = specialProgram(p);
	  }
	  break;
	}
	}

	if(prog==0 || !IsCode(prog)){
	  strMsg(errorMsg,NumberOf(errorMsg),"%w no program for ",&A[op_m_val(PCX)]);
	  saveRegs(PC);
	  raiseError(P,errorMsg,eINVAL);
	  restRegs();
	  continue;
	}
	else{
	  codePo code = codeV(prog);
	  
	  cPC = PC;			/* We will be returning here */
	  cPROG = PROG;
	  cSB = SB;
	  SB = B;
	  
	  PROG = prog;			/* We have a new environment */
	  PC = codeIns(code);
	  Lits = codeLits(code);
	  continue;
	}
      }
    }
      
    case lkawlO:{                  /* Depth,Lit tail recursive call to program */
      ptrI prog = deRefI(&A[op_m_val(PCX)]); /* note that we deref through suspensions */

#ifdef EXECTRACE
	if(traceEscapes)
	  showOCall(P,&prog,&A[1],&A[3]);
#endif

      switch(ptg(prog)){
      case varTg:
	A[op_m_val(PCX)]=thingClass;
	prog = ProgramOf(thingProg);
	break;                          /* This should eventually raise an error */
      case fwdTg:
	strMsg(errorMsg,NumberOf(errorMsg),"%w invalid program label",&A[op_m_val(PCX)]);
	saveRegs(PC);
	raiseError(P,errorMsg,eINVAL);
	restRegs();
	continue;
      default:
      case objTg:{
	objPo p = objV(prog);
	
	if(isGoObject(p))
	  prog = objectCode(goObjV(prog));
	else if(isObjct(p))
	  prog = ProgramOf(programOfClass(objV(p->class)));
	else{
	  assert(isSpecialObject(p));
	  prog = specialProgram(p);
	}
	break;
      }
      }
      
      if(prog==0 || !IsCode(prog)){
	strMsg(errorMsg,NumberOf(errorMsg),"%w no program for ",&A[op_m_val(PCX)]);
	saveRegs(PC);
	raiseError(P,errorMsg,eINVAL);
	restRegs();
	continue;
      }
      else{
	codePo code = codeV(prog);
	
	SB = B;
      
	PROG = prog;			/* We have a new environment */
	PC = codeIns(code);
	Lits = codeLits(code);
	continue;
      }
    }

    case dlkawlO:{                       /* deallocating last call */
      ptrI prog = deRefI(&A[op_m_val(PCX)]); /* note that we deref through suspensions */

#ifdef EXECTRACE
      if(traceEscapes)
	showOCall(P,&prog,&A[1],&A[3]);
#endif

      switch(ptg(prog)){
      case varTg:
	A[op_m_val(PCX)]=thingClass;
	prog = ProgramOf(thingProg);
	break;                          /* This should eventually raise an error */
      case fwdTg:
	strMsg(errorMsg,NumberOf(errorMsg),"%w invalid program label",&A[op_m_val(PCX)]);
	saveRegs(PC);
	raiseError(P,errorMsg,eINVAL);
	restRegs();
	continue;
      default:
      case objTg:{
	objPo p = objV(prog);
	
	if(isGoObject(p))
	  prog = objectCode(goObjV(prog));
	else if(isObjct(p))
	  prog = ProgramOf(programOfClass(objV(p->class)));
	else{
	  assert(isSpecialObject(p));
	  prog = specialProgram(p);
	}
	break;
      }
      }
      
      if(prog==0 || !IsCode(prog)){
	strMsg(errorMsg,NumberOf(errorMsg),"%w no program for ",&A[op_m_val(PCX)]);
	saveRegs(PC);
	raiseError(P,errorMsg,eINVAL);
	restRegs();
      }
      else{
	codePo code = codeV(prog);
        
	SB = B;
          
	PROG = prog;		/* We have a new environment */
	PC = codeIns(code);
	Lits = codeLits(code);
        
	cPC = C->cPC;
	cSB = C->cSB;
	cPROG = C->cPROG;
	C = C->cC;
	Y = (ptrPo)C;
      }
      continue;
    }
      
    case go_to:
      PC+=op_ll_val(PCX);		/* Relative jump */
      continue;

    case escape:{			/* N, escape into 1st level builtins */
      if(P->proc.F){			/* first check for interrupts */
        int arity = op_h_val(PCX);

	cPC = PC;			/* emulate a kawl, return back to this instruction */
	cPROG = PROG;
	cSB = SB;
	SB = B;

	allocframe(PC,cPC,envSize(PC),arity+1); /* allocate a frame to store the current arguments */

	P->proc.F = 0;			/* reset the flag */

        {
          int i;			/* store the current argument registers */

          for(i=1;i<=arity;i++)
            Y[-i] = A[i];
        }

	/* emulate a kawl to the delay handler */
        cPROG = doResume[arity];	/* pick the right code to return via */
        cPC = FirstInstruction(cPROG);

        A[1] = P->proc.trigger;  /* and pick up the trigger list */
	P->proc.trigger = emptyList;
        PROG = ProgramOf(kdelay);	/* The standard delay handler */

        {
          codePo code = codeV(PROG);
          PC = codeIns(code);
          Lits = codeLits(code);
	  SB = B;
          continue;
        }
      }
      else{				/* Enter the regular handling of the escape */
	funpo ef = escapeCode(op_o_val(PCX));
	retCode ret;
	rootPo root = gcCurrRoot(&P->proc.heap);

	cPC = PC;			    /* make sure environment is registered Ok */
	cPROG = PROG;

	saveRegs(PC);

	checkForPause(P);		/* Has a pause been requested? */

#ifdef EXECTRACE
	countEscape(PCX);         /* Count escapes */
	if(ef==NULL){	        /* Something seriously wrong here */
	  syserr("invalid escape function");
	  return;
	}
	if(traceEscapes)
	  showEscape(P,op_o_val(PCX),&A[1],op_h_val(PCX));

#ifdef MEMTRACE
	if(traceMemory)
	  verifyProc(P);		/* Verify this process before the escape...*/
#endif
#endif

	ret = ef(P,A);
	gcRemoveRoot(&P->proc.heap,root); /* reset roots */

	switch(ret){
	case Ok:
	  restRegs();			/* restore registers in case of g/c */
	  assert(op_cde(*PC)==gcmap);
	  PC++;				/* skip over the gcmap instruction */
	  continue;
	case Fail:
	  restRegs();
	  backTrack();
	  continue;
	case Interrupt:			/* We were interrupted */
	  if(!checkForPause(P)){
	    strMsg(errorMsg,NumberOf(errorMsg),"interrupt");
	    saveRegs(PC);
	    raiseError(P,errorMsg,eINTRUPT);
	  }
	  restRegs();
	  continue;
	case Space:			/* Ran out of space */
	  strMsg(errorMsg,NumberOf(errorMsg),"system");
	  saveRegs(PC);
	  raiseError(P,errorMsg,eSPACE);
	  restRegs();
	  continue;
	case Error:			/* Report a run-time error */
	  restRegs();			/* should be already setup */
	  continue;

	default:
	  logMsg(logFile,"Invalid return code `%d' from escape function `%s'",
		 ret,escapeName(op_so_val(PCX)));
	  syserr("Problem in escape function return code");
	}
	continue;
      }
    }

    case alloc:{		/* allocate call record  */
      register int len = envSize(cPC);  /* current length of environment */
      register callPo nC;

      if((ptrPo)C<(ptrPo)B){
        if((ptrPo)T<(ptrPo)C)
          nC = ((callPo)T)-1;
        else
          nC = (callPo)((ptrPo)(C-1)-len);
      }
      else if((ptrPo)B<(ptrPo)T)
        nC = ((callPo)B)-1;
      else
        nC = ((callPo)T)-1;

      assert(op_cde(*cPC)==gcmap);
      
      if(((trailPo)(((ptrPo)nC)-op_o_val(PCX)))-TRAIL_FUDGE<=P->proc.trail){
	saveRegs(PC);
	if(extendStack(P,2,2,0)!=Ok)	/* grow the stack */
	  syserr("Unable to grow process stack");
	restRegs();

        if((ptrPo)C<(ptrPo)B){
          if((ptrPo)T<(ptrPo)C)
            nC = ((callPo)T)-1;
          else
            nC = (callPo)((ptrPo)(C-1)-len);
        }
        else if((ptrPo)B<(ptrPo)T)
          nC = ((callPo)B)-1;
        else
          nC = ((callPo)T)-1;
      }
      
      cC = C;
      C = nC;

      C->cPC = cPC;		/* where to return to */
      C->cSB = cSB;
      C->cPROG = cPROG;
      C->cC = cC;

      Y = (ptrPo)C;		/* negative values used for local variables */

      cPC = PC;	/* this'll be overridden, but cPC must point to valid envSize */
      cPROG = PROG;			/* in case of G/C */
      assert(op_cde(*PC)==gcmap);
      PC++;                             /* step over the GC map psuedo instruction */
      continue;
    }
      
    case dealloc:			/* deallocate and succeed */
      PC = cPC = C->cPC;
      SB = cSB = C->cSB;
      PROG = cPROG = C->cPROG;
      Lits = codeLits(codeV(PROG));

      C = C->cC;
      Y = (ptrPo)C;

      if(P->proc.F){			/* have we triggered anything? */
	SB = B;				/* move into the trigger handler code */
	P->proc.F = 0;			/* reset the flag */

        A[1] = P->proc.trigger;  /* and pick up the trigger list */
	P->proc.trigger = emptyList;
        PROG = ProgramOf(kdelay);	/* The standard delay handler */

        {
          codePo code = codeV(PROG);
          PC = codeIns(code);
          Lits = codeLits(code);
        }
      }
      else
	PC++;				/* skip over the gcmap instruction */
      continue;

    case trgr:	    /* test to see if we need to handle triggered suspensions */
      if(P->proc.F){
        int arity = op_h_val(PCX);

	cPC = PC;	   /* emulate a kawl, return back to this instruction */
	cPROG = PROG;
	cSB = SB;
	SB = B;

	allocframe(PC,cPC,envSize(PC),arity+1); /* allocate a frame to store the current arguments */

	P->proc.F = 0;			/* reset the flag */

        {
          int i;			/* store the current argument registers */

          for(i=1;i<=arity;i++)
            Y[-i] = A[i];
        }

	/* emulate a kawl to the delay handler */
        cPROG = doResume[arity];	/* pick the right code to return via */
        cPC = FirstInstruction(cPROG);

        A[1] = P->proc.trigger;  /* and pick up the trigger list */
	P->proc.trigger = emptyList;
        PROG = ProgramOf(kdelay);	/* The standard delay handler */

        {
          codePo code = programCode(objV(PROG));
          PC = codeIns(code);
          Lits = codeLits(code);
	  SB = B;
          continue;
        }
      }
      else
	PC++;
      continue;

    case resume:{
      register int arity = op_h_val(PCX);

      {
	int i;

	for(i=1;i<=arity;i++)
	  A[i] = Y[-i];
      }

      PC = cPC = C->cPC;
      SB = cSB = C->cSB;
      PROG = cPROG = C->cPROG;

      assert(op_cde(*PC)==gcmap);

      PC--;
      Lits = codeLits(codeV(PROG));

      C = C->cC;
      Y = (ptrPo)C;
      continue;
    }

    case tryme:{		/* ARITY,LBL try this clause */
      register int len = envSize(cPC);

      register short int arity = codeArity(codeV(PROG));

      register choicePo back;
      register short int i;
      register ptrPo ptr;

      if(B<(choicePo)C)
	back = (choicePo)((ptrPo)(B-1)-arity);
      else
	back = (choicePo)((ptrPo)(((choicePo)C)-1)-arity-len);

      if((trailPo)back<=P->proc.trail){
	saveRegs(PC);
	if(extendStack(P,2,2,0)!=Ok)	/* grow the stack */
	  syserr("Unable to grow process stack");
	restRegs();

	if(B<(choicePo)C)	/* reposition the new choice pointer */
	  back = (choicePo)((ptrPo)(B-1)-arity);
	else
	  back = (choicePo)((ptrPo)(((choicePo)C)-1)-arity-len);
      }

      back->AX = arity;
      back->PC = PC+op_ll_val(PCX);

      back->cPC = cPC;
      back->cSB = cSB;
      back->T = T;
      back->C = C;
      back->cPROG = cPROG;
      back->PROG = PROG;
      back->B = B;
      back->trail = P->proc.trail;
      back->H = H;
      trH = H;				/* mark where to make trail entries */

      assert(back->B->H<=H);

      B = back;
      ptr = (ptrPo)(B+1);

      for(i=1;i<=arity;i++)
	*ptr++=A[i];

      continue;
    }
      
    case retryme:{			/* LBL subsequent clause follows */
      register short int arity = B->AX;
      register short int i;

      {
	register trailPo tb = B->trail;
	register trailPo t = P->proc.trail;

	while(t>tb){               /* reset variables since last choice point */
	  t--;
	  *t->var = (ptrI)t->var;	/* restore to unbound */
	  //	  *t->var = t->val;             /* restore to previous value */
	}

	P->proc.trail = t;
      }

      {
	register ptrPo ptr = (ptrPo)(B+1); /* pick up the argument registers */

	for(i=1;i<=arity;i++)
	  A[i]=*ptr++;                  /* unstack the argument registers */
        A[0] = kvoid;
      }

      cPC = B->cPC;                     /* continuation point */
      cPROG = B->cPROG;                 /* continuation closure */
      SB = B->B;                        /* reset the slashback */
      cSB = B->cSB;
      T = B->T;                         /* restore trap handler also */
      C = B->C;
      Y = (ptrPo)C;
      assert(B->H>=(ptrPo)P->proc.heap.base && B->H<=H); /* check this one */
      H = trH = B->H;			/* reset the stack heap */
      P->proc.F = 0;			/* reset the flag */

      B->PC = PC+op_ll_val(PCX);	/* next clause to try */
      continue;
    }
      
    case trustme:{			/* this is last clause to try */
      register short int arity = B->AX;
      register short int i;

      {
	register trailPo tb = B->trail;
	register trailPo t = P->proc.trail;

	while(t>tb){               /* reset variables since last choice point */
	  t--;
	  *t->var = (ptrI)t->var;	/* restore to unbound */
	  //	  *t->var = t->val;             /* restore to previous value */
	}

	P->proc.trail = t;
      }

      {
	register ptrPo ptr = (ptrPo)(B+1); /* pick up the argument registers */

	for(i=1;i<=arity;i++)
	  A[i]=*ptr++;                  /* unstack the argument registers */
        A[0] = kvoid;
      }

      cPC = B->cPC;                     /* continuation point */
      cPROG = B->cPROG;                 /* continuation closure */
      SB = B->B;                        /* reset the slashback */
      cSB = B->cSB;
      T = B->T;                         /* restore trap handler also */
      C = B->C;
      Y = (ptrPo)C;
      assert(B->H>=(ptrPo)P->proc.heap.base && B->H<=H); /* check this one */
      H = trH = B->H;			/* reset the stack heap */
      P->proc.F = 0;			/* reset the flag */

      B = B->B;
      continue;
    }
      
    case trycl:{			/* ARITY,LBL try a clause */
      register int len = envSize(cPC);
      register short int arity = codeArity(codeV(PROG));

      register choicePo back;
      register int i;
      register ptrPo ptr;

      if(B<(choicePo)C)
	back = (choicePo)((ptrPo)(B-1)-arity);
      else
	back = (choicePo)((ptrPo)(((choicePo)C)-1)-arity-len);

      if((trailPo)back<=P->proc.trail){
	saveRegs(PC-1);
	if(extendStack(P,2,2,0)!=Ok)	/* grow the stack */
	  syserr("Unable to grow process stack");
	restRegs();
	PC++;

	if(B<(choicePo)C)	/* reposition the new choice pointer */
	  back = (choicePo)((ptrPo)(B-1)-arity);
	else
	  back = (choicePo)((ptrPo)(((choicePo)C)-1)-arity-len);
      }

      back->AX = arity;
      back->PC = PC;
      back->cPC = cPC;
      back->cSB = cSB;
      back->T = T;
      back->C = C;
      back->cPROG = cPROG;
      back->PROG = PROG;
      back->B = B;
      back->trail = P->proc.trail;
      back->H = trH = H;

      assert(back->B->H<=H);
      assert(op_cde(*back->PC)==retry||op_cde(*back->PC)==trust||
	     op_cde(*back->PC)==retryme||op_cde(*back->PC)==trustme);

      B = back;
      ptr = (ptrPo)(B+1);

      for(i=1;i<=arity;i++)
	*ptr++=A[i];

      PC += op_ll_val(PCX);
      continue;
    }

    case retry:{		/* LBL try a subsequent clause */
      register short int arity = B->AX;
      register short int i;

      {
	register trailPo tb = B->trail;
	register trailPo t = P->proc.trail;

	while(t>tb){               /* reset variables since last choice point */
	  t--;
	  *t->var = (ptrI)t->var;	/* restore to unbound */
	  //	  *t->var = t->val;             /* restore to previous value */
	}

	P->proc.trail = t;
      }

      {
	register ptrPo ptr = (ptrPo)(B+1); /* pick up the argument registers */

	for(i=1;i<=arity;i++)
	  A[i]=*ptr++;                  /* unstack the argument registers */
        A[0] = kvoid;
      }

      cPC = B->cPC;                     /* continuation point */
      cPROG = B->cPROG;                 /* continuation closure */
      SB = B->B;                        /* reset the slashback */
      cSB = B->cSB;
      T = B->T;                         /* restore trap handler also */
      C = B->C;
      Y = (ptrPo)C;
      assert(B->H>=(ptrPo)P->proc.heap.base && B->H<=H); /* check this one */
      H = trH = B->H;			/* reset the stack heap */
      P->proc.F = 0;			/* reset the flag */

      B->PC = PC;
      PC += op_ll_val(PCX);
      continue;
    }
      
    case trust:{		/* LBL last clause to try */
      register short int arity = B->AX;
      register short int i;

      {
	register trailPo tb = B->trail;
	register trailPo t = P->proc.trail;

	while(t>tb){               /* reset variables since last choice point */
	  t--;
	  *t->var = (ptrI)t->var;	/* restore to unbound */
	  //	  *t->var = t->val;             /* restore to previous value */
	}

	P->proc.trail = t;
      }

      {
	register ptrPo ptr = (ptrPo)(B+1); /* pick up the argument registers */

	for(i=1;i<=arity;i++)
	  A[i]=*ptr++;                  /* unstack the argument registers */
        A[0] = kvoid;
      }

      cPC = B->cPC;                     /* continuation point */
      cPROG = B->cPROG;                 /* continuation closure */
      SB = B->B;                        /* reset the slashback */
      cSB = B->cSB;
      T = B->T;                         /* restore trap handler also */
      C = B->C;
      Y = (ptrPo)C;
      assert(B->H>=(ptrPo)P->proc.heap.base && B->H<=H); /* check this one */
      H = trH = B->H;			/* reset the stack heap */
      P->proc.F = 0;			/* reset the flag */

      B = B->B;
      PC += op_ll_val(PCX);             /* new place to go to */
      continue;
    }
	
    case fayl:{                         /* fail execution */
      backTrack();                      /* pick up the failure continuation */
      continue;
    }

    case cut:{
      if(SB!=B){		/* do we have any work to do? */
	register trailPo trS = SB->trail;
	register trailPo trE = trS;

	assert((ptrPo)SB>=P->proc.sBase && (ptrPo)SB<P->proc.sTop);
	assert(trS>=(trailPo)P->proc.sBase && trS<=P->proc.trail);
	    
	/* The core loop that copies down a fragment of the trail */
	while(trS!=P->proc.trail){
	  ptrPo V = trS->var;

	  if(V<(ptrPo)SB->H||V>=(ptrPo)SB)
	    *trE++=*trS++;	/* keep the trail entry */
	  else
	    trS++;		/* dont need the trail any more */
	}

	P->proc.trail = trE;		/* new trail */
	B = SB;			/* new backtrack point */
      }
      continue;
    }

    /* An indexc instruction indexes on characters.
       The first instruction that follows is taken for the variable case,
       after that is a hash table in the form of a jump table.
       It is an error for the argument not to be a symbol.
    */

    case indexc:{		/* Ai,max index symbol access */
      int max = op_o_val(PCX);
      register ptrI vx = deRefI(&A[op_h_val(PCX)]);
      register objPo vl = objV(vx);

      testA(op_h_val(PCX));

      if(!isvar(vx) && IsChar(vx)){
	long ix = (CharVal((charPo)vl))%max;

	PC+=ix+1;
      }
      continue;
    }

    /* The indexl instruction is used to differentiate nil from non-empty lists.
       You might call it a naive reverse instruction.
       The first instruction that follows is taken for the variable case,
       after is the instruction for the empty list, and after that the instruction
       for the non-empty list
    */

    case indexl:{		/* Ai */
      register ptrI vx = deRefI(&A[op_h_val(PCX)]);

      testA(op_h_val(PCX));

      if(!isvar(vx)){
	register objPo vl = objV(vx);

	if(vl->class==nilClass)
	  PC++;
	else{
	  //	  assert(vl->class==listClass);
	  PC+=2;
	}
      }
      continue;
    }



    /* An indexs instruction indexes on constant symbols.
       The first instruction that follows is taken for the variable case,
       after that is a hash table in the form of a jump table.
       It is an error for the argument not to be a symbol.
    */

    case indexs:{		/* Ai,max index symbol access */
      int max = op_o_val(PCX);
      register ptrI vx = deRefI(&A[op_h_val(PCX)]);

      testA(op_h_val(PCX));

      if(IsSymb(vx)){
	integer ix = (uniHash(SymVal(symbV(vx)))%max);

	PC+=ix+1;
      }
      continue;
    }

    /* An indexn instruction indexes on number.
       The first instruction that follows is taken for the variable case,
       after that is a hash table in the form of a jump table.
       It is an error for the argument not to be a number.
    */

    case indexn:{		/* Ai,max index number */
      integer max = op_o_val(PCX);
      register ptrI vx = deRefI(&A[op_h_val(PCX)]);
      register objPo vl = objV(vx);

      testA(op_h_val(PCX));

      if(!isvar(vx) && IsNumber(vl)){
        integer val = (integer)NumberVal(vl);
        
        PC+= val%max+1;
      }
      continue;
    }

    /* An indexx instruction indexes on constructor terms
       The first instruction that follows is taken for the variable case,
       after that is a hash table in the form of a jump table.
       It is an error for the argument not to be a symbol.
    */

    case indexx:{		/* Ai,max index symbol access */
      int max = op_o_val(PCX);
      register ptrI vx = deRefI(&A[op_h_val(PCX)]);
      register objPo vl = objV(vx);

      testA(op_h_val(PCX));

      if(!isvar(vx)){
	assert(isObjct(vl));

	integer ix = ((clssPo)objV(vl->class))->hash%max;

	PC+=ix+1;
      }
      continue;
    }

    case trpblk:{		/* Mark current choice point as trap recovery */
      T = B;
      continue;
    }

    case trpend:{                       /* drop a trap frame */
      T = T->T;
      continue;
    }

    case except:{			/* raise a run-time exception */
      ptrI E = deRefI(&A[op_h_val(PCX)]);
      rootPo root = gcAddRoot(&P->proc.heap,&E);

      switch(freezeTerm(&P->proc.heap,&E,E,
			P->proc.errorMsg,NumberOf(P->proc.errorMsg))){
      case Ok:
	break;
      case Error:
      default:
	syserr("failed to reserve space for an exception");
      }

      gcRemoveRoot(&P->proc.heap,root);

      B = T;				/* We fail also ... */
      PC = T->PC;			/* failure continuation */
      PROG = T->PROG;
      Lits = codeLits(codeV(PROG));

      ptrPo EA = (ptrPo)(T+1);

      EA[0] = E;		       /* The first argument will be the trap */
      continue;
    }
    
    case gc:{                   /* Check for space in heap, and invoke collector if not enough */
      register int size = op_o_val(PCX);

      if(P->proc.pauseRequest){		/* Are we supposed to pause? */
	saveRegs(PC-1);
	checkForPause(P);
	restRegs();
	PC++;
      }
      
      if(H+size>P->proc.sBase){
        saveRegs(PC-1);

#ifdef MEMTRACE
        if(traceMemory)
          outMsg(logFile,"calling GC for %d words, env=%d\n",size,envSize(cPC));
#endif
        gcCollect(&P->proc.heap,size); /* this aborts if there is no memory */
        restRegs();
	PC++;
      }
      continue;
    }
    
    case gcmap:			/* set the current g/c map, we dont do anything here with this though */
      continue;

    /* Unification instructions ... */
    case uAA:{			/* unify A[h],A[m] */
      testA(op_h_val(PCX));
      testA(op_m_val(PCX));
      
      switch(uni(P,B,H,&A[op_h_val(PCX)],&A[op_m_val(PCX)])){
        case Ok:
          continue;
        case Fail:
	  backTrack();
          continue;
        default:
          saveRegs(PC);
          raiseError(P,errorMsg,P->proc.errorCode);
          restRegs();
          continue;
      }
    }

    case uAY:{			/* unify A[h],Y[X] */
      testA(op_h_val(PCX));
      testY(op_o_val(PCX));

      switch(uni(P,B,H,&A[op_h_val(PCX)],Yreg(-op_o_val(PCX)))){
        case Ok:
          continue;
        case Fail:
	  backTrack();
          continue;
        default:
          saveRegs(PC);
          raiseError(P,errorMsg,P->proc.errorCode);
          restRegs();
          continue;
      }
    }

    case uAS:{			/* unify A[h],S++ */
      if(mode==readMode){
	isSvalid(1);		/* test for validity of S register */
        switch(uni(P,B,H,S++,&A[op_h_val(PCX)])){
          case Ok:
            continue;
          case Fail:
	    backTrack();
            continue;
          default:
            saveRegs(PC);
            raiseError(P,errorMsg,P->proc.errorCode);
            restRegs();
            continue;
        }
      }
      else{
        ptrI AA = deRefI(&A[op_h_val(PCX)]);
          
        if(isvar(AA) && localVar((ptrPo)AA)){
          bindVr((ptrPo)AA,unBind(H++)); /* We can globalise into the structure */
        }
        else
	  *H++ = AA;
      }

      continue;
    }

    case ucAS:{			/* unify A[h],S++ with occurs check on write */
      if(mode==readMode){
	isSvalid(1);		/* test for validity of S register */
        switch(uni(P,B,H,S++,&A[op_h_val(PCX)])){
          case Ok:
            continue;
          case Fail:
	    backTrack();
            continue;
          default:
            saveRegs(PC);
            raiseError(P,errorMsg,P->proc.errorCode);
            restRegs();
            continue;
        }
      }
      else{
        ptrPo Ap = deRef(&A[op_h_val(PCX)]);
        ptrI AA = *Ap;
          
        if(isvar(AA) && localVar((ptrPo)AA)){
          bindVr((ptrPo)AA,unBind(H++)); /* We can globalise into the structure */
        }
        else{
          if(occCheck(S,Ap)){
            strMsg(errorMsg,NumberOf(errorMsg),"system");
            saveRegs(PC);
            raiseError(P,errorMsg,eOCCUR);
            restRegs();
            continue;
          }
	  *H++ = AA;
	}
      }
      continue;
    }

    case uAlit:{		/* unify A[h],<lit> */
      register ptrPo ptr = deRef(&A[op_h_val(PCX)]);
      ptrI aVal = *ptr;
      ptrI tmp = Lits[op_o_val(PCX)];

      assert(inGlobalHeap(objV(Lits[op_o_val(PCX)]))||IsChar(Lits[op_o_val(PCX)]));
      testA(op_h_val(PCX));

      switch(ptg(aVal)){
      case varTg:
	bindVr(ptr,tmp);		/* bind to the literal value */
        continue;
      case objTg:{
	switch(uni(P,B,H,&aVal,&tmp)){
	case Ok:
	  continue;
	case Fail:
	  backTrack();
	  continue;
	default:
	  saveRegs(PC);
	  raiseError(P,errorMsg,P->proc.errorCode);
	  restRegs();
	  continue;
	}
      }
      default:
	backTrack();
        continue;
      }
    }

    case uAcns:{		/* unify A[h],f(_,..,_) */
      register ptrPo ptr = deRef(&A[op_h_val(PCX)]);
      register ptrI val = *ptr;
      register objPo obj = objV(val);
      register ptrI clss = Lits[op_o_val(PCX)];
#ifdef EXECTRACE
      register clssPo class = (clssPo)objV(clss);
#endif

      testA(op_h_val(PCX));

      enoughSpace(class->arity+1);	/* Is there room for the constructor */

      if(isvar(val)){
	objPo new = (objPo)H++;
	register ptrI cns = objP(new);

	new->class = clss;

	bindVr(ptr,cns);

	mode = writeMode;	/* we are now in writing mode... */
        validateS(-1);
      }
      else if(obj->class == clss){
	S = objectArgs(obj);		/* point to the first argument */
	mode = readMode;	/* modes only apply to unify instructions */
        validateS(class->arity);	/* validate S for n instructions */
      }
      else
	backTrack();

      continue;
    }

    case uAcns0:{		/* unify A[0],f(_,..,_) */
      register ptrPo ptr = deRef(&A[0]);
      register ptrI val = *ptr;
      register objPo obj = objV(val);
      register ptrI clss = Lits[op_o_val(PCX)];
#ifdef EXECTRACE
      register clssPo class = (clssPo)objV(clss);
#endif

      testA(0);

      enoughSpace(class->arity+1);	/* Is there room for the constructor */

      if(isvar(val)){
	objPo new = (objPo)H++;
	register ptrI cns = objP(new);

	new->class = clss;

	bindVr(ptr,cns);

	mode = writeMode;	/* we are now in writing mode... */
        validateS(-1);
      }
      else if(obj->class == clss){
	S = objectArgs(obj);		/* point to the first argument */
	mode = readMode;	/* modes only apply to unify instructions */
        validateS(class->arity);	/* validate S for n instructions */
      }
      else
	backTrack();

      continue;
    }

    case uAcns1:{		/* unify A[1],f(_,..,_) */
      register ptrPo ptr = deRef(&A[1]);
      register ptrI val = *ptr;
      register objPo obj = objV(val);
      register ptrI clss = Lits[op_o_val(PCX)];
#ifdef EXECTRACE
      register clssPo class = (clssPo)objV(clss);
#endif

      testA(1);

      enoughSpace(class->arity+1);	/* Is there room for the constructor */

      if(isvar(val)){
	objPo new = (objPo)H++;
	register ptrI cns = objP(new);

	new->class = clss;

	bindVr(ptr,cns);

	mode = writeMode;	/* we are now in writing mode... */
        validateS(-1);
      }
      else if(obj->class == clss){
	S = objectArgs(obj);		/* point to the first argument */
	mode = readMode;	/* modes only apply to unify instructions */
        validateS(class->arity);	/* validate S for n instructions */
      }
      else
	backTrack();

      continue;
    }

    case uAcns2:{		/* unify A[2],f(_,..,_) */
      register ptrPo ptr = deRef(&A[2]);
      register ptrI val = *ptr;
      register objPo obj = objV(val);
      register ptrI clss = Lits[op_o_val(PCX)];
#ifdef EXECTRACE
      register clssPo class = (clssPo)objV(clss);
#endif

      testA(2);

      enoughSpace(class->arity+1);	/* Is there room for the constructor */

      if(isvar(val)){
	objPo new = (objPo)H++;
	register ptrI cns = objP(new);

	new->class = clss;

	bindVr(ptr,cns);

	mode = writeMode;	/* we are now in writing mode... */
        validateS(-1);
      }
      else if(obj->class == clss){
	S = objectArgs(obj);		/* point to the first argument */
	mode = readMode;	/* modes only apply to unify instructions */
        validateS(class->arity);	/* validate S for n instructions */
      }
      else
	backTrack();

      continue;
    }

    case uAcns3:{		/* unify A[3],f(_,..,_) */
      register ptrPo ptr = deRef(&A[3]);
      register ptrI val = *ptr;
      register objPo obj = objV(val);
      register ptrI clss = Lits[op_o_val(PCX)];
#ifdef EXECTRACE
      register clssPo class = (clssPo)objV(clss);
#endif

      testA(3);

      enoughSpace(class->arity+1);	/* Is there room for the constructor */

      if(isvar(val)){
	objPo new = (objPo)H++;
	register ptrI cns = objP(new);

	new->class = clss;

	bindVr(ptr,cns);

	mode = writeMode;	/* we are now in writing mode... */
        validateS(-1);
      }
      else if(obj->class == clss){
	S = objectArgs(obj);		/* point to the first argument */
	mode = readMode;	/* modes only apply to unify instructions */
        validateS(class->arity);	/* validate S for n instructions */
      }
      else
	backTrack();

      continue;
    }

    case uAcns4:{		/* unify A[4],f(_,..,_) */
      register ptrPo ptr = deRef(&A[4]);
      register ptrI val = *ptr;
      register objPo obj = objV(val);
      register ptrI clss = Lits[op_o_val(PCX)];
#ifdef EXECTRACE
      register clssPo class = (clssPo)objV(clss);
#endif

      testA(4);

      enoughSpace(class->arity+1);	/* Is there room for the constructor */

      if(isvar(val)){
	objPo new = (objPo)H++;
	register ptrI cns = objP(new);

	new->class = clss;

	bindVr(ptr,cns);

	mode = writeMode;	/* we are now in writing mode... */
        validateS(-1);
      }
      else if(obj->class == clss){
	S = objectArgs(obj);		/* point to the first argument */
	mode = readMode;	/* modes only apply to unify instructions */
        validateS(class->arity);	/* validate S for n instructions */
      }
      else
	backTrack();

      continue;
    }

    case uYY:{			/* unify Y[h],Y[m] */
      testY(op_h_val(PCX));
      testY(op_m_val(PCX));

      switch(uni(P,B,H,Yreg(-op_h_val(PCX)),Yreg(-op_m_val(PCX)))){
        case Ok:
          continue;
        case Fail:
	  backTrack();
          continue;
        default:
          saveRegs(PC);
          raiseError(P,errorMsg,P->proc.errorCode);
          restRegs();
          continue;
      }
    }

    case uYS:{                          /* unify Y[X],S++ */
      if(mode==readMode){
	isSvalid(1);		/* test for validity of S register */
        switch(uni(P,B,H,S++,Yreg(-op_o_val(PCX)))){
          case Ok:
            continue;
          case Fail:
	    backTrack();
            continue;
          default:
            saveRegs(PC);
            raiseError(P,errorMsg,P->proc.errorCode);
            restRegs();
            continue;
        }
      }
      else{			/* write mode: copy Y into H++ */
	register ptrPo ptr = deRef(Yreg(-op_o_val(PCX)));
	register ptrI val = *ptr;

	if(isvar(val)){
	  bindVr(ptr,unBind(H++)); /* We can globalise into the structure */
	}
	else
	  *H++=val;
	continue;
      }
    }

    case ucYS:{			  /* unify Y[X],S++ with occurs check on write*/
      if(mode==readMode){
	isSvalid(1);			/* test for validity of S register */
        switch(uni(P,B,H,S++,Yreg(-op_o_val(PCX)))){
          case Ok:
            continue;
          case Fail:
	    backTrack();
            continue;
          default:
            saveRegs(PC);
            raiseError(P,errorMsg,P->proc.errorCode);
            restRegs();
            continue;
        }
      }
      else{			/* write mode: copy Y into S */
	register ptrPo ptr = deRef(Yreg(-op_o_val(PCX)));
	register ptrI val = *ptr;

	if(isvar(val)){
	  bindVr(ptr,unBind(H++)); /* We can globalise into the structure */
	}
	else{
	  *H++ = val;
	}
	continue;
      }
    }

    case uSlit:{			/* unify S++,<lit> */
      if(mode==readMode){
	register ptrPo ptr = deRef(S);
	ptrI val = *ptr;
        ptrI tmp = Lits[op_o_val(PCX)];

	isSvalid(1);			/* test for validity of S register */
        S++;

        switch(ptg(val)){
        case varTg:
          bindVr(ptr,tmp);              /* bind to the literal value */
          continue;
        case objTg:{
	  switch(uni(P,B,H,ptr,deRef(&tmp))){
	  case Ok:
	    continue;
	  case Fail:
	    backTrack();
	    continue;
	  default:
	    saveRegs(PC);
	    raiseError(P,errorMsg,P->proc.errorCode);
	    restRegs();
	    continue;
          }
        }
        default:
	  backTrack();
          continue;
        }
      }
      else
	*H++ = Lits[op_o_val(PCX)];

      continue;
    }

    case uScns:{                        /* unify S,f(_,..,_) */
      enoughSpace(op_o_val(PCX)+2); /* Verify that we have enough room for the constructor */

      if(mode==readMode){
	register ptrI val = deRefI(S);
	register objPo obj = objV(val);
        register ptrI clss = Lits[op_o_val(PCX)];

	isSvalid(1);			/* test for validity of S register */
	if(isvar(val)){
          objPo cns = (objPo)H++;

          cns->class = clss;
  
	  bindVr((ptrPo)obj,objP(cns));	/* bind to the constructor */

	  mode = writeMode;             /* we are now in writing mode... */
          validateS(-1);
	}
	else if(hasClass(obj,clss)){
	  S = objectArgs(obj);		/* point to the first argument */
          validateS(classArity((clssPo)objV(clss))); /* validate S for n instructions */
        }
	else
	  backTrack();
      }
      else{                             /* write mode */
        register ptrPo hH = H++;
	register objPo new = (objPo)H++;

	new->class = Lits[op_o_val(PCX)];

	*hH = objP(new);
      }
      continue;
    }
    
    /* Move instructions */

    case mAA:{			/* move A[h],A[m] */
      A[op_h_val(PCX)] = A[op_m_val(PCX)];
      //      testA(op_m_val(PCX));
      continue;
    }
      
    case mAY:{			/* move A[h],Y[X] */
      A[op_h_val(PCX)] = deRefI(Yreg(-op_o_val(PCX)));
      testY(op_o_val(PCX));
      continue;
    }
      
    case muAY:{			/* Move unsafe A[h],Y[X] */
      register ptrPo ptr = deRef(Yreg(-op_o_val(PCX)));
      register ptrI yVal = *ptr;

      if(isvar(yVal) && ptr>=(ptrPo)B->H && ptr<(ptrPo)B){ /* unsafe var */
	variablePo vv = (variablePo)H;
	vv->class = varClass;
	yVal = vv->val = (ptrI)(&vv->val);

	bindVr(ptr,yVal);
	H+=VariableCellCount;
      }

      A[op_h_val(PCX)] = yVal;
      continue;
    }
      
    case mAS:{			/* move A[h],S++ */
      if(mode==readMode){
	isSvalid(1);		/* test for validity of S register */
        A[op_h_val(PCX)] = deRefI(S++);
      }
      else
        A[op_h_val(PCX)] = unBind(H++);
      continue;
    }

    case mAlit:{		/* move A[h],<lit> */
      A[op_h_val(PCX)] = Lits[op_o_val(PCX)];
      continue;
    }

    case mAcns:{		/* move A[h],f(_,..,_) */
      register objPo new = (objPo)H++;

      new->class = Lits[op_o_val(PCX)];

      assert(isClass(new->class));

      A[op_h_val(PCX)] = objP(new);

      mode = writeMode;
      validateS(-1);
#ifdef EXECTRACE
      {
	long arity = classArity((clssPo)objV(new->class));
	long i;
	for(i=0;i<arity;i++)
	  new->args[i] = kvoid;
      }
#endif
      continue;
    }

    case mYA:{			/* move Y[X],A[h] */
      register ptrPo yVar = Yreg(-op_o_val(PCX));

      bindVr(yVar,deRefI(&A[op_h_val(PCX)])); /* we need to bind, 'cos we may need to undo this move*/
      
      testA(op_h_val(PCX));
      continue;
    }
      
    case mYY:{			/* move Y[h],Y[m] */
      register ptrPo yVar = Yreg(-op_h_val(PCX));

      bindVr(yVar,deRefI(Yreg(-op_m_val(PCX)))); /* we need to bind, 'cos we may need to undo this move*/
      continue;
    }
      
    case mYS:{			/* move Y[X],S++ */
      register ptrPo yVar = Yreg(-op_o_val(PCX));

      if(mode==readMode){
	isSvalid(1);		/* test for validity of S register */
        bindVr(yVar,deRefI(S++));
      }
      else
        bindVr(yVar,unBind(H++));
      continue;
    }

    case mSA:{			/* Move S++,A[h] */
      register ptrPo ptr = deRef(&A[op_h_val(PCX)]);
      register ptrI val = *ptr;

      if(isvar(val) && localVar(ptr)){
	bindVr(ptr,unBind(H++));
      }
      else
	*H++ = val;
      continue;
    }

    case mSY:{			/* Move S++,Y[X] */
      register ptrPo ptr = deRef(Yreg(-op_o_val(PCX)));
      register ptrI val = *ptr;

      if(isvar(val) && localVar(ptr)){
	bindVr(ptr,unBind(H++));
      }
      else
	*H++ = val;

      continue;
    }

    case mSlit:{			/* Move S++,<lit> */
      *H++ = Lits[op_o_val(PCX)];
      continue;
    }

    case mScns:{			/* Move S++,f(_,..,_) */
      register ptrPo hH = H++;
      register objPo new = (objPo)H++;

      new->class = Lits[op_o_val(PCX)];

      *hH = objP(new);
      mode = writeMode;                 // We are in write mode
      validateS(-1);
      continue;
    }

    case oAU:{                          /* overwrite A[h] with new variable */
      unBind(H);
      A[op_h_val(PCX)] = (ptrI)H++;
      continue;
    }

    case oYU:{                          /* overwrite Y[O] with unbound variable */
      register ptrPo ptr = Yreg(-op_o_val(PCX));

      bindVr(ptr,unBind(ptr));
      continue;
    }

    case oYA:{                          /* overwrite Y[L] with A[h] */
      register ptrPo ptr = Yreg(-op_o_val(PCX));

      bindVr(ptr,deRefI(&A[op_h_val(PCX)]));
      continue;
    }
      
    case oYnil:{                        /* overwrite Y[L] with nil */
      register ptrPo ptr = Yreg(-op_o_val(PCX));

      bindVr(ptr,emptyList);
      continue;
    }

    /* Matching instructions ...  */
    case cAA:{			/* Match A[h],A[m] */
      testA(op_h_val(PCX));
      testA(op_m_val(PCX));
      
      switch(mtch(P,B,H,&A[op_h_val(PCX)],&A[op_m_val(PCX)])){
        case Ok:
          continue;
        case Fail:
	  backTrack();
          continue;
        default:
          saveRegs(PC);
          raiseError(P,errorMsg,P->proc.errorCode);
          restRegs();
          continue;
      }
    }
    
    case cAY:{			/* match A[h],Y[X] */
      testA(op_h_val(PCX));
      testY(op_o_val(PCX));

      switch(mtch(P,B,H,&A[op_h_val(PCX)],Yreg(-op_o_val(PCX)))){
        case Ok:
          continue;
        case Fail:
	  backTrack();
          continue;
        default:
          saveRegs(PC);
          raiseError(P,errorMsg,P->proc.errorCode);
          restRegs();
          continue;
      }
    }

    case cAS:{			/* match A[h],S++ */
      isSvalid(1);		/* test for validity of S register */

      if(mode==readMode){
        switch(mtch(P,B,H,&A[op_h_val(PCX)],S++)){
          case Ok:
            continue;
          case Fail:
	    backTrack();
            continue;
          default:
            saveRegs(PC);
            raiseError(P,errorMsg,P->proc.errorCode);
            restRegs();
            continue;
        }
      }
      else
	backTrack();

      continue;
    }

    case cAlit:{		/* match A[h],<lit> */
      register ptrPo ptr = deRef(&A[op_h_val(PCX)]);
      register ptrI aVal = *ptr;

      assert(inGlobalHeap(objV(Lits[op_o_val(PCX)])));
      testA(op_h_val(PCX));

      switch(ptg(aVal)){
      case varTg:
	backTrack();                   /* not permitted to bind when matching */
        continue;
      case objTg:{
	switch(mtch(P,B,H,ptr,deRef(&Lits[op_o_val(PCX)]))){
	case Ok:
	  continue;
	case Fail:
	  backTrack();
	  continue;
	default:
	  saveRegs(PC);
	  raiseError(P,errorMsg,P->proc.errorCode);
	  restRegs();
	  continue;
	}
      }
      default:
	backTrack();
        continue;
      }
    }

    case cAcns:{		/* match A[h],f(_,..,_) */
      register ptrI val = deRefI(&A[op_h_val(PCX)]);
      register objPo obj = objV(val);
      register ptrI clss = Lits[op_o_val(PCX)];

      testA(op_h_val(PCX));

      if(hasClass(obj,clss)){
	S = objectArgs(obj);		/* point to the first argument */
	mode = readMode;	/* modes only apply to unify instructions */
        validateS(classArity((clssPo)objV(clss)));	/* validate S for n instructions */
      }
      else
	backTrack();

      continue;
    }

    case cAcns0:{		/* match A[0],f(_,..,_) */
      register ptrI val = deRefI(&A[0]);
      register objPo obj = objV(val);
      register ptrI clss = Lits[op_o_val(PCX)];

      testA(0);

      if(hasClass(obj,clss)){
	S = objectArgs(obj);		/* point to the first argument */
	mode = readMode;	/* modes only apply to unify instructions */
        validateS(classArity((clssPo)objV(clss)));	/* validate S for n instructions */
      }
      else
	backTrack();

      continue;
    }
    
    case cAcns1:{		/* match A[1],f(_,..,_) */
      register ptrI val = deRefI(&A[1]);
      register objPo obj = objV(val);
      register ptrI clss = Lits[op_o_val(PCX)];

      testA(1);

      if(hasClass(obj,clss)){
	S = objectArgs(obj);		/* point to the first argument */
	mode = readMode;	/* modes only apply to unify instructions */
        validateS(classArity((clssPo)objV(clss)));	/* validate S for n instructions */
      }
      else
	backTrack();

      continue;
    }
    
    case cAcns2:{		/* match A[2],f(_,..,_) */
      register ptrI val = deRefI(&A[2]);
      register objPo obj = objV(val);
      register ptrI clss = Lits[op_o_val(PCX)];

      testA(2);

      if(hasClass(obj,clss)){
	S = objectArgs(obj);		/* point to the first argument */
	mode = readMode;	/* modes only apply to unify instructions */
        validateS(classArity((clssPo)objV(clss)));	/* validate S for n instructions */
      }
      else
	backTrack();

      continue;
    }
    
    case cAcns3:{		/* match A[3],f(_,..,_) */
      register ptrI val = deRefI(&A[3]);
      register objPo obj = objV(val);
      register ptrI clss = Lits[op_o_val(PCX)];

      testA(3);

      if(hasClass(obj,clss)){
	S = objectArgs(obj);		/* point to the first argument */
	mode = readMode;	/* modes only apply to unify instructions */
        validateS(classArity((clssPo)objV(clss)));	/* validate S for n instructions */
      }
      else
	backTrack();

      continue;
    }
    
    case cAcns4:{		/* match A[4],f(_,..,_) */
      register ptrI val = deRefI(&A[4]);
      register objPo obj = objV(val);
      register ptrI clss = Lits[op_o_val(PCX)];

      testA(4);

      if(hasClass(obj,clss)){
	S = objectArgs(obj);		/* point to the first argument */
	mode = readMode;	/* modes only apply to unify instructions */
        validateS(classArity((clssPo)objV(clss)));	/* validate S for n instructions */
      }
      else
	backTrack();

      continue;
    }
    
    case cYA:{			/* Match Y[X],A[h] */
      testA(op_h_val(PCX));
      testY(op_o_val(PCX));
      
      switch(mtch(P,B,H,Yreg(-op_o_val(PCX)),&A[op_h_val(PCX)])){
        case Ok:
          continue;
        case Fail:
	  backTrack();
          continue;
        default:
          saveRegs(PC);
          raiseError(P,errorMsg,P->proc.errorCode);
          restRegs();
          continue;
      }
    }
    
    case cYS:{                          /* match Y[X],S++ */
      if(mode==readMode){
	isSvalid(1);                    /* test for validity of S register */
        switch(mtch(P,B,H,Yreg(-op_o_val(PCX)),S++)){
        case Ok:
          continue;
        case Fail:
	  backTrack();
          continue;
        default:
          saveRegs(PC);
          raiseError(P,errorMsg,P->proc.errorCode);
          restRegs();
          continue;
        }
      }
      else
	backTrack();

      continue;
    }

    case cSA:{                          /* match S++,A[m] */
      if(mode==readMode){
	isSvalid(1);			/* test for validity of S register */
        switch(mtch(P,B,H,S++,&A[op_m_val(PCX)])){
        case Ok:
          continue;
        case Fail:
	  backTrack();
          continue;
        default:
          saveRegs(PC);
          raiseError(P,errorMsg,P->proc.errorCode);
          restRegs();
          continue;
        }
      }
      else
	backTrack();

      continue;
    }

    case cSY:{                          /* match S++,Y[X] */
      if(mode==readMode){
	isSvalid(1);			/* test for validity of S register */
        switch(mtch(P,B,H,S++,Yreg(-op_o_val(PCX)))){
        case Ok:
          continue;
        case Fail:
	  backTrack();
          continue;
        default:
          saveRegs(PC);
          raiseError(P,errorMsg,P->proc.errorCode);
          restRegs();
          continue;
        }
      }
      else
	backTrack();

      continue;
    }

    case cSlit:{                        /* match S++,<lit> */
      assert(inGlobalHeap(objV(Lits[op_o_val(PCX)]))||IsChar(Lits[op_o_val(PCX)]));
      isSvalid(1);			/* test for validity of S register */

      if(mode==readMode){
	register ptrPo ptr = deRef(S++);
	register ptrI val = *ptr;
        ptrI tmp = Lits[op_o_val(PCX)];

        switch(ptg(val)){
        case varTg:
          bindVr(ptr,tmp);              /* bind to the literal value */
          continue;
        case objTg:{
	  switch(uni(P,B,H,ptr,deRef(&tmp))){
	  case Ok:
	    continue;
	  case Fail:
	    backTrack();
	    continue;
	  default:
	    saveRegs(PC);
	    raiseError(P,errorMsg,P->proc.errorCode);
	    restRegs();
	    continue;
	  }
	}
	default:
	  backTrack();
          continue;
        }
      }
      else
	backTrack();

      continue;
    }

    case cScns:{                        /* match S,f(_,..,_) */
      isSvalid(1);                    /* test for validity of S register */
      lastValid();

      if(mode==readMode){
	register ptrI val = deRefI(S);
	register objPo obj = objV(val);
        ptrI clss = Lits[op_o_val(PCX)];

	if(isvar(val))
	  backTrack();                 /* Not permitted to bind when matching */
	else if(hasClass(obj,clss)){
	  S = objectArgs(obj);        /* point to the first argument */
	  validateS(classArity((clssPo)objV(clss)));	/* validate S for n instructions */
        }
	else
	  backTrack();
      }
      else
	backTrack();

      continue;
    }

    /* Initialization and first-time moves */

    case clAA:{			/* First/clear A[h],A[m] */
      variablePo vv = (variablePo)H;
      register ptrI vr = (ptrI)&vv->val;

      vv->class = varClass;
      vv->val = vr;
      H+=VariableCellCount;
      
      A[op_h_val(PCX)] = A[op_m_val(PCX)] = vr;
      continue;
    }

    case clAY:{			/* First/clear A[h],Y[X] */
      A[op_h_val(PCX)] = unBind(Yreg(-op_o_val(PCX)));
      continue;
    }

    case clAS:{			/* First/clean A[h],S++ */
      assert(mode==writeMode);

      A[op_h_val(PCX)] = unBind(H++); /* make an argument unbound */
      continue;
    }

    case clSA:{			/* First/clear S++,A[m] */
      assert(mode==writeMode);

      A[op_m_val(PCX)] = unBind(H++); /* make an argument unbound */
      continue;
    }

    case clSY:{			/* First/clear S++,Y[X] */
      register ptrPo yReg = Yreg(-op_o_val(PCX));

      assert(mode==writeMode);

      bindVr(yReg,unBind(H++));		/* make an argument unbound */
      continue;
    }

    case vrA:{                          /* Check that A[h] is a variable */
      register ptrPo T = &A[op_h_val(PCX)];

      if(!isvar(deRefI(T)))
	backTrack();                    /* fail, 'cos its not a variable */
      continue;
    }

    case vrY:{                          /* Check that Y[X] is a variable */
      register ptrPo T = Yreg(-op_o_val(PCX));

      if(!isvar(deRefI(T)))
	backTrack();                    /* fail, 'cos its not a variable */
      continue;
    }

    case nvrA:{			/* Check that A[h] is not a variable */
      register ptrPo T = &A[op_h_val(PCX)];

      if(isvar(deRefI(T)))
	backTrack();                    /* fail, 'cos its a variable */

      continue;
    }

    case nvrY:{			/* Check that Y[X] is not a variable */
      register ptrPo T = Yreg(-op_o_val(PCX));

      if(isvar(deRefI(T)))
	backTrack();                    /* fail, 'cos its a variable */

      continue;
    }

    case vdA:{			/* Void A[h] */
      A[op_h_val(PCX)] = kvoid;
      continue;
    }

    case vdAA:{			/* Void A[h],Count */
      ptrPo low = &A[op_h_val(PCX)];
      unsigned int count = op_o_val(PCX);

      while(count-->0)
	*low++ = kvoid;
      continue;
    }

    case vdY:{			/* Void Y[X] */
      Y[-op_o_val(PCX)] = kvoid;
      continue;
    }

    case vdYY:{			/* Void Y[h],Count */
      unsigned int low = op_o_val(PCX);
      unsigned int count = op_h_val(PCX);

      while(count-->0)
	Y[-(low++)] = kvoid;
      continue;
    }

    case clA:{			/* clear A[h] */
      variablePo vv = (variablePo)H;
      register ptrI vr = (ptrI)&vv->val;

      vv->class = varClass;
      vv->val = vr;
      H+=VariableCellCount;
      
      A[op_h_val(PCX)] = vr;
      continue;
    }

    case clY:{			/* clear Y[h] */
      unBind(Yreg(-op_o_val(PCX)));
      continue;
    }

    case clYY:{			/* clear Y[h],Count */
      unsigned int count = op_h_val(PCX);
      ptrPo yReg = Yreg(-op_o_val(PCX));

      for(;count-->0;yReg--){
	unBind(yReg);
      }
      continue;
    }

    case clS:{			/* clear S++ */
      if(mode==readMode){
	isSvalid(1);		/* test for validity of S register */
	S++;
      }
      else
	unBind(H++);
      continue;
    }

    default:			/* illegal instruction */
      syserr("illegal instruction");
    }
  }
}

retCode equal(processPo P,ptrPo T1,ptrPo T2)
{
  trailPo trail = P->proc.trail;
  retCode ret = Ok;
  
  ret = uni(P,P->proc.B,(ptrPo)P->proc.heap.create,T1,T2);
  
  if(ret!=Ok){
    register trailPo t = P->proc.trail;

    while(t>trail){		/* reset variables since last choice point */
      t--;
      *t->var = t->val;	/* restore to previous value */
    }

    P->proc.trail = t;
  }
  if(ret==Error)
    return raiseError(P,P->proc.errorMsg,P->proc.errorCode);
  else
    return ret;
}

static retCode uni(processPo P,choicePo B,ptrPo H,ptrPo T1,ptrPo T2)
{
  register ptrI Tg1 = *(T1=deRef(T1));
  register ptrI Tg2 = *(T2=deRef(T2));

  switch(ptg(Tg1)){                    /* First-level analysis of the pointer */
  case varTg:{
    if(isvar(Tg2)){             /* are we unifying two variables? */
      if(T1!=T2){
	if(!localVar(T2)){	/* is the variable in the heap? */
	  if(localVar(T1)){
            if(occCheck(T1,T2)){
              strMsg(P->proc.errorMsg,NumberOf(P->proc.errorMsg),"occurs check");
              P->proc.errorCode = eOCCUR;
              return Error;
            }
            bindVr(T1,Tg2);	/* make sure heap is not bound to locals */
	  }
	  else if(T1<T2){	/* bind young to old ...*/
            if(occCheck(T2,T1)){
              strMsg(P->proc.errorMsg,NumberOf(P->proc.errorMsg),"occurs check");
              P->proc.errorCode = eOCCUR;
              return Error;
            }
            bindVr(T2,Tg1);
	  }
	  else{
            if(occCheck(T1,T2)){
              strMsg(P->proc.errorMsg,NumberOf(P->proc.errorMsg),"occurs check");
              P->proc.errorCode = eOCCUR;
              return Error;
            }
            bindVr(T1,Tg2);
	  }
          return Ok;
	}
	else{
	  if(localVar(T1)){	/* both are in the stack, bind youngest to oldest */
	    if(T1<T2){
	      bindVr(T1,Tg2);
	    }
	    else{
	      bindVr(T2,Tg1);	/* bind younger to older*/
	    }
	  }
	  else{
	    bindVr(T2,Tg1);	/* bind local to heap*/
	  }
          return Ok;
	}
      }
      else
        return Ok;
    }
    else{
      if(occCheck(T1,T2)){
	strMsg(P->proc.errorMsg,NumberOf(P->proc.errorMsg),"occurs check");
        P->proc.errorCode = eOCCUR;
	return Error;
      }
      bindVr(T1,Tg2);
      return Ok;
    }
  }
  case objTg:{
    switch(ptg(Tg2)){
    case varTg:{
      if(occCheck(T2,T1)){
        strMsg(P->proc.errorMsg,NumberOf(P->proc.errorMsg),"occurs check");
        P->proc.errorCode = eOCCUR;

        return Error;
      }
      else{
        bindVr(T2,Tg1);
        return Ok;
      }
    }
    case objTg:{
      objPo Term1 = objV(Tg1);
      objPo Term2 = objV(Tg2);
      ptrI classT1 = Term1->class;

      if(hasClass(Term2,classT1)){
	if(!IsSpecialClass(classT1)){
	  ptrPo a1 = objectArgs(Term1);
	  ptrPo a2 = objectArgs(Term2);
	  long arity = objectArity(Term1);
	  
	  int ix;
	  retCode ret = Ok;
	  for(ix=0;ret==Ok &&ix<arity;ix++,a1++,a2++)
	    ret = uni(P,B,H,a1,a2);
	  return ret;
	}
	else{
	  specialClassPo sClass = (specialClassPo)objV(classT1);
	  comparison comp = sClass->compFun(sClass,Term1,Term2);
	  if(comp==same)
	    return Ok;
	  else
	    return Fail;
	}
      }
      else
	return Fail;
    }
    default:
      strMsg(P->proc.errorMsg,NumberOf(P->proc.errorMsg),"incomparable values");
      P->proc.errorCode = eUNIFY;
      return Error;
    }
  }
  default:
    strMsg(P->proc.errorMsg,NumberOf(P->proc.errorMsg),"incomparable values");
    P->proc.errorCode = eUNIFY;
    return Error;
  }
}

/* One-way matching, T1 may not be instantiated but T2 may be */
retCode match(processPo P,ptrPo T1,ptrPo T2)
{
  trailPo trail = P->proc.trail;
  retCode ret = Ok;
  
  ret = mtch(P,P->proc.B,(ptrPo)P->proc.heap.create,T1,T2);
  
  if(ret!=Ok){
    register trailPo t = P->proc.trail;

    while(t>trail){		/* reset variables since last choice point */
      t--;
      *t->var = t->val;	        /* restore to previous value */
    }

    P->proc.trail = t;
  }
  if(ret==Error)
    return raiseError(P,P->proc.errorMsg,P->proc.errorCode);
  else
    return ret;
}

/* Trial matching, intended as a rough check to see if unification may be possible */
/* Operates by ignoring variables, and testing everything else */
static retCode test(ptrPo T1,ptrPo T2);

retCode testmatch(ptrPo T1,ptrPo T2)
{
  return test(T1,T2);
}

static retCode test(ptrPo T1,ptrPo T2)
{
  register ptrI Tg1 = *(T1=deRef(T1));
  register ptrI Tg2 = *(T2=deRef(T2));

  switch(ptg(Tg1)){                    /* First-level analysis of the pointer */
  case varTg:
    return Ok;
  case objTg:{
    if(IsFrozenVar(Tg1))
      return Ok;
    else{
      switch(ptg(Tg2)){
      case varTg:
	return Ok;
      case objTg:{
	objPo Term1 = objV(Tg1);
	objPo Term2 = objV(Tg2);
	ptrI classT1 = objV(Term1)->class;

	if(hasClass(Term2,classT1)){
	  if(!IsSpecialClass(classT1)){
	    ptrPo a1 = objectArgs(Term1);
	    ptrPo a2 = objectArgs(Term2);
	    long arity = objectArity(Term1);
	    
	    int ix;
	    retCode ret = Ok;
	    for(ix=0;ret==Ok &&ix<arity;ix++,a1++,a2++)
	      ret = test(a1,a2);
	    return ret;
	  }
	  else{
	    specialClassPo sClass = (specialClassPo)objV(classT1);

	    comparison comp = sClass->compFun(sClass,Term1,Term2);

	    if(comp==same)
	      return Ok;
	    else
	      return Fail;
	  }
	}
	else
	  return Fail;
      }
      default:
	return Fail;
      }
    }
  }
  default:
    return Fail;
  }
}

static retCode mtch(processPo P,choicePo B,ptrPo H,ptrPo T1,ptrPo T2)
{
  register ptrI Tg1 = *(T1=deRef(T1));
  register ptrI Tg2 = *(T2=deRef(T2));

  switch(ptg(Tg1)){                    /* First-level analysis of the pointer */
  case varTg:{
    if(isvar(Tg2)){             /* are we unifying two variables? */
      if(T1!=T2){
	if(occCheck(T2,T1)){
	  strMsg(P->proc.errorMsg,NumberOf(P->proc.errorMsg),"occurs check");
          P->proc.errorCode = eOCCUR;

	  return Error;
	}
	if(localVar(T1) && !localVar(T2)){
	  bindVr(T1,Tg2);
	}
	else{
	  bindVr(T2,Tg1);
	}
        return Ok;
      }
      else
        return Ok;
    }
    else
      return Fail;                      /* not allowed to bind T1 */
  }
  case objTg:{
    switch(ptg(Tg2)){
    case varTg:{
      if(occCheck(T2,T1)){
        strMsg(P->proc.errorMsg,NumberOf(P->proc.errorMsg),"occurs check");
        P->proc.errorCode = eOCCUR;

        return Error;
      }
      else{
        bindVr(T2,Tg1);
        return Ok;
      }
    }
    case objTg:{
      objPo Term1 = objV(Tg1);
      objPo Term2 = objV(Tg2);
      ptrI classT1 = objV(Term1)->class;

      if(hasClass(Term2,classT1)){
	if(!IsSpecialClass(classT1)){
	  ptrPo a1 = objectArgs(Term1);
	  ptrPo a2 = objectArgs(Term2);
	  long arity = objectArity(Term1);
	  int ix;

	  retCode ret = Ok;
	  for(ix=0;ret==Ok &&ix<arity;ix++,a1++,a2++)
	    ret = mtch(P,B,H,a1,a2);
	  return ret;
	}
	else{
	  specialClassPo sClass = (specialClassPo)objV(classT1);

	  comparison comp = sClass->compFun(sClass,Term1,Term2);

	  if(comp==same)
	    return Ok;
	  else
	    return Fail;
	}
      }
      else
	return Fail;
    }
    default:
      strMsg(P->proc.errorMsg,NumberOf(P->proc.errorMsg),"incomparable values");
      P->proc.errorCode = eUNIFY;
      return Error;
    }
  }
  default:
    strMsg(P->proc.errorMsg,NumberOf(P->proc.errorMsg),"incomparable values");
    P->proc.errorCode = eUNIFY;
    return Error;
  }
}

static retCode ident(ptrPo T1,ptrPo T2)
{
  register ptrI Tg1 = *(T1=deRef(T1));
  register ptrI Tg2 = *(T2=deRef(T2));

  switch(ptg(Tg1)){                    /* First-level analysis of the pointer */
  case varTg:{
    if(isvar(Tg2)){             /* are we unifying two variables? */
      if(T1!=T2)
        return Fail;
      else
        return Ok;
    }
    else
      return Fail;
  }
  case objTg:{
    switch(ptg(Tg2)){
    case varTg:
      return Fail;
    case objTg:{
      objPo Term1 = objV(Tg1);
      objPo Term2 = objV(Tg2);
      ptrI classT1 = objV(Term1)->class;

      if(hasClass(Term2,classT1)){
	if(!IsSpecialClass(classT1)){
	  retCode ret = Ok;
	  long ix;
	  long arity = objectArity(Term1);
	  ptrPo a1 = objectArgs(Term1);
	  ptrPo a2 = objectArgs(Term2);

	  for(ix=0;ret==Ok &&ix<arity;ix++,a1++,a2++)
	    ret = ident(a1,a2);
	  return ret;
	}
	else{
	  specialClassPo sClass = (specialClassPo)objV(classT1);
	  comparison comp = sClass->compFun(sClass,Term1,Term2);

	  if(comp==same)
	    return Ok;
	  else
	    return Fail;
	}
      }
      else
	return Fail;
    }
    default:
      return Error;
    }
  }
  default:
    return Error;
  }
}

logical identical(ptrI T1,ptrI T2)
{
  return ident(&T1,&T2)==Ok;
}

static comparison compareObjects(objPo p1,objPo p2)
{
  ptrI cl1 = p1->class;
  ptrI cl2 = p2->class;

  if(IsSpecialClass(cl1)){
    if(cl2==cl1){
      specialClassPo sClass = (specialClassPo)objV(cl1);
      return sClass->compFun(sClass,p1,p2);
    }
    else
      return incomparible;
  }
  else if(isClass(cl1)){
    if(!isClass(cl2))
      return incomparible;
    else{
      clssPo class1 = (clssPo)objV(cl1);
      clssPo class2 = (clssPo)objV(cl2);
      long arity = classArity(class1);

      if(cl1==cl2){
	ptrPo a1 = objectArgs(p1);
	ptrPo a2 = objectArgs(p2);
	long ix = 0;
	comparison comp = same;
	for(ix=0;comp==same && ix<arity;ix++,a1++,a2++)
	  comp = compareObjects(objV(deRefI(a1)),objV(deRefI(a2)));
	return comp;
      }
      else if(arity<classArity(class2))
	return smaller;
      else if(arity>classArity(class2))
	return bigger;
      else{
	int res = uniCmp(className(class1),className(class2));
	if(res<0)
	  return smaller;
	else
	  return bigger;
      }
    }
  }
  else
    return incomparible;
}

/* return same if T1=T2, smaller if T1<T2,  bigger if T1>T2 and incomparible if incomparible */
/* Only equivalent types are comparable */
comparison compTerm(ptrPo T1,ptrPo T2)
{
  ptrPo T1_stack[MAX_DEPTH];
  ptrPo T2_stack[MAX_DEPTH];
  register unsigned int depth = NumberOf(T1_stack)-1;
  
  T1_stack[depth] = T1;
  T2_stack[depth] = T2;
  
  while(depth<NumberOf(T1_stack)){
    register ptrI Tg1 = *(T1=deRef(T1_stack[depth]));
    register ptrI Tg2 = *(T2=deRef(T2_stack[depth]));
    register objPo Term1 = objV(Tg1);
    register objPo Term2 = objV(Tg2);
    
    depth++;                    // Element popped off the stack

    switch(ptg(Tg1)){
    case varTg:
      if(isvar(Tg2)){		/* are we identicaling two variables? */
	if(T1!=T2)
	  return incomparible;	/* not identical, and not comparable */
	else
	  break;
      }
      else
	return incomparible;	/* variables not comparible with any non-var */
    case objTg:{
      switch(ptg(Tg2)){
      case varTg:
        return incomparible;
      case objTg:
	return compareObjects(Term1,Term2);
      case fwdTg:
        return incomparible;
      }
    }
    case fwdTg:
      return incomparible;
    }
  }
  return same;
}

#ifdef DO_OCCURS_CHECK
static logical occCheck(ptrPo x,ptrPo v)
{
  ptrI Vx = *(v=deRef(v));

  switch(ptg(Vx)){
  case varTg:
    return x==v;
  case objTg:{
    objPo p = objV(Vx);
    ptrPo args = objectArgs(p);
    long arity = objectArity(p);
    long ix;
    logical ret = Ok;
    for(ix=0;ret==Ok && ix<arity;ix++,args++)
      ret = occCheck(x,args);
    return ret;
  }
  default:
    return True;                        /* should never happen */
  }
}
#endif

void recoverFromException(processPo P)
{
  register trailPo tb = P->proc.T->trail;
  register trailPo t = P->proc.trail;

  while(t>tb){		/* reset variables since last choice point */
    t--;
    *t->var = t->val;		/* restore to previous value */
  }

  P->proc.trail = t;
  
  P->proc.B = P->proc.T;		/* restore the choice point */
  P->proc.PC = P->proc.B->PC;		/* Pick up trap program pointer */
  P->proc.PROG = P->proc.B->PROG;	/* the local program */
}

retCode raiseException(processPo P,ptrI E)
{
  rootPo root = gcAddRoot(&P->proc.heap,&E);
  uniChar errorMsg[256];

  freezeTerm(&P->proc.heap,&E,E,errorMsg,NumberOf(errorMsg));

  P->proc.errorCode = E;
  ((ptrPo)(P->proc.T+1))[0] = E;

  recoverFromException(P);

  gcRemoveRoot(&P->proc.heap,root);
  return Error;
}

retCode raiseError(processPo P,uniChar *error,ptrI code)
{
  rootPo root = gcAddRoot(&P->proc.heap,&code);

  recoverFromException(P);

  P->proc.errorCode = allocateString(&globalHeap,error,uniStrLen(error));

  ptrI err = objP(permObject(&P->proc.heap,errorClass));
  gcAddRoot(&P->proc.heap,&err);

  updateArg(objV(err),0,P->proc.errorCode);

  freezeTerm(&P->proc.heap,&code,code,P->proc.errorMsg,NumberOf(P->proc.errorMsg));

  updateArg(objV(err),1,code);

  P->proc.errorCode = err;
  ((ptrPo)(P->proc.T+1))[0] = err;

  gcRemoveRoot(&P->proc.heap,root);

  return Error;
}

retCode liberror(processPo P,char *name,ptrI code)
{
  uniChar n[1024];
  
  _uni((unsigned char*)name,n,NumberOf(n));
  return raiseError(P,n,code);
}

// suspend a variable with a goal label if unbound.
// Attaches a suspension to the variable
retCode g_suspend(processPo P,ptrPo a)
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x)){                         /* we need to build a suspension point */
    if(isSuspVar((ptrPo)x)){
      rootPo root = gcAddRoot(&P->proc.heap,&x);

      ptrI nPair = consLsPair(&P->proc.heap,deRefI(&a[2]),((suspensionPo)(((ptrPo)x)-1))->goal);
      suspensionPo susp = (suspensionPo)(((ptrPo)x)-1);
      bndVar(P,&susp->goal,nPair);
      gcRemoveRoot(&P->proc.heap,root);
      return Ok;
    }
    else{
      ptrI susp = allocateSusp(&P->proc.heap,deRefI(&a[2]));
      bndVar(P,deRef(&a[1]),susp);      /* return the bound suspension */
      return Ok;
    }
  }
  else{
    ptrI pair = consLsPair(&P->proc.heap,deRefI(&a[2]),P->proc.trigger);
    P->proc.trigger = pair;
    P->proc.F = SUSP_ACTIVE;
    return Ok;
  }
}

