/* Special call to top-level program loaded at boot-time
  (c) 2001 F.G. McCabe

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

  $Id: call.c,v 1.3 2004/07/16 15:30:05 fmccabe Exp $
  $Log: call.c,v $
  Revision 1.3  2004/07/16 15:30:05  fmccabe
  Adjusted the compiler some

  Revision 1.2  2004/04/29 16:24:27  fmccabe
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
#include "symbols.h"
#include "hash.h"		/* we need access to the hash functions */
#include "debug.h"		/* Debugger access functions */

// __call(package,entry,arguments) -- list of strings
// results in the call:
// package.entry(arguments)
//
retCode g_call(processPo P, ptrPo a)
{
  ptrI pkg = deRefI(&a[1]);
  ptrI entry = deRefI(&a[2]);
  ptrI Arity = deRefI(&a[3]);

  if(isvar(entry)||isvar(pkg) || isvar(Arity))
    return liberror(P,"__call",eINSUFARG);
  else if(!IsSymb(entry) || !IsSymb(pkg) || !IsInt(Arity))
    return liberror(P,"__call",eINVAL);
  else if(!isLoaded(pkg)){
    uniChar errMsg[MAX_MSG_LEN];

    strMsg(errMsg,NumberOf(errMsg),"%U not loaded",SymVal(symbV(pkg)));
    return raiseError(P,errMsg,eCODE);
  }
  else{
    uniChar resolved[MAX_MSG_LEN];      /* compute the entrypoint symbol */
    long arity = integerVal((integerPo)objV(Arity));

    strMsg(resolved,NumberOf(resolved),"%U@%U",SymVal(symbV(pkg)),SymVal(symbV(entry)));

    switchProcessState(P,in_exclusion);
    ptrI prog = newProgramLbl(resolved,arity);
    setProcessRunnable(P);

    if(!IsProgram(prog)){
      strMsg(resolved,NumberOf(resolved),"%U@%U%%%d not defined",SymVal(symbV(pkg)),SymVal(symbV(entry)),arity);
      return raiseError(P,resolved,eCODE);
    }
    else{
      ptrI args = deRefI(&a[4]);

      P->proc.A[1] = args;               /* Pass in the argument list of strings */
                  
      P->proc.cPC = P->proc.PC;                 /* start stacking stuff */
      P->proc.cPROG = P->proc.PROG;
      P->proc.cSB = P->proc.SB;
      P->proc.SB = P->proc.B;
	
      P->proc.PROG = ProgramOf(prog);	/* We have a new program to call */
      P->proc.PC = FirstInstruction(P->proc.PROG);
  
      return Error;       /* We cant use OK, because that checks for GCmap */
    }
  }
}

// __is(pred,arg) -- one argument
// results in the call:
// pred(arg)

retCode g_is(processPo P, ptrPo a)
{
  ptrI prog = deRefI(&a[1]);

  if(isvar(prog))
    return liberror(P,"__is",eINSUFARG);
  else if(!IsProgram(prog)){
    uniChar msg[MAX_SYMB_LEN];
    strMsg(msg,NumberOf(msg),"%w not defined",&prog);
    return raiseError(P,msg,eCODE);
  }
  else{
    ptrI arg = deRefI(&a[2]);

    P->proc.A[1] = arg;               /* Pass in the argument */
                  
    P->proc.cPC = P->proc.PC;		/* start stacking stuff */
    P->proc.cPROG = P->proc.PROG;
    P->proc.cSB = P->proc.SB;
    P->proc.SB = P->proc.B;
  
    P->proc.PROG = ProgramOf(prog);                 /* We have a new program to call */
    P->proc.PC = FirstInstruction(P->proc.PROG);
    
    return Error;       /* We cant use OK, because that checks for GCmap */
  }
}


// __defined(package,entry) -- look for a defined symbol in package
//
retCode g_defined(processPo P, ptrPo a)
{
  ptrI pkg = deRefI(&a[1]);
  ptrI entry = deRefI(&a[2]);
  ptrI Ar = deRefI(&a[3]);

  if(isvar(entry)||isvar(pkg)||isvar(Ar))
    return liberror(P,"__defined",eINSUFARG);
  else if(!IsSymb(entry) || !IsSymb(pkg) || !IsInt(Ar))
    return liberror(P,"__defined",eINVAL);
  else{
    uniChar resolved[MAX_MSG_LEN];      /* compute the entrypoint symbol */
    long arity = IntVal(Ar);

    strMsg(resolved,NumberOf(resolved),"%U@%U",SymVal(symbV(pkg)),SymVal(symbV(entry)));

    switchProcessState(P,in_exclusion);

    ptrI sym = newProgramLbl(resolved,arity);

    setProcessRunnable(P);

    if(IsProgram(sym))
      return Ok;
    else
      return Fail;
  }
}


// This function is the analogy to Prolog's univ function (which is of course written ,..)

retCode g_univ(processPo P,ptrPo a)
{
  ptrI Sym = deRefI(&a[1]);

  if(isvar(Sym))
    return liberror(P,"__univ",eINSUFARG);
  else if(!IsSymb(Sym))
    return liberror(P,"__univ",eINVAL);
  else{
    long arity = ListLen(deRefI(&a[2]));

    if(arity<0)
      return liberror(P,"__univ",eINSUFARG);
    else{
      symbPo sym = symbV(Sym);            // We copy the symbol's text out in case of GC during string creation
      long sLen = SymLen(sym);
      uniChar text[sLen+1];
      heapPo H = &P->proc.heap;
      rootPo root = gcAddRoot(H,&Sym);
      ptrI cons = kvoid;

      gcAddRoot(H,&cons);

      switchProcessState(P,in_exclusion);
      uniCpy(text,sLen,SymVal(sym));
      
      if(text[0]=='\''){
	memmove(&text[0],&text[1],(uniStrLen(text)-1)*sizeof(uniChar));
	Sym = newClassDef(text,arity);
      }

      setProcessRunnable(P);
      cons = objP(allocateObject(H,Sym));

      {
	ptrI xx = deRefI(&a[2]);
	long ix = 0;

	while(IsList(xx)){
	  ptrPo a = listHead(objV(xx));
	  updateArg(objV(cons),ix++,deRefI(a));
	  xx = deRefI(a+1);
	}
      }

      {
	retCode ret = equal(P,&cons,&a[3]);

	gcRemoveRoot(H,root);

	return ret;
      }
    }
  }
}

/*
 * Used in aiding debugging
 */
void showCall(processPo P,ptrI prog,ptrPo args,long arity)
{
  int i;
  uniChar buffer[1024];
  ioPo out = openBufferStr(buffer,NumberOf(buffer),utf16Encoding);

  outMsg(out,"%w: %w(",&P->proc.thread,&prog);

  for(i=0;i<arity;i++)
    outMsg(out,"%w%s",args++,(i<arity-1?",":""));

  outMsg(out,")\n%_");

  long len;
  uniChar *text = getStrText(O_STRING(out),&len);

  outText(logFile,text,len);
  closeFile(out);
}

void showOCall(processPo P,ptrPo obj,ptrPo call,ptrPo this)
{
  if(deRefI(obj)==deRefI(this))
    outMsg(logFile,"%w: %w.%w\n%_",&P->proc.thread,obj,call);
  else
    outMsg(logFile,"%w: %w.%w/%w\n%_",
	   &P->proc.thread,obj,call,this);
}


