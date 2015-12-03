/* 
  Error handling and exiting functions
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

  $Id: error.c,v 1.3 2004/04/29 16:24:27 fmccabe Exp $
  $Log: error.c,v $
  Revision 1.3  2004/04/29 16:24:27  fmccabe
  Completely new type system

*/

#include "config.h"		/* pick up standard configuration header */
#include <stdlib.h>
#include <errno.h>
#include "go.h"
#include "dict.h"
#include "symbols.h"
#include "errors.h"
#include "process.h"
#include "eval.h"
#include "term.h"

ptrI errorClass;			/* standard error class */

/* error symbols */
ptrI eINSUFARG,			       /* Insufficiently instatiated argument */
  eINTNEEDD,				/* Integer required */
  eNUMNEEDD,				/* Number required */
  eVARNEEDD,				/* Unbound variable required */
  eSPACE,				/* Out of heap space */
  eUNIFY,				/* Incomparible values in unification */
  eOCCUR,				// Occurs check violation
  eCODE,				// Attempt to execute unbound variable
  eDIVZERO,				/* Division by zero */
  eLSTNEEDD,				/* List needed */
  eTPLNEEDD,				/* Tuple needed */
  eSYMNEEDD,				/* Symbol needed */
  eCHRNEEDD,				/* Character needed */
  eINVAL,				/* invalid argument */
  eRANGE,				/* out of range of tuple */
  eNOPERM,				/* permission denied */
  eNOFILE,				/* file not found */
  eNOTDIR,				/* not a directory */
  eCFGERR,				/* configuration problem */
  eEOF,					/* read past end-of-file */
  eIOERROR,				/* Error on i/o */
  eABORT,				/* Abort process */
  eNOTFND,				/* Not found */
  eCONNECT,				/* Cant connect */
  eFAIL,				/* unexpected failure */
  eSTRNEEDD,				/* String needed */
  eHANDLE,				// Not a legal handle
  eINVCODE,				// Invalid code type
  eASSIGN,				// Invalid assignment
  eSYSTEM,				// system overflow
  eDEAD,				// Deadlock detected
  eTIME,				// timeout detected
  eDUPLICATE,				// Duplicate request
  eNOIMPL,				/* feature not implemented*/
  eNOTENUF,                             /* not enough arguments */
  eINTRUPT;				// Interrupted

void initErrorSymbols(void)
{
  // Standard error codes
  eINSUFARG = newSymbol("eINSUFARG");
  eVARNEEDD = newSymbol("eVARNEEDD");
  eINTNEEDD = newSymbol("eINTNEEDD");
  eNUMNEEDD = newSymbol("eNUMNEEDD");
  eSPACE = newSymbol("eSPACE");
  eUNIFY = newSymbol("eUNIFY");
  eOCCUR = newSymbol("eOCCUR");
  eCODE = newSymbol("eCODE");
  eDIVZERO = newSymbol("eDIVZERO");
  eLSTNEEDD = newSymbol("eLSTNEEDD");
  eTPLNEEDD = newSymbol("eTPLNEEDD");
  eSYMNEEDD = newSymbol("eSYMNEEDD");
  eCHRNEEDD = newSymbol("eCHRNEEDD");
  eSTRNEEDD = newSymbol("eSTRNEEDD");
  eHANDLE = newSymbol("eHANDLE");
  eINVAL = newSymbol("eINVAL");
  eRANGE = newSymbol("eRANGE");
  eNOPERM = newSymbol("eNOPERM");
  eNOFILE = newSymbol("eNOFILE");
  eNOTDIR = newSymbol("eNOTDIR");
  eCFGERR = newSymbol("eCFGERR");
  eEOF = newSymbol("eEOF");
  eIOERROR = newSymbol("eIOERROR");
  eABORT = newSymbol("eABORT");
  eNOTFND = newSymbol("eNOTFND");
  eCONNECT = newSymbol("eCONNECT");
  eFAIL = newSymbol("eFAIL");
  eINVCODE = newSymbol("eINVCODE");
  eASSIGN = newSymbol("eASSIGN");
  eSYSTEM = newSymbol("eSYSTEM");
  eDEAD = newSymbol("eDEAD");
  eTIME = newSymbol("eTIME");
  eDUPLICATE = newSymbol("eDUPLICATE");
  eNOIMPL = newSymbol("eNOIMPL");
  eNOTENUF = newSymbol("eNOTENUF");
  eINTRUPT = newSymbol("eINTRUPT");

  errorClass = newClassDf("go.stdlib#error",2);
}


ptrI errorString(heapPo H ,ptrI code)
{
  if(!isvar(code)){
    if(code==eINSUFARG)
      return allocateCString(H,"Insufficiently instantiated argument");
    else if(code==eINTNEEDD)
      return allocateCString(H,"Integer required");
    else if(code==eNUMNEEDD)
      return allocateCString(H,"Number required");
    else if(code==eVARNEEDD)
      return allocateCString(H,"Unbound variable required");
    else if(code==eSPACE)
      return allocateCString(H,"Out of heap space");
    else if(code==eUNIFY)
      return allocateCString(H,"Incomparible values in unification");
    else if(code==eDIVZERO)
      return allocateCString(H,"Division by zero");
    else if(code==eLSTNEEDD)
      return allocateCString(H,"List needed");
    else if(code==eTPLNEEDD)
      return allocateCString(H,"Tuple needed");
    else if(code==eSYMNEEDD)
      return allocateCString(H,"Symbol needed");
    else if(code==eSTRNEEDD)
      return allocateCString(H,"String required");
    else if(code==eCHRNEEDD)
      return allocateCString(H,"Character required");
    else if(code==eINVAL)
      return allocateCString(H,"invalid argument");
    else if(code==eNOPERM)
      return allocateCString(H,"permission denied");
    else if(code==eNOFILE)
      return allocateCString(H,"file not found");
    else if(code==eNOTDIR)
      return allocateCString(H,"not a directory");
    else if(code==eCFGERR)
      return allocateCString(H,"configuration problem");
    else if(code==eEOF)
      return allocateCString(H,"read past end-of-file");
    else if(code==eIOERROR)
      return allocateCString(H,"error on i/o");
    else if(code==eABORT)
      return allocateCString(H,"process aborted");
    else if(code==eNOTFND)
      return allocateCString(H,"not found");
    else if(code==eCODE)
      return allocateCString(H,"undefined program");
    else if(code==eFAIL)
      return allocateCString(H,"unexpected failure");
    else if(code==eHANDLE)
      return allocateCString(H,"not a valid handle");
    else if(code==eINVCODE)
      return allocateCString(H,"incorrect code type");
    else if(code==eASSIGN)
      return allocateCString(H,"assignment not allowed");
    else if(code==eDEAD)
      return allocateCString(H,"deadlock detected");
    else if(code==eSYSTEM)
      return allocateCString(H,"system overflow");
    else if(code==eDUPLICATE)
      return allocateCString(H,"duplicate request");
    else if(code==eNOIMPL)
      return allocateCString(H,"feature not implemented");
    else if(code==eNOTENUF)
      return allocateCString(H,"insufficient arguments given");
    else if(code==eCONNECT)
      return allocateCString(H,"cannot connect to host");
    else if(code==eINTRUPT)
      return allocateCString(H,"interrupted");
    else{
      uniChar buf[MAX_MSG_LEN];

      strMsg(buf,NumberOf(buf),"Unknown error code: %w",&code);
      return allocateString(H,buf,uniStrLen(buf));
    }
  }
  else{
    uniChar buf[MAX_MSG_LEN];

    strMsg(buf,NumberOf(buf),"Invalid error code: %w",&code);
    return allocateString(H,buf,uniStrLen(buf));
  }
}

retCode g_errorcode(processPo P,ptrPo a)
{
  ptrI S = errorString(&P->proc.heap,deRefI(&a[1]));
  
  return equal(P,&S,&a[2]);
}

/* Fatal system error */
void syserr(char *msg)
{
  outMsg(logFile,"Fatal error: %s\n", msg);
  go_exit(EXIT_FAIL);
}



