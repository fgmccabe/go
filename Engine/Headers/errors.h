/* 
  Standard error number definitions
  (c) 2000 F.G. McCabe 

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
*/

#include "config.h"		/* pick up standard configuration header */

void initErrorSymbols(void);
void syserr(char *msg);
retCode liberror(processPo P,char *name,ptrI code);
ptrI errorString(heapPo H,ptrI code);

extern ptrI eINSUFARG,                  // Insufficiently instatiated argument
  eINTNEEDD,                            /* Integer required */
  eNUMNEEDD,                            /* Number required */
  eVARNEEDD,                            /* Unbound variable required */
  eSPACE,                               /* Out of heap space */
  eUNIFY,                               /* Incomparible values in unification */
  eOCCUR,                               // Occurs check
  eCODE,                                // Attempt to execute unbound variable
  eDIVZERO,                             /* Division by zero */
  eLSTNEEDD,                            /* List needed */
  eTPLNEEDD,                            /* Tuple needed */
  eCHRNEEDD,                            /* Character needed */
  eSTRNEEDD,                            /* String needed */
  eSYMNEEDD,                            /* Symbol needed */
  eINVAL,                               /* invalid argument */
  eRANGE,                               /* out of range argument */
  eNOPERM,                              /* permission denied */
  eNOFILE,                              /* file not found */
  eNOTDIR,				/* not a directory */
  eCFGERR,                              /* configuration problem */
  eEOF,                                 /* read past end-of-file */
  eIOERROR,                             /* Error on i/o */
  eABORT,                               /* Abort process */
  eNOTFND,                              /* Not found */
  eCONNECT,                             /* Problem with connection */
  eHANDLE,                              // Not a legal handle
  eFAIL,                                /* unexpected failure */
  eINVCODE,                             // Invalid code type
  eASSIGN,                              // invalid attempt at assignment
  eSYSTEM,                              // system overflow
  eDEAD,                                // Deadlock detected
  eTIME,                                // Timeout detected
  eDUPLICATE,                           // Duplicate requested
  eNOIMPL,                              // Not implemented
  eNOTENUF,                             /* not enough arguments */
  eINTRUPT;				// Interrupted

#ifdef GOXLIB
extern ptrI eNOX;			/* no X connection */
#endif


