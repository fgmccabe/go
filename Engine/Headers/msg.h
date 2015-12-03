/*
 * header file for message handling
 */
#ifndef _ENGINE_MSG_H_
#define _ENGINE_MSG_H_

#include "word.h"

/* message processing functions */
void initMailBoxes(void);               /* initialize message queues etc. */

logical IsRealMailBox(ptrI M);
processPo mailBoxOwner(ptrI M);
integer mailBoxSequence(ptrI M);

ptrI newMailBox(ptrI owner);
void closeMailBox(ptrI M);

retCode showMailBox(ioPo f,ptrI M);

#endif
