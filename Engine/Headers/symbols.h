/* 
   Symbol related definitions for the Go! engine
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
*/

#ifndef _GO_SYMBOLS_H_
#define _GO_SYMBOLS_H_

#include <string.h>

/*
 Standard classes
*/
extern ptrI thingClass;
extern ptrI thingProg;
extern ptrI classClass;

extern ptrI errorClass;
extern ptrI threadClass;
extern ptrI procClass;
extern ptrI filePtrClass;

extern ptrI commaClass;

/* Standard symbols */

extern ptrI kvoid;
extern ptrI emptyList,emptySymbol,zero;

extern ptrI bootProg;
extern ptrI dieProg;                   /* program just dies */
extern ptrI exitProg;                /* program that succeeds out of process */
extern ptrI trapProg;                  /* default trap handler */
extern ptrI kprocessFlag;

extern ptrI trueClass,falseClass;

extern ptrI kfifo,kdir,kcharfile,kblock,kplain,ksymlink,ksock,kunknown;

extern ptrI kloadflag;                  /* The loaded property */
extern ptrI kversion;                   /* The $version property */
extern ptrI universal;                  /* The universal package version */
extern ptrI kdefined;                   /* The names defined in a package  */
extern ptrI klabel;                     /* The $label property */
extern ptrI kmain;			/* The main entry point */
extern ptrI kmainThread;		/* The main thread */

extern ptrI kstart;                     /* entry point for new threads */
extern ptrI kdelay;                     /* delay response handler */

extern ptrI doResume[GO_REGS];               /* array of resume exit points */

/* Symbol structure */
typedef struct _symb_record_ {
  ptrI class;                            /* == symbolClass */
  uniChar data[ZEROARRAYSIZE];		/* The symbol's print name */
} symbolRec, *symbPo;

extern ptrI symbolClass;

static inline logical IsSymb(ptrI o)
{
  return HasClass(o,symbolClass);
}

static inline logical isSymb(objPo p)
{
  return hasClass(p,symbolClass);
}

static inline symbPo symbV(ptrI x)
{
  assert(IsSymb(x));
  return (symbPo)objV(x);
}

static inline uniChar *SymVal(symbPo p)
{
  assert(isSymb((objPo)p));

  return p->data;
}

static inline logical isSymSym(symbPo p)
{
  return p->data[0]=='\'';
}

static inline long SymLen(symbPo p)
{
  assert(isSymb((objPo)p));

  return uniStrLen(p->data);
}

#endif

