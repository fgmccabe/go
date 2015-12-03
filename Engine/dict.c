/*
  Dictionary and symbol handling functions for Go!
  (c) 1994-2007 F.G. McCabe

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

  $Id: dict.c,v 1.5 2004/04/29 16:24:27 fmccabe Exp $
  $Log: dict.c,v $
  Revision 1.5  2004/04/29 16:24:27  fmccabe
  Completely new type system
*/

#include "config.h"
#include <string.h>		/* Access string defs */
#include <stdlib.h>		/* Memory allocation etc. */
#include <assert.h>		/* Run-time predicate verification */

#include "go.h"
#include "dict.h"
#include "lock.h"
#include "symbols.h"
#include "char.h"
#include "perms.h"
#include "hashtable.h"

/* standard symbols */
ptrI kvoid;			    		/* void value marker */

ptrI emptySymbol;				/* the null symbol */
ptrI zero;              		/* the 0 number */

ptrI kmain;				/* Main entry point */
ptrI kmainThread;			/* The main thread name */

ptrI bootProg;                             /* The bootstrap entry point */
ptrI dieProg;				/* program just dies */
ptrI exitProg;			      /* program that succeeds out of process */
ptrI trapProg;				/* default trap handler */

ptrI kprocessFlag;                      /* The property that a thread is stored under */

ptrI varClass;				/* Standard globalized variable */
ptrI suspClass;				/* Standard suspension class */

ptrI kfifo,kdir,kcharfile,kblock,kplain,ksymlink,ksock,kunknown;

ptrI kloadflag;                         /* the loaded flag */
ptrI kversion;                          /* which package version is loaded */
ptrI kdefined;                          /* the imported package description */
ptrI universal;                         /* universal package version */
ptrI klabel;                            /* The special $label property */
ptrI kstart;                            /* entry point for new threads */
ptrI doResume[GO_REGS];                 /* array of special exit points for  */
ptrI kdelay;				/* standard delay handler */

/* Standard dictionaries ... */
static hashPo dictionary;	/* The main symbol table */

static long sySizeFun(specialClassPo class,objPo o);
static comparison syCompFun(specialClassPo class,objPo o1,objPo o2);
static retCode syOutFun(specialClassPo class,ioPo out,objPo o);
static retCode syScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o);
static objPo syCopyFun(specialClassPo class,objPo dst,objPo src);
static uinteger syHashFun(specialClassPo class,objPo o);

void initSymbolClass(void)
{
  symbolClass=newSpecialClass("go.stdlib#symbol",sySizeFun,syCompFun,
			      syOutFun,syCopyFun,syScanFun,syHashFun);
}

static long sySizeFun(specialClassPo class,objPo o)
{
  assert(o->class==symbolClass);
  symbPo s = (symbPo)o;

  return CellCount(sizeof(symbolRec)+(uniStrLen(s->data)+1)*sizeof(uniChar));
}

static comparison syCompFun(specialClassPo class,objPo o1,objPo o2)
{
  if(o1==o2)
    return same;
  else if(o1->class==symbolClass && o2->class==symbolClass){
    symbPo s1 = (symbPo)o1;
    symbPo s2 = (symbPo)o2;

    int comp = uniCmp(SymVal(s1),SymVal(s2));

    if(comp==0)
      return same;
    else if(comp<0)
      return smaller;
    else
      return bigger;
  }
  else
    return incomparible;
}

static retCode syOutFun(specialClassPo class,ioPo out,objPo o)
{
  symbPo s = (symbPo)o;
  uniChar *sym = SymVal(s);
  retCode r = outChar(out,'\'');
	
  while(r==Ok && *sym!=0)
    r = wStringChr(out,*sym++);
  if(r==Ok)
    r = outChar(out,'\'');
  return r;
}

static retCode syScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o)
{
  return Ok;
}

static objPo syCopyFun(specialClassPo class,objPo dst,objPo src)
{
  long size = sySizeFun(class,src);
  memmove((void*)dst,(void*)src,size*sizeof(ptrI));

  return (objPo)(((ptrPo)dst)+size);
}

static uinteger syHashFun(specialClassPo class,objPo o)
{
  assert(o->class==symbolClass);

  symbPo s = (symbPo)o;
  return uniHash(SymVal(s));
}

ptrI permUniSymbol(const uniChar *txt)
{
  long symlen = uniStrLen(txt);
  long len = CellCount(sizeof(symbolRec)+(symlen+1)*sizeof(uniChar));
  symbPo new = (symbPo)permAllocate(len);

  new->class = symbolClass;
  memcpy(new->data,txt,(symlen+1)*sizeof(uniChar)); /* copy symbol's text */
  return objP(new);
}

/* Locate a symbol in the dictionary */
ptrI newSymbol(const char *name)
{
  int slen = strlen(name);
  uniChar buff[slen+1];

  _uni((unsigned char*)name,buff,slen+1);
  return newUniSymbol(buff);
}

ptrI newUniSymbol(const uniChar *name)
{
  ptrI sym = (ptrI)Search((void*)name,dictionary);

  if(objV(sym)==NULL){		/* A new entry in the table */
    sym = permUniSymbol((uniChar*)name);

    Install(SymVal(symbV(sym)),(void*)sym,dictionary); 
  }
  return sym;			/* return the symbol */
}

void installSymbol(symbPo s)
{
  uniChar *name = SymVal(s);
  ptrI sym = (ptrI)Search(name,dictionary);

  if(objV(sym)==NULL)		/* A new entry in the table */
    Install(name,(void*)objP(s),dictionary); /* Install in dictionary */
}

ptrI symbolPresent(uniChar *s)
{
  return (ptrI)Search(s,dictionary);
}


void initDict()				/* Initialize the dictionary */
{
  dictionary = NewHash(256,(hashFun)uniHash,(compFun)uniCmp, NULL);

  standardClasses();			/* fill in the initial set of classes */

  /* Declare run-time symbols */

  initErrorSymbols();

  emptySymbol=newSymbol("");
  zero = permInteger(0);

  kmain = newProgLbl("main",1);
  kmainThread = newEnumSym("go.stdlib#rootThread");
  bootProg = newSymbol("go.boot");

  kprocessFlag = newSymbol("$process");
  
  /* special value symbols */

  kfifo = newEnumSym("go.io#fifoSpecial");
  kdir = newEnumSym("go.io#directory");
  kcharfile = newEnumSym("go.io#charSpecial");
  kblock = newEnumSym("go.io#blockSpecial");
  kplain = newEnumSym("go.io#plainFile");
  ksymlink = newEnumSym("go.io#symlink");
  ksock = newEnumSym("go.io#socket");
  kunknown = newEnumSym("go.io#unknownFileType");

  kloadflag = newSymbol("$loaded");     /* This property is set on a package as it is loaded */

  kversion = newSymbol("$version");     /* This property is set to the loaded version */
  universal = newSymbol("*");
  kdefined = newSymbol("$defined");     /* definition of imported package */

  klabel = newSymbol("$label");

  kstart = newEnumSym("start_thread%0"); /* first call to a thread */
  kdelay = newProgLbl("go.stdlib@delayHandler",1);
}

/* remove all entries from the dictionary */
typedef struct {
  globalGcPo G;
  hashPo newDict;
} DInfoRec;

static retCode remSym(void *n,void *r,void *c)
{
  DInfoRec *I = (DInfoRec *)c;
  ptrI S = (ptrI)r;

  Uninstall(n,dictionary);

  objPo o = objV(S);

  /* This fragment allows code to be garbage collected - except for code loaded as part of a package */
  if(oldGeneration(o))
    Install(SymVal(symbV(S)),(void*)S,I->newDict); /* put symbol directly into the new dictionary */

  return Ok;
}

void restartDictionary(globalGcPo G)
{
  DInfoRec help = {G,NewHash(256,(hashFun)uniHash,(compFun)uniCmp, NULL)};
  hashPo currDict = dictionary;

  dictionary = help.newDict;
  ProcessTable(remSym,currDict,&help);

  DelHash(currDict);			/* clear down the existing dictionary */

  markStandardClasses(G);
  restartChars(G);
  markPrograms(G);
  
  kmain = scanPtr(G,kmain);
  kmainThread = scanPtr(G,kmainThread);
  bootProg = scanPtr(G,bootProg);
  dieProg = scanPtr(G,dieProg);
  exitProg = scanPtr(G,exitProg);
  trapProg = scanPtr(G,trapProg);
  kprocessFlag = scanPtr(G,kprocessFlag);

  kvoid = scanPtr(G,kvoid);		/* void value marker */
  emptySymbol = scanPtr(G,emptySymbol);
  zero = scanPtr(G,zero);

  kfifo = scanPtr(G,kfifo);
  kdir = scanPtr(G,kdir);
  kcharfile = scanPtr(G,kcharfile);
  kblock = scanPtr(G,kblock);
  kplain = scanPtr(G,kplain);
  ksymlink = scanPtr(G,ksymlink);
  ksock = scanPtr(G,ksock);
  kunknown = scanPtr(G,kunknown);

  kloadflag = scanPtr(G,kloadflag);
  kversion = scanPtr(G,kversion);
  universal = scanPtr(G,universal);
  kdefined = scanPtr(G,kdefined);
  klabel = scanPtr(G,klabel);

  kstart = scanPtr(G,kstart);

  {
    int ii;
    for(ii=0;ii<NumberOf(doResume);ii++)
      doResume[ii] = scanPtr(G,doResume[ii]);
  }

  kdelay = scanPtr(G,kdelay);

  eINSUFARG = scanPtr(G,eINSUFARG);
  eVARNEEDD = scanPtr(G,eVARNEEDD);
  eINTNEEDD = scanPtr(G,eINTNEEDD);
  eNUMNEEDD = scanPtr(G,eNUMNEEDD);
  eSPACE = scanPtr(G,eSPACE);
  eUNIFY = scanPtr(G,eUNIFY);
  eOCCUR = scanPtr(G,eOCCUR);
  eCODE = scanPtr(G,eCODE);
  eDIVZERO = scanPtr(G,eDIVZERO);
  eLSTNEEDD = scanPtr(G,eLSTNEEDD);
  eTPLNEEDD = scanPtr(G,eTPLNEEDD);
  eSYMNEEDD = scanPtr(G,eSYMNEEDD);
  eCHRNEEDD = scanPtr(G,eCHRNEEDD);
  eSTRNEEDD = scanPtr(G,eSTRNEEDD);
  eHANDLE = scanPtr(G,eHANDLE);
  eINVAL = scanPtr(G,eINVAL);
  eRANGE = scanPtr(G,eRANGE);
  eNOPERM = scanPtr(G,eNOPERM);
  eNOFILE = scanPtr(G,eNOFILE);
  eNOTDIR = scanPtr(G,eNOTDIR);
  eCFGERR = scanPtr(G,eCFGERR);
  eEOF = scanPtr(G,eEOF);
  eIOERROR = scanPtr(G,eIOERROR);
  eABORT = scanPtr(G,eABORT);
  eNOTFND = scanPtr(G,eNOTFND);
  eCONNECT = scanPtr(G,eCONNECT);
  eFAIL = scanPtr(G,eFAIL);
  eINVCODE = scanPtr(G,eINVCODE);
  eASSIGN = scanPtr(G,eASSIGN);
  eSYSTEM = scanPtr(G,eSYSTEM);
  eDEAD = scanPtr(G,eDEAD);
  eTIME = scanPtr(G,eTIME);
  eDUPLICATE = scanPtr(G,eDUPLICATE);
  eNOIMPL = scanPtr(G,eNOIMPL);
  eNOTENUF = scanPtr(G,eNOTENUF);
  eINTRUPT = scanPtr(G,eINTRUPT);
}

#ifdef ALLTRACE
static retCode dSym(void *n,void *r,void *c)
{
  ioPo f = (ioPo)c;
  symbPo sym = symbV((ptrI)r);

  outMsg(f,"%U\n",SymVal(sym));

  return Ok;
}

void dumpDict(ioPo out)
{
  ProcessTable(dSym,dictionary,out);
  flushOut();
}

void dDict(void)
{
  dumpDict(logFile);
}

void showSymbol(char *s)
{
  int slen = strlen(s);
  uniChar buff[MAX_SYMB_LEN];

  _uni((unsigned char*)s,buff,slen+1);

  ptrI sym = (ptrI)Search(buff,dictionary);

  if(objV(sym)!=NULL){
    symbPo sm = symbV(sym);

    outMsg(logFile,"%U\n",SymVal(sm));
  }
  else
    outMsg(logFile,"%s not found in dictionary\n",s);
  flushOut();
}
  

#endif

