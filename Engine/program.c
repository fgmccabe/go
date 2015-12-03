/* 
   Program management module
   (c) 2007 F.G. McCabe

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
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
 
#include <assert.h>
#include <unistd.h>
#include <dirent.h>
#include <pwd.h>
#include <sys/types.h>

#include <ctype.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <limits.h>

#include <signal.h>

#include "go.h"
#include "process.h"
#include "dict.h"
#include "symbols.h"
#include "clock.h"
#include "fileio.h"
#include "term.h"
#include "str.h"
#include "char.h"
#include "encoded.h"             /* pick up the term encoding definitions */
#include "hashtable.h"
#include "process.h"

/* Dictionary of known programs ... */
static hashPo programs;

static long pSizeFun(specialClassPo class,objPo o);
static comparison pCompFun(specialClassPo class,objPo o1,objPo o2);
static retCode pOutFun(specialClassPo class,ioPo out,objPo o);
static retCode pScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o);
static objPo pCopyFun(specialClassPo class,objPo dst,objPo src);
static uinteger pHashFun(specialClassPo class,objPo o);

void initPrograms(void)
{
  programs = NewHash(256,(hashFun)uniHash,(compFun)uniCmp,NULL);

  programClass = newSpecialClass("#program",pSizeFun,pCompFun,
				 pOutFun,pCopyFun,pScanFun,pHashFun);
}

static long pSizeFun(specialClassPo class,objPo o)
{
  assert(o->class==programClass);

  programPo cl = (programPo)o;

  return CellCount(sizeof(programRec)+sizeof(uniChar)*(uniStrLen(cl->name)+1));
}

static comparison pCompFun(specialClassPo class,objPo o1,objPo o2)
{
  if(o1==o2)
    return same;
  else
    return incomparible;
}

static retCode pOutFun(specialClassPo class,ioPo out,objPo o)
{
  programPo cl = (programPo)o;

  return outMsg(out,"%U%%%d",cl->name,cl->arity);
}

static retCode pScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o)
{
  assert(o->class==programClass);

  programPo d = (programPo)o;

  return helper(&d->code,c);
}

static objPo pCopyFun(specialClassPo class,objPo dst,objPo src)
{
  long size = pSizeFun(class,src);
  memmove((void*)dst,(void*)src,size*sizeof(ptrI));

  return (objPo)(((ptrPo)dst)+size);
}

static uinteger pHashFun(specialClassPo class,objPo o)
{
  programPo cl = (programPo)o;

  return uniHash(cl->name);
}

static ptrI allocateProgramLabel(heapPo H,const uniChar *name,long arity,ptrI code)
{
  rootPo root = gcAddRoot(H,&code);

  long symlen = uniStrLen(name);
  long len = CellCount(sizeof(programRec)+sizeof(uniChar)*(symlen+1));
  programPo new = (programPo)permAllocate(len);

  new->class = programClass;
  new->arity = arity;
  new->code = code;

  memcpy(new->name,name,(symlen+1)*sizeof(uniChar));
  gcRemoveRoot(H,root);

  return objP(new);
}

ptrI newProgramLbl(const uniChar *name,long arity)
{
  ptrI def = (ptrI)Search((void*)name,programs); 

  if(objV(def)==NULL){
    def = allocateProgramLabel(&globalHeap,name,arity,kvoid);

    Install(programName(objV(def)),(void*)def,programs);

    return def;
  }
  else{
    assert(programArity(objV(def))==arity);
    return def;
  }
}

ptrI newProgLbl(const char *name,long arity)
{
  int slen = strlen(name);
  uniChar buff[slen+1];

  _uni((unsigned char*)name,buff,slen+1);

  return newProgramLbl(buff,arity);
}

ptrI programByName(uniChar *name)
{
  return (ptrI)Search(name,programs);
}

ptrI programOfClass(objPo o)
{
  assert(isTermClass(o));

  clssPo cl = (clssPo)o;
  ptrI pr = (ptrI)Search(className(cl),programs);

  if(pr==0){
    uniChar buff[MAX_SYMB_LEN];
    uniCpy(buff,NumberOf(buff),className(cl));
    return newProgramLbl(buff,OBJECT_ARITY);
  }
  else
    return pr;
}

ptrI programOfTerm(ptrI x)
{
  if(isvar(x))
    return ProgramOf(thingProg);
  else
    return programOfClass(objV(objV(x)->class));
}

void defineProg(ptrI defn,ptrI code)
{
  assert(IsProgLbl(defn));
  assert(identical(code,kvoid)||IsCode(code));

  setCode((programPo)objV(defn),code);
}

// Handle GC of program labels

typedef struct {
  globalGcPo G;
  hashPo newDict;
} DInfoRec;

static retCode markProgram(void *n,void *r,void *c)
{
  DInfoRec *I = (DInfoRec *)c;
  ptrI S = (ptrI)r;

  objPo o = objV(S);

  /* This fragment allows code to be garbage collected - except for code loaded as part of a package */
  if(oldGeneration(o))
    Install(programName(objV(S)),(void*)S,I->newDict); /* put symbol directly into the new dictionary */
  else
    scanPtr(I->G,S); /* We keep defined programs FIXME */

  return Ok;
}

void markPrograms(globalGcPo G)
{
  DInfoRec help = {G,NewHash(256,(hashFun)uniHash,(compFun)uniCmp, NULL)};
  hashPo currTable = programs;

  programs = help.newDict;
  ProcessTable(markProgram,currTable,&help);

  thingProg = scanPtr(G,thingProg);
}

void installProgram(objPo p)
{
  uniChar *name = programName(p);
  ptrI sym = (ptrI)Search(name,programs);

  if(objV(sym)==NULL)		/* A new entry in the table */
    Install(name,(void*)objP(p),programs); /* Install in program dictionary */
}

