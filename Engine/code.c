/*
  Code handling functions for Go!
  (c) 1994-2006 Imperial College & F.G. McCabe

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

  $Id: code.c,v 1.2 2004/04/29 16:24:27 fmccabe Exp $
  $Log: code.c,v $
  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system

*/

#include "config.h"
#include <string.h>		/* Access string defs */
#include <stdlib.h>		/* Memory allocation etc. */
#include <assert.h>		/* Run-time predicate verification */

#include "go.h"
#include "heap.h"
#include "code.h"

ptrI codeClass;

static long cdeSizeFun(specialClassPo class,objPo o);
static comparison cdeCompFun(specialClassPo class,objPo o1,objPo o2);
static retCode cdeOutFun(specialClassPo class,ioPo out,objPo o);
static retCode cdeScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o);
static objPo cdeCopyFun(specialClassPo class,objPo dst,objPo src);
static uinteger cdeHashFun(specialClassPo class,objPo o);

void initCodeClass(void)
{
  codeClass = newSpecialClass("#code",cdeSizeFun,cdeCompFun,
			      cdeOutFun,cdeCopyFun,
			      cdeScanFun,cdeHashFun);
}

static long cdeSizeFun(specialClassPo class,objPo o)
{
  codePo c = (codePo)o;

  assert(c->class==codeClass);

  return CellCount(sizeof(codeRec)+(c->size+c->litCnt)*sizeof(ptrI));
}

static retCode cdeScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o)
{
  long ix = 0;
  retCode ret = Ok;
  codePo cde = (codePo)o;
  ptrPo lits = codeLits(cde);
  long count = codeLitCount(cde);

  for(ix=0;ret==Ok && ix<count;ix++,lits++)
    ret = helper(lits,c);
  return ret;
}

static objPo cdeCopyFun(specialClassPo class,objPo dst,objPo src)
{
  long size = cdeSizeFun(class,src);
  memmove((void*)dst,(void*)src,size*sizeof(ptrI));

  return (objPo)(((ptrPo)dst)+size);
}

static comparison cdeCompFun(specialClassPo class,objPo o1,objPo o2)
{
  if(o1==o2)
    return same;
  else
    return incomparible;
}

static retCode cdeOutFun(specialClassPo class,ioPo out,objPo o)
{
  codePo cde = (codePo)o;
  return outMsg(out,"code[%d/%d]",cde->arity,cde->litCnt);
}

static uinteger cdeHashFun(specialClassPo class,objPo o)
{
  uinteger hash = 0;
  codePo cde = (codePo)o;
  
  long ix = 0;
  long count = codeInsCount(cde);
  insPo pc = codeIns(cde);

  for(ix=0;ix<count;ix++)
    hash+=*pc++;

  return hash;
}

ptrI permCode(long size,long litCnt)
{
  codePo block = (codePo)permAllocate(CodeCellCount(size,litCnt));

  block->class = codeClass;
  block->size = size;
  block->litCnt = litCnt;

  int i;
  ptrPo lits = codeLits(block);

  for(i=0;i<litCnt;i++)
    lits[i]=kvoid;
  
  return objP(block);
}


