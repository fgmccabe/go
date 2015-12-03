/*
  Property management

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

ptrI dynamicClass;

static long dySizeFun(specialClassPo class,objPo o);
static comparison dyCompFun(specialClassPo class,objPo o1,objPo o2);
static retCode dyOutFun(specialClassPo class,ioPo out,objPo o);
static retCode dyScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o);
static objPo dyCopyFun(specialClassPo class,objPo dst,objPo src);
static uinteger dyHashFun(specialClassPo class,objPo o);

void initDynamicClass(void)
{
  dynamicClass=newSpecialClass("#dynamic",dySizeFun,dyCompFun,
			       dyOutFun,dyCopyFun, dyScanFun,dyHashFun);
}

static long dySizeFun(specialClassPo class,objPo o)
{
  return CellCount(sizeof(dynRec));
}

static comparison dyCompFun(specialClassPo class,objPo o1,objPo o2)
{
  if(o1==o2)
    return same;
  else
    return incomparible;
}

static retCode dyOutFun(specialClassPo class,ioPo out,objPo o)
{
  dynPo dObj = (dynPo)o;

#ifdef EXECTRACE
  if(debugging)
    return outMsg(out,"obj#%ld%w",dObj->hash,&dObj->props);
#endif
  return outMsg(out,"obj#%ld",dObj->hash);
}

static retCode dyScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o)
{
  assert(o->class==dynamicClass);

  dynPo d = (dynPo)o;

  retCode ret = helper(&d->props,c);

  if(ret==Ok)
    ret = helper(&d->code,c);
  return ret;
}

static objPo dyCopyFun(specialClassPo class,objPo dst,objPo src)
{
  long size = dySizeFun(class,src);
  memmove((void*)dst,(void*)src,size*sizeof(ptrI));

  return (objPo)(((ptrPo)dst)+size);
}

static uinteger dyHashFun(specialClassPo class,objPo o)
{
  assert(o->class==dynamicClass);

  dynPo d = (dynPo)o;

  return d->hash;
}

static long objNumber = 0;
  
ptrI dynamicObject(heapPo H)
{
  dynPo perm = (dynPo)permAllocate(CellCount(sizeof(dynRec)));

  perm->class = dynamicClass;
  perm->props = kvoid;
  perm->code = kvoid;
  perm->lock = newLock();
  perm->hash = objNumber++;

  return objP(perm);
}

void setDynamicCode(ptrI O,ptrI code)
{
  assert(hasClass(objV(O),dynamicClass));
  dynPo o = (dynPo)objV(O);
  o->code = code;
}

static lockPo getLock(ptrI S)
{
  dynPo s = goObjV(S);
  if(s->lock==NULL)
    s->lock = newLock();
  return s->lock;
}


retCode setProperty(heapPo H,ptrI sy,ptrI ky,ptrI vl)
{
  rootPo root = gcAddRoot(H,&sy);
  retCode ret = Ok;
  ptrI hash;
  assert(IsGoObject(sy));
  dynPo d = goObjV(sy);

  gcAddRoot(H,&ky);
  gcAddRoot(H,&vl);

#ifdef LOCKTRACE
  if(traceLock)
    outMsg(logFile,RED_ESC_ON "getting lock 0x%x on %w"RED_ESC_OFF"\n%_",d->lock,&sy);
#endif

  acquireLock(d->lock,0);	  /* important that we set up the roots first */

  if(identical(goObjV(sy)->props,kvoid)){
    hash = newHash(H,3);	      /* We start with space for 3 properties */
  }
  else
    hash = goObjV(sy)->props;

  ret = insertHash(H,&hash,ky,vl);

  if(ret==Ok){
    dynPo d = goObjV(sy);
    d->props = hash;  /* update the hash table -- it may have been recomputed */
    markGrey((objPo)d);
  }

  gcRemoveRoot(H,root);

  releaseLock(getLock(sy));
  return ret;
}

retCode g_setProp(processPo P,ptrPo a)
{
  ptrI t = deRefI(&a[1]);
  ptrI ky = deRefI(&a[2]);

  if(isvar(t))
    return liberror(P,"__setProp",eINSUFARG);
  else if(!IsGoObject(t))
    return liberror(P,"__setProp",eINVAL);
  else if(!isobj(ky) || !IsSymb(ky))
    return liberror(P,"__setProp",eINVAL);
  else{
    ptrI vl=kvoid;
    heapPo H = &P->proc.heap;
    uniChar eMsg[1024];

    gcAddRoot(H,&ky);
    gcAddRoot(H,&vl);
    gcAddRoot(H,&t);

    switchProcessState(P,in_exclusion);
    
    retCode ret = freezeTerm(H,&vl,deRefI(&a[3]),eMsg,NumberOf(eMsg));

    setProcessRunnable(P);

    if(ret!=Ok){
      strMsg(P->proc.errorMsg,NumberOf(P->proc.errorMsg),
	     "problem %U in assigning %w in %w to %w: %U",eMsg,&ky,&a[3],&t);
      return raiseError(P,P->proc.errorMsg,eINSUFARG);
    }

    return setProperty(H,t,ky,vl);
  }
}

retCode getProperty(ptrI sy,ptrI ky,ptrPo tgt)
{
  assert(IsGoObject(sy));

  dynPo d = goObjV(sy);
  if(IsHash(d->props))
    return searchHash(hashV(d->props),&ky,tgt);
  else
    return Fail;
}

retCode g_getProp(processPo P,ptrPo a)
{
  ptrI t = deRefI(&a[1]);

  if(isvar(t))
    return Fail;
  else if(!IsGoObject(t))
    return liberror(P,"__getProp",eINVAL);
  else{
    ptrI val = kvoid;
    ptrI sy = deRefI(&a[2]);
    dynPo d = goObjV(t);
    lockPo lk = d->lock;    /* we need to the lock the symbol while in use */

    acquireLock(lk,0);			/* We need to lock this guy */

    switch(getProperty(t,sy,&val)){
    case Ok:{
      releaseLock(lk);			/* We are OK now */
      return equal(P,&val,&a[3]);
    }
    case Error:
      releaseLock(lk);
      return liberror(P,"__getProp",eINVAL);
    default:
      releaseLock(lk);
      return Fail;
    }
  }
}

logical definedProperty(ptrI sy,ptrI ky)
{
  dynPo d = goObjV(sy);
  ptrI tgt;

  if(IsHash(d->props) && searchHash(hashV(d->props),&ky,&tgt)==Ok)
    return True;
  else
    return False;
}

retCode delProperty(ptrI sy,ptrI ky)
{
  assert(IsGoObject(sy));

  if(IsHash(goObjV(sy)->props))
    deleteHash(hashV(goObjV(sy)->props),&ky);
  return Ok;
}

retCode g_delProp(processPo P,ptrPo a)
{
  ptrI t = deRefI(&a[1]);

  if(isvar(t))
    return liberror(P,"__delProp",eINSUFARG);
  else if(!IsGoObject(t))
    return liberror(P,"__delProp",eINVAL);
  else{
    if(IsHash(goObjV(t)->props))
      deleteHash(hashV(goObjV(t)->props),deRef(&a[2]));
    return Ok;
  }
}

