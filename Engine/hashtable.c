/*
  Hash table management functions
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

#include "config.h"		/* pick up standard configuration header */
#include "go.h"
#include <stdlib.h>
#include <assert.h>
#include "opcodes.h"
#include "pool.h"
#include "debug.h"
#include "term.h"
#include "hashtable.h"
#include "encoded.h"             /* pick up the term encoding definitions */

/*
 * A hash table is represented as a permanent term of the form:
 * #(void,...,el,....,void,...el)
 * There are two sentinel values: void means that this entry is virgin, emptylist means that
 * it represents a deleted element. Searching must scan past emptylists, it may stop at a void
 */

ptrI hashClass;

static long hSizeFun(specialClassPo class,objPo o);
static comparison hCompFun(specialClassPo class,objPo o1,objPo o2);
static retCode hOutFun(specialClassPo class,ioPo out,objPo o);
static retCode hScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o);
static objPo hCopyFun(specialClassPo class,objPo dst,objPo src);
static uinteger hHashFun(specialClassPo class,objPo o);

void initHashClass(void)
{
  hashClass = newSpecialClass("#",hSizeFun,hCompFun,hOutFun,
			      hCopyFun,hScanFun,hHashFun);
}

static long hSizeFun(specialClassPo class,objPo o)
{
  assert(o->class==hashClass);

  hashTablePo h = (hashTablePo)o;

  return CellCount(sizeof(HashTableRec)+(h->size)*sizeof(ptrI));
}

static comparison hCompFun(specialClassPo class,objPo o1,objPo o2)
{
  if(o1==o2)
    return same;
  else
    return incomparible;
}

static retCode hOutFun(specialClassPo class,ioPo out,objPo o)
{
  assert(o->class==hashClass);

  hashTablePo h = (hashTablePo)o;
  retCode r = outStr(out,"{#");

  long ix;
  long count = h->size;
  char *sep = "";

  for(ix=0;r==Ok && ix<count;ix++){
    ptrI ky,vl;

    if(IsBinOp(&h->data[ix],commaClass,&ky,&vl)){
      r = outMsg(out,"%s%w=%w",sep,&ky,&vl);
      sep = ", ";
    }
  }
  if(r==Ok)
    r = outStr(out,"#}");

  return r;
}

static retCode hScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o)
{
  assert(o->class==hashClass);

  hashTablePo h = (hashTablePo)o;
  retCode r = Ok;
  long ix;
  long count = h->size;
  ptrPo arg = h->data;

  for(ix=0;r==Ok && ix<count;ix++,arg++){
    r = helper(arg,c);
  }

  return r;
}

static objPo hCopyFun(specialClassPo class,objPo dst,objPo src)
{
  long size = hSizeFun(class,src);
  memmove((void*)dst,(void*)src,size*sizeof(ptrI));

  return (objPo)(((ptrPo)dst)+size);
}

static uinteger hHashFun(specialClassPo class,objPo o)
{
  assert(o->class==hashClass);

  uinteger hash = 0;
  hashTablePo h = (hashTablePo)o;

  long ix;
  long count = h->size;

  for(ix=0;ix<count;ix++){
    ptrI ky,vl;

    if(IsBinOp(&h->data[ix],commaClass,&ky,&vl)){
      termHash(&ky,&hash);
      termHash(&vl,&hash);
    }
  }
  return hash;
}

retCode g_hash_term(processPo P,ptrPo a){
  uinteger hash = 0;
  retCode ret = termHash(&a[1],&hash);

  if(ret==Ok){
    ptrI val = allocateInteger(&P->proc.heap,hash);
    return equal(P,&val,&a[2]);
  }
  else
    return ret;
}

// Compute the hash code of a term
retCode termHash(ptrPo p,uinteger *hash)
{
  ptrI xx = deRefI(p);

  switch(ptg(xx)){
  case varTg:
    return Fail;                        /* cant hash variables */
  case objTg:{
    objPo o = objV(xx);
    clssPo class = classOf(o);

    if(isSpecialClass(class)){
      specialClassPo sClass = (specialClassPo)class;
      *hash += sClass->hashFun(sClass,o);
      return Ok;
    }
    else{
      *hash += class->hash;
      long ix;

      ptrPo a = objectArgs(o);
      long arity = objectArity(o);
      retCode ret = Ok;

      for(ix=0;ret==Ok && ix<arity;ix++,a++)
	ret = termHash(a,hash);
      return ret;
    }
  }
  default:
    syserr("unexpected word in termhash");
    return Error;
  }
}

retCode searchHash(hashTablePo table,ptrPo key,ptrPo val)
{
  integer tlen = hashTableSize(table);
  uinteger hash = 0;
  retCode ret = termHash(key,&hash);
  
  if(ret!=Ok)
    return ret;
  else{
    ptrPo bucket = &table->data[hash%tlen];
    ptrPo limit = &table->data[tlen];
    ptrPo el = bucket;

    // Phase one -- look for our item until we hit the end of the table
    while(el<limit){
      if(*el==kvoid)
	return Fail;	   /* Its not there -- because we found an empty slot */

      if(!identical(*el,emptyList) && testmatch(nthArg(objV(*el),0),key)==Ok){
        *val = deRefI(nthArg(objV(*el),1));
        return Ok;
      }
      el++;
    }

    // Phase two -- look from the start until the hash point (i.e., loop around)
    limit = bucket;
    el = &table->data[0];
    while(el<limit){
      if(*el==kvoid)
	return Fail;		/* Its not there -- because we found an empty slot */
      if(!identical(*el,emptyList) && testmatch(nthArg(objV(*el),0),key)==Ok){
        *val = deRefI(nthArg(objV(*el),1));
        return Ok;
      }
      el++;
    }

    return Fail;                        // Not found
  }
}


// Look for a key/value pair in the hash table
retCode g_search(processPo P,ptrPo a)
{
  ptrI t = deRefI(&a[1]);
  
  if(isvar(t) || IsFrozenVar(t))
    return liberror(P,"__hashsearch",eINSUFARG);
  else if(!IsHash(t))
    return liberror(P,"__hashsearch",eINVAL);
  else{
    ptrI val;

    switch(searchHash(hashV(t),&a[2],&val)){
    case Fail:
      return Fail;
    default:
    case Error:
      return liberror(P,"__hashsearch",eINVAL);
    case Ok:
      return equal(P,&val,&a[3]);
    }
  }
}

integer hashSize(ptrI t)
{
  hashTablePo table = hashV(t);
  integer len = hashTableSize(table);
  integer count = 0;
  ptrPo p = &table->data[0];
    
  for(;len-->0;p++){
    if(!identical(*p,kvoid) && !identical(*p,emptyList))
      count++;
  }

  return count;
}

// Count the number of elements in the table
retCode g_count(processPo P,ptrPo a)
{
  ptrI t = deRefI(&a[1]);
  
  if(isvar(t) || IsFrozenVar(t))
    return liberror(P,"__hashcount",eINSUFARG);
  else if(!IsHash(t))
    return liberror(P,"__hashcount",eINVAL);
  else{
    integer ix = hashSize(t);
      
    t = allocateInteger(&P->proc.heap,ix);
    
    return equal(P,&t,&a[2]);
  }
}

// Insert a new element in the table

static retCode insert(hashTablePo table,ptrI key,ptrI val);

retCode insertHash(heapPo H,ptrPo tb,ptrI key,ptrI val)
{
  ptrI tbl = deRefI(tb);
  rootPo root = gcAddRoot(H,&key);

  gcAddRoot(H,&val);
  gcAddRoot(H,&tbl);

  ptrI bk = objP(permObject(H,commaClass)); /* A comma tuple pair */
    
  gcAddRoot(H,&bk);

  updateArg(objV(bk),0,key);
  updateArg(objV(bk),1,val);
  markGrey(objV(bk));

  switch(insert(hashV(tbl),key,bk)){
  case Ok:
    *tb = tbl;
    gcRemoveRoot(H,root);
    return Ok;
  default:
    return Error;
  case Fail:{
    integer tlen = hashTableSize(hashV(tbl));
    integer nextLen = nextPrime(tlen+(tlen>>1)); /* 50% growth rate */
    ptrI nT = newHash(H,nextLen);
    hashTablePo nH = hashV(nT);
    integer i;
    ptrPo el = &hashV(tbl)->data[0];	/* We process the current table ... */

    gcRemoveRoot(H,root);

    if(insert(nH,key,bk)!=Ok)		// insert the new element first
      return Error;			/* something went wrong ...*/

    for(i=0;i<tlen;i++,el++){
      if(!identical(*el,kvoid) && !identical(*el,emptyList)){
	if(insert(nH,deRefI(nthArg(objV(*el),0)),*el)!=Ok)
	  return Error;
      }
    }

    *tb = nT;
    return Ok;
  }
  }
}

static retCode insert(hashTablePo table,ptrI key,ptrI val)
{
  uinteger hash = 0;
  retCode ret = termHash(&key,&hash);
  
  if(ret!=Ok)
    return Error;
  else{
    integer tlen = hashTableSize(table);
    ptrPo bucket = &table->data[hash%tlen];
    ptrPo limit = &table->data[tlen];
    ptrPo el = bucket;

    // Phase one -- look for our item until we hit the end of the table
    while(el<limit){
      if(identical(*el,kvoid)){
        *el = val;		/* insert into the empty slot*/
	markGrey((objPo)table);
        return Ok;
      }
      
      if(!identical(*el,emptyList) && testmatch(nthArg(objV(*el),0),&key)==Ok){
        *el = val;		/* update the entry in the table*/
	markGrey((objPo)table);
        return Ok;
      }
      el++;
    }

    // Phase two -- look from the start until the hash point (i.e., loop around)
    limit = bucket;
    el = &table->data[0];
    while(el<limit){
      if(identical(*el,kvoid)){
        *el = val;		/* insert into the empty slot*/
	markGrey((objPo)table);
        return Ok;
      }
      
      if(!identical(*el,emptyList) && testmatch(nthArg(objV(*el),0),&key)==Ok){
        *el = val;		/* update the entry in the table*/
	markGrey((objPo)table);
        return Ok;
      }
      el++;
    }
    
    // Phase three -- look again, but now we can smash empty lists
    limit = &table->data[tlen];

    while(el<limit){
      if(identical(*el,kvoid)||identical(*el,emptyList)){
        *el = val;		/* insert into the empty slot*/
	markGrey((objPo)table);
        return Ok;
      }
      
      el++;
    }

    // Phase four -- look from the start
    limit = bucket;
    el = &table->data[0];
    while(el<limit){
      if(identical(*el,kvoid) || identical(*el,emptyList)){
        *el = val;		/* insert into the empty slot*/
	markGrey((objPo)table);
        return Ok;
      }
      
      el++;
    }
    
    return Fail;                        // The table is full!
  }
}

retCode g_insert(processPo P,ptrPo a)
{
  ptrI t = deRefI(&a[1]);
  
  if(isvar(t) || IsFrozenVar(t))
    return liberror(P,"__hashinsert",eINSUFARG);
  else if(!IsHash(t))
    return liberror(P,"__hashinsert",eINVAL);
  else{
    ptrI kz = kvoid;
    ptrI vl = kvoid;
    heapPo H = &P->proc.heap;
    uniChar eMsg[MAX_SYMB_LEN];

    gcAddRoot(H,&t);
    gcAddRoot(H,&kz);
    gcAddRoot(H,&vl);

    switchProcessState(P,in_exclusion);

    retCode ret = freezeTerm(H,&kz,deRefI(&a[2]),eMsg,NumberOf(eMsg));

    if(ret==Ok)
      ret = freezeTerm(H,&vl,deRefI(&a[3]),eMsg,NumberOf(eMsg));

    if(ret==Ok)
      ret = insertHash(H,&t,kz,vl);

    setProcessRunnable(P);

    switch(ret){
    case Ok:
      return equal(P,&t,&a[4]);
    default:
      return liberror(P,"__hashinsert",eINVAL);
    }
  }
}

retCode deleteHash(hashTablePo table,ptrPo key)
{
  integer tlen = hashTableSize(table);
  uinteger hash = 0;
  retCode ret = termHash(key,&hash);
  
  if(ret!=Ok)
    return ret;
  else{
    ptrPo base = &table->data[0];
    ptrPo bucket = &table->data[hash%tlen];
    ptrPo limit = &table->data[tlen];
    ptrPo el = bucket;

    // Phase one -- look for our item until we hit the end of the table
    while(el<limit){
      if(identical(*el,kvoid))
	return Fail;		/* Its not there -- because we found an empty slot */

      if(!identical(*el,emptyList) && testmatch(nthArg(objV(*el),0),key)==Ok){
	*el = emptyList;	/* We delete the entry, with the emptyList sentinel*/
	markGrey((objPo)table);
        return Ok;
      }
      el++;
    }

    // Phase two -- look from the start until the hash point (i.e., loop around)
    limit = bucket;
    el = base;
    while(el<limit){
      if(identical(*el,kvoid))
	return Fail;		/* Its not there -- because we found an empty slot */
      if(!identical(*el,emptyList) && testmatch(nthArg(objV(*el),0),key)==Ok){
	*el = emptyList;
	markGrey((objPo)table);
        return Ok;
      }
      el++;
    }

    return Fail;                        // Not found
  }
}

retCode g_delete(processPo P,ptrPo a)
{
  ptrI t = deRefI(&a[1]);

  if(isvar(t) || IsFrozenVar(t))
    return liberror(P,"__hashdelete",eINSUFARG);
  else if(!IsHash(t))
    return liberror(P,"__hashdelete",eINVAL);
  else{
    switch(deleteHash(hashV(t),&a[2])){
    case Fail:
      return Fail;
    case Ok:
      return Ok;
    default:
      return liberror(P,"__hashdelete",eINVAL);
    }
  }
}

retCode hashContents(heapPo heap,ptrI tbl,ptrPo out)
{
  assert(IsHash(tbl));

  {
    ptrI result = emptyList;
    rootPo root = gcAddRoot(heap,&result);
    integer tlen = hashTableSize(hashV(tbl));
    integer i;
    ptrI xx = kvoid;                    // Use this as a temporary source
    ptrI yy = kvoid;                    // Use this as a temporary target
    ptrI el = kvoid;
    
    gcAddRoot(heap,&xx);
    gcAddRoot(heap,&yy);                     // Ensure GC safety
    gcAddRoot(heap,&el);
    gcAddRoot(heap,&tbl);
      
    for(i=0;i<tlen;i++){
      hashTablePo h = hashV(tbl);		  /* it may be moved by GC */
      xx = h->data[i];				  /* pick up the table entry */
	
      if(HasClass(xx,commaClass)){
	result = consLsPair(heap,xx,result);
      }
    }

    *out = result;
    
    gcRemoveRoot(heap,root);
    return Ok;
  }
}

retCode g_contents(processPo P,ptrPo a)
{
  ptrI t = deRefI(&a[1]);
  
  if(isvar(t) || IsFrozenVar(t))
    return liberror(P,"__hashcontents",eINSUFARG);
  else if(!IsHash(t))
    return liberror(P,"__hashcontents",eINVAL);
  else{
    ptrI result = emptyList;
    retCode ret = hashContents(&P->proc.heap,t,&result);

    if(ret!=Ok)
      return liberror(P,"__hashcontents",eINVAL);
    else
      return equal(P,&result,&a[2]);
  }
}  

retCode g_keys(processPo P,ptrPo a)
{
  ptrI t = deRefI(&a[1]);
  
  if(isvar(t) || IsFrozenVar(t))
    return liberror(P,"__hashkeys",eINSUFARG);
  else if(!IsHash(t))
    return liberror(P,"__hashkeys",eINVAL);
  else{
    ptrI result = emptyList;
    integer tlen = hashTableSize(hashV(t));
    integer i;
    ptrI xx = kvoid;                    // Use this as a temporary source
    
    gcAddRoot(&P->proc.heap,&result);
    gcAddRoot(&P->proc.heap,&xx);
    gcAddRoot(&P->proc.heap,&t);
      
    for(i=0;i<tlen;i++){
      hashTablePo h = hashV(t);		/* table can move during GC */
      xx = h->data[i];			/* pick up the table entry */
	
      if(!identical(xx,kvoid) && !identical(xx,emptyList) && hasClass(objV(xx),commaClass)){
	result = consLsPair(&P->proc.heap,deRefI(nthArg(objV(xx),0)),result);
      }
    }

    return equal(P,&result,&a[2]);
  }
}      

retCode g_values(processPo P,ptrPo a)
{
  ptrI t = deRefI(&a[1]);
  
  if(isvar(t) || IsFrozenVar(t))
    return liberror(P,"__hashvalues",eINSUFARG);
  else if(!IsHash(t))
    return liberror(P,"__hashvalues",eINVAL);
  else{
     ptrI result = emptyList;
     integer tlen = hashTableSize(hashV(t));
     integer i;
     ptrI xx = kvoid;                    // Use this as a temporary source

     gcAddRoot(&P->proc.heap,&result);
     gcAddRoot(&P->proc.heap,&xx);
     gcAddRoot(&P->proc.heap,&t);

     for(i=0;i<tlen;i++){
       hashTablePo h = hashV(t);		/* table can move during GC */
       xx = h->data[i];			/* pick up the table entry */

       if(!identical(xx,kvoid) && !identical(xx,emptyList) && hasClass(objV(xx),commaClass)){
	 result = consLsPair(&P->proc.heap,deRefI(nthArg(objV(xx),1)),result);
       }
     }
     return equal(P,&result,&a[2]);
  }
}

ptrI newHash(heapPo H,long len)
{
  hashTablePo nT = (hashTablePo)permAllocate(HashCellCount(len));
  integer i;

  nT->sign = hashClass;
  nT->size = len;

  for(i=0;i<len;i++)
    nT->data[i]=kvoid;
  return objP(nT);
}

retCode g_newhash(processPo P,ptrPo a)
{
  ptrI ii = deRefI(&a[1]);
  objPo io = objV(ii);

  if(isvar(ii) || IsFrozenVar(ii))
    return liberror(P,"__newhash",eINSUFARG);
  else if(!isInteger(io))
    return liberror(P,"__newhash",eINTNEEDD);
  else{
    ptrI hash = newHash(&P->proc.heap,integerVal((integerPo)io));

    return equal(P,&hash,&a[2]);
  }
}

