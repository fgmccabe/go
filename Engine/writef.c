/*
  Data value and formatted write programs
  (c) 1994-1998 Imperial College and F.G. McCabe

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

  $Id: writef.c,v 1.3 2004/04/29 16:24:28 fmccabe Exp $
  $Log: writef.c,v $
  Revision 1.3  2004/04/29 16:24:28  fmccabe
  Completely new type system

*/
#include "config.h"		/* pick up standard configuration header */
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>

#include "go.h"
#include "term.h"
#include "hashtable.h"

/*
 * write a cell in a basic format
 * The depth argument limits the depth that tuples are printed to
 */

retCode wStringChr(ioPo f,uniChar ch)
{
  switch(ch){
  case '\a':
    return outStr(f,"\\a");
  case '\b':
    return outStr(f,"\\b");
  case '\x7f':
    return outStr(f,"\\d");
  case '\x1b':
    return outStr(f,"\\e");
  case '\f':
    return outStr(f,"\\f");
  case '\n': 
    return outStr(f,"\\n");
  case '\r': 
    return outStr(f,"\\r");
  case '\t': 
    return outStr(f,"\\t");
  case '\v': 
    return outStr(f,"\\v");
  case '\\':
    return outStr(f,"\\\\");
  case '\"':
    return outStr(f,"\\\"");
  default:
    if(ch<' '||!isChar(ch))
      return outMsg(f,"\\+%x;",ch);
    else
      return outChar(f,ch);
  }
  return Ok;
}

static retCode outC(ioPo f,ptrPo x,long depth,long prec,logical alt);

retCode outCell(ioPo f,ptrPo x,long depth,long prec,logical alt) 
{
  if(x!=NULL)
    return outC(f,x,depth,prec,alt);
  else
    return outStr(f,"(NULL)");
}

static retCode outC(ioPo f,ptrPo x,long depth,long prec,logical alt)
{
  ptrI vx = *(x=deRef(x));
  objPo p = objV(vx);
  retCode r = Ok;

  switch(ptg(vx)){
  case varTg:
    if(isSuspVar((ptrPo)vx))
      r = outMsg(f,"_*%x",vx);
    else
      r =outMsg(f,"_%x",vx);
    break;
  case objTg:{
    ptrI class = p->class;

    if(isfwd(class)){
      outMsg(f,"*");
      class = *((ptrPo)objV(class));
    }

    if(class==integerClass)
      r = outInteger(f,integerVal((integerPo)p),10,0,prec,' ',False,"",False);
    else if(class==floatClass)
      r = outFloat(f,floatVal((floatPo)p));
    else if(class==symbolClass){
      symbPo s = (symbPo)p;
      uniChar *sym = SymVal(s);
	
      r = outChar(f,'\'');

      while(r==Ok && *sym!=0)
	r = wStringChr(f,*sym++);
      if(r==Ok)
	r = outChar(f,'\'');
    }
    else if(class==charClass){
      r = outChar(f,'`');
      if(r==Ok)
	r = wStringChr(f,CharVal((charPo)p));
    }
    else if(class==classClass){
      clssPo cl = (clssPo)p;
      uniChar *clName = className(cl);

      r = outText(f,clName,uniStrLen(clName));
      if(r==Ok)
	r = outChar(f,'/');
      if(r==Ok)
	r = outInteger(f,cl->arity,10,0,prec,' ',False,"",False);
    }
    else if(isGroundString(x)==Ok){
      if(depth>0 || ListLen(vx)<=3){
	outChar(f,'"');

	while(r==Ok && IsList(vx)){
	  ptrPo ll = listHead(objV(vx));
	  uniChar ch = CharVal(charV(deRefI(ll)));

	  r = wStringChr(f,ch);
	  
	  vx = deRefI(ll+1);
	}
      }
      else{
	outChar(f,'"');
	
	while(r==Ok && prec-->0 && IsList(vx)){
	  ptrPo ll = listHead(objV(vx));
	  uniChar ch = CharVal(charV(deRefI(ll)));

	  r = wStringChr(f,ch);
	      
	  vx = deRefI(ll+1);
	}
      }
      if(r==Ok){
	if(identical(vx,emptyList))
	  r = outStr(f,"\"");
	else
	  r = outStr(f,"...\"");
      }
    }
    else if(class==listClass){
      if(depth>0){
	long maxLen = (prec!=0?prec*2:INT_MAX); /* How many elements to show */

	r = outChar(f,'[');

	while(r==Ok && IsList(vx)){
	  ptrPo ll = listHead(objV(vx));

	  r=outC(f,ll++,depth-1,prec,alt);

	  vx = deRefI(ll);
	  
	  if(r==Ok && IsList(vx)){
	    if(maxLen--<=0){
	      r = outStr(f,"...");        /* only show a certain length */

	      goto exit_list;	/* what a hack, need a double break */
	    }
	    else
	      r = outChar(f,',');
	  }
	}
	if(r==Ok && !identical(vx,emptyList)){
	  r = outStr(f,",..");

	  if(r==Ok)
	    r = outC(f,&vx,depth-1,prec,alt);
	}
      exit_list:
	if(r==Ok)
	  r = outStr(f,"]");
      }
      else
	r = outStr(f,"[...]");
    }
    else if(class==commaClass){		/* show comma structures as tuples */
      if(depth>0){
	char *sep = "";
	ptrI h,t;
          
	outChar(f,'(');
          
	while(r==Ok&&IsBinOp(x,commaClass,&h,&t)){
	  r = outStr(f,sep);
	  sep = ", ";
	  r = outC(f,&h,depth-1,prec,alt);
	  x = &t;
	}
        
	if(r==Ok)
	  r= outStr(f,sep);
	
	if(r==Ok)
	  r = outC(f,x,depth-1,prec,alt);
	
	if(r==Ok)
	  r= outChar(f,')');
      }
      else
	r= outStr(f,"(...)");
    }
    else if(class==hashClass){
      hashTablePo h = (hashTablePo)p;
      long size = hashTableSize(h);
      long i;
      char *sep = "";

      r = outStr(f,"{#");
      for(i=0;r==Ok && i<size;i++){
	ptrI ky,vl;

	if(IsBinOp(&h->data[i],commaClass,&ky,&vl)){
	  r = outMsg(f,"%s%w=%w",sep,&ky,&vl);
	  sep = ", ";
	}
      }
      if(r==Ok)
	r = outStr(f,"#}");
    }
    else if(IsTermClass(class)){
      uniChar *name = objectClassName(p);
      uniChar *localName;

      if((localName=uniSearch(name,uniStrLen(name),'#'))!=NULL)
      	name = localName+1;

      r = outMsg(f,"%U",name);
      
      if(depth>0){
	long arity = objectArity(p);

	if(arity>0){
	  ptrPo a = objectArgs(p);
	  long i;
	  char *sep = "";
          
	  outChar(f,'(');
          
	  for(i=0;r==Ok&&i<arity;i++,a++){
	    r = outStr(f,sep);
	    sep = ", ";
	    r = outC(f,a,depth-1,prec,alt);
	  }
	  if(r==Ok)
	    r= outChar(f,')');
	}
      }
      else
	r= outStr(f,"(...)");
    }
    else if(IsSpecialClass(class)){
      specialClassPo sClass = (specialClassPo)objV(class);
      r = sClass->outFun(sClass,f,p);
    }
    return r;
  }

  case fwdTg:{			       /* special case to help with debugging */
    outMsg(f,"[0x%x]-->",x);
    return outC(f,(ptrPo)p,depth-1,prec,alt);
  }

  default:
    outMsg(f,"illegal cell found at [%x]",p);
    return Error;
  }

  return r;
}

retCode quoteString(ioPo f,uniChar *s,long len)
{
  long i;
  retCode ret = outChar(f,'\"');
  
  for(i=0;ret==Ok && i<len;i++)
   ret = wStringChr(f,*s++);
   
  if(ret==Ok)
    ret = outChar(f,'\"');
  return ret;
}

void dc(ptrPo trm)
{
  if(trm!=NULL)
    outMsg(logFile,"0x%x -> %,20w\n",trm,trm);
  else
    outMsg(logFile,"NULL\n");
  flushFile(logFile);
}

void dO(objPo trm)
{
  ptrI T = objP(trm);
  outMsg(logFile,"0x%x : %,20w\n",trm,&T);
  flushFile(logFile);
}
