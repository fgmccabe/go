/* Access to the environment variables
  (c) 2001 F.G. McCabe

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

  $Id: envir.c,v 1.2 2004/04/29 16:24:27 fmccabe Exp $
  $Log: envir.c,v $
  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system

*/
#include "config.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <assert.h>
#include "go.h"			/* main header file */
#include "dict.h"		/* Dictionary handling stuff */
#include "symbols.h"
#include "debug.h"		/* Debugger access functions */

retCode g_getenv(processPo P,ptrPo a)
{
  ptrI k = deRefI(&a[1]);
  
  if(isvar(k))
    return liberror(P,"getenv",eINSUFARG);
  else if(!IsSymb(k))
    return liberror(P,"getenv",eINVAL);
  else{
    symbPo key = symbV(k);
    long sLen = SymLen(key)+1;
    char eVar[sLen];
                     
    _utf(SymVal(key),(unsigned char*)eVar,sLen);
    
    {
       char *val = getenv(eVar);
       
       if(val!=NULL){
         ptrI L = allocateCString(&P->proc.heap,val);
         
         return equal(P,&a[3],&L);
       }
       else
         return equal(P,&a[2],&a[3]);
    }
  }
}

retCode g_setenv(processPo P,ptrPo a)
{
  ptrI k = deRefI(&a[1]);
  ptrI v = deRefI(&a[2]);
  
  if(isvar(k) || !IsSymb(k) || isGroundString(&v)!=Ok)
    return liberror(P,"setenv",eINVAL);
  else{
    symbPo key = symbV(k);
    long kLen = SymLen(key)*sizeof(uniChar)+1;
    char eVar[kLen];
    long vLen = StringLen(&v)*sizeof(uniChar)+1;
    uniChar uVal[vLen];
    char eVal[vLen];
                     
    _utf(SymVal(key),(unsigned char*)eVar,kLen);  // Map the key to UTF-8
    String2Uni(&v,uVal,vLen);     // We do this is two stages, 'cos we're lazy
    _utf(uVal,(unsigned char*)eVal,vLen);         // map the value to UTF-8

    if(setenv(eVar,eVal,1)==0)
      return Ok;
    else
      return liberror(P,"setenv",eSPACE);
  }
}

retCode g_envir(processPo P,ptrPo a)
{
  extern char **environ;
  int i,cnt;
  ptrI lst = emptyList;
  ptrI el = kvoid;
  ptrI ky = kvoid;
  ptrI vl = kvoid;
  heapPo H = &P->proc.heap;
  rootPo root = gcAddRoot(H,&lst);

  gcAddRoot(H,&el);
  gcAddRoot(H,&ky);
  gcAddRoot(H,&vl);
  
  for(cnt=0;environ[cnt]!=NULL;cnt++)
    ;

  switchProcessState(P,in_exclusion);

  for(i=cnt-1;i>0;i--){
    char *pt = strchr(environ[i],'=');

    if(pt!=NULL){
      long len = strlen(environ[i]);
      uniChar envVar[len+1];
      *pt = '\0';           /* Split off the key from the value */
      
      strMsg(envVar,len+1,"%s",environ[i]); // Construct the symbol's print name
            
      ky = newUniSymbol(envVar);
      vl = allocateCString(H,pt+1);
      el = objP(allocateObject(H,commaClass));
      
      updateArg(objV(el),0,ky);
      updateArg(objV(el),1,vl);
      
      lst = consLsPair(H,el,lst);
      *pt = '=';          /* restore the value */
    }
  }
  
  setProcessRunnable(P);
  gcRemoveRoot(H,root);
  return equal(P,&lst,&a[1]);
}
  
retCode g_getlogin(processPo P,ptrPo a)
{
  ptrI k = deRefI(&a[1]);
  
  if(!isvar(k))
    return liberror(P,"getlogin",eVARNEEDD);
  else{
    ptrI Login = allocateCString(&P->proc.heap,getlogin());
    
    return equal(P,&a[1],&Login);
  }
}
