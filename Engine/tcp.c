/* 
   Socket and TCP handling functions
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

  $Id: tcp.c,v 1.3 2004/04/29 16:24:28 fmccabe Exp $
  $Log: tcp.c,v $
  Revision 1.3  2004/04/29 16:24:28  fmccabe
  Completely new type system

*/ 

#include "config.h"		/* pick up standard configuration header */
#include <string.h>
#include <stdlib.h>
 
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
#include "perms.h"

/* Open up a socket for listening to */

retCode g_listen(processPo P,ptrPo a)
{
  ptrI Port = deRefI(&a[1]);
  
  if(isvar(Port)||!isInteger(objV(Port)))
    return liberror(P,"__listen",eINTNEEDD);
  else if(!isvar(deRefI(&a[2])))
    return liberror(P,"__listen",eVARNEEDD);
  else{
    integer port = integerVal(intV(Port));
    uniChar nBuff[MAX_MSG_LEN];
    ioPo listen;
    
    strMsg(nBuff,NumberOf(nBuff),"listen@%ld",port);
    switchProcessState(P,wait_io);
    listen = O_IO(listeningPort(nBuff,port));
    setProcessRunnable(P);

    if(listen==NULL)
      return liberror(P,"_listen",eNOPERM);
    else{
      ptrI t2 = allocFilePtr(listen); /* return open file descriptor */
      ptrPo res = deRef(&a[2]);

      bindVar(P,res,t2);
      return Ok;
    }
  }
}

/* accept allows a connection from a connect socket and returns the
   socket number of the connection and information about the connecting
   host */
retCode g_accept(processPo P,ptrPo a)
{
  ptrI Port = deRefI(&a[1]);
  objPo o1 = objV(Port);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__accept",eINVAL);
  else{
    ioPo listen = filePtr(Port);
    ioPo inC,outC;

    switchProcessState(P,wait_io);
    retCode ret = acceptConnection(O_SOCK(listen),pickEncoding(deRefI(&a[7])),
				   &inC,&outC);

    setProcessRunnable(P);

    switch(ret){
    case Ok:{
      int port;
      uniChar pBuff[MAX_MSG_LEN];
      uniChar *peerN = peerName(O_SOCK(inC),&port);
      uniChar *peerI = peerIP(O_SOCK(inC),&port,&pBuff[0],NumberOf(pBuff));
      ptrI txtList;
                
      if(peerN==NULL||peerI==NULL){
	closeFile(inC);
	closeFile(outC);
	return liberror(P,"__accept",eNOTFND);
      }
      
      ptrI tI = allocFilePtr(inC); /* return open file descriptor */
      bindVar(P,deRef(&a[2]),tI);
      ptrI tO = allocFilePtr(outC);
      bindVar(P,deRef(&a[3]),tO);

      txtList = allocateString(&P->proc.heap,peerN,uniStrLen(peerN));
          
      bindVar(P,deRef(&a[4]),txtList); /* Bind the peername of the connection */
          
      ptrI pt = allocateInteger(&P->proc.heap,port);
      bindVar(P,deRef(&a[6]),pt);   /* Bind the port number of the connection */
        
      txtList = allocateString(&P->proc.heap,peerI,uniStrLen(peerI));
      bindVar(P,deRef(&a[5]),txtList);      /* Bind the IP address of the connection */
        
      return Ok;
    }
    default:
      return liberror(P,"__accept",eIOERROR);
    }
  }
}


/* Attempt a connection with a server 
   specified as a pair: hostname(or ip address)/port
*/
retCode g_connect(processPo P,ptrPo a)
{
  ptrI Host = deRefI(&a[1]);
  ptrI Port = deRefI(&a[2]);

  if(isvar(Host) || isGroundString(&Host)!=Ok)
    return liberror(P,"__connect",eSTRNEEDD);
  else if(isvar(Port) || !isInteger(objV(Port)))
    return liberror(P,"__connect",eINTNEEDD);
  else if(isvar(deRefI(&a[3])))
    return liberror(P,"__connect",eINVAL);
  else{
    int port = integerVal(intV(Port));
    uniChar host[MAX_MSG_LEN];
    retCode ret;

    if(port==0||String2Uni(&Host,host,NumberOf(host))!=Ok)
      return liberror(P,"__connect",eINVAL);

    switchProcessState(P,wait_io);
    ioPo inC,outC;
    ret = connectRemote(host,port,pickEncoding(deRefI(&a[3])),True,&inC,&outC);

    setProcessRunnable(P);

    switch(ret){
    case Ok: {
      ptrI RemIn = allocFilePtr(inC);
      ptrI RemOut = allocFilePtr(outC);
      equal(P,&a[4],&RemIn);
      return equal(P,&a[5],&RemOut);
    }
    default:
      logMsg(logFile,"Failed to establish connection: %U",host);
      return liberror(P,"__connect",eCONNECT);
    }
  }
}


#if 0
** *  Fix me later ** *

/* Open up a UDP socket for listening to */

retCode g_udpPort(processPo P,ptrPo a)
{
  ptrI Port = deRefI(&a[1]);
  
  if(isvar(Port)||!isInteger(objV(Port)))
    return liberror(P,"__udpPort",eINTNEEDD);
  else if(!isvar(deRefI(&a[2])))
    return liberror(P,"__udpPort",eVARNEEDD);
  else{
    long portNo = integerVal(intV(Port));
    uniChar nBuff[MAX_MSG_LEN];
    ioPo sock;
    
    strMsg(nBuff,NumberOf(nBuff),"udpPort:%d",portNo);
    sock = udpPort(nBuff,portNo);

    if(sock==NULL)
      return liberror(P,"__udpPort",eNOPERM);
    else{
      ptrI t2 = allocFilePtr(sock); /* return open file descriptor */
      ptrPo res = deRef(&a[2]);

      bindVar(P,res,t2);
      return Ok;
    }
  }
}

/* Send a block of text down a UDP port */
retCode g_udpSend(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__udpSend",eINVAL);
  else{
    ioPo file = filePtr(t1);

    if(isUDPport(file)!=Ok)
      return liberror(P,"__udpSend",eINVAL);
    else if(isGroundString(&a[2])!=Ok||isGroundString(&a[3])!=Ok)
      return liberror(P,"__udpSend",eSTRNEEDD);
    else if(isvar(t1=deRefI(&a[4])) || !isInteger(objV(t1)))
      return liberror(P,"__udpSend",eINTNEEDD);
    else{
      long len = StringLen(&a[2])+1;
      uniChar buff[len];
      long plen = StringLen(&a[3])+1;
      uniChar peer[plen];
      int port = integerVal(intV(t1));
      
      if(String2Uni(&a[2],buff,len)!=Ok)
        return liberror(P,"__udpSend",eSTRNEEDD);
      else if(String2Uni(&a[3],peer,plen)!=Ok)
        return liberror(P,"__udpSend",eSTRNEEDD);
        
      switch(udpSend(file,buff,len-1,peer,port)){
      case Ok:
        return Ok;
	    
      default:
	return liberror(P,"__udpSend",eIOERROR);
      }
    }
  }
}

/* read a message from a UDP port */

retCode g_udpGet(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__udpGet",eINVAL);
  else{
    ioPo file = filePtr(t1);
    uniChar txt[16384];
    long len = NumberOf(txt);
    uniChar peer[1024];
    long plen = NumberOf(peer);
    int port = 0;

    if(isUDPport(file)!=Ok)
      return liberror(P,"__udpGet",eNOPERM);
      
    switch(udpRead(file,txt,&len,peer,plen,&port)){
      case Eof:
        return liberror(P,"__udpGet",eEOF);
      case Ok:{
        ptrI el = allocateString(&P->proc.heap,txt,len);
      
        if(equal(P,&el,&a[2])!=Ok)
          return Fail;
      
        el = allocateString(&P->proc.heap,peer,uniStrLen(peer));
      
        if(equal(P,&el,&a[3])!=Ok)
          return Fail;
      
        el = allocateInteger(&P->proc.heap,port);
      
        return equal(P,&el,&a[4]);
      }
      default:
	return liberror(P,"__intext",eIOERROR);
    }
  }
}
#endif

/* Access host name functions */
/* return IP addresses of a host */
retCode g_hosttoip(processPo P,ptrPo a)
{
  ptrI Host = deRefI(&a[1]);

  if(isvar(Host) || isGroundString(&Host)!=Ok)
    return liberror(P,"hosttoip",eSTRNEEDD);
  else{
    long i;
    uniChar ip[MAX_MSG_LEN];
    uniChar host[MAX_MSG_LEN];
    ptrI l = emptyList;
    ptrI el = kvoid;
    rootPo root = gcAddRoot(&P->proc.heap,&l);
    
    String2Uni(&Host,host,NumberOf(host));

    gcAddRoot(&P->proc.heap,&el);

    for(i=0;getNthHostIP(host,i,ip,NumberOf(ip))!=NULL;i++){
      el = allocateString(&P->proc.heap,ip,uniStrLen(ip));

      l = consLsPair(&P->proc.heap,el,l);
    }

    gcRemoveRoot(&P->proc.heap,root);
    return equal(P,&a[2],&l);
  }
}

/* Access host name from IP address */
retCode g_iptohost(processPo P,ptrPo a)
{
  ptrI IP = deRefI(&a[1]);

  if(isvar(IP) || isGroundString(&IP)!=Ok)
    return liberror(P,"iptohost",eSTRNEEDD);
  else{
    uniChar ip[MAX_MSG_LEN];
    uniChar *host = getHostname(ip);

    String2Uni(&IP,ip,NumberOf(ip));
    
    if(host!=NULL){
      ptrI Host = allocateString(&P->proc.heap,host,uniStrLen(host));
      return equal(P,&Host,&a[2]);
    }
    else
      return liberror(P,"iptohost",eNOTFND);
  }
}

