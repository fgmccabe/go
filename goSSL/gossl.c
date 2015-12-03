/* 
  Establish and manage openSSL connections
  (c) 2006 F.G.McCabe

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
#include "go.h"		/* Main header file */
#include "lock.h"
#include "debug.h"
#include "../Engine/Headers/fileio.h"
#include "io.h"
#include "gosslP.h"
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h> 
#include <netdb.h>
#include <arpa/inet.h>
#include <unistd.h>
#ifdef SYSV
#include <stropts.h>
#endif
#include <errno.h>

#ifndef SOCKET_ERROR
#define SOCKET_ERROR -1
#endif

#ifndef INVALID_SOCKET
#define INVALID_SOCKET -1
#endif

#ifdef SSLTRACE
logical traceSSL = False;
#endif

static pthread_once_t initOnce = PTHREAD_ONCE_INIT;

static void initSSL(void)
{
  SSL_load_error_strings();		/* Readable error messages */
  SSL_library_init();
  // TODO: add some randomness
}

/* Set up the SSL socket class */

static void initSSLClass(classPo class,classPo request);
static void SSLDestroy(objectPo o);
static void SSLInit(objectPo list,va_list *args);

static retCode refillSSL(filePo f);
static retCode sslFlush(ioPo f,long count);
static retCode sslSeek(ioPo f,long count);

SSLClassRec SSLClass = {
  {
    (classPo)&SockClass,		   /* parent class is a socket */
    "ssl",                                 /* this is the ssl class */
    NULL,				   /* inherit from Sock */
    initSSLClass,			   /* SSL class initializer */
    O_INHERIT_DEF,			   /* SSL object element creation */
    SSLDestroy,                            /* SSL object destruction */
    O_INHERIT_DEF,			   /* erasure */
    SSLInit,				   /* initialization of an SSL object */
    sizeof(SSLObject),
    NULL,				    /* pool of file values */
    PTHREAD_ONCE_INIT,			    /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {
    NULL
  },
  {
    O_INHERIT_DEF,                        /* inChar  */
    O_INHERIT_DEF,                        /* outChar  */
    O_INHERIT_DEF,                        /* ungetChar  */
    O_INHERIT_DEF,                        /* inBytes  */
    O_INHERIT_DEF,                        /* outBytes  */
    O_INHERIT_DEF,                        /* backByte  */
    O_INHERIT_DEF,			  /* Are we at EOF? */
    O_INHERIT_DEF,                        /* readyIn  */
    O_INHERIT_DEF,                        /* readyOut  */
    sslFlush,				  /* flush  */
    sslSeek,				  /* seek  */
    O_INHERIT_DEF                         /* close  */
  },
  {
    O_INHERIT_DEF,			/* configure a socket */
    refillSSL				/* refill an SSL socket */
  },
  {					/* Socket part of class */
  },
  {					/* SSL part of class */
  }
};

classPo sslClass = (classPo)&SSLClass;

static void initSSLClass(classPo class,classPo request)
{
  assert(request->pool!=NULL);
  pthread_once(&initOnce,&initSSL);
  SSLClassRec *sslClass = (SSLClassRec*)request;
  sslClass->sslPart.ctx = SSL_CTX_new(SSLv23_method());
  if(sslClass->sslPart.ctx==NULL){
    syserr("could not initialize SSL");
  }
}

static void SSLInit(objectPo o,va_list *args)
{
  sslPo s = (sslPo)o;

  SSL *ssl = s->ssl.ssl = SSL_new(((SSLClassRec*)s->object.class)->sslPart.ctx);

  if(ssl!=NULL){
    if(!SSL_set_fd(ssl,s->file.fno))
      syserr("Could not set FD on SSL object");
  }
}

static void SSLDestroy(objectPo o)
{
  sslPo s = (sslPo)o;

  if(s->ssl.ssl!=NULL){
    if(SSL_shutdown(s->ssl.ssl)==0)
      SSL_shutdown(s->ssl.ssl);
    SSL_free(s->ssl.ssl);
    s->ssl.ssl = NULL;
  }
}

static retCode sslSeek(ioPo f,long count){
  return Fail;
}

/* SSL socket reading and writing functions */
static retCode refillSSL(filePo f)
{
  sslPo s = O_SSL(f);

  if(s->file.in_pos>=s->file.in_len){	/* nead to read more input? */
    int len;
    int lerrno;                         /* local copy of errno */
    
    stopAlarm();                        /* Stop the time interrupt */
    len = SSL_read(s->ssl.ssl,s->file.in_line,MAXLINE);
    lerrno = errno;
    startAlarm();                       /* Restart the timer interrupt */
    
    if(len<0){                          /* something wrong? */
      switch(SSL_get_error(s->ssl.ssl,len)){
      case SSL_ERROR_NONE:
	return Interrupt;		/* Will try again */
      case SSL_ERROR_ZERO_RETURN:
        s->file.in_pos = s->file.in_len = 0;
        s->io.status = Eof;             /* we have reach end of file */
	if(SSL_shutdown(s->ssl.ssl)==0)	/* Shut down the connection */
	  SSL_shutdown(s->ssl.ssl);
	SSL_free(s->ssl.ssl);
	s->ssl.ssl = NULL;		/* Just in case */
        return Eof;
      case SSL_ERROR_WANT_READ:
      case SSL_ERROR_WANT_WRITE:
      case SSL_ERROR_WANT_CONNECT:
      case SSL_ERROR_WANT_ACCEPT:
      case SSL_ERROR_WANT_X509_LOOKUP:
	return Interrupt;		/* Failed, will try again */
      case SSL_ERROR_SYSCALL:
      case SSL_ERROR_SSL:
      default:
	return Error;
      }
    }
    else{
      s->file.in_pos = 0;
      s->file.in_len = len;

      if(len==0){			/* we have reach end of file */
        s->io.status = Eof;             /* we have reach end of file */
	if(SSL_shutdown(s->ssl.ssl)==0)	/* Shut down the connection */
	  SSL_shutdown(s->ssl.ssl);
	SSL_free(s->ssl.ssl);
	s->ssl.ssl = NULL;		/* Just in case */
        s->io.status = Eof;
      }
      else
        s->io.status = Ok;
      return s->io.status;
    }
  }
  else
    return Ok;			/* Already got stuff in there */
}

static retCode sslFlush(ioPo io,long count)
{
  sslPo s = O_SSL(io);
  long written;
  long remaining = s->file.out_pos;
  byte *cp = s->file.out_line;
  long writeGap = 0;

  if(count>0 && s->file.out_pos+count<NumberOf(s->file.out_line))
    return Ok;
  
  while(remaining>0 && (written=SSL_write(s->ssl.ssl,cp,remaining))!=remaining){
    if(written>0){
      cp+=written;
      writeGap += written;
      remaining-=written;
    }
    else{
      switch(SSL_get_error(s->ssl.ssl,written)){
      case SSL_ERROR_NONE:
	return Ok;
      case SSL_ERROR_ZERO_RETURN:
	return Eof;
      case SSL_ERROR_WANT_READ:
      case SSL_ERROR_WANT_WRITE:
      case SSL_ERROR_WANT_CONNECT:
      case SSL_ERROR_WANT_ACCEPT:
      case SSL_ERROR_WANT_X509_LOOKUP:
        if(writeGap>0){
          memmove(&s->file.out_line[0],cp,sizeof(byte)*remaining);
          s->file.out_pos=remaining;
        }
        return Interrupt;		/* report an interrupted transfer */
      case SSL_ERROR_SYSCALL:
        if(writeGap>0){
          memmove(&s->file.out_line[0],cp,sizeof(byte)*remaining);
          s->file.out_pos=remaining;
        }
        return Fail;
      case SSL_ERROR_SSL:
      default:
        return ioErrorMsg(O_IO(s),
			  "Problem %s (%d) in writing to %U",
			  strerror(errno),errno,
                          fileName((ioPo)s));
      }
    }
  }
  s->file.out_pos = 0;
  s->io.status = Ok;
  return Ok;
}

/* acceptSSL implements the server accept part of the SSL protocol.
   The connection must already have been established
 */
static retCode acceptSSL(sslPo s)
{
  do{
    int ret = SSL_accept(s->ssl.ssl);

    if(ret==1)
      return Ok;
    else{
      switch(SSL_get_error(s->ssl.ssl,ret)){
      case SSL_ERROR_NONE:
	return Ok;
      case SSL_ERROR_ZERO_RETURN:
	return Eof;
      case SSL_ERROR_WANT_READ:
      case SSL_ERROR_WANT_WRITE:
	continue;
      case SSL_ERROR_WANT_CONNECT:
      case SSL_ERROR_WANT_ACCEPT:
	continue;
      case SSL_ERROR_WANT_X509_LOOKUP:
	continue;
      case SSL_ERROR_SYSCALL:
	continue;
      case SSL_ERROR_SSL:
      default:
	return Error;
      }
    }
  }
  while(True);
}

/* connectSSL implements the client connect part of the SSL protocol.
   The connection must already have been established
 */
static retCode connectSSL(sslPo s)
{
  do{
    int ret = SSL_connect(s->ssl.ssl);

    if(ret==1)
      return Ok;
    else{
      switch(SSL_get_error(s->ssl.ssl,ret)){
      case SSL_ERROR_NONE:
	return Ok;
      case SSL_ERROR_ZERO_RETURN:
	return Eof;
      case SSL_ERROR_WANT_READ:
      case SSL_ERROR_WANT_WRITE:
	continue;
      case SSL_ERROR_WANT_CONNECT:
      case SSL_ERROR_WANT_ACCEPT:
	continue;
      case SSL_ERROR_WANT_X509_LOOKUP:
	continue;
      case SSL_ERROR_SYSCALL:
	continue;
      case SSL_ERROR_SSL:
      default:
	return Error;
      }
    }
  }
  while(True);
}

/*
 * Note that this accept is executed by an SSL server, and is done
 * after a regular TCP connection is established.
 * The original connection should be kept open, but not read from or written
 * to; until the connection is closed down
 */
retCode s_acceptSSL(processPo P,ptrPo a)
{
  ptrI Conn = deRefI(&a[1]);
  objPo o1 = objV(Conn);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__acceptSSL",eINVAL);
  else{
    sockPo chnnl = O_SOCK(filePtr(Conn));
    ioEncoding encoding = fileEncoding(O_FILE(chnnl));
    int lSock = chnnl->file.fno;
    sslPo sslChnnl = O_SSL(newObject(sslClass,fileName(O_IO(chnnl)),
				     lSock,encoding,ioREAD|ioWRITE));

    switchProcessState(P,wait_io);
      
    retCode ret = acceptSSL(sslChnnl);

    setProcessRunnable(P);

    switch(ret){
    case Ok:{
      ptrI tI = allocFilePtr(&P->proc.heap,O_IO(sslChnnl)); /* return open file descriptor */
      bindVar(P,deRef(&a[2]),tI);
      return Ok;
    }
    default:
      return liberror(P,"__acceptSSL",eIOERROR);
    }
  }  
}

/* Attempt a SSL connection with a server 
   over an existing TCP connection
*/
retCode s_connectSSL(processPo P,ptrPo a)
{
  ptrI Conn = deRefI(&a[1]);
  objPo o1 = objV(Conn);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__acceptSSL",eINVAL);
  else{
    sockPo chnnl = O_SOCK(filePtr(Conn));
    ioEncoding encoding = fileEncoding(O_FILE(chnnl));
    int lSock = chnnl->file.fno;
    sslPo sslChnnl = O_SSL(newObject(sslClass,fileName(O_IO(chnnl)),
				     lSock,encoding,ioREAD|ioWRITE));

    switchProcessState(P,wait_io);

    retCode ret = connectSSL(sslChnnl);

    setProcessRunnable(P);

    switch(ret){
    case Ok: {
      ptrI Ch = allocFilePtr(&P->proc.heap,O_IO(sslChnnl));
      return equal(P,&a[2],&Ch);
    }
    default:
      logMsg(logFile,"Failed to establish SSL connection");
      return liberror(P,"__connectSSL",eCONNECT);
    }
  }
}
