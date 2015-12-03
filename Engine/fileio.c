/* 
   File I/O functions
   (c) 2000-2007 F.G. McCabe

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
#include "hashtable.h"
#include "formioP.h"
#include "encoded.h"             /* pick up the term encoding definitions */
#include "perms.h"

ptrI filePtrClass;				/* The file class structure */

static retCode cellMsg(ioPo f,void *data,long depth,long precision,logical alt);
static void clearFilePointer(ptrI p);

static long flSizeFun(specialClassPo class,objPo o);
static comparison flCompFun(specialClassPo class,objPo o1,objPo o2);
static retCode flOutFun(specialClassPo class,ioPo out,objPo o);
static retCode flScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o);
static objPo flCopyFun(specialClassPo class,objPo dst,objPo src);
static uinteger flHashFun(specialClassPo class,objPo o);

typedef struct {
  ptrI class;				/* == filePtrClass */
  ioPo file;
} fileRec, *fPo;

void initFileIo(void)
{
  installMsgProc('w',cellMsg);	/* extend outMsg to cope with cell structures */
  installMsgProc('L',writeString); /* extend outMsg to cope with string structures */
}

void initFiles(void)
{
  filePtrClass = newSpecialClass("go.stdlib#file",flSizeFun,flCompFun,
				 flOutFun,flCopyFun,flScanFun,flHashFun);
}

static long flSizeFun(specialClassPo class,objPo o)
{
  return CellCount(sizeof(fileRec));
}

static comparison flCompFun(specialClassPo class,objPo o1,objPo o2)
{
  if(o1->class==filePtrClass && o2->class==filePtrClass){
    fPo f1 = (fPo)o1;
    fPo f2 = (fPo)o2;

    if(f1==f2)
      return same;
    else
      return incomparible;
  }
  else
    return incomparible;
}

static retCode flOutFun(specialClassPo class,ioPo out,objPo o)
{
  assert(o->class==filePtrClass);

  fPo f = (fPo)o;

  return outMsg(out,"%U[%x]",fileName(f->file),f->file);
}

static retCode flScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o)
{
  return Ok;
}

static objPo flCopyFun(specialClassPo class,objPo dst,objPo src)
{
  long size = flSizeFun(class,src);
  memmove((void*)dst,(void*)src,size*sizeof(ptrI));

  return (objPo)(((ptrPo)dst)+size);
}

static uinteger flHashFun(specialClassPo class,objPo o)
{
  assert(o->class==filePtrClass);

  return (uinteger)((PTRINT)(((fPo) o)->file));
}

/* check the permission bits on a file */
retCode filePerms(char *file,unsigned long *mode)
{
  struct stat buf;

  if(stat(file, &buf) == -1)
    return Error;
  else{
    if(mode!=NULL)
      *mode = buf.st_mode;
    return Ok;
  }
}

/* return True if a file is really an executable program or not.
   Not checked out for windows at this time
   */
logical executableFile(char *file)
{
  uid_t id = geteuid();
  gid_t grp = getegid();

  struct stat buf;

  if(stat(file,&buf)==-1 || !S_ISREG(buf.st_mode))
    return False;

  if(buf.st_mode&S_IXOTH)	/* anyone can execute this file */
    return True;
  if(buf.st_mode&S_IXGRP){
    if(buf.st_gid==grp)
      return True;
    else{
     int ngroups = getgroups(0,NULL); /* The number of supplementary groups */

     if(ngroups>0){
       gid_t *groups = (gid_t*)calloc(ngroups,sizeof(gid_t));
       int i;

       getgroups(ngroups,groups);

       for(i=0;i<ngroups;i++)
	 if(groups[i]==buf.st_gid){
	   free(groups);
	   return True;
	 }

       free(groups);
     }
    }
  }
  if(buf.st_mode&S_IXUSR)
    return id==buf.st_uid;
  return False;
}


/*
 * Construct a new URL from a root URL and an offset
 */

retCode g_mergeURL(processPo P,ptrPo a)
{
  ptrI Base = deRefI(&a[1]);
  ptrI Url = deRefI(&a[2]);

  if(isvar(Base) || isGroundString(&Base)!=Ok)
    return liberror(P,"__mergeURL",eSTRNEEDD);
  else if(isvar(Url) || isGroundString(&Url)!=Ok)
    return liberror(P,"__mergeURL",eSTRNEEDD);
  else{
    uniChar base[MAX_MSG_LEN];
    uniChar url[MAX_MSG_LEN];
    uniChar ufn[MAX_MSG_LEN];
    uniChar *actual;

    String2Uni(&Base,base,NumberOf(base));
    String2Uni(&Url,url,NumberOf(url));

    actual = mergeURLs(base,url,ufn,NumberOf(ufn));
    
    if(actual==NULL)            /* Illegal URL request */
      return liberror(P,"__mergeURL",eINVAL);
    else{
      ptrI L = allocateString(&P->proc.heap,ufn,uniStrLen(ufn));
    
      return equal(P,&L,&a[3]);
    }
  }
}

/*
 * Check a URL from a base and a potential url
 */
retCode g_checkRoot(processPo P,ptrPo a)
{
  ptrI Base = deRefI(&a[1]);
  ptrI Url = deRefI(&a[2]);

  if(isvar(Base) || isGroundString(&Base)!=Ok)
    return liberror(P,"__checkRoot",eSTRNEEDD);
  else if(isvar(Url) || isGroundString(&Url)!=Ok)
    return liberror(P,"__checkRoot",eSTRNEEDD);
  else{
    uniChar base[MAX_MSG_LEN];
    uniChar url[MAX_MSG_LEN];

    String2Uni(&Base,base,NumberOf(base));
    String2Uni(&Url,url,NumberOf(url));
    
    switch(checkRoot(goSysPath,base,url)){
      case Ok:
        return Ok;
      case Fail:
        return Fail;
      default:
        return liberror(P,"__checkRoot",eINVAL);
    }
  }
}

ioEncoding pickEncoding(ptrI k)
{
  switch(integerVal(intV(k))){
  case 0:
    return rawEncoding;
  case 1:
    return utf16Encoding;
  case 2:
    return utf16EncodingSwap;
  case 3:
    return utf8Encoding;
  default:
    return unknownEncoding;
  }
}

/*
 * Open a URL, used for inputting only
 */
retCode g_openURL(processPo P,ptrPo a)
{
  ptrI Base = deRefI(&a[1]);
  ptrI Url = deRefI(&a[2]);

  if(isvar(Base)||isGroundString(&Base)!=Ok)
    return liberror(P,"__openURL",eSTRNEEDD);
  else if(isvar(Url)||isGroundString(&Url)!=Ok)
    return liberror(P,"__openURL",eSTRNEEDD);
  else if(!isvar(deRefI(&a[3])))
    return liberror(P,"__openURL",eVARNEEDD);
  else if(isvar(deRefI(&a[4])))
    return liberror(P,"__openURL",eINVAL);
  else{
    uniChar base[MAX_MSG_LEN];
    uniChar url[MAX_MSG_LEN];
    uniChar ufn[MAX_MSG_LEN];
    uniChar *actual;
    ioPo file;    

    String2Uni(&Base,base,NumberOf(base));
    String2Uni(&Url,url,NumberOf(url));

    actual = mergeURLs(base,url,ufn,NumberOf(ufn));

    switchProcessState(P,wait_io);
    if((file=openURL(goSysPath,actual,pickEncoding(deRefI(&a[4]))))==NULL){
      setProcessRunnable(P);
      return liberror(P,"__openURL",eNOFILE);
    }
    else{
      ptrI L = allocateString(&P->proc.heap,actual,uniStrLen(actual));
      ptrI t2 = allocFilePtr(file);	/* return open file descriptor */
      
      setProcessRunnable(P);
      if(equal(P,&L,&a[3])!=Ok)
        return Fail;
      
	
      return equal(P,&t2,&a[5]);
    }
  }
}

/*
 * openInFile(fd,fm,encoding)
 * 
 * openInFile a file to read
 */

retCode g_openInFile(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  uniChar ufn[MAX_MSG_LEN];
  ioPo file=NULL;

  if(isvar(t1)||isGroundString(&t1)!=Ok)
    return liberror(P,"__openInFile", eSTRNEEDD);
  else if(isvar(deRefI(&a[2])))
    return liberror(P,"__openInFile",eINVAL);

  String2Uni(&t1,ufn,NumberOf(ufn));

  switchProcessState(P,wait_io);
  file = openInFile(ufn,pickEncoding(deRefI(&a[2])));

  ptrI t2 = allocFilePtr(file);		/* return open file descriptor */
  setProcessRunnable(P);

  if(file==NULL)
    return liberror(P,"__openInFile",eNOFILE);

  return equal(P,&t2,&a[3]);
}

/*
 * openOutFile(fd,fm,enc)
 * 
 * openOutFile a file to write to
 */

retCode g_openOutFile(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  uniChar ufn[MAX_MSG_LEN];
  ioPo file=NULL;

  if(isvar(t1)||isGroundString(&t1)!=Ok)
    return liberror(P,"__openOutFile", eSTRNEEDD);
  else if(isvar(deRefI(&a[2])))
    return liberror(P,"__openOutFile",eINVAL);

  String2Uni(&t1,ufn,NumberOf(ufn));

  switchProcessState(P,wait_io);
  file = newOutFile(ufn,pickEncoding(deRefI(&a[2])));
  setProcessRunnable(P);

  if(file==NULL)
    return liberror(P,"__openOutFile",eNOFILE);

  ptrI t2 = allocFilePtr(file);		/* return open file descriptor */
  return equal(P,&t2,&a[3]);
}

/*
 * openAppenFile(fd,fm)
 * 
 * openAppendFile a file to append to 
 */

retCode g_openAppendFile(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  uniChar ufn[MAX_MSG_LEN];
  ioPo file=NULL;

  if(isvar(t1)||isGroundString(&t1)!=Ok)
    return liberror(P,"__openAppendFile", eSTRNEEDD);
  else if(isvar(deRefI(&a[2])))
    return liberror(P,"__openAppendFile",eINVAL);

  String2Uni(&t1,ufn,NumberOf(ufn));

  switchProcessState(P,wait_io);
  file = openAppendFile(ufn,pickEncoding(deRefI(&a[2])));
  setProcessRunnable(P);

  if(file==NULL)
    return liberror(P,"__openAppendFile",eNOFILE);

  ptrI t2 = allocFilePtr(file);		/* return open file descriptor */
  return equal(P,&t2,&a[3]);
}

/*
 * openAppendIOFile(fd,fm)
 * 
 * openAppendIOFile a file to read/append to 
 */

retCode g_openAppendIOFile(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  uniChar ufn[MAX_MSG_LEN];
  ioPo file=NULL;

  if(isvar(t1)||isGroundString(&t1)!=Ok)
    return liberror(P,"__openAppendIOFile", eSTRNEEDD);
  else if(isvar(deRefI(&a[2])))
    return liberror(P,"__openAppendIOFile",eINVAL);

  String2Uni(&t1,ufn,NumberOf(ufn));

  switchProcessState(P,wait_io);
  file = openInOutAppendFile(ufn,pickEncoding(deRefI(&a[2])));
  setProcessRunnable(P);

  if(file==NULL)
    return liberror(P,"__openAppendIOFile",eNOFILE);

  ptrI t2 = allocFilePtr(file); /* return open file descriptor */
  return equal(P,&t2,&a[3]);
}

/*
 * Open a URL, used for writing to only
 */
retCode g_createURL(processPo P,ptrPo a)
{
  ptrI Base = deRefI(&a[1]);
  ptrI Url = deRefI(&a[2]);

  if(isvar(Base)||isGroundString(&Base)!=Ok)
    return liberror(P,"__createURL",eSTRNEEDD);
  else if(isvar(Url)||isGroundString(&Url)!=Ok)
    return liberror(P,"__createURL",eSTRNEEDD);
  else if(!isvar(deRefI(&a[3])))
    return liberror(P,"__createURL",eVARNEEDD);
  else if(isvar(deRefI(&a[4])))
    return liberror(P,"__createURL",eINVAL);
  else{
    uniChar base[MAX_MSG_LEN];
    uniChar url[MAX_MSG_LEN];
    uniChar ufn[MAX_MSG_LEN];
    uniChar *actual;
    ioPo file;

    String2Uni(&Base,base,NumberOf(base));
    String2Uni(&Url,url,NumberOf(url));

    actual = mergeURLs(base,url,ufn,NumberOf(ufn));

    switchProcessState(P,wait_io);
    if((file=createURL(goSysPath,actual,pickEncoding(deRefI(&a[4]))))==NULL){
      setProcessRunnable(P);
      return liberror(P,"__createURL",eNOFILE);
    }
    else{
      ptrI L = allocateString(&P->proc.heap,actual,uniStrLen(actual));
      equal(P,&L,&a[3]);

      setProcessRunnable(P);
      ptrI t2 = allocFilePtr(file); /* return open file descriptor */
      return equal(P,&t2,&a[4]);
    }
  }
}

// Open a file and return the file pointers associated with the input and the output channels

retCode g_popen(processPo P,ptrPo a)
{
  ptrI Cmd = deRefI(&a[1]);
  ptrI Ags = deRefI(&a[2]);
  ptrI Env = deRefI(&a[3]);
  int argCount = ListLen(Ags);
  int envCount = ListLen(Env);

  if(isvar(Cmd)||isGroundString(&Cmd)!=Ok)
    return liberror(P,"__popen",eSTRNEEDD);
  else if(isvar(Ags) || argCount<0 || isvar(Env) || envCount<0)
    return liberror(P,"__popen",eINSUFARG);
  else{
    long len = StringLen(&Cmd)+1;
    char cmd[3*len];
    ioPo inPipe,outPipe,errPipe;

    String2Utf8(&Cmd,cmd,3*len);
          
    switchProcessState(P,wait_io);
    if(access(cmd,F_OK|R_OK|X_OK)!=0){
      setProcessRunnable(P);
      return liberror(P,"__popen",eNOTFND);
    }
    else if(!executableFile(cmd)){
      setProcessRunnable(P);
      return liberror(P,"__popen",eNOPERM);
    }
    else{
      char **argv = (char **)calloc(argCount + 2, sizeof(char *));
      char **envp = (char **)calloc(envCount + 1, sizeof(char *));
      long i;

      argv[0] = cmd;
      
      for(i=1;IsList(Ags);i++,Ags = deRefI(listTail(objV(Ags)))){
        ptrPo arg = listHead(objV(Ags));

        if(isGroundString(arg)!=Ok){
	  setProcessRunnable(P);
          return liberror(P,"__popen",eINSUFARG);
	}
        else{
          long al = StringLen(arg);
          char astr[al*3];
          
          String2Utf8(arg,astr,al*3);
          argv[i]=strdup(astr);
        }
      }
      
      argv[i]=NULL;
      
      for(i=0;IsList(Env);i++,Env = deRefI(listTail(objV(Env)))){
        ptrPo l = listHead(objV(Env));
        ptrI envKey;
        ptrI envVal;
        if(!IsBinOp(l,commaClass,&envKey,&envVal)
           || !IsSymb(envKey)
           || isGroundString(&envVal)!=Ok){
	  setProcessRunnable(P);
          return liberror(P,"__popen",eINVAL);
	}
        else{
          uniChar *key = SymVal(symbV(envKey));
          long al = StringLen(&envVal)+uniStrLen(key)+4;
          uniChar buffer[al];
          char estr[al*3];

          strMsg(buffer,al,"%U = %L",key,&envVal);
          
          _utf(buffer,(unsigned char*)estr,al*3);       /* convert to utf */

          envp[i]=strdup(estr);
        }
      }
      
      envp[i]=NULL;

      switch(openPipe(argv[0],argv,envp,&inPipe,&outPipe,&errPipe,pickEncoding(deRefI(&a[7])))){
      case Ok:{
        ptrI in = allocFilePtr(inPipe); /* return open file descriptor */
        rootPo root = gcAddRoot(&P->proc.heap,&in);
        ptrI out = allocFilePtr(outPipe);
        ptrI errIn;
        retCode ret;
      
	setProcessRunnable(P);
        gcAddRoot(&P->proc.heap,&out);
      
        errIn = allocFilePtr(errPipe);
      
        gcRemoveRoot(&P->proc.heap,root);
      
        ret = equal(P,&in,&a[4]);
      
        if(ret==Ok)
          ret = equal(P,&out,&a[5]);
      
        if(ret==Ok)
          ret = equal(P,&errIn,&a[6]);
      
        return ret;
      }
      default:{
        int i=0;
        for(i=0;i<argCount;i++)
	  free(argv[i]);		/* release the strings we allocated */
        for(i=0;i<envCount;i++)
	  free(envp[i]);		/* release the strings we allocated */

	setProcessRunnable(P);
        return liberror(P,"__popen",eIOERROR);
      }
      }
    }
  }
}

/* Set the encoding on a file */
retCode g_setfileencoding(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);
  ptrI e = deRefI(&a[2]);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__setfileencoding",eINVAL);
  else if(!IsSymb(e))
    return liberror(P,"__setfileencoding",eINVAL);
  else{
    ioPo file = filePtr(t1);
    
    switchProcessState(P,wait_io);
    setEncoding(O_FILE(file),pickEncoding(e));
    setProcessRunnable(P);
    return Ok;
  }
}


/*
 * fclose()
 * 
 * close file or pipe 
 */

retCode g_fclose(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__close",eINVAL);
  else{
    ioPo file = filePtr(t1);

    if(file!=NULL){
      clearFilePointer(t1);
      switchProcessState(P,wait_io);
      retCode ret = closeFile(file);
      setProcessRunnable(P);
      return ret;
    }
    else
      return Ok;
  }
}

retCode g_stdfile(processPo P,ptrPo a)
{
  long fno = integerVal(intV(deRefI(&a[1])));
  ptrI res;

  switch(fno){
  case 0:
    res=allocFilePtr(OpenStdin());	/* return stdin file descriptor */
    break;
  case 1:
    res=allocFilePtr(OpenStdout());	/* return stdout file descriptor */
    break;
  case 2:
    res=allocFilePtr(OpenStderr());	/* return stderr file descriptor */
    break;
  default:
    return liberror(P,"__stdfile",eNOPERM);
  }
  return equal(P,&res,&a[2]);
}

/*
 * eof()
 * returns "true" if the file associated with process is at the end of file
 * This function reads character forward in order to position the EOF flag.
 * the character is then un-read by ungetc()
 */
retCode g_eof(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__eof",eINVAL);
  else{
    ioPo file = filePtr(t1);

    if(isReadingFile(file)!=Ok)
      return liberror(P,"__eof",eNOPERM);

    switchProcessState(P,wait_io);

    retCode ret = isFileAtEof(file);

    setProcessRunnable(P);

    if(ret==Eof)
      return Ok;
    else
      return Fail;
  }
}

/*
 * ready()
 * returns "true" if the file is ready -- i.e., an eof test would not suspend
 */
retCode g_ready(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__ready",eINVAL);
  else{
    ioPo file = filePtr(t1);

    if(isReadingFile(file)!=Ok)
      return liberror(P,"__ready",eNOPERM);

    switchProcessState(P,wait_io);

    retCode ret = isInReady(file);

    setProcessRunnable(P);

    if(ret==Ok)
      return Ok;
    else
      return Fail;
  }
}

/*
 * position()
 * returns the position of the file 
 */
retCode g_fposition(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__fposition",eINVAL);
  else{
    ioPo file = filePtr(t1);

    if(isReadingFile(file)==Ok){
      ptrI pos = allocateInteger(&P->proc.heap,inCPos(file));
      return equal(P,&a[2],&pos);
    }
    else if(isWritingFile(file)==Ok){
      ptrI pos = allocateInteger(&P->proc.heap,outCPos(file));
      return equal(P,&a[2],&pos);
    }
    else
      return liberror(P,"__fposition",eINVAL);
  }
}


/*
 * seek(p)
 * resets the position of the file 
 */
retCode g_fseek(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);
  ptrI t2 = deRefI(&a[2]);
  objPo o2 = objV(t2);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__fseek",eINVAL);
  else if(isvar(t2))
    return liberror(P,"__fseek",eINSUFARG);
  else if(!isInteger(o2))
    return liberror(P,"__fseek",eINVAL);
  else{
    ioPo file = filePtr(t1);
    integer pos = integerVal((integerPo)o2);

  again:
    switchProcessState(P,wait_io);
    retCode ret = ioSeek(file,pos);
    setProcessRunnable(P);
    
    switch(ret){
    case Ok:
      return Ok;
    case Fail:
      return Fail;
    case Interrupt:
      if(checkForPause(P))
	goto again;
      else
	return liberror(P,"__fseek",eINTRUPT);
    default:
      return liberror(P,"__fseek",eIOERROR);
    }
  }
}

/*
 * inbytes(file,count)
 * 
 * get a block of bytes from file attached to process, returned as a list of numbers
 * Complicated 'cos it allows suspension during the read of a block of bytes
 */

retCode g_inbytes(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);
  objPo t2 = objV(deRefI(&a[2]));
  long count;

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__inbytes",eINVAL);
  else if(!isInteger(t2) || (count=integerVal((integerPo)t2))<=0)
      return liberror(P,"__inbytes",eINTNEEDD);
  else{
    ioPo file = filePtr(t1);
    long ix=0;
    byte buff[MAX_SYMB_LEN];
    byte *buffer = (count>NumberOf(buff)?
		    (byte*)malloc(sizeof(byte)*count):
		    buff);
    long remaining = count;

    if(isReadingFile(file)!=Ok)
      return liberror(P,"__inbytes",eNOPERM);

    while(remaining>0){
      long bytesRead;
    again:				/* come back in case of interrupt */
      switchProcessState(P,wait_io);
      retCode ret = inBytes(file,&buffer[ix],remaining,&bytesRead);
      setProcessRunnable(P);
      
      switch(ret){ 
      case Eof:{
	if(bytesRead==0){
	  if(ix==0)
	    return liberror(P,"__inbytes",eEOF);
	  else{
	    ix+=bytesRead;
	    remaining-=bytesRead;
	    break;
	  }
	}
	else{
	  ix+=bytesRead;
	  remaining-=bytesRead;
	  break;
	}
      }
      case Ok:				/* We are able to stuff successfully */
	ix+=bytesRead;
	remaining-=bytesRead;
	continue;
      case Interrupt:
	if(checkForPause(P))
	  goto again;
	else{
	  return liberror(P,"__inbytes",eINTRUPT);
	}
      default:
	logMsg(logFile,"%U",ioMessage(O_IO(file)));
	return liberror(P,"__inbytes",eIOERROR);
      }
    }

      /* grab the result */
    ptrI reslt = allocateByteList(&P->proc.heap,buffer,ix);

    if(buffer!=buff)
      free(buffer);
      
    return equal(P,&a[3],&reslt);
  }
}

/*
 * inchars(file,count)
 * 
 * get a block of characters from file attached to process, returned as a string
 */

retCode g_inchars(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);
  objPo t2 = objV(deRefI(&a[2]));
  long count;

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__inchars",eINVAL);
  else if(!isInteger(t2) || (count=integerVal((integerPo)t2))<=0)
      return liberror(P,"__inchars",eINTNEEDD);

  else{
    ioPo file = filePtr(t1);
    long ix=0;
    uniChar buff[MAX_SYMB_LEN];
    uniChar *buffer = (count>NumberOf(buff)?
		       (uniChar*)malloc(sizeof(uniChar)*count):
		       buff);

    if(isReadingFile(file)!=Ok)
      return liberror(P,"__inchars",eNOPERM);

    for(;ix<count;ix++){
      uniChar ch;
    again:				/* come back in case of interrupt */
      switchProcessState(P,wait_io);

      switch(inChar(file,&ch)){	        /* Attempt to read a character */
      case Eof:
	if(ix==0){			/* attempt to read past EOF */
	  setProcessRunnable(P);
	  return liberror(P,"__inchars",eEOF);
	}
	else{
	  count = ix;
	  break;			/* results in an early exit */
	}
      case Ok:		/* We are able to read uniChar successfully */
	buffer[ix++]=ch;
	continue;
      case Interrupt:
	if(checkForPause(P))
	  goto again;
	else{
	  setProcessRunnable(P);
	  return liberror(P,"__inchars",eINTRUPT);
	}
      default:
	setProcessRunnable(P);
	return liberror(P,"__inchars",eIOERROR);
      }
    }

    setProcessRunnable(P);

      /* grab the result */
    ptrI reslt = allocateString(&P->proc.heap,buffer,count);

    if(buffer!=buff)
      free(buffer);
      
    return equal(P,&a[3],&reslt);
  }
}

/*
 * inchar(count)
 * 
 * get a single character from file attached to process, 
 * returned as a single character
 */

retCode g_inchar(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__inchar",eINVAL);
  else{
    ioPo file = filePtr(t1);
    uniChar ch;

    if(isReadingFile(file)!=Ok)
      return liberror(P,"__inchar",eNOPERM);

  again:
    switchProcessState(P,wait_io);
    retCode ret = inChar(file,&ch);
    ptrI Ch = newChar(ch);
    setProcessRunnable(P);

    switch(ret){
    case Ok:
      return funResult(P,a,2,Ch);
    case Eof:
      setProcessRunnable(P);
      return liberror(P,"__inchar",eEOF); /* eof error */
    case Interrupt:
      if(checkForPause(P))
	goto again;			/* try again after GC is finished */
      else{
	setProcessRunnable(P);
	return liberror(P,"__inchar",eINTRUPT);
      }
    default:
      setProcessRunnable(P);
      return liberror(P,"__inchar",eINVAL); /* other error */
    }
  }
}

/*
 * inbyte
 * 
 * get a single character from file attached to process, 
 * returned as a single integer
 */

retCode g_inbyte(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__inbyte",eINVAL);
  else{
    ioPo file = filePtr(t1);
    byte ch;

    if(isReadingFile(file)!=Ok)
      return liberror(P,"__inbyte",eNOPERM);
  again:
    switchProcessState(P,wait_io);
    retCode ret = inByte(file,&ch);
    setProcessRunnable(P);

    switch(ret){
    case Ok:{
      ptrI ans = allocateInteger(&P->proc.heap,ch);

      return equal(P,&a[2],&ans);
    }
    case Interrupt:
      if(checkForPause(P))
	goto again;			/* try again */
      else
	return liberror(P,"__inbyte",eINTRUPT);
    case Eof:
      return liberror(P,"__inbyte",eEOF); /* eof error */
    default:
      return liberror(P,"__inbyte",eIOERROR); /* other error */
    }
  }
}

/*
 * inline(file,match)
 * 
 * get a single line from a file, returned as a string
 * get all characters until either EOF or the match string is found
 */
retCode g_inline(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);
  ptrI t2 = deRefI(&a[2]);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__inline",eINVAL);
  else if(isvar(t2))
    return liberror(P,"__inline",eINSUFARG);
  else if(isGroundString(&t2)!=Ok)
    return liberror(P,"__inline",eSTRNEEDD);
  else{
    ioPo file = filePtr(t1);
    uniChar chr;
    integer tlen = StringLen(&t2);
    uniChar term[tlen+1];
    uniChar *trm = &term[0];

    if(isReadingFile(file)!=Ok)
      return liberror(P,"__inline",eNOPERM);

    if(String2Uni(&a[2],term,tlen+1)!=Ok)
      return liberror(P,"__inline",eINVAL);

    ioPo str = openOutStr(utf16Encoding);

    while(True){
      switchProcessState(P,wait_io);
      retCode ret = inChar(file,&chr);
      setProcessRunnable(P);

      switch(ret){
      case Eof:
	if(emptyOutStr(O_STRING(str))==Ok){
	  closeFile(str);
	  return liberror(P,"__inline",eEOF);		/* cant read past the end of the file */
	}
	else{
          long len;
          uniChar *buff=getStrText(O_STRING(str),&len);
          ptrI reslt = allocateString(&P->proc.heap,buff,len); /* grab the result */
      
          closeFile(str);		/* we are done reading */
          return equal(P,&a[3],&reslt);
        }
      case Ok:
	if(*trm==chr)
	  trm++;			/* move on termination string */

        if(*trm=='\0'){                 // Read at least one character
          ptrI RR = kvoid;              // This is clumsy, but necessary
          rootPo root = gcAddRoot(&P->proc.heap,&RR);
          retCode ret = closeOutString(str,&P->proc.heap,&RR);
          if(ret==Ok)
            ret = equal(P,&RR,&a[3]);
          gcRemoveRoot(&P->proc.heap,root);
	  return ret;    
        }
	else if(chr==EOF_CHAR && emptyOutStr(O_STRING(str))!=Ok){
          long len;
          uniChar *buff=getStrText(O_STRING(str),&len);
          ptrI reslt = allocateString(&P->proc.heap,buff,len); /* grab the result */
      
          closeFile(str);		/* we are done reading */

          return equal(P,&a[3],&reslt);
	}
	else{
	  trm = &term[0];		// Reset the terminating string
	  outChar(str,chr);
	}
	continue;
      case Interrupt:
	if(checkForPause(P))
	  continue;			/* We were interrupted for a GC */
	else
	  return liberror(P,"__inline",eINTRUPT);
      default:
	return liberror(P,"__inline",eIOERROR);
      }
    }
  }
}

/*
 * intext(file,search)
 * 
 * return all available text, until either of EOF is true or one of the characters in search is found
 */
retCode g_intext(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);
  ptrI t2 = deRefI(&a[2]);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__intext",eINVAL);
  else if(isvar(t2))
    return liberror(P,"__intext",eINSUFARG);
  else if(isGroundString(&t2)!=Ok)
    return liberror(P,"__intext",eSTRNEEDD);
  else{
    ioPo file = filePtr(t1);
    uniChar chr;
    integer tlen = StringLen(&t2);
    uniChar term[tlen+1];

    if(isReadingFile(file)!=Ok)
      return liberror(P,"__intext",eNOPERM);

    if(String2Uni(&a[2],term,tlen+1)!=Ok)
      return liberror(P,"__intext",eINVAL);

    ioPo str = openOutStr(utf16Encoding);

    while(True){
      switchProcessState(P,wait_io);
      retCode ret = inChar(file,&chr);
      setProcessRunnable(P);

      switch(ret){
      case Eof:
	if(emptyOutStr(O_STRING(str))==Ok){
	  closeFile(str);
	  return liberror(P,"__intext",eEOF);		/* cant read past the end of the file */
	}
	else{
          long len;
          uniChar *buff=getStrText(O_STRING(str),&len);
          ptrI reslt = allocateString(&P->proc.heap,buff,len); /* grab the result */
      
          closeFile(str);		/* we are done reading */

          return equal(P,&a[3],&reslt);
        }
      case Ok:      
      	if(uniSearch(term,tlen,chr)!=NULL){ /* have we found a terminating byte? */
          ptrI RR = kvoid;              // This is clumsy, but necessary
          rootPo root = gcAddRoot(&P->proc.heap,&RR);
          retCode ret = closeOutString(str,&P->proc.heap,&RR);
          if(ret==Ok)
            ret = equal(P,&RR,&a[3]);
          gcRemoveRoot(&P->proc.heap,root);
	  return ret;    
	}
	else if(chr==EOF_CHAR && emptyOutStr(O_STRING(str))!=Ok){
          long len;
          uniChar *buff=getStrText(O_STRING(str),&len);
          ptrI reslt = allocateString(&P->proc.heap,buff,len); /* grab the result */
      
          closeFile(str);		/* we are done reading */

          return equal(P,&a[3],&reslt);
	}
	else
	  outChar(str,chr);
	continue;
      case Interrupt:
	if(checkForPause(P))
	  continue;			/* We were interrupted for a GC */
	else
	  return liberror(P,"__intext",eINTRUPT);
      default:
	return liberror(P,"__intext",eIOERROR);
      }
    }
  }
}

/*
 * outch(file,ch)
 * 
 * write a single character on file controlled by process
 */

retCode g_outch(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__outch",eINVAL);
  else{
    ioPo file = filePtr(t1);
    ptrI tc = deRefI(&a[2]);

    if(isWritingFile(file)!=Ok)
      return liberror(P,"__outch",eNOPERM);
    else if(isvar(deRefI(&a[1])) || !IsChar(tc))
      return liberror(P,"__outch",eCHRNEEDD);
    else{
    again:
      switchProcessState(P,wait_io);
      retCode ret = outChar(file,CharVal(charV(tc)));
      setProcessRunnable(P);

      switch(ret){
      case Ok:
	if(CharVal(charV(tc))=='\n')
	  flushFile(file);
	return Ok;
      case Interrupt:
	if(checkForPause(P))
	  goto again;
	else
	  return liberror(P,"__outch",eINTRUPT);
      default:
	return liberror(P,"__outch",eIOERROR);
      }
    }
  }
}

/*
 * outbyte(file,ch)
 * 
 * write a single byte on file controlled by process
 */

retCode g_outbyte(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__outbyte",eINVAL);
  else{
    ioPo file = filePtr(t1);
    objPo t2 = objV(deRefI(&a[2]));

    if(isWritingFile(file)!=Ok)
      return liberror(P,"__outbyte",eNOPERM);
    else if(isvar(deRefI(&a[1])) || !isInteger(t2))
      return liberror(P,"__outbyte",eINTNEEDD);
    else{
    again:
      switchProcessState(P,wait_io);
      retCode ret = outChar(file,integerVal((integerPo)t2));
      setProcessRunnable(P);

      switch(ret){
      case Ok:
	return Ok;
      case Interrupt:
	if(checkForPause(P))
	  goto again;
	else
	  return liberror(P,"__outbyte",eINTRUPT);
      default:
	return liberror(P,"__outbyte",eIOERROR);
      }
    }
  }
}

/*
 * outtext(file,str)
 * 
 * write a string on file
 */

retCode g_outtext(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__outtext",eINVAL);
  else{
    ioPo file = filePtr(t1);
    
    if(isWritingFile(file)!=Ok)
      return liberror(P,"__outtext",eNOPERM);
    else if(isGroundString(&a[2])!=Ok)
      return liberror(P,"__outtext",eSTRNEEDD);
    else{
      switchProcessState(P,wait_io);
      retCode ret = outMsg(file,"%L",&a[2]);
      setProcessRunnable(P);

      switch(ret){
      case Ok:
        return Ok;
      case Interrupt:			/* should never happen */
	return liberror(P,"__outtext",eINTRUPT);
      default:
	return liberror(P,"__outtext",eIOERROR);
      }
    }
  }
}

/*
 * outsym(file,str)
 * 
 * write a symbol to a file
 */

retCode g_outsym(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);
  ptrI S = deRefI(&a[2]);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__outsym",eINVAL);
  else if(!IsSymb(S))
    return liberror(P,"__outsym",eINVAL);
  else{
    ioPo file = filePtr(t1);
    
    if(isWritingFile(file)!=Ok)
      return liberror(P,"__outsym",eNOPERM);
    else if(isGroundString(&a[2])!=Ok)
      return liberror(P,"__outsym",eSTRNEEDD);
    else{
      switchProcessState(P,wait_io);
      retCode ret = outMsg(file,"%U",SymVal(symbV(S)));
      setProcessRunnable(P);

      switch(ret){
      case Ok:
        return Ok;
      case Interrupt:			/* should never happen */
	return liberror(P,"__outsym",eINTRUPT);
      default:
	return liberror(P,"__outsym",eIOERROR);
      }
    }
  }
}

/* Write a term in default notation onto a file channel */
retCode g_outterm(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__outterm",eINVAL);
  else{
    ioPo file = filePtr(t1);

    if(isWritingFile(file)!=Ok)
      return liberror(P,"__outterm",eNOPERM);

    switchProcessState(P,wait_io);
    retCode ret = outCell(file,&a[2],INT_MAX/4,0,False);
    setProcessRunnable(P);

    switch(ret){
    case Ok:
      return Ok;
    case Interrupt:			/* should never happen */
      return liberror(P,"__outterm",eINTRUPT);
    default:
      return liberror(P,"__outterm",eIOERROR);
    }
  }
}

/* Post a message in the log */
retCode g_logmsg(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);

  if(isGroundString(&t1)!=Ok)
    return liberror(P,"__logmsg",eSTRNEEDD);
  else{
    long buffLen = (StringLen(&t1)+1);
    uniChar buff[buffLen];
    
    String2Uni(&t1,buff,buffLen);

    switchProcessState(P,wait_io);
    retCode ret = logMsg(logFile,"%U\n%_",buff);
    setProcessRunnable(P);

    switch(ret){
    case Ok:
      return Ok;
    case Interrupt:			/* should never happen */
      return liberror(P,"__logmsg",eINTRUPT);
    default:
      return liberror(P,"__logmsg",eIOERROR);
    }
    return Ok;
  }
}

/*
 * flush(file)
 * flush remaining buffered output of an output stream. 
 */

retCode g_flush(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__flush",eINVAL);
  else{
    ioPo file = filePtr(t1);
    retCode ret;

    if(isFileOpen(file)!=Ok)
      return liberror(P,"__flush",eNOPERM);

    if(isWritingFile(file)!=Ok)
      return liberror(P,"__flush",eIOERROR);

    switchProcessState(P,wait_io);

    while((ret=flushFile(file))==Fail)
      ;
    setProcessRunnable(P);

    if(ret==Error)
      return liberror(P,"__flush",eIOERROR);
    else
      return Ok;
  }
}

retCode g_flushall(processPo P,ptrPo a)
{
  switchProcessState(P,wait_io);
  flushOut();
  setProcessRunnable(P);
  return Ok;
}

/* This must be installed into outMsg */
static retCode cellMsg(ioPo f,void *data,long depth,long precision,logical alt)
{
  ptrPo ptr = (ptrPo)data;
        
  if(ptr!=NULL){
    if(precision<=0)
      precision=32767;
    outCell(f,ptr,depth,precision,alt);
  }
  else
    outStr(f,"(NULL)");
  return Ok;
}

ptrI allocFilePtr(ioPo file)
{
  fPo f = (fPo)allocateSpecial(&globalHeap,filePtrClass);

  f->file = file;
  return objP(f);
}

ioPo filePtr(ptrI p)
{
  objPo o = objV(p);
  assert(hasClass(o,filePtrClass));
  return ((fPo)o)->file;
}

static void clearFilePointer(ptrI p)
{
  objPo o = objV(p);
  assert(hasClass(o,filePtrClass));

  ((fPo)o)->file = NULL;
}


