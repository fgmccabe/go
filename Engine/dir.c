/* 
   Directory and file handling functions
   (c) 1994-2002 Imperial College and F.G. McCabe

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

  $Id: dir.c,v 1.4 2004/07/16 15:30:05 fmccabe Exp $
  $Log: dir.c,v $
  Revision 1.4  2004/07/16 15:30:05  fmccabe
  Adjusted the compiler some

  Revision 1.3  2004/06/30 04:28:12  fmccabe
  Some bug fixes, new grammar operator and new term operator

  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system

*/ 

#include "config.h"		/* pick up standard configuration header */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
 
#include <unistd.h>
#include <dirent.h>
#include <pwd.h>
#include <sys/types.h>

#include <ctype.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <signal.h>

#include "go.h"
#include "symbols.h"
#include "perms.h"
#include "clock.h"

// Report current directory
retCode g_cwd(processPo P,ptrPo a)
{
  switchProcessState(P,wait_io);
  char *cwd = getcwd(NULL,0);           /* compute current working directory */
  setProcessRunnable(P);

  if(cwd!=NULL){
    ptrI CWD = allocateCString(&P->proc.heap,cwd);

    free(cwd);
    return equal(P,&CWD,&a[1]);
  }
  else
    return Error;
}

// Change current directory
retCode g_cd(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);

  if(isvar(t1)||isGroundString(&t1)!=Ok)
    return liberror(P,"__cd", eSTRNEEDD);
  else{
    uniChar ufn[MAX_MSG_LEN];
    char fBuff[MAX_MSG_LEN];

    String2Uni(&t1,ufn,NumberOf(ufn));
    _utf(ufn,(unsigned char*)fBuff,NumberOf(fBuff)); /* Convert string to regular chars */
    
tryAgain:
    switchProcessState(P,wait_io);
    
    if(chdir(fBuff) != -1){
      setProcessRunnable(P);
      return Ok;
    }
    else{
      setProcessRunnable(P);

      switch(errno){
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        return liberror(P,"__cd",eNOPERM);
      case EBUSY:
        return liberror(P,"__cd",eFAIL);
      case ENOTDIR:
        return liberror(P,"__cd",eNOTDIR);
      case ENOENT:
      case ELOOP:
        return liberror(P,"__cd",eINVAL);
      default:
        return liberror(P,"__cd",eIOERROR);
      }
    }
  }
}

// Delete a file

retCode g_rm(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);

  if(isvar(t1)||isGroundString(&t1)!=Ok)
    return liberror(P,"__rm", eSTRNEEDD);
  else{
    uniChar ufn[MAX_MSG_LEN];
    char fBuff[MAX_MSG_LEN];

    String2Uni(&t1,ufn,NumberOf(ufn));
    _utf(ufn,(unsigned char*)fBuff,NumberOf(fBuff)); /* Convert string to regular chars */
    
tryAgain:
    switchProcessState(P,wait_io);
    if(unlink(fBuff) != -1){
      setProcessRunnable(P);
      return Ok;
    }
    else{
      setProcessRunnable(P);
      switch(errno){
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        return liberror(P,"__rm",eNOPERM);
      case EBUSY:
        return liberror(P,"__rm",eFAIL);
      case ENOENT:
      default:
        return liberror(P,"__rm",eIOERROR);
      }
    }
  }
}

// Rename file

retCode g_mv(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  ptrI t2 = deRefI(&a[2]);

  if(isvar(t1)||isGroundString(&t1)!=Ok||isvar(t2)||isGroundString(&t2)!=Ok)
    return liberror(P,"__mv", eSTRNEEDD);
  else{
    char fn1[MAX_MSG_LEN],fn2[MAX_MSG_LEN];
    uniChar ufn[MAX_MSG_LEN];

    String2Uni(&t1,ufn,NumberOf(ufn));
    _utf(ufn,(unsigned char*)fn1,NumberOf(fn1));                    /* Convert file names to regular chars */
    String2Uni(&t2,ufn,NumberOf(ufn));
    _utf(ufn,(unsigned char*)fn2,NumberOf(fn2));

tryAgain:
    switchProcessState(P,wait_io);
    if(rename(fn1,fn2)!=-1){
      setProcessRunnable(P);
      return Ok;
    }
    else{
      setProcessRunnable(P);
      switch(errno){
      case EINTR:
        goto tryAgain;
      case EACCES:
        return liberror(P,"__mv",eNOPERM);
      case EBUSY:
      case ENOENT:
        return liberror(P,"__mv",eFAIL);
      default:
        return liberror(P,"__mv",eIOERROR);
      }
    }
  }
}

/*
 * mkdir : create a new directory
 */

retCode g_mkdir(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  ptrI t2 = deRefI(&a[2]);

  if(isvar(t1)||isGroundString(&t1)!=Ok)
    return liberror(P,"__mkdir", eSTRNEEDD);
  else{
    uniChar ufn[MAX_MSG_LEN];
    char fBuff[MAX_MSG_LEN];
    unsigned int acmode = (unsigned int)integerVal(intV(t2));

    String2Uni(&t1,ufn,NumberOf(ufn));
    _utf(ufn,(unsigned char*)fBuff,NumberOf(fBuff)); /* Convert string to regular chars */

  tryAgain:
    switchProcessState(P,wait_io);
    if(mkdir(fBuff,acmode)==-1){
      setProcessRunnable(P);
      switch(errno){
      case EINTR:
	goto tryAgain;
      case EEXIST:
        return liberror(P,"__mkdir", eFAIL);
      default:
        return liberror(P,"__mkdir", eINVAL);
      }
    }
    else{
      setProcessRunnable(P);
      return Ok;
    }
  }
}


/*
 * rmdir : remove a directory
 */
retCode g_rmdir(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);

  if(isvar(t1)||isGroundString(&t1)!=Ok)
    return liberror(P,"__rmdir", eSTRNEEDD);
  else{
    uniChar ufn[MAX_MSG_LEN];
    char fBuff[MAX_MSG_LEN];

    String2Uni(&t1,ufn,NumberOf(ufn));
    _utf(ufn,(unsigned char*)fBuff,NumberOf(fBuff));                    /* Convert string to regular chars */
    
tryAgain:
    switchProcessState(P,wait_io);
    if(rmdir(fBuff)==0){
      setProcessRunnable(P);
      return Ok;
    }
    else{
      setProcessRunnable(P);
      switch(errno){
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        return liberror(P,"__rmdir",eNOPERM);
      case EBUSY:
      case ENOENT:
        return liberror(P,"__rmdir",eFAIL);
      default:
        return liberror(P,"__rmdir",eIOERROR);
      }
    }
  }
}

/*
 * chmod : set permissions on a file or directory
 */

retCode g_chmod(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  ptrI t2 = deRefI(&a[2]);

  if(isvar(t1)||isGroundString(&t1)!=Ok)
    return liberror(P,"__chmod", eSTRNEEDD);
  else{
    uniChar ufn[MAX_MSG_LEN];
    char fBuff[MAX_MSG_LEN];
    unsigned int acmode = (unsigned int)integerVal(intV(t2));

    String2Uni(&t1,ufn,NumberOf(ufn));
    _utf(ufn,(unsigned char*)fBuff,NumberOf(fBuff)); /* Convert string to regular chars */

  tryAgain:
    switchProcessState(P,wait_io);
    if(chmod(fBuff,acmode) == -1){
      setProcessRunnable(P);
      switch(errno){
      case EINTR:
	goto tryAgain;		/* A mega hack */
      case EACCES:
        return liberror(P,"__chmod",eNOPERM);
      case EPERM:
        return liberror(P,"__chmod",eNOPERM);
      default:
        return liberror(P,"__chmod",eNOPERM);
      }
    }
    setProcessRunnable(P);
  }
  return Ok;
}

/*
 * fmode : gets permissions of a file or directory as a mode string
 */
 
retCode g_fmode(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);

  if(isvar(t1)||isGroundString(&t1)!=Ok)
    return liberror(P,"__fmode", eSTRNEEDD);
  else if(!isvar(deRefI(&a[2])))
    return liberror(P,"__fmode",eVARNEEDD);
  else{
    uniChar ufn[MAX_MSG_LEN];
    char fBuff[MAX_MSG_LEN];
    struct stat buf;

    String2Uni(&t1,ufn,NumberOf(ufn));
    _utf(ufn,(unsigned char*)fBuff,NumberOf(fBuff)); /* Convert string to regular chars */

  tryAgain:
    switchProcessState(P,wait_io);
    if(stat(fBuff, &buf) == -1){
      setProcessRunnable(P);

      switch(errno){
      case EINTR:
	goto tryAgain;
      case ENOTDIR:
	return liberror(P,"__fmode",eNOFILE);
      case ENAMETOOLONG:
	return liberror(P,"__fmode",eINVAL);
      case ENOENT:
	return liberror(P,"__fmode",eNOTFND);
      case EACCES:
	return liberror(P,"__fmode",eNOPERM);
      case ELOOP:
	return liberror(P,"__fmode",eINVAL);
      case EIO:
	return liberror(P,"__fmode",eIOERROR);
      case EFAULT:
	return liberror(P,"__fmode",eINVAL);
      default:
	return liberror(P,"__fmode",eNOTFND);
      }
    }
    else{
      ptrI modes = allocateInteger(&P->proc.heap,buf.st_mode);
      
      setProcessRunnable(P);
      return equal(P,&modes,&a[2]);
    }
  }
}

/*
 * file_type check out the type of the file
 */

retCode g_file_type(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);

  if(isvar(t1)||isGroundString(&t1)!=Ok)
    return liberror(P,"__file_type", eSTRNEEDD);
  else if(!isvar(deRefI(&a[2])))
    return liberror(P,"__file_type",eVARNEEDD);
  else{
    uniChar ufn[MAX_MSG_LEN];
    char fBuff[MAX_MSG_LEN];
    struct stat buf;

    String2Uni(&t1,ufn,NumberOf(ufn));

    _utf(ufn,(unsigned char*)fBuff,NumberOf(fBuff));
    
  tryAgain:
    switchProcessState(P,wait_io);
    if(stat(fBuff, &buf) == -1){
      setProcessRunnable(P);

      switch(errno){
      case EINTR:
	goto tryAgain;
      case ENOTDIR:
	return liberror(P,"__file_type",eNOFILE);
      case ENAMETOOLONG:
	return liberror(P,"__file_type",eINVAL);
      case ENOENT:
	return liberror(P,"__file_type",eNOTFND);
      case EACCES:
	return liberror(P,"__file_type",eNOPERM);
      case ELOOP:
	return liberror(P,"__file_type",eINVAL);
      case EIO:
	return liberror(P,"__file_type",eIOERROR);
      case EFAULT:
	return liberror(P,"__file_type",eINVAL);
      default:
	return liberror(P,"__file_type",eNOTFND);
      }
    }
    
    setProcessRunnable(P);
    
    if(S_ISFIFO(buf.st_mode))
      return equal(P,&kfifo,&a[2]);
    else if(S_ISCHR(buf.st_mode))
      return equal(P,&kcharfile,&a[2]);
    else if(S_ISDIR(buf.st_mode))
      return equal(P,&kdir,&a[2]);
    else if(S_ISBLK(buf.st_mode))
      return equal(P,&kblock,&a[2]);
    else if(S_ISREG(buf.st_mode))
      return equal(P,&kplain,&a[2]);
    else if(S_ISLNK(buf.st_mode))
      return equal(P,&ksymlink,&a[2]);
    else
      return liberror(P,"__file_type",eINVAL);
  }
}


/*
 * ffile(file)
 * succeeds if file is present, false otherwise 
 */

retCode g_file_present(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  uniChar ufn[MAX_MSG_LEN];

  if(isvar(t1))
    return liberror(P,"__file_present", eSTRNEEDD);
  else if(isGroundString(&t1)!=Ok)
    return liberror(P,"__file_present", eSTRNEEDD);

  String2Uni(&t1,ufn,NumberOf(ufn));

  switchProcessState(P,wait_io);
  logical present = filePresent(ufn);
  setProcessRunnable(P);

  if(present)
    return Ok;
  else
    return Fail;
}

/*
 * __file_size() - return file size 
 */

retCode g_file_size(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);

  if(isvar(t1)||isGroundString(&t1)!=Ok)
    return liberror(P,"__file_size", eSTRNEEDD);
  else if(!isvar(deRefI(&a[2])))
    return liberror(P,"__file_size",eVARNEEDD);
  else{
    uniChar ufn[MAX_MSG_LEN];
    char fBuff[MAX_MSG_LEN];
    struct stat buf;

    String2Uni(&t1,ufn,NumberOf(ufn));
    _utf(ufn,(unsigned char*)fBuff,NumberOf(fBuff));
    
  tryAgain:
    switchProcessState(P,wait_io);
    if(stat(fBuff, &buf) == -1){
      setProcessRunnable(P);

      switch(errno){
      case EINTR:
	goto tryAgain;
      case ENOTDIR:
	return liberror(P,"__file_size",eNOTDIR);
      case ENAMETOOLONG:
	return liberror(P,"__file_size",eINVAL);
      case ENOENT:
	return liberror(P,"__file_size",eNOTFND);
      case EACCES:
	return liberror(P,"__file_size",eNOPERM);
      case ELOOP:
	return liberror(P,"__file_size",eINVAL);
      case EIO:
	return liberror(P,"__file_size",eIOERROR);
      case EFAULT:
	return liberror(P,"__file_size",eINVAL);
      default:
	return liberror(P,"__file_size",eNOTFND);
      }
    }
    else{
      ptrI details = allocateInteger(&P->proc.heap,buf.st_size);
      
      setProcessRunnable(P);
      return equal(P,&details,&a[2]);
    }
  }
}

/*
 * __file_date(file,) - return file creation and modification 
 */

retCode g_file_date(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);

  if(isvar(t1)||isGroundString(&t1)!=Ok)
    return liberror(P,"__file_date", eSTRNEEDD);
  else if(!isvar(deRefI(&a[2])))
    return liberror(P,"__file_date",eVARNEEDD);
  else{
    uniChar ufn[MAX_MSG_LEN];
    char fBuff[MAX_MSG_LEN];
    struct stat buf;

    String2Uni(&t1,ufn,NumberOf(ufn));

    _utf(ufn,(unsigned char*)&fBuff[0],NumberOf(fBuff));
    
  tryAgain:
    switchProcessState(P,wait_io);

    if(stat(fBuff, &buf) == -1){
      setProcessRunnable(P);

      switch(errno){
      case EINTR:
	goto tryAgain;
      case ENOTDIR:
	return liberror(P,"__file_date",eNOFILE);
      case ENAMETOOLONG:
	return liberror(P,"__file_date",eINVAL);
      case ENOENT:
	return liberror(P,"__file_date",eNOTFND);
      case EACCES:
	return liberror(P,"__file_date",eNOPERM);
      case ELOOP:
	return liberror(P,"__file_date",eINVAL);
      case EIO:
	return liberror(P,"__file_date",eIOERROR);
      case EFAULT:
	return liberror(P,"__file_date",eINVAL);
      default:
	return liberror(P,"__file_date",eNOTFND);
      }
    }
    else{
      setProcessRunnable(P);
      /*	number accessTime = buf.st_atimespec.tv_sec+
	((number)buf.st_atimespec.tv_nsec)/NANO;
        ptrI details = allocateNumber(&P->proc.heap,accessTime);
      */

      ptrI details = allocateFloat(&P->proc.heap,buf.st_atime);
      retCode ret = equal(P,&details,&a[2]);
        
      if(ret==Ok){
	details = allocateFloat(&P->proc.heap,buf.st_mtime);
	ret = equal(P,&details,&a[3]);
      }
        
      if(ret==Ok){
	details = allocateFloat(&P->proc.heap,buf.st_ctime);
	ret = equal(P,&details,&a[4]);
      }
      return ret;
    }
  }
}

/*
 * ls lists files in a directory
 */
retCode g_ls(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);

  if(isvar(t1)||isGroundString(&t1)!=Ok)
    return liberror(P,"__ls", eSTRNEEDD);
  else if(!isvar(deRefI(&a[2])))
    return liberror(P,"__ls",eVARNEEDD);
  else{
    uniChar ufn[MAX_MSG_LEN];
    char fBuff[MAX_MSG_LEN];
    DIR *directory;

    switchProcessState(P,wait_io);

    String2Uni(&t1,ufn,NumberOf(ufn));
    _utf(ufn,(unsigned char*)fBuff,NumberOf(fBuff)); /* Convert unicode string to regular chars */
    
    if((directory=opendir(fBuff)) == NULL){
      setProcessRunnable(P);
      switch(errno){
      case EACCES:
      case EMFILE:
      case ENFILE:
        return liberror(P,"__ls", eNOPERM);
      case ENOENT:
        return liberror(P,"__ls", eNOTFND);
      case ENAMETOOLONG:
      case ENOTDIR:
        return liberror(P,"__ls", eINVAL);
      default:
        return liberror(P,"__ls", eNOTFND);
      }
    }
    else{
      ptrI dir = emptyList;
      ptrI dirEntry = emptyList;
      ptrI name = emptyList;
      rootPo root = gcAddRoot(&P->proc.heap,&dir);
      struct dirent *ent;
      heapPo H = &P->proc.heap;

      gcAddRoot(H,&dirEntry);
      gcAddRoot(H,&name);
    
      while((ent=readdir(directory)) != NULL){
      /* skip special entries "." and ".." */
        if(strcmp(ent->d_name, ".")!=0 && strcmp(ent->d_name, "..")!=0){
          name = allocateCString(H,ent->d_name);
          dirEntry = objP(allocateObject(H,commaClass));

          updateArg(objV(dirEntry),0,name);

          switch(ent->d_type){
          case DT_FIFO:
            updateArg(objV(dirEntry),1,kfifo);
            break;
          case DT_CHR:
            updateArg(objV(dirEntry),1,kcharfile);
            break;
          case DT_DIR:
            updateArg(objV(dirEntry),1,kdir);
            break;
          case DT_BLK:
            updateArg(objV(dirEntry),1,kblock);
            break;
          case DT_REG:
            updateArg(objV(dirEntry),1,kplain);
            break;
          case DT_SOCK:
            updateArg(objV(dirEntry),1,ksock);
            break;
          case DT_LNK:
            updateArg(objV(dirEntry),1,ksymlink);
            break;
          default:
            updateArg(objV(dirEntry),1,kunknown);
            break;
          }

          dir = consLsPair(H,dirEntry,dir); /* directory ends up in reverse order */
        }
      }

      closedir(directory);              /* Close the directory stream */

      gcRemoveRoot(H,root);
      
      setProcessRunnable(P);
      return equal(P,&a[2],&dir);
    }
  }
}
