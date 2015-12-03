#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>

int main(int argc, char **argv)
{
  while(*++argv!=NULL){
    if(unlink(*argv)){
      switch(errno){
      	case ENOTDIR:
      	  fprintf(stderr,"%s not a directory\n",*argv);
      	  break;
      	case ENAMETOOLONG:
      	  fprintf(stderr,"%s name too long\n",*argv);
      	  break;
      	case ENOENT:
          fprintf(stderr,"%s doesnt exist\n",*argv);
      	  break;
      	case EACCES:
      	  fprintf(stderr,"%s: permission denied\n",*argv);
      	  break;
      	case ELOOP:
      	  fprintf(stderr,"%s: too many symbolic links\n",*argv);
      	  break;
      	case EPERM:
      	  fprintf(stderr,"%s: permission denied\n",*argv);
      	  break;
      	case EBUSY:
      	  fprintf(stderr,"%s: busy\n",*argv);
      	  break;
      	case EIO:
      	  fprintf(stderr,"%s: I/O error\n",*argv);
      	  break;
      	case EROFS:
      	  fprintf(stderr,"%s: R/O file system\n",*argv);
      	  break;
      	case EFAULT:
      	  fprintf(stderr,"%s: Bad path\n",*argv);
      	  break;
      }
    }
  }
}

