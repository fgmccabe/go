#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Generate a Go! module, that knows about the standard operators */

#undef lastInfOp
#define lastInfOp
#undef lastPreOp
#define lastPreOp
#undef lastPostOp
#define lastPostOp

#undef infixOp
#define infixOp(op,left,prior,right,cmt) genInfix(out,op,left,prior,right,cmt);

#undef prefixOp
#define prefixOp(op,prior,right,cmt)  genPrefix(out,op,prior,right,cmt);

#undef postfixOp
#define postfixOp(op,left,prior,cmt) genPostfix(out,op,left,prior,cmt);

static void genInfix(FILE *out,char *op,int left,int prior,int right,char *cmt);
static void genPrefix(FILE *out,char *op,int prior,int right,char *cmt);
static void genPostfix(FILE *out,char *op,int left,int prior,char *cmt);

static char buff[1024];

static char *pS(char *s)
{
  char *p = buff;
  
  while(*s!='\0'){
    if(*s=='\\')
      *p++='\\';
    *p++=*s++;
  }
  *p++='\0';
  return buff;
}

/*
static unsigned long long strhash(char *name)
{
  register long long hash = 0;

  if(name)
    while(*name)
      hash = hash*37+*name++;

  return hash;
}*/

int main(int argc,char **argv)
{
  FILE *out=stdout;

  if(argc>=2)
    out = fopen(argv[1],"w");

  fprintf(out,"/* Automatically generated, do not edit */\n");
  fprintf(out,"ops{\n");

  fprintf(out,"  infOp:[symbol,integer,integer,integer]{}.\n");
  fprintf(out,"  preOp:[symbol,integer,integer]{}.\n");
  fprintf(out,"  pstOp:[symbol,integer,integer]{}.\n\n");

#include "operators.h"
  
  fprintf(out,"}.\n\n");
  fclose(out);
  exit(0);
}

static void genInfix(FILE *out,char *op,int left,int prior,int right,char *cmt)
{
  fprintf(out,"  infOp(\'%s\',%d,%d,%d).\t\t-- %s\n",pS(op),left,prior,right,cmt);
}

static void genPrefix(FILE *out,char *op,int prior,int right,char *cmt)
{
  fprintf(out,"  preOp(\'%s\',%d,%d).\t\t-- %s\n",pS(op),prior,right,cmt);
}

static void genPostfix(FILE *out,char *op,int left,int prior,char *cmt)
{
  fprintf(out,"  pstOp(\'%s\',%d,%d).\t\t-- %s\n",pS(op),left,prior,cmt);
}
