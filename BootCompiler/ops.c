#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Generate an April module, that knows about the standard operators */

static void genInfix(FILE *out,char *op,int left,int prior,int right,char *cmt);
static void genPrefix(FILE *out,char *op,int prior,int right,char *cmt);
static void genPostfix(FILE *out,char *op,int left,int prior,char *cmt);

#undef lastOp
#define lastOp sep = "\t";

static char *sep = ",";

static char *pS(char *s)
{
  static char buff[1024];
  char *p = buff;
  
  while(*s!='\0'){
    if(*s=='\\')
      *p++='\\';
    *p++=*s++;
  }
  *p++='\0';
  return buff;
}


int main(int argc,char **argv)
{
  FILE *out=stdout;

  if(argc>=2)
    out = fopen(argv[1],"w");

  fprintf(out,"/* Automatically generated, do not edit */\n");
  //  fprintf(out,"#include \"ops.ah\";\n\n");
  fprintf(out,"module{\n");

  fprintf(out,"  infops = [\n");
  
  sep = ",";
  
#undef lastInfOp
#define lastInfOp sep = "\t";
#undef lastPreOp
#define lastPreOp
#undef lastPostOp
#define lastPostOp


#undef infixOp
#define infixOp(op,left,prior,right,cmt) genInfix(out,op,left,prior,right,cmt);

#undef prefixOp
#define prefixOp(op,prior,right,cmt) 

#undef postfixOp
#define postfixOp(op,left,prior,cmt)
#include "operators.h"
  
  fprintf(out,"    ];\n");
  fprintf(out,"    \n");
  fprintf(out,"    prefops = [\n");
  
  sep = ",";

#undef lastInfOp
#define lastInfOp
#undef lastPreOp
#define lastPreOp sep = "\t";
#undef lastPostOp
#define lastPostOp


#undef infixOp
#define infixOp(op,left,prior,right,cmt) 

#undef prefixOp
#define prefixOp(op,prior,right,cmt) genPrefix(out,op,prior,right,cmt);

#undef postfixOp
#define postfixOp(op,left,prior,cmt)
#include "operators.h"
  
  fprintf(out,"    ];\n\n");
  fprintf(out,"    postops = [\n");
  
  sep = ",";

#undef lastInfOp
#define lastInfOp
#undef lastPreOp
#define lastPreOp
#undef lastPostOp
#define lastPostOp sep = "\t";

#undef infixOp
#define infixOp(op,left,prior,right,cmt)

#undef prefixOp
#define prefixOp(op,prior,right,cmt) 

#undef postfixOp
#define postfixOp(op,left,prior,cmt) genPostfix(out,op,left,prior,cmt);
#include "operators.h"
  
  fprintf(out,"    ];\n\n");
  
  fprintf(out,"  isOperator(N) => (N,_,_,_) in infops || (N,_,_) in prefops || \n");
  fprintf(out,"                (N,_,_) in postops;\n");

  fprintf(out,"prefixPr(N) => {\n");
  fprintf(out," if (N,O,R) in prefops then\n");
  fprintf(out,"   (O,R)\n");
  fprintf(out," else\n");
  fprintf(out,"   (10000,10000)\n");
  fprintf(out,"};\n");

  fprintf(out,"postfixPr(N) => {\n");
  fprintf(out," if (N,L,O) in postops then\n");
  fprintf(out,"   (L,O)\n");
  fprintf(out," else\n");
  fprintf(out,"   (10000,10000)\n");
  fprintf(out,"};\n");

  fprintf(out,"infixPr(N) => {\n");
  fprintf(out,"  if (N,L,O,R) in infops then\n");
  fprintf(out,"    (L,O,R)\n");
  fprintf(out,"  else\n");
  fprintf(out,"   (10000,10000,10000)\n");
  fprintf(out,"};\n");
  fprintf(out,"\n");
  fprintf(out,"} export (isOperator, prefixPr, infixPr, postfixPr);  -- end of generated module\n");
  fclose(out);
  exit(0);
}

static void genInfix(FILE *out,char *op,int left,int prior,int right,char *cmt)
{
  fprintf(out,"    (\"%s\",%d,%d,%d)%s\t-- %s\n",pS(op),left,prior,right,sep,cmt);
}

static void genPrefix(FILE *out,char *op,int prior,int right,char *cmt)
{
  fprintf(out,"    (\"%s\",%d,%d)%s\t-- %s\n",pS(op),prior,right,sep,cmt);
}

static void genPostfix(FILE *out,char *op,int left,int prior,char *cmt)
{
  fprintf(out,"    (\"%s\",%d,%d)%s\t-- %s\n",pS(op),left,prior,sep,cmt);
}
