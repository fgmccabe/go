#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* Generate latex table of the standard operators */

static void genInfix(FILE *out,char *op,int left,int prior,int right,char *cmt);
static void genPrefix(FILE *out,char *op,int prior,int right,char *cmt);
static void genPostfix(FILE *out,char *op,int left,int prior,char *cmt);



int main(int argc,char **argv)
{
  FILE *out=stdout;

  if(argc>=2)
    out = fopen(argv[1],"w");

  fprintf(out,"%% Automatically generated, do not edit \n");
  fprintf(out,"\\setlongtables\n");
  fprintf(out,"\\begin{longtable}{|llll|}\n");
  fprintf(out,"\\caption{\\go standard operators}\\label{grammar:operators}\\\\ \n");
  fprintf(out,"\\hline\n");
  fprintf(out,"\\small{}Operator&\\small{}Priority&\\small{}Assoc.&Description\\\\\n");
  fprintf(out,"\\hline\n");
  fprintf(out,"\\endfirsthead\n");
  fprintf(out,"\\multicolumn{4}{c}{\n");
  fprintf(out,"{Table \\ref{grammar:operators} \\go standard operators (cont.)}}\\\\\n");
  fprintf(out,"\\hline\n");
  fprintf(out,"\\small{}Operator&\\small{}Priority&\\small{}Assoc.&\\small{}Description\\\\\n");
  fprintf(out,"\\hline\n");
  fprintf(out,"\\endhead\n");
  fprintf(out,"\\hline\\multicolumn{4}{r}{\\small\\emph{continued\\ldots}}\\\n");
  fprintf(out,"\\endfoot\n");
  fprintf(out,"\\hline\n");
  fprintf(out,"\\endlastfoot\n");
  
#undef lastInfOp
#define lastInfOp
#undef lastPreOp
#define lastPreOp
#undef lastPostOp
#define lastPostOp

#undef infixOp
#define infixOp(op,left,prior,right,cmt) genInfix(out,op,left,prior,right,cmt);

#undef prefixOp
#define prefixOp(op,prior,right,cmt) genPrefix(out,op,prior,right,cmt);

#undef postfixOp
#define postfixOp(op,left,prior,cmt) genPostfix(out,op,left,prior,cmt);
#include "operators.h"
  
  fprintf(out,"\\end{longtable}\n");
  fclose(out);
  exit(0);
}

static char *pS(char *s)
{
  static char buff[1024];
  char *p = buff;
  
  while(*s!='\0'){
    if(*s=='$'||*s=='_'||*s=='#'||*s=='&'||*s=='%'){
      *p++='\\';
      *p++=*s++;
    } else if(*s==' '||*s=='^'||*s=='&'){
      *p++='\\';
      *p++='c';*p++='h';*p++='a';*p++='r';*p++='`';*p++='\\';
      *p++=*s++;
    } else if(*s=='~'){
      *p++='\\';
      *p++='t';*p++='i';*p++='l';*p++='d';*p++='a';*p++='{';*p++='}';
      s++;
    } else if(*s=='\\'){
      *p++='\\';
      *p++='c';*p++='h';*p++='a';*p++='r';*p++='\'';*p++='1';*p++='3';*p++='4';
      s++;
    } else
      *p++=*s++;
  }
  *p++='\0';
  return buff;
}


static void genInfix(FILE *out,char *op,int left,int prior,int right,char *cmt)
{
   char *assoc = left<prior&&right<prior?"infix":left<prior?"right":"left";
   
   fprintf(out,"\\tt %s&\\small{}%d&\\small{}%s&\\small{}%s\\\\\n",pS(op),prior,assoc,cmt);
}

static void genPrefix(FILE *out,char *op,int prior,int right,char *cmt)
{
   char *assoc = right<prior?"prefix":"assoc prefix";
   
   fprintf(out,"\\tt\\small %s&\\small{}%d&\\small{}%s&\\small{}%s\\\\\n",pS(op),prior,assoc,cmt);
}

static void genPostfix(FILE *out,char *op,int left,int prior,char *cmt)
{
   char *assoc = left<prior?"postfix":"assoc postfix";
   
   fprintf(out,"\\tt\\small %s&\\small{}%d&\\small{}%s&\\small{}%s\\\\\n",pS(op),prior,assoc,cmt);
}
