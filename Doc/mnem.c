#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "opcodes.h"

/* Generate a latex file that generates a table of the GRTE instruction set */

#undef instruction
#define instruction(M,O,A1,A2,cmt) genIns(out,#M,O,A1,A2,cmt);

static void genIns(FILE *out,char *mnem,int op,opAndSpec A1,opAndSpec A2,char *cmt);

int main(int argc,char **argv)
{
  FILE *out=stdout;

  if(argc>=2)
    out = fopen(argv[1],"w");

  fprintf(out,"%% Automatically generated, do not edit \n");
  fprintf(out,"\\setlongtables\n");
  fprintf(out,"\\begin{longtable}{|cll|}\n");
  fprintf(out,"\\caption{\\go instruction set summary}\\label{howitworks:table}\\\\ \n");
  fprintf(out,"\\hline\n");
  fprintf(out,"OpCode&Mnemonic&Summary\\\\\n");
  fprintf(out,"\\hline\n");
  fprintf(out,"\\endfirsthead\n");
  fprintf(out,"\\multicolumn{3}{c}{\n");
  fprintf(out,"{Table \\ref{howitworks:table} \\go instruction set summary (cont.)}}\\\\\n");
  fprintf(out,"\\hline\n");
  fprintf(out,"OpCode&Mnemonic&Summary\\\\\n");
  fprintf(out,"\\hline\n");
  fprintf(out,"\\endhead\n");
  fprintf(out,"\\hline\\multicolumn{3}{r}{\\small\\emph{continued\\ldots}}\\\n");
  fprintf(out,"\\endfoot\n");
  fprintf(out,"\\hline\n");
  fprintf(out,"\\endlastfoot\n");
#include "../Headers/instructions.h"
  fprintf(out,"\\end{longtable}\n");
  fclose(out);
  exit(0);
}

static char *pS(char *s)
{
  static char buff[1024];
  char *p = buff;
  
  while(*s!='\0'){
    if(*s=='_')
      *p++='\\';
    *p++=*s++;
  }
  *p++='\0';
  return buff;
}

static void showOpand(FILE *out,opAndSpec A)
{
  switch(A){
  case nOp:                             // No operand
    return;
  case iAh:                             // input argument register in upper slot (0..255)
  case oAh:                             // output argument register in upper slot (0..255)
    fprintf(out,"\\put(5,5){\\tt h}");
    return;
  case iAm:                             // input argument register in middle slot (0..255)
  case oAm:                             // output argument register in middle slot (0..255)
    fprintf(out,"\\put(60,5){\\tt Xm}");
    return;
  case iLh:                             // input local variable offset (0..255)
  case oLh:                             // output local variable offset  (0..255)
    fprintf(out,"\\put(5,5){\\tt Yh}");
    return;
  case iLm:                             // input local variable offset (0..255)
  case oLm:                             // output local variable offset  (0..255)
    fprintf(out,"\\put(60,5){\\tt Ym}");
    return;
  case iLl:                             // input local variable offset (0..255)
  case oLl:                             // output local variable offset  (0..255)
  case iLc:                             // input local variable offset (0..65535)
  case oLc:                             // output local variable offset  (0..65535)
    fprintf(out,"\\put(55,5){\\tt Y\\sub{hi}}");
    fprintf(out,"\\put(105,5){\\tt Y\\sub{lo}}");
    return;
  case iSt:                             // input at current structure pointer
  case oSt:                             // output to current structure pointer
    return;
  case oAr:
  case uAr:                             // Arity in upper slot
  case uLt:                             // small literal in upper slot (-128..127)
    fprintf(out,"\\put(5,5){\\tt h}");
    return;
  case Lt:                              // 16bit literal (-32768..32767)
  case vSz:                             // Size of local variable vector
  case lSz:                             // Size of local variable vector
  case cSz:             		// Structure size
  case Es:                              // escape code (0..65535)
  case pcr:                             // program counter relative offset (-32768..32767)
  case ltl:                             // literal number (0..65535)
    fprintf(out,"\\put(55,5){\\tt X\\sub{hi}}");
    fprintf(out,"\\put(105,5){\\tt X\\sub{lo}}");
    return;
  case pcl:                             // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
    return;
  default:
    return;
  }
}
      
static void showShape(FILE *out,int op,opAndSpec A1,opAndSpec A2)
{
  char *pstAmble="\\end{picture}";

  fprintf(out,"\\setlength{\\unitlength}{0.1mm}\\begin{picture}(200,50)\n");
  fprintf(out,"\\put(0,0){\\framebox(200,40){}}\n");
  fprintf(out,"\\multiput(50,0)(50,0){3}{\\line(0,1){10}}\n");
  
  fprintf(out,"\\put(155,5){\\tt %02x}",op);
  
  showOpand(out,A1);
  showOpand(out,A2);
  fprintf(out,"%s\n",pstAmble);
}
  
static char *showOpAnd(FILE *out,char *sep,opAndSpec A)
{
  switch(A){
  case nOp:                             // No operand
    return sep;
  case iAh:                             // input argument register in upper slot (0..255)
  case oAh:                             // output argument register in upper slot (0..255)
    fprintf(out,"%sA[h]",sep);
    return ",";
  case iAm:                             // input argument register in middle slot (0..255)
  case oAm:                             // output argument register in middle slot (0..255)
    fprintf(out,"%sA[m]",sep);
    return ",";
  case iLh:				// input local variable offset (0..255)
  case oLh:				// output local variable offset (0..255)
    fprintf(out,"%sY[h]",sep);
    return ",";
  case iLm:				// input local variable offset (0..255)
  case oLm:				// output local variable offset (0..255)
    fprintf(out,"%sY[m]",sep);
    return ",";
  case iLl:				// input local variable offset (0..255)
  case oLl:				// output local variable offset (0..255)
    fprintf(out,"%sY[l]",sep);
    return ",";
  case iLc:                             // input local variable offset (0..65535)
  case oLc:                             // output local variable offset  (0..65535)
    fprintf(out,"%sY[X]",sep);
    return ",";
  case iSt:                             // input at current structure pointer
  case oSt:                             // output to current structure pointer
    fprintf(out,"%sS++",sep);
    return ",";
  case oAr:
  case uAr:                             // Arity in upper slot
  case uLt:                             // small literal in upper slot (-128..127)
    fprintf(out,"%sh",sep);
    return ",";
  case Lt:                              // 16bit literal (-32768..32767)
  case vSz:                             // Size of local variable vector
  case lSz:                             // Size of local variable vector
  case cSz:             		// Structure size
    fprintf(out,"%sX",sep);
    return ",";
  case Es:                              // escape code (0..65535)
    fprintf(out,"%sX",sep);
    return ",";
  case pcr:                             // program counter relative offset (-32768..32767)
    fprintf(out,"%sPC+X",sep);
    return ",";
  case pcl:                             // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
    fprintf(out,"%sPC+long",sep);
    return ",";
  case ltl:                             // literal number (0..65535)
    fprintf(out,"%sLits[X]",sep);
    return ",";
  default:
    return NULL;
  }
}

static void genIns(FILE *out,char *mnem,int op,opAndSpec A1,opAndSpec A2,char *cmt)
{
  char *sep = "";
  
  showShape(out,op,A1,A2);
  fprintf(out,"&\\tt %s ",pS(mnem));

  sep = showOpAnd(out,sep,A1);
  sep = showOpAnd(out,sep,A2);

  fprintf(out,"&%s\\\\\n",pS(cmt));
}
