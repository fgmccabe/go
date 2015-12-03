#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "config.h"
#include "opcodes.h"

#undef instruction
#define instruction(M,O,A1,A2,cmt)  genIns(out,#M,A1,A2,cmt);

static void genIns(FILE *out,char *op,opAndSpec A1,opAndSpec A2,char *cmt);

int main(int argc,char **argv)
{
  FILE *out=stdout;

  if(argc>=2)
    out = fopen(argv[1],"w");

  fprintf(out,"/* Automatically generated, do not edit */\n");
  
  fprintf(out,"instruction ::=              -- type defining the opcodes\n");
  fprintf(out,"   lbl(symbol)               -- label in code stream\n");
  fprintf(out," | cmt(string)               -- comment code stream\n");

#include "instructions.h"
    
  fprintf(out,";                           -- end of instruction type\n");
  fclose(out);
  exit(0);
}

static char *genOpAnd(FILE *f,char *sep,opAndSpec A)
{
  switch(A){
  case nOp:                             // No operand
    return sep;
  case iAh:                             // input argument register in upper slot (0..255)
  case oAh:                             // output argument register in upper slot (0..255)
  case iAm:                             // input argument register in middle slot (0..255)
  case oAm:                             // output argument register in middle slot (0..255)
  case iAl:                             // input argument register in lower slot (0..255)
  case oAl:                             // output argument register in lower slot (0..255)
  case iLh:				/* input local, offset 0..255 */
  case iLm:				/* input local, offset 0..255 */
  case iLl:				/* input local, offset 0..255 */
  case iLc:                             // input local variable offset (0..65535)
  case oLh:				/* output local offet 0..255 */
  case oLm:				/* output local offet 0..255 */
  case oLl:				/* output local offet 0..255 */
  case oLc:                             // output local variable offset  (0..65535)
    fprintf(f,"%snumber",sep);
    return ",";
  case iSt:                             // input at current structure pointer
  case oSt:                             // output to current structure pointer
    return sep;
  case oAr:				/* Result arity in upper slot */
  case uAr:                             // Arity in upper slot
  case uLt:                             // small literal in upper slot (-128..127)
  case Lt:                              // 16bit literal (-32768..32767)
  case vSz:                             // Size of local variable vector
  case lSz:                             // Size of local variable vector
  case cSz:             		// Structure size
    fprintf(f,"%snumber",sep);
    return ",";
  case Es:                              // escape code (0..65535)
    fprintf(f,"%schar[]",sep);
    return ",";
  case pcr:                             // program counter relative offset (-32768..32767)
  case pcl:                             // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
  case ltl:                             // literal number (0..65535)
    fprintf(f,"%ssymbol",sep);
    return ",";
  default:
    printf("Problem in generating opcode type\n");
    exit(11);
  }
}
        
static void genIns(FILE *out,char *op,opAndSpec A1,opAndSpec A2,char *cmt)
{
  char *sep = "(";
  
  fprintf(out," | %s",op);
  
  sep = genOpAnd(out,sep,A1);
  sep = genOpAnd(out,sep,A2);
  
  if(strcmp(sep,",")==0)
    fprintf(out,")\t\t-- %s\n",cmt);
  else
    fprintf(out,"\t\t-- %s\n",cmt);
}
