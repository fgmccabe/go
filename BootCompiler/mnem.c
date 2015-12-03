#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "opcodes.h"

/* Generate an April module, that knows about instructions and their
   mnemonics */

#undef instruction
#define instruction(M,O,A1,A2,cmt) genIns(out,#M,O,A1,A2,cmt);

static void genIns(FILE *out,char *mnem,int op,opAndSpec A1,opAndSpec A2,char *cmt);

int main(int argc,char **argv)
{
  FILE *out=stdout;

  if(argc>=2)
    out = fopen(argv[1],"w");

  fprintf(out,"/* Automatically generated, do not edit */\n");
  fprintf(out,"#include \"instr.ah\";\n\n");
  fprintf(out,"module\n");
  fprintf(out,"import {\n");
  fprintf(out,"  interface \"ecode.af\" \n");
  fprintf(out,"  and interface <stdio.af> \n");
  fprintf(out,"}in{\n");

  fprintf(out,"  mnem(L,Lits) => collect{\n");
  fprintf(out,"    pc : 0;\t\t-- compute label offsets\n");
  fprintf(out,"    Lbls = collect{\n");
  fprintf(out,"      for I in L do{\n");
  fprintf(out,"        case I in {\n");
  fprintf(out,"          lbl(Lb) ->  elemis(Lb,pc)\n");
  fprintf(out,"        | cmt(_) ->  {}\n");
  fprintf(out,"        | _ -> pc+:=1\n");
  fprintf(out,"        };\n");
  fprintf(out,"      };\n");
  fprintf(out,"    };\n\n");
  fprintf(out,"    pc:=0;\n\n");
  fprintf(out,"    for X in L do {\n");
  fprintf(out,"      case X in {\n");
  fprintf(out,"        lbl(_) -> {}\t\t-- label\n");
  fprintf(out,"      | cmt(_) -> {}\t\t-- comment\n");

#include "instructions.h"
    
  fprintf(out,"      }\n");
  fprintf(out,"    }\n");
  fprintf(out,"  };\n\n");

  fprintf(out,"pcGap(pc,V,Lbls,mx) => valof{\n");
  fprintf(out,"    if (V,P) in Lbls then{\n");
  fprintf(out,"      gap = P-pc;\n");
  fprintf(out,"\n");
  fprintf(out,"      if gap< -mx || gap>mx then\n");
  fprintf(out,"	exception error(\"pc relative offset \"++gap^0++\" for label \"++V^0,'inval);\n");
  fprintf(out,"      valis gap;\n");
  fprintf(out,"    }\n");
  fprintf(out,"    else exception error(\"assembler -- cant find label \"++V^0,'inval);\n");
  fprintf(out,"  };\n\n");
  
  fprintf(out,"  ltOff(V,Lits) => valof{\n");
  fprintf(out,"    Lno : 0;\n");
  fprintf(out,"    for (Lb,_) in Lits do{\n");
  fprintf(out,"      if Lb==V then valis Lno;\n");
  fprintf(out,"      Lno := Lno+1;");
  fprintf(out,"    };\n");
  fprintf(out,"    exception error(\"assembler -- cant find literal \"++V^0,\'inval)\n");
  fprintf(out,"  };\n");
  fprintf(out,"} export mnem               -- end of generated module\n");
  fclose(out);
  exit(0);
}

static char *genArg(FILE *out,char *sep,int *V,opAndSpec A)
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
  case iLh:				// input local variable offset (0..255)
  case iLm:				// input local variable offset (0..255)
  case iLl:				// input local variable offset (0..255)
  case iLc:			       // input local variable offset (0..65535)
  case oLh:			      /* output local variable, offset 0..255 */
  case oLm:			      /* output local variable, offset 0..255 */
  case oLl:			      /* output local variable, offset 0..255 */
  case oLc:			     // output local variable offset  (0..65535)
    fprintf(out,"%sV%d",sep,(*V)++);
    return ",";
  case iSt:                             // input at current structure pointer
  case oSt:                             // output to current structure pointer
    return sep;
  case oAr:				/* Arity in upper slot */
  case uAr:                             // Arity in upper slot
  case uLt:                             // small literal in upper slot (-128..127)
  case Lt:                              // 16bit literal (-32768..32767)
  case vSz:                             // Size of local variable vector
  case lSz:                             // Size of local variable vector
  case cSz:             		// Structure size
    fprintf(out,"%sV%d",sep,(*V)++);
    return ",";
  case Es:                              // escape code (0..65535)
    fprintf(out,"%sV%d",sep,(*V)++);
    return ",";
  case pcr:                             // program counter relative offset (-32768..32767)
  case pcl:                             // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
  case ltl:                             // literal number (0..65535)
    fprintf(out,"%sV%d",sep,(*V)++);
    return ",";
  default:
    printf("Problem in generating opcode type\n");
    exit(11);
  }
}
  
static void genCode(FILE *out,int *V,opAndSpec A)
{
  switch(A){
  case nOp:                             // No operand
    return;
  case iAh:                             // input argument register in upper slot (0..255)
  case oAh:                             // output argument register in upper slot (0..255)
  case oAr:				/* Arity in upper slot */
  case uAr:                             // Arity in upper slot
  case uLt:                             // small literal in upper slot (-128..127)
  case iLh:				/* input local variable, offset 0..255 */
  case oLh:				/* output local variable, offset 0..255 */
    fprintf(out,"          Code := bor(Code,bleft(band(V%d,0xff),24));\n",(*V)++);
    return;
  case iAm:                             // input argument register in middle slot (0..255)
  case oAm:                             // output argument register in middle slot (0..255)
  case iLm:				/* input local variable, offset 0..255 */
  case oLm:				/* output local variable, offset 0..255 */
    fprintf(out,"          Code := bor(Code,bleft(band(V%d,0xff),16));\n",(*V)++);
    return;
  case iAl:                             // input argument register in lower slot (0..255)
  case oAl:                             // output argument register in lower slot (0..255)
  case iLl:				/* input local variable, offset 0..255 */
  case oLl:				/* output local variable, offset 0..255 */
    fprintf(out,"          Code := bor(Code,bleft(band(V%d,0xff),8));\n",(*V)++);
    return;
  case iLc:                             // input local variable offset (0..65535)
  case oLc:                             // output local variable offset  (0..65535)
  case vSz:                             // Size of local variable vector
  case lSz:                             // Size of local variable vector
  case cSz:             		// Structure size
    fprintf(out,"          Code := bor(Code,bleft(band(V%d,0xffff),8));\n",(*V)++);
    return;
  case iSt:                             // input at current structure pointer
  case oSt:                             // output to current structure pointer
    return;
  case Lt:                              // 16bit literal (-32768..32767)
    fprintf(out,"          Code := bor(Code,bleft(band(V%d,0xffff),8));\n",(*V)++);
    return;
  case Es:                              // escape code (0..65535)
    fprintf(out,"          (EC,_) = escCode(V%d);\n",(*V)++);
    fprintf(out,"          Code := bor(Code,bleft(EC,8));\n");
    return;
  case pcr:                             // program counter relative offset (-32768..32767)
    fprintf(out,"          Code := bor(Code,bleft(band(pcGap(pc,V%d,Lbls,32767),0xffff),8));\n",(*V)++);
    return;
  case pcl:                             // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
    fprintf(out,"          gap = pcGap(pc,V%d,Lbls,0xffffff);\n",(*V)++);
    fprintf(out,"          Code := bor(Code,bleft(gap,8));\n");
    return;
  case ltl:                             // literal number (0..65535)
    fprintf(out,"          off = ltOff(V%d,Lits);\n",(*V)++);
    fprintf(out,"          Code := bor(Code,bleft(band(off,0xffff),8));\n");
    return;
  default:
    fprintf(stderr,"Unknown instruction type code\n");
    exit(1);
  }
}
  
static void genIns(FILE *out,char *mnem,int op,opAndSpec A1,opAndSpec A2,char *cmt)
{
  char *sep = "(";
  int V = 0;

  fprintf(out,"      | %s",mnem);
  
  sep=genArg(out,sep,&V,A1);
  sep=genArg(out,sep,&V,A2);
  
  if(strcmp(sep,",")==0)
    sep = ")";
  else
    sep = "";
    
  fprintf(out,"%s -> {\t\t-- %s\n"
	    "          Code:%d;\n"
	    "          pc := pc+1;\n",sep,cmt,op);

  V = 0;

  genCode(out,&V,A1);
  genCode(out,&V,A2);

  fprintf(out,"          elemis Code;\n");
  fprintf(out,"        }\n");
}
