/*
 * Generate the escapes.ap module to allow the compiler access to the 
 * standard escapes in the Go! run-time
 */

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "signature.h"
#include "logical.h"

typedef WORD64 integer;

#undef escape
#define escape(name,fun,code,priv,secret,type,cmt) if(!secret)genEsc(out,name,code,type,cmt);

static void genEsc(FILE *out,char *name,int code,char *tp,char *cmt);

static char *linesep = "    ";


int main(int argc,char **argv)
{

  FILE *out = stdout;

  fprintf(out,"/* Automatically generated, do not edit */\n");
  fprintf(out,"#include \"go.ah\";\n\n");
  fprintf(out,"module\n{\n");

  fprintf(out,"  escTypes =  collect{\t\t -- standard escape types\n");

#include "escapes.h"

  fprintf(out,"  };\n");
  fprintf(out,"} export escTypes;\n");
  fclose(out);
  exit(0);
}

static char *genOpType(FILE *f,char *tp);


static char *genOpTypeArgs(FILE *f,char *tp)
{
  int ar = *tp++;		/* pick up the arity */
  char *sep = "";
  fprintf(f,"[");
  while(ar--){
    fprintf(f,"%s",sep);
    sep = ",";
    tp = genOpType(f,tp);
  }
  fprintf(f,"]");
  return tp;
}

static char *genTypeArgs(FILE *f,char *tp)
{
  int ar = *tp++;		/* pick up the arity */
  char *sep = "";
  fprintf(f,"[");
  while(ar--){
    char Mode = *tp++;
    fprintf(f,"%s(",sep);
    sep = ",";

    assert(Mode=='+'||Mode=='-'||Mode=='?');

    tp = genOpType(f,tp);
    fprintf(f,",");

    switch(Mode){
    case '+':
      fprintf(f,"inpMode");
      break;
    case '-':
      fprintf(f,"outMode");
      break;
    case '?':
      fprintf(f,"biMode");
      break;
    }

    fprintf(f,")");
  }
  fprintf(f,"]");
  return tp;
}

static char *genOpType(FILE *f,char *tp)
{
  switch(*tp++){
  case integer_sig:
    fprintf(f,"uType(\"go.stdlib\",\"integer\",[])");
    return tp;
    
  case float_sig:
    fprintf(f,"uType(\"go.stdlib\",\"float\",[])");
    return tp;
    
  case number_sig:
    fprintf(f,"uType(\"go.stdlib\",\"number\",[])");
    return tp;
    
  case char_sig:
    fprintf(f,"uType(\"go.stdlib\",\"char\",[])");
    return tp;
    
  case symbol_sig:
    fprintf(f,"uType(\"go.stdlib\",\"symbol\",[])");
    return tp;
    
  case string_sig:
    fprintf(f,"uType(\"go.stdlib\",\"list\",[uType(\"go.stdlib\",\"char\",[])])");
    return tp;
    
  case top_sig:
    fprintf(f,"topType");
    return tp;
    
  case logical_sig:
    fprintf(f,"uType(\"go.stdlib\",\"logical\",[])");
    return tp;
    
  case type_sig:
    fprintf(f,"typeType");
    return tp;
    
  case opaque_sig:
    fprintf(f,"uType(\"go.stdlib\",\"opaque\",[])");
    return tp;
    
  case list_sig:
    fprintf(f,"uType(\"go.stdlib\",\"list\",[");
    tp = genOpType(f,tp);
    fprintf(f,"])");
    return tp;
    
  case funct_sig:{                    // Function type
    fprintf(f,"funType(");
    tp = genTypeArgs(f,tp);         /* The argument types */
    fprintf(f,",");
    tp = genOpType(f,tp);             /* the result type */
    fprintf(f,")");
    return tp;
  }
    
  case proc_sig:{                     // Predicate type
    fprintf(f,"predType(");
    tp = genTypeArgs(f,tp);         /* The argument types */
    fprintf(f,")");
    return tp;
  }
    
  case grammar_sig:{                  // grammar type
    fprintf(f,"gramType(");
    tp = genTypeArgs(f,tp);         /* The argument types */
    fprintf(f,",");
    
    tp = genOpType(f,tp);	           /* the stream type */
    fprintf(f,")");
    return tp;
  }
    
  case action_sig:{                     // Action type
    fprintf(f,"actType(");
    tp = genTypeArgs(f,tp);         /* The argument types */
    fprintf(f,")");
    return tp;
  }
    
  case var_sig:
    fprintf(f,"bound(\"_t%d\")",*tp++);
    return tp;
    
  case forall_sig:
    fprintf(f,"allType(\"_t%d\",",*tp++);
    tp = genOpType(f,tp);
    fprintf(f,",");
    tp = genOpType(f,tp);
    fprintf(f,")");
    return tp;
    
  case poly_sig:{
    char delim = *tp++;

    if(*tp=='#'){
      tp++;
      fprintf(f,"uType(\"go.stdlib\",\"");
    }
    else if(strchr(tp,'#')!=NULL){
      fprintf(f,"uType(\"");
      while(*tp!='#')
	fprintf(f,"%c",*tp++);
      fprintf(f,"\",\"");
      tp++;
    }
    else
      fprintf(f,"uType(\"*\",\"");
    
    while(*tp!=delim)
      fprintf(f,"%c",*tp++);
    
    fprintf(f,"\",");
    tp = genOpTypeArgs(f,tp+1);
    
    fprintf(f,")");
    return tp;
  }
  default:
    printf("Problem in generating opcode type: %s\n",tp);
    exit(11);
  }
}

static char *pS(char *s,char delim)
{
  static char buff[1024];
  char *p = buff;
  
  while(*s!=delim){
    if(*s=='\\')
      *p++='\\';
    *p++=*s++;
  }
  *p++='\0';
  return buff;
}
  
static void genEsc(FILE *out,char *name,int code,char *tp,char *cmt)
{
  fprintf(out,"%selemis (\"%s\",varBind,",linesep,pS(name,'\0'));

  genOpType(out,tp);
  fprintf(out,")");
  linesep = ";\n    ";
}


