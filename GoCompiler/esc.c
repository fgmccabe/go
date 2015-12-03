/*
 * Generate the escapes.go module to allow the compiler access to the 
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

static void writeName(FILE *out,char *name,char *lead,char *tail);

#undef escape
#define escape(name,fun,code,priv,secr,type,cmt) if(!secr)genEsc(out,name,code,type,cmt);

static void genEsc(FILE *out,char *name,int code,char *tp,char *cmt);

static char *linesep = "    ";


int main(int argc,char **argv)
{

  if(argc<2){
    printf("Expecting output file name\n");
    exit(1);
  }
  else{
    FILE *out = fopen(argv[1],"w");

    fprintf(out,"/* Automatically generated, do not edit */\n");
    fprintf(out,"esc{\n");
    fprintf(out,"  import types.\n\n");

    fprintf(out,"  escTypes:dict =  dict([\t\t -- standard escape types\n");

#include "escapes.h"
    fprintf(out,linesep);
    fprintf(out,"],emptyDict).\n");

    fprintf(out," isEscape:[symbol]{}.\n");
    fprintf(out," isEscape(Nm) :- escTypes.isbound(Nm,_,_)!.\n");
    fprintf(out,"}.\n");
    fclose(out);
    exit(0);
  }
}

static char *genOpType(FILE *f,char *tp);

static char *genOpTypeArgs(FILE *f,char *tp)
{
  int ar = *tp++;                   /* pick up the arity */
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

static char *genPredTypeArgs(FILE *f,char *tp)
{
  int ar = *tp++;		/* pick up the arity */
  char *sep = "";
  fprintf(f,"[");
  while(ar--){
    char Mode = *tp++;
    fprintf(f,"%s(",sep);
    sep = ",";

    tp = genOpType(f,tp);
    fprintf(f,",");

    assert(Mode=='+'||Mode=='-'||Mode=='?');

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
      fprintf(f,"uType('go.stdlib',\'integer\',[])");
      return tp;
      
    case float_sig:
      fprintf(f,"uType('go.stdlib',\'float\',[])");
      return tp;
      
    case number_sig:
      fprintf(f,"uType('go.stdlib',\'number\',[])");
      return tp;
      
    case char_sig:
      fprintf(f,"uType('go.stdlib',\'char\',[])");
      return tp;
      
    case symbol_sig:
      fprintf(f,"uType('go.stdlib',\'symbol\',[])");
      return tp;
      
    case string_sig:
      fprintf(f,"uType('go.stdlib',\'list\',[uType('go.stdlib',\'char\',[])])");
      return tp;

    case top_sig:
      fprintf(f,"topType");
      return tp;
      
    case logical_sig:
      fprintf(f,"uType('go.stdlib',\'logical\',[])");
      return tp;
      
    case type_sig:
      fprintf(f,"typeType");
      return tp;
      
    case opaque_sig:
      fprintf(f,"uType('go.stdlib',\'opaque\',[])");
      return tp;
      
    case list_sig:
      fprintf(f,"uType('go.stdlib','list',[");
      tp = genOpType(f,tp);
      fprintf(f,"])");
      return tp;
    
    case funct_sig:{                    // Function type
      fprintf(f,"funType(");
      tp = genPredTypeArgs(f,tp);       /* The argument types */
      fprintf(f,",");
    
      tp = genOpType(f,tp);               /* result type */
      fprintf(f,")");
      return tp;
    }
    
    case proc_sig:{                     // Predicate type
      fprintf(f,"predType(");
      tp = genPredTypeArgs(f,tp);       /* The argument types */
      fprintf(f,")");
      return tp;
    }
    
    case grammar_sig:{                  // grammar type
      fprintf(f,"gramType(");
      tp = genPredTypeArgs(f,tp);       /* The argument types */
      fprintf(f,",");
      genOpType(f,tp);
      fprintf(f,")");
      return tp;
    }
    
    case action_sig:{                   // Action type
      fprintf(f,"actType(");
      tp = genPredTypeArgs(f,tp);       /* The argument types */
      fprintf(f,")");
      return tp;
    }

    case tuple_sig:{                 // Tuple type
      int ar = *tp++;	             /* pick up the arity */
      char *sep = "";
      
      fprintf(f,"tplType([");
      while(ar--){
        fprintf(f,"%s",sep);
        sep = ",";
        tp = genOpType(f,tp);
      }
      fprintf(f,"])");
      return tp;
    }
    
    case var_sig:
      fprintf(f,"undef(\'_t%d\')",*tp++);
      return tp;

    case forall_sig:
      fprintf(f,"allType(\'_t%d\',",*tp++);
      tp = genOpType(f,tp);
      fprintf(f,",");
      tp = genOpType(f,tp);
      fprintf(f,")");
      return tp;

    case poly_sig:{
      char delim = *tp++;

      if(*tp=='#'){
	tp++;
	fprintf(f,"uType('go.stdlib','");
      }
      else if(strchr(tp,'#')!=NULL){
	fprintf(f,"uType('");
	while(*tp!='#')
	  fprintf(f,"%c",*tp++);
	assert(*tp=='#');
	tp++;
	fprintf(f,"','");
      }
      else
	fprintf(f,"uType('*','");

      while(*tp!=delim)
	fprintf(f,"%c",*tp++);

      fprintf(f,"\',");
      tp = genOpTypeArgs(f,tp+1);
    
      fprintf(f,")");
      return tp;
    }
    default:
      printf("Problem in generating opcode type: %s\n",tp);
      exit(11);
  }
}

static void writeName(FILE *out,char *name,char *lead,char *tail)
{
  fputs(lead,out);
  while(*name!='\0'){
    char ch = *name++;
    
    switch(ch){
      case '\a':
        fprintf(out,"\\a");
        continue;
      case '\b':
        fprintf(out,"\\b");
        continue;
      case '\x7f':
        fprintf(out,"\\d");
        continue;
      case '\x1b':
        fprintf(out,"\\e");
        continue;
      case '\f':
        fprintf(out,"\\f");
        continue;
      case '\n': 
        fprintf(out,"\\n");
        continue;
      case '\r': 
        fprintf(out,"\\r");
        continue;
      case '\t': 
        fprintf(out,"\\t");
        continue;
      case '\v': 
        fprintf(out,"\\v");
        continue;
      case '\\':
        fprintf(out,"\\\\");
        continue;
      case '\"':
        fprintf(out,"\\\"");
        continue;
      default:
        if((ch&0177)<' '){
          fputc('\\',out);
          fputc('0'|((ch>>6)&7),out);
          fputc('0'|((ch>>3)&7),out);
          fputc('0'|(ch&7),out);
        }
        else
          fputc(ch,out);
    }
  }
  if(tail!=NULL)
    fprintf(out,tail);
}

static void genEsc(FILE *out,char *name,int code,char *tp,char *cmt)
{
  fprintf(out,"%svS(",linesep);
  writeName(out,name,"\'","\',varBind,");
  genOpType(out,tp);
  linesep = "),\n    ";
}



