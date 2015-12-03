#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "signature.h"
#include "logical.h"

/* Generate a latex file that generates a table of the GRTE standard escape */

static char *pS(char *s,char delim);
static char *pC(char c);

#undef escape
#define escape(name,fun,code,priv,secr,type,cmt) genEsc(out,name,priv,code,type,cmt);
static void genEsc(FILE *out,char *name,logical priv,int code,char *tp,char *cmt);

#undef constr
#define constr(name,K,type)

#undef tdf
#define tdf(name,spec,type)

int main(int argc,char **argv)
{
  FILE *out=stdout;

  if(argc>=2)
    out = fopen(argv[1],"w");

  fprintf(out,"%% Automatically generated, do not edit \n");
  fprintf(out,"\\setlongtables\n");
  fprintf(out,"\\begin{longtable}{|ll|}\n");
  fprintf(out,"\\caption{\\go escapes summary}\\label{howitworks:esc}\\\\ \n");
  fprintf(out,"\\hline\n");
  fprintf(out,"Escape:type&Summary\\\\\n");
  fprintf(out,"\\hline\n");
  fprintf(out,"\\endfirsthead\n");
  fprintf(out,"\\multicolumn{2}{c}{\n");
  fprintf(out,"{Table \\ref{howitworks:esc} \\go escapes summary (cont.)}}\\\\\n");
  fprintf(out,"\\hline\n");
  fprintf(out,"Escape:type&Summary\\\\\n");
  fprintf(out,"\\hline\n");
  fprintf(out,"\\endhead\n");
  fprintf(out,"\\hline\\multicolumn{2}{r}{\\small\\emph{continued\\ldots}}\\\n");
  fprintf(out,"\\endfoot\n");
  fprintf(out,"\\hline\n");
  fprintf(out,"\\endlastfoot\n");
#include "../Headers/escapes.h"
  fprintf(out,"\\end{longtable}\n");
  fclose(out);
  exit(0);
}


static char *skipSig(char *tp)
{
  assert(tp!=NULL);
  
  switch(*tp++){
  case integer_sig:
  case float_sig:
  case number_sig:
  case char_sig:
  case symbol_sig:
  case string_sig:
  case top_sig:
  case logical_sig:
  case opaque_sig:
  case type_sig:
    return tp;
  case list_sig:
    return skipSig(tp);
  case funct_sig:{
    int ar = *tp++;
    
    while(ar-->0)
      tp = skipSig(tp+1);
    return skipSig(tp);
  }
  case grammar_sig:{
    int ar = *tp++;
    
    while(ar-->0)
      tp = skipSig(tp+1);
    return skipSig(tp);
  }
  case proc_sig:{
    int ar = *tp++;
    
    while(ar-->0)
      tp = skipSig(tp+1);
    return tp;
  }

  case action_sig:{
    int ar = *tp++;
    
    while(ar-->0)
      tp = skipSig(tp+1);
    return tp;
  }
  case var_sig:
    return tp+1;
  case forall_sig:
    return skipSig(skipSig(tp+1));
  case poly_sig:{
    char delim = *tp++;
    
    while(*tp++!=delim)
      ;
    
    int ar = *tp++;
    
    while(ar-->0)
      tp = skipSig(tp);
    return tp;
  }
  case face_sig:{
    int ar = *tp++;                     // the number of elements in the interface
    
    while(ar-->0){
      tp = skipSig(tp);
    }
    return tp;
  }
    
  default:
    return NULL;
  }
}

static int sigArity(char *spec)
{
  switch(*spec){
    case funct_sig:
      return (int)(spec[1])+1;
    case proc_sig:
      return (int)(spec[1]);
    case action_sig:
      return (int)(spec[1]);
    case grammar_sig:
      return (int)(spec[1])+2;
    case forall_sig:
      return sigArity(skipSig(spec+2));
    default:
      assert(False);
      return 0;
  }
}

static char *genOpType(FILE *f,char *tp);

static char *genArgTypes(FILE *f,char *tp)
{
  int ar = *tp++;		/* pick up the arity */
  if(ar>0){
    char *sep = "";

    fprintf(f,"[");
    while(ar--){
      fprintf(f,"%s",sep);
      sep = ", ";
      tp = genOpType(f,tp);
    }
    fprintf(f,"]");
  }
  return tp;
}


static char *genPredTypeArgs(FILE *f,char *tp)
{
  int ar = *tp++;		/* pick up the arity */
  char *sep = "";
  fprintf(f,"[");
  while(ar--){
    char Mode = *tp++;
    fprintf(f,"%s",sep);
    sep = ",";

    tp = genOpType(f,tp);

    assert(Mode=='+'||Mode=='-'||Mode=='?');

    switch(Mode){
    case '+':
      fprintf(f,"+");
      break;
    case '-':
      fprintf(f,"-");
      break;
    case '?':
      fprintf(f,"-+");
      break;
    }
  }
  fprintf(f,"]");
  return tp;
}

static char *genOpType(FILE *f,char *tp)
{
  switch(*tp++){
    case integer_sig:
      fprintf(f,"integer");
      return tp;
    case float_sig:
      fprintf(f,"float");
      return tp;
    case number_sig:
      fprintf(f,"number");
      return tp;
    case char_sig:
      fprintf(f,"char");
      return tp;
    case symbol_sig:
      fprintf(f,"symbol");
      return tp;
    case string_sig:
      fprintf(f,"string");
      return tp;
    case top_sig:
      fprintf(f,"top");
      return tp;
    case logical_sig:
      fprintf(f,"logical");
      return tp;
    case type_sig:
      fprintf(f,"type");
      return tp;
    case opaque_sig:
      fprintf(f,"opaque");
      return tp;
    case list_sig:
      fprintf(f,"list[");
      tp = genOpType(f,tp);
      fprintf(f,"]");
      return tp;

    case funct_sig:
      tp = genPredTypeArgs(f,tp);
      fprintf(f,"=>");
      return genOpType(f,tp);
    
    case proc_sig:
      tp = genPredTypeArgs(f,tp);
      fprintf(f,"\\{\\}");
      return tp;
    
    case grammar_sig:
      tp = genPredTypeArgs(f,tp);
      fprintf(f,"-->");
      return genOpType(f,tp);

    case action_sig:
      tp = genPredTypeArgs(f,tp);
      fprintf(f,"*");
      return tp;

    case var_sig:
      fprintf(f,"t\\sub{%d}",*tp++);
      return tp;
      
    case forall_sig:{
      char *sep = "";
    
      fprintf(f,"[");
      tp--;
      while(*tp==forall_sig){
        tp++;
        fprintf(f,"%st\\sub{%d}",sep,*tp++);
        tp = skipSig(tp);
        sep = ",";
      }
      fprintf(f,"]-(");
      tp = genOpType(f,tp);
      fprintf(f,")");
      return tp;
    }
    
    case poly_sig:{
      char delim = *tp++;
      char buffer[32];
      int pos = 0;

      tp = strchr(tp,'#')+1;

      while(*tp!='#' && *tp!=delim)
        buffer[pos++]=*tp++;

      buffer[pos]='\0';

      while(*tp!=delim)
        tp++;
        
      if(strcmp(buffer,",")==0){
        fprintf(f,"(");
        tp = genOpType(f,tp+2);
        fprintf(f,",");
        tp = genOpType(f,tp);
        fprintf(f,")");
        return tp;
      }
      else{
        int i;
        for(i=0;buffer[i]!='\0';i++)
        fprintf(f,"%s",pC(buffer[i]));
        return genArgTypes(f,tp+1);
      }
    }
    
    default:
      printf("Problem in generating opcode type: %s\n",tp);
      exit(11);
  }
}

static char *pC(char c)
{
  static char buff[10];
  
  if(c=='_'||c=='#'){
    buff[0]='\\';
    buff[1]=c;
    buff[2]='\0';
  }
  else if(c=='\\')
    strcpy(buff,"\\char'134");          // This is the pesky backslash character
  else{
    buff[0]=c;
    buff[1]='\0';
  }
  return buff;
}
    

static char *pS(char *s,char delim)
{
  static char buff[1024];
  char *p = buff;
  
  while(*s!=delim){
    if(*s=='_'||*s=='#'){
      *p++='\\';
      *p++=*s++;
    }
    else if(*s=='\\'){
      *p++='\\'; *p++='c'; *p++='h'; *p++='a'; *p++='r'; ; *p++='\''; *p++='1'; *p++='3'; *p++='4';
      s++;
    }
    else
    *p++=*s++;
  }
  *p++='\0';
  return buff;
}
  
static void genEsc(FILE *out,char *name,logical priv,int code,char *tp,char *cmt)
{  
  fprintf(out,"\\parbox{3.25in}{\\tt\\small %s:",pS(name,'\0'));
  genOpType(out,tp);
  fprintf(out,"}&\\parbox{1.75in}{\\small{}%s\\hfil}\\\\\n",pS(cmt,'\0'));
  fprintf(out,"\\hline\n");
}


