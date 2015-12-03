/* Generate an April module, that knows about escape codes */

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "logical.h"
#include "signature.h"


#undef escape
#define escape(name,fun,code,priv,secr,type,cmt) genEsc(out,name,code,type,cmt);

#undef constr
#define constr(name,K,type) 

#undef tdf
#define tdf(name,sp,type) 

static void genEsc(FILE *out,char *name,int code,char *tp,char *cmt);
static char *linesep = "     ";

int main(int argc,char **argv)
{

  if(argc<2){
    printf("Expecting output file name\n");
    exit(1);
  }
  else{
    FILE *out = fopen(argv[1],"w");

    fprintf(out,"/* Automatically generated, do not edit */\n");
    fprintf(out,"module\n{\n");

    fprintf(out,"  escCode(X) => { case X in {     -- return escape and arity\n");

#include "escapes.h"

    fprintf(out,"    | _ => (-1,-1)              -- dummy for non-escapes\n");
    fprintf(out,"  }};                        -- end of escCode\n");
    fprintf(out,"} export escCode             -- end of generated module\n");
    fclose(out);
    exit(0);
  }
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
      tp = skipSig(tp);
    return skipSig(tp);
  }
  case grammar_sig:{
    int ar = *tp++;
    
    while(ar-->0)
      tp = skipSig(tp);
    return skipSig(tp);
  }
  case proc_sig:
  case action_sig:{
    int ar = *tp++;
    
    while(ar-->0)
      tp = skipSig(tp);
    return tp;
  }
  case tuple_sig:{
    int ar = *tp++;
    
    while(ar-->0)
      tp = skipSig(tp);
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

static int tpArity(char *tp)
{
  switch(*tp){
  case funct_sig:
    return tp[1]+1;
  case proc_sig:
  case action_sig:
    return tp[1];
  case grammar_sig:
    return tp[1]+2;
  case forall_sig:
    return tpArity(skipSig(tp+2));
  default:
    printf("Cant compute arity of %s\n",tp);
    exit(1);
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
  fprintf(out,"%s\"%s\" => (%d,%d)\t\t-- %s\n",linesep,pS(name,'\0'),code,tpArity(tp),cmt);
  linesep = "    |";
}
