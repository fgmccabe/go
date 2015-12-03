/*
  Install standard escapes into Go! system
  (c) 2000 F.G.McCabe

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Contact: Francis McCabe <frankmccabe@mac.com>

  $Id: escapes.c,v 1.2 2004/04/29 16:24:27 fmccabe Exp $
  $Log: escapes.c,v $
  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system

 */
#include "config.h"		/* pick up standard configuration header */
#include <string.h>		/* String functions */
#include "go.h"
#include "dict.h"
#include "process.h"
#include "hash.h"		/* access the hash functions */
#include "signature.h"          // Access the signature definitions
#include "esc.h"

#define MAXESC 1024		/* initial size of the escape hash table */

#ifdef EXECTRACE
logical traceEscapes=False;	/* Trace escape calls */
#endif

typedef struct {
  funpo escape_code;		/* What is the escape function code? */
  logical priv;			/* Execute in privilidged mode only? */
  int arity;
  char *name;			/* Name of this escape function */
} EscapeTemplate, *escpo;

static hashPo escapes;		/* The hash table of escape functions */

static EscapeTemplate escFuns[MAXESC]; /* The escape tables themselves */

static void install_escape(char *escape_fn,funpo escape_code,int code,
			   logical pr,char *spec);

#undef escape
#define escape(name,cname,code,secr,pr,spec,cmnt) {\
  extern retCode cname(processPo,ptrPo); \
  install_escape(name,cname,code,pr,spec);\
};

#undef constr
#define constr(A,B,C)

#undef tdf
#define tdf(A,S,B)


/* Set up the escape table */
// This must be executed before any threads are spawned off.
void install_escapes(void)
{
  int i;

  /* set up the escapes */
  escapes = NewHash(MAXESC,NULL,NULL,NULL);

  for(i=0;i<MAXESC;i++)
    escFuns[i].escape_code = NULL; /* Clear the table */

#include "escapes.h"
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
  case proc_sig:{
    int ar = *tp++;
    
    while(ar-->0)
      tp = skipSig(tp+1);
    return tp;
  }
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

/* Install a symbol into the procedure escapes table */
static void install_escape(char *escape_fn,funpo escape_code,int code,
			   logical pr,char *spec)
{
  escpo e = (escpo)Search(escape_fn,escapes);

  if(e==NULL && escFuns[code].escape_code==NULL){ /* Verify escape unused */
    e = &escFuns[code];
    e->escape_code = escape_code;
    e->priv = pr;
    e->name = strdup(escape_fn);
    e->arity = sigArity(spec);
    
    Install(e->name,e,escapes);
  }
}

funpo escapeCode(unsigned int code)
{
  return escFuns[code].escape_code;
}

char *escapeName(int code)
{
  if(code<0 || code>=MAXESC)
    return "unknown";
  else
    return escFuns[code].name;
}

int escapeOpcode(char *name)
{
  escpo e = (escpo)Search(name,escapes);
  if(e==NULL)
    return -1;
  else
    return e-escFuns;
}

funpo getescape(symbPo name)
{
  escpo e = (escpo)Search(name,escapes);
  if(e==NULL)
    return NULL;
  else
    return e->escape_code;
}

logical validEscape(unsigned int code,int arity)
{
  if(code<0 || code>=NumberOf(escFuns))
    return False;
  else if(escFuns[code].escape_code==NULL || escFuns[code].arity!=arity)
    return False;
  else
    return True;
}

void showEscape(processPo P,long code,ptrPo args,long arity)
{
  int i;
  uniChar buffer[1024];
  ioPo out = openBufferStr(buffer,NumberOf(buffer),utf16Encoding);

  outMsg(out,"%w: ",&P->proc.thread);
  outMsg(out,"%s(",escapeName(code));

  for(i=0;i<arity;i++)
    outMsg(out,"%w%s",args++,(i<arity-1?",":""));

  outMsg(out,")\n%_");

  long len;
  uniChar *text = getStrText(O_STRING(out),&len);

  outText(logFile,text,len);
  closeFile(out);
}

/* garbage collection function to handle type structures of escape funs */
void ScanEscapes(void)
{
}

void markEscapes(void)
{
}
