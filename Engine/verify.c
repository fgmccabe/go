/*
  Code verifier module -- check that a code segment satisfies basic 
  sanity constraints.
  (c) 1994-2001 Imperial College and F.G. McCabe

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

  $Id: verify.c,v 1.3 2004/05/26 18:26:35 fmccabe Exp $
  $Log: verify.c,v $
  Revision 1.3  2004/05/26 18:26:35  fmccabe
  Added more line number info, added keys to hash table

  Revision 1.2  2004/04/29 16:24:28  fmccabe
  Completely new type system

*/

#include "config.h"		/* pick up standard configuration header */
#include "go.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "opcodes.h"
#include "pool.h"
#include "debug.h"
#include "esc.h"

#define MAXLOCALS 65535		    /* Maximum number of local variables */
#define MAXCODE 1024            /* Maximum number of code fragments */

typedef struct {
  logical inited;		/* True if cell has real value */
  logical read;			/* Has this cell been read? */
} Var, *varPo;

typedef struct _segment_ *segPo;

typedef struct _segment_ {
  Var args[GO_REGS];
  int lCount;                           /* number of locals in use */
  varPo locals;				/* entry state for this segment */
  unsigned int arity;			/* Arity of the code */
  unsigned int strcount;	      /* number of valid structure references */
  unsigned int litCount;		/* number of literals */
  unsigned int lclHp;		      /* how much local heap can we allocate? */
  unsigned int gblHp;		     /* how much global heap can we allocate? */
  logical inited;	     /* Has the segment's variables been initialized? */
  logical checked;			/* Has this segment been checked? */
  codePo cde;			      /* Pointer to the code structure itself */
  insPo pc;				/* base intruction of this segment */
  unsigned long insCount;	       /* No. of instructions in this segment */
  segPo prev;				/* previous physical segment */
  segPo next;				/* next physical segment */
  segPo alt;
  unsigned long entryPoints;	     /* how many entry points are there here? */
} segRecord;

typedef struct {
  segPo *table;			  /* vector of ptrs into the list of segments */
  unsigned int top;			/* current top of the vector */
  unsigned int size;			/* current size of the vector */
} segVect, *segVectPo;

#ifdef VERIFYTRACE
extern logical traceVerify;
static void showSegs(segPo root);
#endif

static poolPo segpool = NULL;

static segPo initVerify(codePo cde,unsigned long length)
{
  if(segpool==NULL)
    segpool = newPool(sizeof(segRecord),16);
    
  {
    segPo seg = (segPo)allocPool(segpool);
    unsigned int i;
    
    for(i=0;i<NumberOf(seg->args);i++){
      seg->args[i].inited = False;
      seg->args[i].read = False;
    }
    
    seg->cde = cde;
    seg->pc = codeIns(cde);
    seg->arity = codeArity(cde);
    seg->insCount = length;
    seg->next = seg->prev = NULL;
    seg->alt = NULL;
    seg->entryPoints = 1;
    seg->checked = False;
    seg->inited = False;
    seg->locals = NULL;
    seg->lCount = 0;
    seg->strcount = 0;
    seg->litCount = 0;
    return seg;
  }
}

static void clearSegs(segPo seg)
{
  while(seg!=NULL){
    segPo next = seg->next;
    
    if(seg->locals!=NULL)
      free(seg->locals);
    
    freePool(segpool,seg);
    
    seg = next;
  }
}

#undef instruction
#define instruction(Op,Cde,A1,A2,Cmt)\
    case Op:\
      ret=testBreak(seg,pc,pcx,A1);\
      if(ret==NULL)\
        ret=testBreak(seg,pc,pcx,A2);\
      continue;

static char *testBreak(segPo seg,insPo pc,insWord pcx,opAndSpec A);

static char *splitPhase(insPo pc,long length,segPo seg)
{
  insPo limit = pc+length;
  char *ret = NULL;
  
  while(ret==NULL && pc<limit){
    insWord pcx = *pc++;
    switch(op_cde(pcx)){
#define NO_CLEAR_INS
#include "instructions.h"
    default:
      return "invalid instruction";
    }
  }
  
#ifdef VERIFYTRACE
  if(traceVerify)
    showSegs(seg);
#endif

  return ret;
}

static segPo findSeg(segPo seg,insPo pc)
{
  if(pc<seg->pc){
    while(seg!=NULL && pc<seg->pc)
      seg = seg->prev;
  }
  else{
    while(seg!=NULL && pc>=seg->pc+seg->insCount)
      seg = seg->next;
  }
  
  if(seg==NULL || pc<seg->pc || pc>=seg->pc+seg->insCount)
    return NULL;
  return seg;
}


static segPo splitSeg(segPo root,insPo pc,insPo tgt,logical isAlt)
{
  segPo base = findSeg(root,pc);
  segPo seg = findSeg(root,tgt);
  
  if(tgt==seg->pc){
    seg->entryPoints++;
    if(isAlt){
      assert(base->alt==NULL);
      base->alt=seg;
    }

    return seg;                  // Nothing to do here
  }
  else{
    segPo new = (segPo)allocPool(segpool);
    segPo next = seg->next;
    unsigned int i;
    
    *new = *seg;                  // Copy everything across
    
    seg->insCount = tgt-seg->pc;
    if(isAlt){
      assert(base->alt==NULL);
      base->alt = new;
    }
    for(i=0;i<NumberOf(seg->args);i++){
      new->args[i].inited = True;
      new->args[i].read = False;
    }
    new->insCount-=seg->insCount;
    new->entryPoints = 1;
    new->checked = False;
    new->inited = False;
    new->locals = NULL;
    new->lCount = 0;
    new->strcount = 0;
    new->litCount = root->litCount;
    new->alt=NULL;
    new->pc=tgt;
    new->prev = seg;
    new->next = next;
    new->cde = seg->cde;
    seg->next=new;
    if(next!=NULL)
      next->prev=new;
    
    return new;
  }
}

#ifdef VERIFYTRACE

static void showSegs(segPo root)
{
  int segNo = 0;
  segPo seg = root;
  
  while(seg!=NULL){
    if(seg->alt!=NULL){
      int sN = 0;
      segPo s = root;
      
      while(s!=NULL && s->pc<seg->alt->pc){
        s=s->next;
        sN++;
      }
      assert(s==seg->alt);
      
      outMsg(logFile,"%d (%d) [0x%x-0x%x](%d), alt=%d [0x%x]\n",
             segNo,seg->entryPoints,seg->pc,seg->pc+seg->insCount,seg->insCount,sN,s->pc);
    }
    else
      outMsg(logFile,"%d (%d) [0x%x-0x%x](%d):\n",segNo,seg->entryPoints,seg->pc,seg->pc+seg->insCount,seg->insCount);
    segNo++;
    
    assert(seg->next==NULL || seg->pc+seg->insCount==seg->next->pc);
    
    seg = seg->next;
  }
  flushFile(logFile);
}

static void showSeg(segPo seg)
{
  unsigned int i;
  unsigned int max = NumberOf(seg->args)-1;
  
  outMsg(logFile,"(%d) [0x%x-0x%x](%d) ",seg->entryPoints,seg->pc,seg->pc+seg->insCount,seg->insCount);
  
  while(max>0 && !seg->args[max].inited)
    max--;
  
  for(i=0;i<=max;i++)
    outMsg(logFile," A[%d]%s",i,seg->args[i].inited?"*":"");
  if(seg->locals!=NULL)
    for(i=1;i<=seg->lCount;i++)
      outMsg(logFile," Y[%d]%s",i,seg->locals[i].inited?"*":"");
  outMsg(logFile,"\n");
  flushFile(logFile);
}

#endif

static char *testBreak(segPo seg,insPo pc,insWord pcx,opAndSpec A)
{
  switch(A){
  case nOp:                             // No operand
  case iAh:                             // input argument register in upper slot (0..255)
  case oAh:                             // output argument register in upper slot (0..255)
  case iAm:                             // input argument register in middle slot (0..255)
  case iAl:                             // input argument register in lower slot (0..255)
  case oAl:                             // input argument register in lower slot (0..255)
  case oAm:                             // output argument register in middle slot (0..255)
  case iLh:                             // input local variable offset (0..255)
  case iLm:                             // input local variable offset (0..255)
  case iLl:                             // input local variable offset (0..255)
  case iLc:                             // input local variable offset (0..65535)
  case oLh:                             // output local variable offset (0..255)
  case oLm:                             // output local variable offset (0..255)
  case oLl:                             // output local variable offset (0..255)
  case oLc:                             // output local variable offset  (0..65535)
  case iSt:                             // input at current structure pointer
  case oSt:                             // output to current structure pointer
  case uAr:                             // Arity in upper slot
  case oAr:                             // Arity in upper slot
  case uLt:                             // small literal in upper slot (-128..127)
  case Lt:                              // 16bit literal (-32768..32767)
  case vSz:                             // Size of local variable vector
  case lSz:                             // Size of local variable vector
  case cSz:				// Structure size
  case Es:                              // escape code (0..65535)
  case ltl:                             // literal offset (0..65535)
    return NULL;
  case pcr:{                            // program counter relative offset (-32768..32767)
    segPo alt = splitSeg(seg,pc-1,pc+op_o_val(pcx),True);
    if(alt==NULL || splitSeg(seg,pc-1,pc,False)==NULL)
      return "invalid destination address";
    else
      return NULL;
  }
  case pcl:{                            // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
    segPo alt = splitSeg(seg,pc-1,pc+op_ll_val(pcx),True);
    if(alt==NULL || splitSeg(seg,pc-1,pc,False)==NULL)
      return  "invalid destination address";
    else
      return NULL;
  }
  default:
      return "Invalid instruction operand specification";
  }
}

#undef instruction
#define instruction(Mn,O,A1,A2,Cmt)\
  case Mn:\
    msg = checkInstruction(seg,oPc,pc,pcx,A1,A2);\
    continue;
    
static char *checkInstruction(segPo seg,insPo opc,insPo pc,insWord pcx,opAndSpec A1,opAndSpec A2);


static char *checkSegment(segPo seg)
{
  insPo pc = seg->pc;
  insPo limit = pc+seg->insCount;
  char *msg = NULL;
  
#ifdef VERIFYTRACE
  if(traceVerify){
    outMsg(logFile,"On entry to segment:\n");
    showSeg(seg);
    showInstructions(seg->cde,seg->pc,seg->insCount);
  }
#endif

  seg->checked = True;
  
  while(msg==NULL&&pc<limit){
    insPo oPc = pc;
    insWord pcx = *pc++;
    
    switch(op_cde(pcx)){
#include "instructions.h"
    default:
      return "illegal instruction";
    }
  }
#ifdef VERIFYTRACE
  if(traceVerify){
    outMsg(logFile,"After checking segment:\n");
    showSeg(seg);
  }
#endif

  return msg;
}

static int noOfSegments(segPo root)
{
  int i;
  
  for(i=0;root!=NULL;i++,root=root->next)
    ;
  return i;
}

static char *mergeSegVars(segPo seg,segPo next)
{
  unsigned int i;
  
  if(next->locals==NULL && seg->locals!=NULL){
    next->lCount = seg->lCount;
    next->locals = (varPo)malloc(sizeof(Var)*(next->lCount+1));
          
    for(i=0;i<=next->lCount;i++)
      next->locals[i] = seg->locals[i];
  }
  else if(next->lCount>seg->lCount)
    return "improper reallocation of locals";
  else if(seg->locals!=NULL){
    for(i=0;i<=next->lCount;i++){
      next->locals[i].inited &= seg->locals[i].inited;
      next->locals[i].read |= seg->locals[i].read;
    }
  }
  for(i=0;i<NumberOf(seg->args);i++){
    next->args[i].inited &= seg->args[i].inited;
    next->args[i].read |= seg->args[i].read;
  }
  
  return NULL;
}

static char *checkSegments(segPo root)
{
  int count = noOfSegments(root);
  segPo seg,stack[count];
  int top = 0;
  
  stack[top++]=root;
  
  while(top>0){
    segPo seg = stack[--top];
    char *msg = checkSegment(seg);
    
    if(msg!=NULL)
      return msg;

    if(seg->next!=NULL){
      segPo next = seg->next;
      
      if(!next->checked){
        if((msg=mergeSegVars(seg,next))!=NULL)
          return msg;

        if(--next->entryPoints==0)
          stack[top++]=next;
      }
    }
    if(seg->alt!=NULL){
      segPo alt = seg->alt;
      
      if(!alt->checked){
        if((msg=mergeSegVars(seg,alt))!=NULL)
          return msg;

        if(--alt->entryPoints==0)
          stack[top++]=alt;
      }
    }
  }
  
  for(seg=root;seg!=NULL;seg=seg->next)
    if(!seg->checked)
      return "unreachable segment";
  return NULL;
}

static char *checkInOperand(segPo seg,insPo pc,insWord pcx,opAndSpec A)
{
  switch(A){
  case nOp:                             // No operand
    return NULL;
  case iAh:{                            // input argument register in upper slot (0..255)
    int regNo = op_h_val(pcx);          // Pick up input register name
      
    if(regNo>GO_REGS || !seg->args[regNo].inited)
      return "attempted to access unset argument register";
    else
      seg->args[regNo].read=True;
    return NULL;
  }
  case oAh:                             // output argument register in upper slot (0..255)
    return NULL;
  case iAm:{                            // input argument register in middle slot (0..255)
    int regNo = op_m_val(pcx);          // Pick up input register name
      
    if(regNo>GO_REGS || !seg->args[regNo].inited)
      return "attempted to access unset argument register";
    else
      seg->args[regNo].read=True;
    return NULL;
  }
  case oAm:                             // output argument register in middle slot (0..255)
    return NULL;
  case iAl:{                            // input argument register in lower slot (0..255)
    int regNo = op_l_val(pcx);          // Pick up input register name
      
    if(regNo>GO_REGS || !seg->args[regNo].inited)
      return "attempted to access unset argument register";
    else
      seg->args[regNo].read=True;
    return NULL;
  }
  case oAl:                             // output argument register in lower slot (0..255)
    return NULL;
  case iLh:{                            // variable in upper slot
    unsigned int lcNo = op_h_val(pcx);           // Pick up local variable
      
    if(seg->lCount<lcNo || !seg->locals[lcNo].inited)
      return "attempted to access unset variable";
    else
      seg->locals[lcNo].read=True;
    return NULL;
  }
  case iLm:{                            // variable in midle slot
    unsigned int lcNo = op_m_val(pcx);           // Pick up local variable
      
    if(seg->lCount<lcNo || !seg->locals[lcNo].inited)
      return "attempted to access unset variable";
    else
      seg->locals[lcNo].read=True;
    return NULL;
  }
  case iLl:{                            // variable in lower slot
    unsigned int lcNo = op_l_val(pcx);           // Pick up local variable
      
    if(seg->lCount<lcNo || !seg->locals[lcNo].inited)
      return "attempted to access unset variable";
    else
      seg->locals[lcNo].read=True;
    return NULL;
  }
  case iLc:{                            // variable in local variable (0..65535)
    unsigned int lcNo = op_o_val(pcx);           // Pick up local variable
      
    if(seg->lCount<lcNo || !seg->locals[lcNo].inited)
      return "attempted to access unset variable";
    else
      seg->locals[lcNo].read=True;
    return NULL;
  }
  case oLh:				/* variable in upper slot */
  case oLm:				/* variable in middle slot */
  case oLl:				/* variable in lower slot */
  case oLc:                             // output local variable offset  (0..65535)
    return NULL;

  case iSt:                             // input at current structure pointer
  case oSt:                             // output to current structure pointer
    if(seg->strcount--<0)
      return "too many accesses to structures";
    return NULL;
  case uAr:{                            // Arity in upper slot
    int regNo = op_h_val(pcx);          // Pick up register name
    int i;
    
    for(i=1;i<=regNo;i++)
      if(!seg->args[i].inited)
        return "uninitialized argument";
    return NULL;
  }
  case oAr:				/* Output arity in upper slot */
  case uLt:                             // small literal in upper slot (-128..127)
  case Lt:                              // 16bit literal (-32768..32767)
  case vSz:                             // Size of local variable vector
  case cSz:             		        // Structure size
    return NULL;

  case lSz:{                            // Size of local variable vector
    /*    unsigned int i;
    int count = op_so_val(pcx);

    
        if(count>0){
      if(seg->locals==NULL)
        return "locals not allocated yet";
      else{
        for(i=1;i<=count;i++){
          if(!seg->locals[i].inited){
            static char msg[256];
      
            sprintf(msg,"local Y[%d/%d] not initialized",i,count);
            return msg;
          }
        }
      }
    }
    */
    return NULL;
  }

  case Es:{                             // escape code (0..65535)
    unsigned int esc = op_o_val(pcx);
      
    if(!validEscape(esc,op_h_val(pcx))){
      static char msg[256];
      
      sprintf(msg,"invalid escape code [%s/%d]",escapeName(esc),op_h_val(pcx));
      return msg;
    }
    return NULL;
  }
  case pcr:                             // program counter relative offset (-32768..32767)
  case pcl:                             // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
    return NULL;
  case ltl:{                            // literal number (0..65535)
    unsigned int lit = op_o_val(pcx);
      
    if(seg->litCount<=lit)
      return "attempted to access invalid literal";
    return NULL;
  }
  default:
    return "Problem in checking opcode type";
  }
}

static char *checkOutOperand(segPo seg,insPo pc,insWord pcx,opAndSpec A)
{
  switch(A){
  case nOp:                             // No operand
  case iAh:                             // input argument register in upper slot (0..255)
    return NULL;
  case oAh:{                            // output argument register in upper slot (0..255)
    int regNo = op_h_val(pcx);          // Pick up input register name
      
    if(regNo>GO_REGS )
      return "attempted to set non-existent register";
    else
      seg->args[regNo].inited=True;
    return NULL;
  }
  case iAm:                             // input argument register in middle slot (0..255)
    return NULL;
  case oAm:{                            // output argument register in middle slot (0..255)
    int regNo = op_m_val(pcx);          // Pick up input register name
      
    if(regNo>GO_REGS)
      return "attempted to set non-existent register";
    else
      seg->args[regNo].inited=True;
    return NULL;
  }
  case iAl:                             // input argument register in lower slot (0..255)
    return NULL;
  case oAl:{                            // output argument register in lower slot (0..255)
    int regNo = op_l_val(pcx);          // Pick up input register name
      
    if(regNo>GO_REGS)
      return "attempted to set non-existent register";
    else
      seg->args[regNo].inited=True;
    return NULL;
  }
  case iLh:                             // input local variable upper slot
  case iLm:                             // input local variable middle slot
  case iLl:                             // input local variable lower slot
  case iLc:                             // input local variable offset (0..65535)
    return NULL;
  case oLh:{                            // output local variable upper slot
    unsigned int lcNo = op_h_val(pcx);           // Pick up local variable
      
    if(seg->lCount<lcNo)
      return "attempted to set out of bounds variable";
    else
      seg->locals[lcNo].inited=True;
    return NULL;
  }
  case oLm:{                            // output local variable middle slot
    unsigned int lcNo = op_m_val(pcx);           // Pick up local variable
      
    if(seg->lCount<lcNo)
      return "attempted to set out of bounds variable";
    else
      seg->locals[lcNo].inited=True;
    return NULL;
  }
  case oLl:{                            // output local variable lower slot
    unsigned int lcNo = op_l_val(pcx);           // Pick up local variable
      
    if(seg->lCount<lcNo)
      return "attempted to set out of bounds variable";
    else
      seg->locals[lcNo].inited=True;
    return NULL;
  }
  case oLc:{                            // output local variable offset  (0..65535)
    unsigned int lcNo = op_o_val(pcx);           // Pick up local variable
      
    if(seg->lCount<lcNo)
      return "attempted to set out of bounds variable";
    else
      seg->locals[lcNo].inited=True;
    return NULL;
  }
  case iSt:                             // input at current structure pointer
  case oSt:                             // output to current structure pointer
    return NULL;
  case uAr:{                            // Arity in upper slot
    int regNo = op_h_val(pcx);          // Pick up register name
    int i;

    for(i=regNo+1;i<GO_REGS;i++)
      seg->args[i].inited=False;
    return NULL;
  }
  case oAr:{                            // Arity in upper slot
    int regNo = op_h_val(pcx);          // Pick up register name
    int i;

    for(i=1;i<=regNo;i++)
      seg->args[i].inited=True;

    for(i=regNo+1;i<GO_REGS;i++)
      seg->args[i].inited=False;
    return NULL;
  }

  case uLt:                             // small literal in upper slot (-128..127)
  case Lt:                              // 16bit literal (-32768..32767)
    return NULL;
  case vSz:{                            // Size of local variable vector
    unsigned int i;
    int count = seg->lCount = op_so_val(pcx);

    if(count>=0){
      if(seg->locals!=NULL)
        seg->locals = (varPo)realloc(seg->locals,sizeof(Var)*(count+1));
      else
        seg->locals = (varPo)malloc(sizeof(Var)*(count+1));
      
      for(i=0;i<=count;i++){
        seg->locals[i].inited=False;
        seg->locals[i].read=False;
      }
    }

    return NULL;
  }
  case lSz:                            // Size of local variable vector
    return NULL;

  case cSz:             		        // Structure size
    seg->strcount = op_o_val(pcx);
    return NULL;
  case Es:                              // escape code (0..65535)
  case pcr:                             // program counter relative offset (-32768..32767)
  case pcl:                             // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
  case ltl:                             // literal number (0..65535)
    return NULL;
  default:
    return "Problem in checking opcode type";
  }
}

static char *checkInstruction(segPo seg,insPo opc,insPo pc,insWord pcx,opAndSpec A1,opAndSpec A2)
{
  char *ret;

  ret = checkInOperand(seg,pc,pcx,A1);
  
  if(ret==NULL)
    ret = checkInOperand(seg,pc,pcx,A2);
  if(ret==NULL)
    ret = checkOutOperand(seg,pc,pcx,A1);
  if(ret==NULL)
    ret = checkOutOperand(seg,pc,pcx,A2);
    
  // We have to hack the special aspects for now
  switch(op_cde(pcx)){
  case dealloc: case dlkawlO: case dlkawl:
    seg->lCount = 0;
    free(seg->locals);
    seg->locals = NULL;
  case trycl: case tryme:
  case retry: case trust: case retryme: case trustme:{
    unsigned int i;
    for(i=1;i<=seg->arity;i++){
      seg->args[i].inited = True;
      seg->args[i].read = False;
    }
    break;
  }
  case vdYY:
  case clYY:{
    unsigned int i;
    unsigned int low = op_o_val(pcx);
    unsigned int hi = low+op_h_val(pcx);
    
    for(i=low;i<hi;i++)
      seg->locals[i].inited=True;
    break;
  }
  case vdAA:{
    unsigned int i;
    unsigned int low = op_h_val(pcx);
    unsigned int hi = low+op_o_val(pcx);
    
    for(i=low;i<hi;i++)
      seg->args[i].inited=True;
    break;
  }
  default:
    ;
  }
  
#ifdef VERIFYTRACE
  if(traceVerify && ret!=NULL){
    outMsg(logFile,"Problem@%x: %s in instruction:\n",opc,ret);
    showInstructions(seg->cde,opc,1);
    flushFile(logFile);
  }
#endif
  
  return ret;
}

static pthread_mutex_t verifyMutex = PTHREAD_MUTEX_INITIALIZER;

retCode verifyCode(ptrI prog)
{
  pthread_mutex_lock(&verifyMutex);	/* We synchronize all verification */

  codePo cde = codeV(prog);
  int i;
  segPo segs = initVerify(cde,codeInsCount(cde));
  char *msg;
  int arity = segs->arity = codeArity(cde);
                  
  for(i=1;i<=arity;i++){
    segs->args[i].inited = True;
    segs->args[i].read = False;
  }
  
  segs->litCount = codeLitCount(cde);
  
#ifdef VERIFYTRACE
  if(traceVerify){
    outMsg(logFile,"Verify code: #%d\n",arity);
    showInstructions(cde,segs->pc,segs->insCount);
  }
#endif

  msg = splitPhase(codeIns(cde),codeInsCount(cde),segs);

  if(msg==NULL)
    msg=checkSegments(segs);
    
  clearSegs(segs);

  pthread_mutex_unlock(&verifyMutex);	/* We can now allow others to verify */
  
  if(msg==NULL)
    return Ok;
  else{
    logMsg(logFile,"Problem in loading code %s",msg);
    return Error;
  }
}

