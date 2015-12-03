/* 
   Statistics measurement for the Go! engine
   (c) 2000 F.G. McCabe

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

  $Id: stats.c,v 1.2 2004/04/29 16:24:28 fmccabe Exp $
  $Log: stats.c,v $
  Revision 1.2  2004/04/29 16:24:28  fmccabe
  Completely new type system

*/

#include "config.h"

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include "go.h"			/* main header file */
#include "opcodes.h"		/* The definitions of the opcodes */
#include "dict.h"		/* Dictionary handling stuff */
#include "process.h"		/* Process handling */
#include "symbols.h"
#include "clock.h"
#include "hash.h"		/* we need access to the hash functions */
#include "debug.h"		/* Debugger access functions */

#ifdef STATSTRACE

long pcCount = 0;
long insCount[256];
long escCount[256];             /* A count of the escapes executed */

logical traceCount=False;	/* if we want to dump instruction counts */

void countEscape(insWord PCX)
{
  escCount[op_o_val(PCX)]++;
}

void countIns(insWord PCX)
{
  insCount[op_cde(PCX)]++;
}

static void sortCounts(long count,long base[],long sorted[])
{
  long i,j;

  for(i=0;i<count;i++)
      sorted[i]=i;

  for(i=0;i<count;i++){ /* simple sort -- only executed once */
    long max = base[sorted[i]];
    long maxI = i;

    for(j=i+1;j<count;j++){
      if(base[sorted[j]]>max){
	max = base[sorted[j]];
	maxI = j;
      }
    }

    if(maxI!=i){
      int swap = sorted[i];
      sorted[i] = sorted[maxI];
      sorted[maxI] = swap;
    }
  }
}

#undef instruction
#define instruction(Op,Cd,A1,A2,Cmt) \
    case Op:                      /* Cmt */\
      outMsg(logFile,#Op ": %d = %5.2f%%\n",insCount[Op], (insCount[Op]/(double)pcCount)*100);\
      break;

#undef escape
#define escape(Nm,Fun,Op,Sc,Priv,Tp,Cmt) \
    case Op:                      /* Cmt */\
      outMsg(logFile,Nm ": %d " Cmt "\n",escCount[Op]);\
      break;
#undef constr
#define constr(Nm,Tp,T)

#undef tdf
#define tdf(Nm,Sp,Tp)

void dumpInsCount(void)
{
  if(traceCount){
    unsigned long i;
    long sorted[1024];		/* used to sort the instruction counts */
    long count = 0;
    
    outMsg(logFile,"%d instructions executed\n",pcCount);

    sortCounts(NumberOf(escCount),escCount,sorted);
    
    outMsg(logFile,"Escape functions\n");

    for(i=0;i<NumberOf(escCount);i++)
      if(escCount[sorted[i]]!=0){
        count+=escCount[sorted[i]];
        
	switch(sorted[i]){
#include "escapes.h"
	default:
	  outMsg(logFile,"unknown[%x]",i);
	}
      }
    
    outMsg(logFile,"%d escapes called\n",count);

    sortCounts(NumberOf(insCount),insCount,sorted);

    for(i=0;i<NumberOf(insCount);i++)
      if(insCount[sorted[i]]!=0){
        switch(sorted[i]){
#include "instructions.h"
        default:
          outMsg(logFile,"unknown[%x]",i);
        }
      }
    outMsg(logFile,"%d instructions executed\n",pcCount);

#ifdef MEMTRACE
    {
      extern long gcCount;
      outMsg(logFile,"%d gc collections\n",gcCount);
    }
#endif

    flushFile(logFile);
  }
}

#endif
