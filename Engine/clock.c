/*
  Clock and interval timer management for the Go! system 
  (c) 1994-2000 Imperial College and F.G. McCabe

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

  $Id: clock.c,v 1.2 2004/04/29 16:24:27 fmccabe Exp $
  $Log: clock.c,v $
  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system


 */
 
#include "config.h"		/* pick up standard configuration header */
#include <signal.h>
#include <time.h>
#include <math.h>
#include <errno.h>
#include "go.h"
#include "process.h"
#include "clock.h"
#include "symbols.h"

long timezone_offset;		// offset in seconds from GMT

struct timeval initial_time;    // Time when the engine started

void init_time(void)
{
  time_t tloc;
  struct tm *tmptr;

  gettimeofday(&initial_time, NULL);

  tloc = initial_time.tv_sec;
  tmptr = localtime(&tloc);
  tmptr->tm_hour = 0;
  tmptr->tm_min = 0;
  tmptr->tm_sec = 0;

  timezone_offset = mktime(tmptr) - (tloc - tloc % SECSINDAY);
}

/*
 * reset the interval timer for the new period
 */

retCode g_delay(processPo P, ptrPo a)
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"delay",eINSUFARG);
  else{
    objPo A1 = objV(x);

    if(!IsNumber(A1))
      return liberror(P,"delay",eNUMNEEDD);
    else{
      struct timespec tm;
      double seconds;
      double fraction = modf(NumberVal(A1),&seconds);

#define NANO (1000000000)

      tm.tv_sec=(long)seconds;
      tm.tv_nsec=(long)(fraction*NANO);	/* Convert microseconds to nanoseconds */
      switchProcessState(P,wait_timer);
      if(nanosleep(&tm,NULL)!=0){
	setProcessRunnable(P);
	switch(errno){
	case EINTR:
	  return liberror(P,"delay",eINTRUPT);
	case EINVAL:
	case ENOSYS:
	default:
	  return liberror(P,"delay",eINVAL);
	}
      }
      else{
	setProcessRunnable(P);
	return Ok;
      }
    }
  }
}

retCode g_sleep(processPo P, ptrPo a)
{
  ptrI x = deRefI(&a[1]);

  if(isvar(x))
    return liberror(P,"sleep",eINSUFARG);
  else{
    objPo A1 = objV(x);

    if(!IsNumber(A1))
      return liberror(P,"sleep",eNUMNEEDD);
    else{
      number f = NumberVal(A1);
      struct timeval now;
      double seconds;
      double fraction = modf(f,&seconds);

      gettimeofday(&now, NULL);

      if(seconds<now.tv_sec ||
	 (seconds==now.tv_sec && (fraction*1000000)<now.tv_usec))
	return Ok;
      else{
	struct timespec tm;

	tm.tv_sec=(long)seconds;
	tm.tv_nsec=(long)(fraction*NANO);	/* Convert microseconds to nanoseconds */

	tm.tv_sec=(long)seconds-now.tv_sec;
	tm.tv_nsec=(long)(fraction*NANO)-now.tv_usec*1000; /* Convert microseconds to nanoseconds */
	if(tm.tv_nsec>NANO){
	  tm.tv_nsec-=NANO;
	  tm.tv_sec++;
	}
	else if(tm.tv_nsec<0){
	  tm.tv_nsec+=NANO;
	  tm.tv_sec--;
	}

	switchProcessState(P,wait_timer);
	if(nanosleep(&tm,NULL)!=0){
	  setProcessRunnable(P);
	  switch(errno){
	  case EINTR:
	    return liberror(P,"sleep",eINTRUPT);
	  case EINVAL:
	  case ENOSYS:
	  default:
	    return liberror(P,"sleep",eINVAL);
	  }
	}
	else{
	  setProcessRunnable(P);
	  return Ok;
	}
      }
    }
  }
}

/* Return the current time */
retCode g_now(processPo P, ptrPo a)
{
  ptrI x = deRefI(&a[1]);

  if(!isvar(x))
    return liberror(P,"now",eVARNEEDD);
  else{
    ptrI now = allocateFloat(&P->proc.heap,get_time());

    return equal(P,&a[1],&now);
  }
}

/* Return the time at midnight */
retCode g_today(processPo P, ptrPo a)
{
  ptrI x = deRefI(&a[1]);

  if(!isvar(x))
    return liberror(P,"now",eVARNEEDD);
  else{
    ptrI now = allocateInteger(&P->proc.heap,get_date());

    return equal(P,&a[1],&now);
  }
}

/*
 *  returns the current ticks
 */

number get_ticks(void)
{
  return ((number)clock())/CLOCKS_PER_SEC;
}

retCode g_ticks(processPo p, ptrPo a)
{
  ptrI T=allocateFloat(&p->proc.heap,get_ticks());

  return equal(p,&T,&a[1]);
}

/*
 *  returns the current time
 */
number get_time(void)
{
  struct timeval t;

  gettimeofday(&t, NULL);

  return t.tv_sec+t.tv_usec/1.0e6;
}

/*
 *  returns the time at midnight this morning
 */
integer get_date(void)
{
  struct timeval t;

  gettimeofday(&t, NULL);

  t.tv_sec -= t.tv_sec % SECSINDAY;
  return (t.tv_sec + timezone_offset);
}
