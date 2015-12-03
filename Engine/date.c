/*
  Time and date functions for the Go! system 
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

  $Id: date.c,v 1.2 2004/04/29 16:24:27 fmccabe Exp $
  $Log: date.c,v $
  Revision 1.2  2004/04/29 16:24:27  fmccabe
  Completely new type system

 */
 
#include "config.h"		/* pick up standard configuration header */
#include <signal.h>
#include <time.h>
#include <math.h>
#include "go.h"
#include "process.h"
#include "clock.h"
#include "symbols.h"

/*
 * Implementation of time related escapes 
 */

/* Convert a time to a date:-
   time=>(dow,day,mon,year,hour,min,sec,utc,zone)
*/

#define DATE_DOW 1
#define DATE_DAY 2
#define DATE_MON 3
#define DATE_YEAR 4
#define DATE_HOUR 5
#define DATE_MIN 6
#define DATE_SEC 7
#define DATE_UTC 8
#define DATE_ZONE 9
#define DATE_LEN 9
#define DATE_TIME 9

retCode g_tval2date(processPo P, ptrPo a)
{
  ptrI T = deRefI(&a[1]);
  
  if(isvar(T))
    return liberror(P,"__time2date",eINSUFARG);
  else{
    time_t when = (isInteger(objV(T))?integerVal(intV(T)):floor(floatVal(floatV(T))));
    number fraction = 0;
    struct tm *now = localtime((time_t*)&when);
    ptrI val = kvoid;
    retCode ret = Ok;
    rootPo root = gcAddRoot(&P->proc.heap,&val);

    if(isFloat(objV(T))){
      number ff;
      fraction = modf(floatVal(floatV(T)),&ff);
    }

    val = allocateInteger(&P->proc.heap,now->tm_year+1900); /* Current year */
    ret = equal(P,&val,&a[DATE_YEAR+1]);

    if(ret==Ok){
      val = allocateInteger(&P->proc.heap,now->tm_mon+1); /* Current month */
      ret = equal(P,&val,&a[DATE_MON+1]);
    }

    if(ret==Ok){
      val = allocateInteger(&P->proc.heap,now->tm_mday); /* Day of the month */
      ret = equal(P,&val,&a[DATE_DAY+1]);
    }

    if(ret==Ok){
      val = allocateInteger(&P->proc.heap,now->tm_wday);        // Day of the week
      ret = equal(P,&val,&a[DATE_DOW+1]);
    }

    if(ret==Ok){
      val = allocateInteger(&P->proc.heap,now->tm_hour); /* Hour in the day */
      ret = equal(P,&val,&a[DATE_HOUR+1]);
    }

    if(ret==Ok){
      val = allocateInteger(&P->proc.heap,now->tm_min); /* Minutes in the hour */
      ret = equal(P,&val,&a[DATE_MIN+1]);
    }

    if(ret==Ok){
      if(fraction!=0.0)
        val = allocateFloat(&P->proc.heap,now->tm_sec+fraction);
      else
        val = allocateInteger(&P->proc.heap,now->tm_sec); /* Seconds in the minutes */
    
      ret = equal(P,&val,&a[DATE_SEC+1]);
    }

    if(ret==Ok){
#ifdef HAVE_TM_ZONE
      val = allocateInteger(&P->proc.heap,now->tm_gmtoff);     // UTC offset
#else
      val = allocateInteger(&P->proc.heap,0); // dummy
#endif
      ret = equal(P,&val,&a[DATE_UTC+1]);
    }

    if(ret==Ok){
#ifdef HAVE_TM_ZONE
      val = allocateCString(&P->proc.heap,now->tm_zone);       // Time zone
#else
      val = allocateInteger(&P->proc.heap,0); // dummy
#endif
      ret = equal(P,&val,&a[DATE_ZONE+1]);
    }

    gcRemoveRoot(&P->proc.heap,root);
    return ret;
  }
}

/* Convert a time to a utc date:-
   time=>date(year,month,day,hours,mins,seconds)
*/
retCode g_tval2utc(processPo P, ptrPo a)
{
  ptrI T = deRefI(&a[1]);
  
  if(isvar(T))
    return liberror(P,"time2utc",eINSUFARG);
  else{
    time_t when = (isInteger(objV(T))?integerVal(intV(T)):floor(floatVal(floatV(T))));
    number fraction = 0;
    struct tm *now = gmtime((time_t*)&when);
    ptrI val = kvoid;
    rootPo root = gcAddRoot(&P->proc.heap,&val);
    retCode ret = Ok;
    
    if(isFloat(objV(T))){
      number ff;
      fraction = modf(floatVal(floatV(T)),&ff);
    }
    
    gcAddRoot(&P->proc.heap,&val);

    val = allocateInteger(&P->proc.heap,now->tm_year+1900); /* Current year */
    ret = equal(P,&val,&a[DATE_YEAR+1]);

    if(ret==Ok){
      val = allocateInteger(&P->proc.heap,now->tm_mon+1); /* Current month */
      ret = equal(P,&val,&a[DATE_MON+1]);
    }

    if(ret==Ok){
      val = allocateInteger(&P->proc.heap,now->tm_mday); /* Day of the month */
      ret = equal(P,&val,&a[DATE_DAY+1]);
    }

    if(ret==Ok){
      val = allocateInteger(&P->proc.heap,now->tm_wday);        // Day of the week
      ret = equal(P,&val,&a[DATE_DOW+1]);
    }

    if(ret==Ok){
      val = allocateInteger(&P->proc.heap,now->tm_hour); /* Hour in the day */
      ret = equal(P,&val,&a[DATE_HOUR+1]);
    }

    if(ret==Ok){
      val = allocateInteger(&P->proc.heap,now->tm_min); /* Minutes in the hour */
      ret = equal(P,&val,&a[DATE_MIN+1]);
    }

    if(ret==Ok){
      if(fraction!=0.0)
        val = allocateFloat(&P->proc.heap,now->tm_sec+fraction);
      else
        val = allocateInteger(&P->proc.heap,now->tm_sec); /* Seconds in the minutes */
    
      ret = equal(P,&val,&a[DATE_SEC+1]);
    }

    if(ret==Ok){
#ifdef HAVE_TM_ZONE
      val = allocateInteger(&P->proc.heap,now->tm_gmtoff);     // UTC offset
#else
      val = allocateInteger(&P->proc.heap,0); // dummy
#endif
      ret = equal(P,&val,&a[DATE_UTC+1]);
    }

    if(ret==Ok){
#ifdef HAVE_TM_ZONE
      val = allocateCString(&P->proc.heap,now->tm_zone);       // Time zone
#else
      val = allocateInteger(&P->proc.heap,0); // dummy
#endif
      ret = equal(P,&val,&a[DATE_ZONE+1]);
    }

    gcRemoveRoot(&P->proc.heap,root);
    return ret;
  }
}

   
// Construct a time value from a date structure

#define DAY_ARG 1
#define MON_ARG 2
#define YEAR_ARG 3
#define HOUR_ARG 4
#define MIN_ARG 5
#define SEC_ARG 6
#define UTC_ARG 7
#define TIME_ARG 8


retCode g_date2tval(processPo P, ptrPo a)
{
  struct tm now;
  time_t when;
  ptrI El;
  number fraction = 0.0;
    
  if(!isvar(El=deRefI(&a[YEAR_ARG])) && isInteger(objV(El)))
    now.tm_year = integerVal(intV(El))-1900; /* Extract the year */
  else
    return liberror(P,"__date2time",eINVAL); /* Invalid inverse */
      
  if(!isvar(El=deRefI(&a[MON_ARG])) && isInteger(objV(El)))
    now.tm_mon = integerVal(intV(El))-1; /* Extract the month */
  else
    return liberror(P,"__date2time",eINVAL); /* Invalid inverse */
      
  if(!isvar(El=deRefI(&a[DAY_ARG])) && isInteger(objV(El)))
    now.tm_mday = integerVal(intV(El));          /* Extract the day of the month */
  else
    return liberror(P,"__date2time",eINVAL);              /* Invalid inverse */
      
  if(!isvar(El=deRefI(&a[HOUR_ARG])) && isInteger(objV(El)))
    now.tm_hour = integerVal(intV(El));           /* Extract the hour */
  else
    return liberror(P,"__date2time",eINVAL);              /* Invalid inverse */
      
  if(!isvar(El=deRefI(&a[MIN_ARG])) && isInteger(objV(El)))
    now.tm_min = integerVal(intV(El));           /* Extract the minute */
  else
    return liberror(P,"__date2time",eINVAL);              /* Invalid inverse */
      
  if(!isvar(El=deRefI(&a[SEC_ARG]))){
    if(isInteger(objV(El)))
      now.tm_sec = integerVal(intV(El));           /* Extract the second */
    else if(isFloat(objV(El))){
      number foo;
        
      fraction = modf(floatVal(floatV(El)),&foo);
      now.tm_sec = foo;
    }
  }
  else
    return liberror(P,"__date2time",eINVAL);              /* Invalid inverse */
      
  if(!isvar(El=deRefI(&a[UTC_ARG])) && isInteger(objV(El)))
#ifdef HAVE_TM_ZONE
    now.tm_gmtoff = integerVal(intV(El));           /* Extract the gm offset */
#else
  ;
#endif
  else
    return liberror(P,"__date2time",eINVAL);              /* Invalid inverse */

  now.tm_isdst = -1;		                /* dont know about daylight savings */

  when = mktime(&now);

  {
    ptrI T = allocateFloat(&P->proc.heap,when+fraction);

    return equal(P,&T,&a[TIME_ARG]);
  }
}


// Construct a time value from a date structure

retCode g_utc2tval(processPo P, ptrPo a)
{
#if HAVE_TIMEGM
  struct tm now;
  time_t when;
  ptrI El;
  number fraction = 0.0;
    
  if(!isvar(El=deRefI(&a[DATE_YEAR+1])) && isInteger(objV(El)))
    now.tm_year = integerVal(intV(El))-1900; /* Extract the year */
  else
    return liberror(P,"__utc2time",eINVAL); /* Invalid inverse */
      
  if(!isvar(El=deRefI(&a[DATE_MON+1])) && isInteger(objV(El)))
    now.tm_mon = integerVal(intV(El))-1; /* Extract the month */
  else
    return liberror(P,"__utc2time",eINVAL); /* Invalid inverse */
      
  if(!isvar(El=deRefI(&a[DATE_DAY+1])) && isInteger(objV(El)))
    now.tm_mday = integerVal(intV(El));          /* Extract the day of the month */
  else
    return liberror(P,"__utc2time",eINVAL);              /* Invalid inverse */
      
  if(!isvar(El=deRefI(&a[DATE_HOUR+1])) && isInteger(objV(El)))
    now.tm_hour = integerVal(intV(El));           /* Extract the hour */
  else
    return liberror(P,"__utc2time",eINVAL);              /* Invalid inverse */
      
  if(!isvar(El=deRefI(&a[DATE_MIN+1])) && isInteger(objV(El)))
    now.tm_min = integerVal(intV(El));           /* Extract the minute */
  else
    return liberror(P,"__utc2time",eINVAL);              /* Invalid inverse */
      
  if(!isvar(El=deRefI(&a[DATE_SEC+1]))){
    if(isInteger(objV(El)))
      now.tm_sec = integerVal(intV(El));           /* Extract the second */
    else if(isFloat(objV(El))){
      number foo;
        
      fraction = modf(floatVal(floatV(El)),&foo);
      now.tm_sec = foo;
    }
  }
  else
    return liberror(P,"__utc2time",eINVAL);              /* Invalid inverse */
      
  if(!isvar(El=deRefI(&a[DATE_UTC+1])) && isInteger(objV(El)))
#ifdef HAVE_TM_ZONE
    now.tm_gmtoff = integerVal(intV(El));           /* Extract the minute */
#else
  ;
#endif
  else
    return liberror(P,"__utc2time",eINVAL);              /* Invalid inverse */

  now.tm_isdst = -1;		                /* dont know about daylight savings */

  when = timegm(&now);

  {
    ptrI T = allocateFloat(&P->proc.heap,when+fraction);

    return equal(P,&T,&a[DATE_LEN+1]);
  }
#else
  return liberror(P,"__utc2time",eNOTFND);
#endif
}
