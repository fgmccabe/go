/*
  A date ond time class
  (c) 2004-2005 F.G. McCabe
 
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
 */
go.datelib{
  import go.stdparse.

  dow ::= monday | tuesday | wednesday | thursday | friday | saturday | sunday.

  date <~ {less:[date]{}.
        time:[]=>float.                      -- raw time
        clock:[]=>(integer,integer,float). -- The time in ordinary time
        date:[]=>(integer,integer,integer,integer).
        tzone:[]=>float.
        zone:[]=>string.
        dow:[]=>dow.
        }.

  dateParse <~ { parse:[date]-->string }.

  dte:[float,integer,integer,integer,integer,
       integer,integer,float,float,string]@=date.

  dte(T,Dow,Dy,Mon,Yr,Hr,Mn,Sc,Utc,Zone)..{
    time()=>T.
    clock()=>(Hr,Mn,Sc).
    date()=>(Dow,Dy,Mon,Yr).
    dow()=>wkDay(Dow).
    tzone()=>Utc/3600.0.
    zone()=>Zone.
    less(T2) :-
        time()<T2.time().

    wkDay:[integer]=>dow.
    wkDay(0)=>sunday.
    wkDay(1)=>monday.
    wkDay(2)=>tuesday.
    wkDay(3)=>wednesday.
    wkDay(4)=>thursday.
    wkDay(5)=>friday.
    wkDay(6)=>saturday.

    show() => flatten([showDow(Dow)," ",showMonth(Mon),"/",Dy^,"/",Yr^,"  ",
                       showHour(Hr),":",Mn^,":",Sc^," ",showAmPm(Hr)]).

    flatten:[list[list[s]]]=>list[s].

    flatten([]) => [].
    flatten([S,..L]) => flt(S,L).
  
    flt:[list[t],list[list[t]]]=>list[t].

    flt([],L) => flatten(L).
    flt([C,..S],L) => [C,..flt(S,L)].

    showMonth:[integer]=>string.
    showMonth(1) => "Jan".
    showMonth(2) => "Feb".
    showMonth(3) => "Mar".
    showMonth(4) => "Apr".
    showMonth(5) => "May".
    showMonth(6) => "Jun".
    showMonth(7) => "Jul".
    showMonth(8) => "Aug".
    showMonth(9) => "Sep".
    showMonth(10) => "Oct".
    showMonth(11) => "Nov".
    showMonth(12) => "Dec".
    
    showDow:[integer]=>string.
    showDow(0) => "Sun".
    showDow(1) => "Mon".
    showDow(2) => "Tue".
    showDow(3) => "Wed".
    showDow(4) => "Thu".
    showDow(5) => "Fri".
    showDow(6) => "Sat".
    
    showHour:[integer]=>string.
    showHour(N)::N<12 => N.show().
    showHour(N) => (N-12).show().
    
    showAmPm:[integer]=>string.
    showAmPm(N)::N<12 => "am".
    showAmPm(_) => "pm".
  }.

  time2date:[float]=>date.
  time2date(N)::__time2date(N,Dow,Day,Mon,Yr,Hr,Mn,Sc,Utc,Zone) => 
      dte(N,Dow,Day,Mon,Yr,Hr,Mn,Sc,Utc,Zone).

  time2utc:[float]=>date.
  time2utc(N)::__time2utc(N,Dow,Day,Mon,Yr,Hr,Mn,Sc,Utc,Zone) =>
      dte(N,Dow,Day,Mon,Yr,Hr,Mn,Sc,Utc,Zone).

  dateOf:[integer,integer,integer,integer,integer,number,number]=>date.
  dateOf(Day,Mon,Yr,Hr,Mn,Sc,Utc)::
	  Time=__date2time(Day,Mon,Yr,Hr,Mn,Sc,Utc*3600),
	  __time2date(Time,nDow,nDay,nMon,nYr,nHr,nMin,nSec,nU,nZ) => 
      dte(Time,nDow,nDay,nMon,nYr,nHr,nMin,nSec,nU,nZ).

  private rfc_parse:[]@=dateParse.
  rfc_parse..{
    parse(Dte) -->  skipWhiteSpace(),(wkday(_)?skipWhiteSpace(),","|""),
        skipWhiteSpace(),day(Day),
        skipWhiteSpace(),month(Mon),
        skipWhiteSpace(),(",",skipWhiteSpace()|""),year(Yr),
        skipWhiteSpace(),time(Hr,Mn,Sc),
        skipWhiteSpace(),zone(Utc,_Z),
        { Time=__date2time(Day,Mon,Yr,Hr,Mn,Sc,Utc),
          __time2date(Time,nDow,nDay,nMon,nYr,nHr,nMin,nSec,nU,nZ),
          Dte = dte(Time,nDow,nDay,nMon,nYr,nHr,nMin,nSec,nU,nZ)}.
    
    wkday:[integer]-->string.
    wkday(1) --> "Mon".
    wkday(2) --> "Tue".
    wkday(3) --> "Wed".
    wkday(4) --> "Thu".
    wkday(5) --> "Fri".
    wkday(6) --> "Sat".
    wkday(0) --> "Sun".
    wkday(1) --> "mon".
    wkday(2) --> "tue".
    wkday(3) --> "wed".
    wkday(4) --> "thu".
    wkday(5) --> "fri".
    wkday(6) --> "sat".
    wkday(0) --> "sun".
  
    month:[integer]-->string.
    month(1) --> "Jan".
    month(2) --> "Feb".
    month(3) --> "Mar".
    month(4) --> "Apr".
    month(5) --> "May".
    month(6) --> "Jun".
    month(7) --> "Jul".
    month(8) --> "Aug".
    month(9) --> "Sep".
    month(10) --> "Oct".
    month(11) --> "Nov".
    month(12) --> "Dec".
    month(1) --> "jan".
    month(2) --> "feb".
    month(3) --> "mar".
    month(4) --> "apr".
    month(5) --> "may".
    month(6) --> "jun".
    month(7) --> "jul".
    month(8) --> "aug".
    month(9) --> "sep".
    month(10) --> "oct".
    month(11) --> "nov".
    month(12) --> "dec".
    
    day:[integer]-->string.
    day(Dy) --> integerOf(Dy).
    
    dig2:[integer]-->string.
    dig2(N) --> [H],{__isNdChar(H)},[L],{__isNdChar(L)},N=__digitCode(H)*10+__digitCode(L).
    
    year:[integer]-->string.
    year(Yr) --> dig2(Y), ([H],{__isNdChar(H)},
                           [L],{__isNdChar(L)},Yr=Y*100+__digitCode(H)*10+__digitCode(L)
                         | Yr = guessYear(Y)).
    
    guessYear:[integer]=>integer.
    guessYear(Y)::Y>30 => 1900+Y.
    guessYear(Y)::Y=<30 => 2000+Y.

    time:[integer-,integer-,integer-]-->string.
    time(Hr,Mn,Sc) --> dig2(Hr),":",dig2(Mn),":",dig2(Sc).
    
    zone:[integer-,string-]-->string.
    zone(U,Z) --> skipWhiteSpace(), z(U,Z)!.

    z:[integer,string]-->string.
    z(0,"UT") --> "UT".
    z(0,"UT") --> "GMT".
    z(-18000,"EST") --> "EST".
    z(-14400,"EDT") --> "EDT".
    z(-21600,"CST") --> "CST".
    z(-18000,"CDT") --> "CDT".
    z(-25200,"MST") --> "MST".
    z(-21600,"MDT") --> "MDT".
    z(-28800,"PST") --> "PST".
    z(-25200,"PDT") --> "PDT".
    z(Off,[`-,..L]) --> "-",offset(L,O),Off=-O.
    z(Off,[`+,..L]) --> "+",offset(L,Off).
    z(_,_) --> raise error("time zone expected",'fail').
    
    offset:[list[char],integer]-->string.
    offset([H1,H2,M1,M2],Off) -->
        [H1],{__isNdChar(H1)},
        [H2],{__isNdChar(H2)},H=__digitCode(H1)*10+__digitCode(H2),
        [M1],{__isNdChar(M1)},
        [M2],{__isNdChar(M2)},M=__digitCode(M1)*10+__digitCode(M2),
        Off=3600*H+60*M.
  }.
  
  /* Parse standard dates */  
  rfc822_date:[date] --> string.
  rfc822_date(Dte) --> 
      rfc_parse.parse(Dte).

}