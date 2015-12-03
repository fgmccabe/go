test {
  import go.stdparse.
  import go.datelib.

  subStr([_,..SS], N, L) :: N > 0 => subStr(SS, N-1, L).
  subStr([S,..SS], 0, L) :: L > 0 => [S,..subStr(SS, 0, L-1)].
  subStr(_, 0, 0) => "".

  -- Time point code:
  MaxTimePoint = 100.		 -- 100 is the maximum time point
  BaseDate="20040101000000".

  DateConverter[] <~ {
	toSeconds : (()=>number),
	toDate : (()=>date[])
      }.
  
  DateConvert(Date:string):DateConverter[] .. {

    years   = natural%%subStr(Date, 0, 4).
    months  = natural%%subStr(Date, 4, 2).
    days    = natural%%subStr(Date, 6, 2).
    hours   = natural%%subStr(Date, 8, 2).
    minutes = natural%%subStr(Date, 10, 2).
    seconds = numeric%%subStr(Date, 12, 2).

    toDate() => date(days + 1,	 -- day
		     0,		 -- dow
		     months + 1, -- month
		     years,	 -- year
		     hours,	 -- hour
		     minutes,	 -- min
		     seconds,	 -- seconds
		     0,		 -- utc
		     "GMT"	 -- zone
		).
    toSeconds() => toDate().time() - DateConvert(BaseDate).toDate().time().
  }.

  main(_) ->
      stdout.outLine(DateConvert("20041119000000").toSeconds()^0).
}
      