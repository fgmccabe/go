/* Test the various date & time functions */

date{
  import go.io.
  import go.datelib.
  
  main([A]) ->
      N = now();
      D = time2date(N);
      stdout.outLine("Time is "<>N^0<>" date = "<>D.show());
      U = time2utc(N);
      stdout.outLine("UTC equivalent = "<>U.show());
      stdout.outLine("Converted back = "<>D.time()^0);
      stdout.outLine("Converted back (UTC) = "<>U.time()^0);
      ( (rfc822_date(R)-->A) ?
          stdout.outLine("Parsed argument is "<>R.show())
      | stdout.outLine("Couldn't parse "<>A)
      ).
}

  
