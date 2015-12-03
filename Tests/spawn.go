spawn{
  import go.io.
  import go.stdparse.

  sleepForEver:[]*.
  sleepForEver() ->
      stdout.outLine("Hello, world.");
      delay(1); 
      sleepForEver().

  sleepSome:[integer]*.
  sleepSome(T) -> 
      delay(T);
      stdout.outLine(__thread().show()<>"done sleeping").

  spawnMany:[integer]*.
  spawnMany(C) ->
      P in reverse({ spawn{sleepSome(N)} .. N in iota(1,C) }) *>
      ( stdout.outLine("waiting for "<>P.show());waitfor(P)).

  main([C,.._]) ->
      spawnMany(naturalOf%%C).

}
