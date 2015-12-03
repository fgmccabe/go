race{
  -- This is a little race - between threads
  import go.io.
  import go.stdparse.

  racer:[integer]*.
  racer(0)->{}.
  racer(N)::N rem 1000 == 0.0 -> 
      stdout.outLine(__thread().show()<>" at "<>N.show());
      racer(N-1).
  racer(N) ->
      racer(N-1).

  main([N,C,.._]) ->
      NN = naturalOf%%N;
      CC = naturalOf%%C;
      ( T in { spawn{racer(NN)} .. _ in iota(1,CC) } *>
	waitfor(T) );
      stdout.outLine("That was fun!").
  main([]) ->
      main(["100000","100"]).
}