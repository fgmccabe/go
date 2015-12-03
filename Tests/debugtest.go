debugtest{
  import go.io.

  -- This is intended as a kind of test of the debugger

  -- First of all we have a function to debug
  f1:[integer]=>integer.
  f1(0) => 1.
  f1(X) => valof{
	     I = f1(X-1);
	     valis I*X
	   }.

  main(_) ->
      stdout.outLine("f1(7) = "<>f1(7).show()).
}