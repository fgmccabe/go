err{
  import go.io.

  foo:[integer]{}.
  foo(0) :- raise error("This is an error",'eTEST').
  foo(_).

  errExp:[]=>integer.
  errExp() => raise error("This is a bad expression",'eTEST').

  main(_) ->
      { foo(errExp() onerror (
	     error(A,B) => 23))} onerror (
       E -> stdout.outLine("We had the error: "<>E.show())
      ).
}
