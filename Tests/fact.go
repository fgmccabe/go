/*
 * Factorial
 */
 
fact{
  import go.io.
  import go.unit.

  fact:[integer]=>integer.
  fact(0) => 1.
  fact(N)::N>0 => N*fact(N-1).

  facttest:[]@=harness.
  facttest<=harness.
  facttest..{
    doPred() :-
        fact(10)=3628800.
    errorAction() ->
        fact(-1)=_.
  }.
   
   main(_) ->
      checkUnit(facttest).
 }.
