arith{
  /*
   * Test some of the arithmetic functions of the Go! language
   */

  import go.io.
  import go.unit.

  arithtest:[]@=harness.
  arithtest<=harness.
  arithtest..{
    plusOk:[]{}.
    plusOk() :-
	1+2=3,
	1.2+2.3=3.5,
	1.0+3.4=4.4.
    doPred() :-
	plusOk().
  }.

  main(_) ->
      checkUnit(arithtest).
}