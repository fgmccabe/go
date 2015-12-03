valtest{
  import go.io.
  import go.unit.

  -- Test some features of action/istrue

  val:[symbol]{}.
  val(k) :-
      action{
        stdout.outLine("Trying Ok route");
        istrue k=='Ok';
        stdout.outLine("Exiting k= "<>k.show())
      }.

  testval:[symbol]@=harness.
  testval(_)<=harness.
  testval(Ok)..{
    doPred() :-
        val(Ok).
    doPred() :-
        action{
          stdout.outLine("doPred failed, as expected")
        }.
  }.

  main([]) ->
      checkUnit(testval('Ok'));
      checkUnit(testval('not Ok'))
}
  
