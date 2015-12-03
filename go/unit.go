/*
 * unit testing harness
 */
go.unit{
  import go.io.

  harness <~ { doAction:[]*. doPred:[]{}. errorAction:[]* }.

  harness:[]@=harness.
  harness..{
    doAction()->{}.
    doPred().
    errorAction()->raise error("",'').
  }.

  checkAction:[harness]*.
  checkAction(T:harness) ->
      ( T.doAction()
        onerror(
         E -> stdout.outLine(E.show()<>" in action test "<>T.show()); exit(1)
        )
      ).

  errorAction:[harness]*.
  errorAction(T:harness) ->
      ( ( T.errorAction(); 
          stdout.outLine(T.show()<>" should have failed");
          exit(10)
        )
        onerror(
         _ -> {}
        )
      ).

  checkPred:[harness]*.
  checkPred(T:harness) ->
      ( { 
          T.doPred()
        | action{ stdout.outLine("predicate test "<>T.show()<>" failed"); exit(1)}
        }
        onerror(
         E -> stdout.outLine(E.show()<>" in predicate test "<>T.show()); exit(1)
        )
      ).

  checkUnit:[harness]*.
  checkUnit(T:harness) ->
      stdout.outLine("starting unit "<>T.show());
      checkAction(T);
      checkPred(T);
      errorAction(T);
      stdout.outLine("unit "<>T.show()<>" checks out ok").
}


