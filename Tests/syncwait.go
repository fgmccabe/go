/* Test the conditional sync with three threads -- two will fire but block and the third
   will enable the other two to run
*/
syncwait{
  import go.io.
  import go.stdparse.
  import go.unit.
  import go.cell.

  test:[cell[t],t]{}.
  test(C,X) :-
      action{ stdout.outLine(C^<>" has value "<>C.get()^) }, C.get()==X.
  
  waiter:[cell[symbol]]*.
  waiter(C) ->
      sync(C){
        test(C,'yes') ->
            stdout.outLine("I can go")
      }.

  doer:[cell[t]]*.
  doer(C) ->
      delay(1);
--      stdout.outLine("setting to yes");
      C.set('yes').
  
  check:[]@=harness.
  check<=harness.
  check..{
    doAction() ->
        C = cell('no');
        spawn{ doer(C) };
        T0 = spawn{ waiter(C) };
        waiter(C);
        waitfor(T0).
  }.

  main(_) ->
      checkUnit(check).
}
