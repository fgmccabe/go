-- Test the sync action

sync{
  import go.io.
  import go.unit.

  account <~ ( balance:[]=>float. update:[float]* ).

  account:[float]@>account.
  account(I)..{
    bal:float:=I.
  
    balance() => valof{
                   sync{
                     valis bal
                   }
                 }.
  
    update(K) ->
        sync{
          bal := bal+K
        }.

    show()=>"Account balance: "<>bal.show().
  }.

  synctest:[]@=harness.
  synctest<=harness.
  synctest..{
    doAction() ->
        AC = account(0.0);
        spawn{th1(AC)};
        spawn{th2(AC)};
        spawn{th1(AC)};
        spawn{th2(AC)};
        sync(AC){
          AC.balance()>500.0 -> stdout.outLine("Balance now = "<>AC.balance().show())
        }.
    
    th1:[account]*.
    th1(AC) ->
        K = rand(10.0);
        stdout.outLine(__thread().show()<>": updating ac with "<>K.show());
        AC.update(K);delay(rand(0.1));
        th1(AC).
    
    th2:[account]*.
    th2(AC) ->
        stdout.outLine(__thread().show()<>": current val of ac is "<>
                       AC.balance().show());
        delay(rand(0.11));
        th2(AC).
  }.
    
  main(_) ->
      checkUnit(synctest).
}.
