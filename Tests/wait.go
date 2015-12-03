wait{
  import go.io.
  import go.stdparse.
  import go.unit.


  waiter:[integer]@=harness.
  waiter(_)<=harness.
  waiter(Count)..{
    doAction() ->
        waiting(spLoop(Count)).
  
    spLoop:[integer]=>list[thread].
    spLoop(0) => [].
    spLoop(N) => [spawn {
                    stdout.outLine(__thread().show()<>" starting"); 
		    delay(rand(10));
                    stdout.outLine(__thread().show()<>" ending")
                  },..spLoop(N-1)].
  
    waiting:[list[thread]]*.
    waiting([])-> {}.
    waiting([H,..L]) -> stdout.outLine("waiting for "<>H.show()); waitfor(H); waiting(L).
  }.
  
  main([K]) ->
      checkUnit(waiter(naturalOf%%K)).
  main([]) ->
      checkUnit(waiter(5)).
}.