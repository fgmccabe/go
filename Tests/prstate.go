/*
 * Test process state etc
 */
prstate{
  import go.io.
  import go.cell.
  import go.unit.
  import go.stdparse.

  Sem:cell[integer] = cell(0).		 -- only used as a sync point

  pr:[]*.
  pr() ->
      sync(Sem){
        T = rand(2);
        stdout.outLine("Got the Semaphore for "<>T.show()<>" seconds");
        delay(T)
      };
      pr().

  report:[list[thread]]*.
  report(Ts) ->
      T in Ts *>
      stdout.outLine("Thread "<>T.show()<>" in state "<>thread_state(T)^).

  contReport:[list[thread],number]*.
  contReport(Ts,D) ->
      report(Ts);
      delay(D);                         -- delay for a short while
      contReport(Ts,D).

  failure:[number]*.
  failure(D) ->
      delay(D);                         -- wait for a short period
      stdout.outLine("going ..");
      raise error("Im out of here",'eNONE').

  prstate:[float]@=harness.
  prstate(_)<=harness.
  prstate(T)..{
    doAction() ->
        T1 = spawn{pr()};
        T2 = spawn{pr()};
        T3 = spawn{failure(2)};
        spawn{stdout.outLine("reporting");contReport([T1,T2,T3],0.5)};
        delay(T).
  }.

  main([]) ->
      checkUnit(prstate(5.0)).
  main([T]) ->
      checkUnit(prstate(floatOf%%T)).
}
      
      

  