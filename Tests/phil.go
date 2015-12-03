/* 
 * This is a version of the dining philosophers problem
 */
 
phil{
  import go.io.
  import go.unit.
  import go.stdparse.
  import go.setlib.
  import go.mbox.

  semaphore <~ { get:[]*. put:[]* }.

  semaphore:[integer]@>semaphore.
  semaphore(count)..{
    R:integer := count.

    get() -> sync(this){
          R>0 -> R := R-1; 
	      stdout.outLine(__thread().show()<>" gets fork"<>show())
        }.

    put() -> sync(this){
          R := R+1; stdout.outLine(__thread().show()<>" releases fork"<>show())
        }.
  }.

  philtest:[integer]@>harness.
  philtest(_)<=harness.
  philtest(limit)..{
    waiter:semaphore = semaphore(3).         -- control the number of seated philosophers
  
    getFork:[semaphore]*.
    getFork(Frk) -> Frk.get().

    putFork:[semaphore]*.
    putFork(Frk) -> Frk.put().

    phil:[string,semaphore,semaphore,integer]*.
    phil(Id,_,_,Count)::Count=<0 -> stdout.outLine(Id<>" leaving the table").
    phil(Id,Lft,Rgt,Count) ->
        delay(rand(1.5));
        stdout.outLine(Id<>" hungry");
        getFork(Lft);
        getFork(Rgt);
        stdout.outLine(Id<>" eating");
        delay(rand(1));
        putFork(Lft);
        putFork(Rgt);
        stdout.outLine(Id<>" stuffed");
        phil(Id,Lft,Rgt,Count-1).
    
    doAction() ->
        F0 = semaphore(1);
        F1 = semaphore(1);
        F2 = semaphore(1);
        F3 = semaphore(1);

        waitforall([spawn{ stdout.outLine("socrates arrived"); phil("socrates",F0,F1,limit)},
                    spawn{ stdout.outLine("plato arrived"); phil("plato",F1,F2,limit)},
                    spawn{ stdout.outLine("kant arrived"); phil("kant",F2,F3,limit)},
                    spawn{ stdout.outLine("russel arrived"); phil("russell",F3,F0,limit)}]).

    waitforall:[list[thread]]*.
    waitforall(Thrs) ->
        Thr in Thrs *>  waitfor(Thr).
  }.

  main([]) ->
      checkUnit(philtest(5)).
  main([T]) ->
      checkUnit(philtest(naturalOf%%T)).
}

