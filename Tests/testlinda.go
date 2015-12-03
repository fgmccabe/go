testlinda {
  import go.io.
  import go.dynamic.
  import go.unit.
  import go.stdparse.

  -- Linda
  linda[T]<~ dynamic[T].
  linda[T] <~{replace:[T,T]*. delw:[T]*. memw:[T]{}. notw:[T]{}}.

  linda:[list[T]]@>linda[T].

  linda(Ext) <= dynamic(Ext). 
  linda(_)..{
   replace(Trm1,Trm2) ->
       -- self=hdl(_,Nm);
       sync{del(Trm1);add(Trm2)}.
   delw(Term:T) ->
	sync{mem(Term)-> del(Term)}.
   memw(Term) :-
	action {sync{ mem(Term) -> {}}}.
        
   notw(Term) :-
	action {sync{ \+ mem(Term)-> {}}}.
  }.

  -- Agent Linda
  agentlinda[T] <~ linda[T].
  agentlinda[T] <~ { 
	modTime:[]=>integer. 
	wait_for_update_after:[integer]*
      }.
  agentlinda:[symbol,list[B]]@>agentlinda[B].

  agentlinda(_,Init)<=linda(Init).
  agentlinda(Name,_)..{
    updateTime:integer := 0.
    modTime() => valof { sync { valis updateTime } }.
    add(B) -> 
	sync {
	  linda.add(B);
	  updateTime := updateTime + 1
	}.
    del(B) -> 
	sync {
	  linda.del(B);
	  updateTime := updateTime + 1
	}.
    delall(BP) -> 
	sync {
	  linda.delall(BP);
	  updateTime := updateTime + 1
	}.
    mem(B) :-  
	linda.mem(B).
    replace(B1,B2) -> sync {
	  linda.replace(B1,B2);
	  updateTime := updateTime + 1
	}.
    wait_for_update_after(Time) ->
	sync { updateTime > Time -> {} }. 
 }.

  spawnThreads : [agentlinda[integer], integer] => list[thread].
  spawnThreads(_, 0) => [].
  spawnThreads(bel, n) =>
      [spawn {
	  stdout.outLine("t" <> n.show() <> " waiting");
	  bel.wait_for_update_after(0);
	  stdout.outLine("t" <> n.show() <> " awake")
       },.. spawnThreads(bel, n-1)].


  waitThreads : [list[thread]]*.
  waitThreads([])->{}.
  waitThreads([t,..ts])->waitfor(t);waitThreads(ts).

  lindaTest:[integer]@=harness.
  lindaTest(_)<=harness.
  lindaTest(Count)..{
    doAction() ->
	bel = agentlinda('beliefs', []);
	threads = spawnThreads(bel, Count);
	spawn{ delay(0.3); stdout.outLine("Adding to bel"); bel.add(1) };
	waitThreads(threads).
  }.

  -- Main
  main(_) ->
      checkUnit(lindaTest(10)).
  main([C,.._]) ->
      checkUnit(lindaTest(naturalOf%%C)).
}