dynchurn{
  import go.io.
  import go.stdparse.
  import go.dynamic.
  import go.unit.

  churn:[integer,integer]@>harness.
  churn(_,_) <= harness.
  churn(ThCount,LnCount)..{
    XX:dynamic[list[integer]] = dynamic([]).

    run:[integer]*.

    run(0)->stdout.outLine("XX left at "<>XX.ext().show()).
    run(C)::even(C) ->
	XX.delc(:dynTest[list[integer]]..{
		  check(L) :-
		      action{
			stdout.outLine("Testing element: "<>L.show())
		      },
		      listlen(L) rem 3==0.0.
		});
	run(C-1).
    run(C) ->
	XX.add(iota(1,C));
	stdout.outLine("XX now "<>XX.ext().show());
	run(C-1).

    even:[integer]{}.
    even(X) :- X rem 2==0.0.
  
    spawner:[integer,integer]=>list[thread].
    spawner(0,_)=>[].
    spawner(N,F)=>[spawn{run(N*F quot 2)},..spawner(N-1,F)].
    
    doAction() ->
	Th in spawner(ThCount,LnCount) *> waitfor(Th).
  }.

  main([]) -> checkUnit(churn(10,5)).

  main([C,F,.._]) -> checkUnit(churn(naturalOf%%C,naturalOf%%F)).
}      