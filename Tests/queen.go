/*
 * 8 queens
 */

queen{
  import go.io.
  import go.unit.
  import go.stdparse.

  pick:[list[t],t,list[t]]{}.
  pick([X,..Y],X,Y).
  pick([X,..Y],A,[X,..Z]) :- pick(Y,A,Z).

  check:[integer,integer,list[integer]]{}.
  check(_,_,[]).
  check(R,D,[X,..Y]) :- R+D!=X,R-D!=X,check(R,D+1,Y).

  queen:[list[integer],list[integer]]{}.
  queen([],[]).
  queen(L,[X,..B]) :-
    pick(L,X,L1),
    queen(L1,B),
    check(X,1,B).

  qtest:[integer]@=harness.
  qtest(_)<=harness.
  qtest(N)..{
    doAction() ->
	start = 1;
--	( queen(iota(start,N),Bd) *>
--	  stdout.outLine("Board = "<>showBoard(Bd))).
        stdout.outLine("Boards=\n"<>showSet({Bd || queen(iota(start,N),Bd)})).

    doPred() :-
        queen(iota(1,N),Bd),
        ( I in iota(1,N) *> I in Bd).
  }.
    
  showPos:[integer]=>string.
  showPos(1) => " Q".
  showPos(N) => [` ,` ,..showPos(N-1)].
  
  showBoard:[list[integer]]=>string.
  showBoard([]) => "".
  showBoard([N,..R]) => showPos(N)<>"\n"<>showBoard(R).
  
  showSet:[list[list[integer]]]=>string.
  showSet([]) => "".
  showSet([B,..R]) => B.show()<>":\n"<>showBoard(B)<>"\n"<>showSet(R).
    
  main([]) -> 
      checkUnit(qtest(8)).
  main([N]) ->
      checkUnit(qtest(naturalOf%%N)).
}.
  
