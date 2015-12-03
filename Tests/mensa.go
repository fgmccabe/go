/*
 * Some mensa-style tests
 */
mensa{
  import go.unit.
  import go.io.

  sub:[integer,integer,integer,integer,integer]{}.
  sub(B,X,Y,0,Z)::X-B>Y :-- Z=X-B-Y.
  sub(B,X,Y,1,Z) :-- Z=X+10-B-Y.

  pick:[list[t],t,list[t]]{}.
  pick([X,..Y],X,Y).
  pick([X,..Y],A,[X,..Z]) :- pick(Y,A,Z).

  solve:[list[integer],list[integer],list[integer]]{}.
  solve([A,N,T,E],[E,T,N,A],[N,E,A,T]) :-
      R10 = [1,2,3,4,5,6,7,8,9],
      pick(R10,A,R9),
      pick(R9,E,R8),
      sub(0,E,A,B1,T),
      pick(R8,T,R7),
      pick(R7,N,R6),
      sub(B1,T,N,B2,A),
      sub(B2,N,T,B3,E),
      sub(B3,A,E,0,N).

  divtest:[integer]{}.
  divtest(N) :-
      N in iota(10,99),
      imod(N*2+1,3)=0,
      imod(N*3+1,4)=0,
      imod(N*4+1,5)=0.

  testmensa:[]@=harness.
  testmensa<=harness.
  testmensa..{
    doAction() ->
        (solve(A,B,C) *>
	 stdout.outLine(A.show()<>"-"<>B.show()<>"="<>C.show()));
	(divtest(N) *>
	 stdout.outLine(N.show()<>"is what you want")).
    
  }.

  main(_) ->
      checkUnit(testmensa).
}.




