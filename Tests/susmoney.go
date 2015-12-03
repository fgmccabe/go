/*
 * S E N D + M O R E = M O N E Y
 * Version using variable suspensions
 */

susmoney{
  import go.unit.
  import go.io.

  pick:[list[t],t,list[t]]{}.
  pick([X,..Y],X,Y).
  pick([X,..Y],A,[X,..Z]) :- pick(Y,A,Z).

  solve:[list[integer],list[integer],list[integer]]{}.
  solve([S,E,N,D],[M,O,R,E],[M,O,N,E,Y]) :-
      M=1, C4=1, R9 = [0,2,3,4,5,6,7,8,9],
      pick(R9,S@@S>0,R8),
      C3 in [0,1],
      C2 in [0,1],
      C1 in [0,1],
      pick(R8,O@@O = S+M+C3-10*C4,R7),
      pick(R7,E,R6),
      pick(R6,N@@N = E+O+C2-10*C3,R5),
      pick(R5,R@@R = 10*C2+E-N-C1,R4),
      pick(R4,D,R3),
      pick(R3,Y@@Y = D+E-10*C1,_).

  testmoney:[]@=harness.
  testmoney<=harness.
  testmoney..{
    doAction() ->
        {solve(A,B,C)};
        stdout.outLine(A.show()<>"+"<>B.show()<>"="<>C.show()).
  }.

  main(_) ->
      checkUnit(testmoney).
}.
