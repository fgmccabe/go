/*
 * S E N D + M O R E = M O N E Y
 */

money{
  import go.unit.
  import go.io.

  add:[integer,integer,integer,integer,integer]{}.
  add(I,X,Y,0,Z)::I+X+Y<10 :-- Z=I+X+Y.
  add(I,X,Y,1,Z) :-- Z=I+X+Y-10.

  pick:[list[t],t,list[t]]{}.
  pick([X,..Y],X,Y).
  pick([X,..Y],A,[X,..Z]) :- pick(Y,A,Z).

  solve:[list[integer],list[integer],list[integer]]{}.
  solve([S,E,N,D],[M,O,R,E],[M,O,N,E,Y]) :-
    M=1, C4=1, R9 = [0,2,3,4,5,6,7,8,9],
    pick(R9,S,R8),
    pick(R8,O,R7),
    pick(R7,E,R6),
    pick(R6,N,R5),
    pick(R5,R,R4),
    pick(R4,D,R3),
    S>0,
    C3 in [0,1],
    O = S+M+C3-10*C4,
    C2 in [0,1],
    N = E+O+C2-10*C3,
    C1 in [0,1],
    R = 10*C2+E-N-C1,
    Y = D+E-10*C1,
    pick(R3,Y,_).

  solveQ:[list[integer],list[integer],list[integer]]{}.
  solveQ([S,E,N,D],[M,O,R,E],[M,O,N,E,Y]) :-
    M=1, C4=1, R9 = [0,2,3,4,5,6,7,8,9],
    pick(R9,S,R8),
    S>0,
    C3 in [0,1],
    O = S+M+C3-10*C4,
    pick(R8,O,R7),
    pick(R7,E,R6),
    C2 in [0,1],
    N = E+O+C2-10*C3,
    pick(R6,N,R5),
    C1 in [0,1],
    R = 10*C2+E-N-C1,
    pick(R5,R,R4),
    pick(R4,D,R3),
    Y = D+E-10*C1,
    pick(R3,Y,_).

  testmoney:[]@=harness.
  testmoney<=harness.
  testmoney..{
    doAction() ->
        {solve(A,B,C)};
        stdout.outLine(A.show()<>"+"<>B.show()<>"="<>C.show()).
        
    doPred() :-
        solve(A,B,C),
        solveQ(U,V,W),
        A=U,
        B=V,
        C=W.
  }.

  main(_) ->
      checkUnit(testmoney).
}.


  
