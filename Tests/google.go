/*
 * WWWDOT - GOOGLE = DOTCOM
 */

google{
  import go.unit.
  import go.io.

  -- single step addition, with explicit carry in and carry out
  add:[integer,integer,integer,integer,integer]{}.
  add(I,X,Y,0,Z)::I+X+Y<10 :-- Z=I+X+Y .
  add(I,X,Y,1,Z) :-- Z=I+X+Y-10.

  -- single step subtraction, with explicit carry in and carry out
  sub:[integer,integer,integer,integer,integer]{}.
  sub(I,X,Y,0,Z) :: X-Y-I>=0 :-- Z=X-Y-I.
  sub(I,X,Y,1,Z) :-- Z=X-Y-I+10.

  -- single step permutation, pick an element and leave the rest
  pick:[list[t],t,list[t]]{}.
  pick([X,..Y],X,Y).
  pick([X,..Y],A,[X,..Z]) :- pick(Y,A,Z).

  -- specify the sum as a test on variables
  solve:[list[integer],list[integer],list[integer]]{}.
  solve([W,W,W,D,O,T],[G,O,O,G,L,E],[D,O,T,C,O,M]) :-
      R10 = [0,1,2,3,4,5,6,7,8,9],      -- start out with all the digits
      pick(R10,T,R9),
      pick(R9,E,R8),
      pick(R8,M,R7),     -- it makes a difference to performance to test quickly
      sub(0,T,E,C1,M),                   -- T-E=M
      pick(R7,O,R6),
      pick(R6,L,R5),
      sub(C1,O,L,C2,O),                  -- O-L-C1=O
      pick(R5,D,R4),
      pick(R4,G,R3),
      pick(R3,C,R2),
      sub(C2,D,G,C3,C),                 -- D-G-C2=C
      pick(R2,W,_),
      sub(C3,W,O,C4,T),                 -- W-O=T
      sub(C4,W,G,0,D).

  -- verify that we got the right answer. Too lazy to do by hand
  -- so, this is long subtraction
  chck:[list[integer],list[integer],list[integer],integer]{}.
  chck([a,..A],[b,..B],[c,..C],cI) :-   -- A*-B*=C*
      ( a-b-cI >= 0 ?                   -- check for carry out
          a-b-cI = c,
          chck(A,B,C,0)
      | a-b-cI+10=c,
        chck(A,B,C,1)
      ).
  chck([],[],[],0).

  check:[list[integer],list[integer],list[integer]]{}.
  check(A,B,C) :-
      chck(reverse(A),reverse(B),reverse(C),0). -- we need to reverse the lists, because long subtraction is RtoL
      
  testgoogle:[]@=harness.
  testgoogle<=harness.                  -- use our test harness
  testgoogle..{
    doAction() ->
        solve(A,B,C) *>
        stdout.outLine(A.show()<>"-"<>B.show()<>"="<>C.show()).
        
    doPred() :-
        solve(A,B,C),
        check(A,B,C).
  }.

  main(_) ->
      checkUnit(testgoogle).
}.
