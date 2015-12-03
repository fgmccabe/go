pairTest{
  import go.io.

  person <~ {name:string}.
  student <~ person.
  student <~ {college:string}.

  personPair::= pair(person,person).

  person:[string]@=person.
  person(N)..{name=N}.

  student:[string,string]@=student.
  student(N,_) <= person(N).
  student(_,C)..{college=C}.

  r0:[person+]{}.
  r1:[person-,person-]{}.
  r11:[person-,person-]{}.
  r2:[personPair-]{}.
--  r22:[personPair-]{}.

  r3:[student+,student-]{}.

  r0(_).
  
  r1(P1,P2) :- P1=student("bill","imperial"),P2=student("john","kings").
  r11((student("bill","imperial")<~person),(student("john","kings")<~person)).


  r2(pair((student("bill","imperial")<~person),
          (student("john","kings")<~person))).
--  r22(pair(P1,P2)) :- P1=student("bill","imperial"),P2=student("john","kings").

  r3(S,S).

  r111:[(person,person)-]{}.
  r111((P1,P2)) :- P1=student("bill","imperial"),P2=student("john","kings").

  -- main:[list[string]]*.
  main(_) -> 
      { r111(Tuple),
	r2(Pair)};
      stdout.outLine(Tuple.show());
   --   stdout.outLine((P1,P2).show());
      stdout.outLine(Pair.show());
      {r3(student("bill","imperial"),S)};
      (r0(S) ? stdout.outLine(N.show()) | stdout.outLine(S.show())).
}
