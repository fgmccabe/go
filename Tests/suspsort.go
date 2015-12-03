suspsort{
  import go.io.
  import go.unit.

  delete:[integer,list[integer],list[integer]]{}.
  delete(X,[X,..L],L).
  delete(X,[Y,..L],[Y,..DL]) :- delete(X,L,DL).

  permute:[list[integer],list[integer]]{}.
  permute([U],[U]).
  permute(L,[X,..PDL]) :- 
      delete(X,L,DL),
      permute(DL,PDL).

  sort:[list[integer],list[integer]]{}.
  sort([U],[U]):--true.
  sort(L,SL) :-- ordered(SL),permute(L,SL).

  ordered:[list[integer]++]{}.
  ordered([U,..L]):-
      lessOrdered(U,L).
 
  lessOrdered:[integer++,list[integer]++]{}.
  lessOrdered(_,[]).
  lessOrdered(U,[V,..L]) :- V@@U<V,lessOrdered(V,L).

  susptest:[]@=harness.
  susptest <= harness.
  susptest..{
    doAction() ->
	{sort([-3,4,2,-4,1],S)};stdout.outLine(S.show()).

    doPred() :-
	sort([-3,4,2,1],S),
	S = [-3,1,2,4].
  }.

  main(_) -> 
      checkUnit(susptest).
}
