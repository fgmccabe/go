/*
 * experiments in comparability
 */
comp{
  import go.io.
  import go.unit.

  comp <~ { eq:[comp]{} }.

  cmp <~ comp.
  cmp <~ { less:[cmp]{} }.

  i:[integer]@=cmp.
  i(N) .. {
    less(i(M)) :- N<M.

    eq(X) :- i(N)<=X.
    show()=>"i("<>int2str(N,10,0,` )<>")".
  }.

  sort:[list[cmp]]=>list[cmp].
  sort([]) => [].
  sort([L]) => [L].
  sort([E,..L])::split(E,L,L1,L2) => sort(L1)<>[E,..sort(L2)].

  split:[cmp,list[cmp],list[cmp],list[cmp]]{}.
  split(_,[],[],[]).
  split(E,[D,..L],[D,..L1],L2) :- D.less(E), split(E,L,L1,L2).
  split(E,[D,..L],L1,[D,..L2]) :- split(E,L,L1,L2).

  comptest:[]@=harness.
  comptest<=harness.
  comptest..{
    doPred() :-
        checksorted(sort([i(1),i(-23),i(34),i(10),i(-11)])),
        checksorted(sort({i(irand(N))..N in iota(1,100)})).

    doAction() ->
        stdout.outLine(sort({i(irand(N))..N in iota(1,100)}).show()).

    checksorted:[list[cmp]]{}.
    checksorted([E,..L]) :- verifysorted(E,L).

    verifysorted:[cmp,list[cmp]]{}.
    verifysorted(_,[]).
    verifysorted(E,[D,..L]) :- \+D.less(E),verifysorted(D,L).
  }.

  main(_) ->
      checkUnit(comptest).
}