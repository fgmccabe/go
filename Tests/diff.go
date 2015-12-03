diff{
  import go.io.
  import go.unit.

  sets[L]<~{union:[sets[L]]=>sets[L]. member:[L]{}}.

  sets:[list[L]]@=sets[L].
  sets(I)..{
    union(sets(K)) => sets(__union(K,I)).

    __union:[list[t],list[t]]=>list[t].
    __union([],K) => K.
    __union([E,..l],K)::E in K => __union(l,K).
    __union([E,..l],K) => [E,..__union(l,K)].

    member:[L]{}.
    member(X) :- X in I.

    show() => flatten(["{",..showList(I,"")]).

    showList:[list[L],string]=>list[string].
    showList([],_)=>["}"].
    showList([E,..S],sep) => [sep,E.show(),..showList(S,", ")].

    flatten:[list[list[s]]]=>list[s].
    flatten([]) => [].
    flatten([S,..L]) => flt(S,L).
  
    flt:[list[t],list[list[t]]]=>list[t].

    flt([],L) => flatten(L).
    flt([C,..S],L) => [C,..flt(S,L)].
  }.

  testsets:[]@=harness.
  testsets<=harness.
  testsets..{
    doPred() :-
        D = sets([1,2,3]),
        D.member(2),
        D.member(1),
        D.member(3),
        \+D.member(4),
        D.union(sets([4])).member(4),
        U = D.union(sets([4])),
        U.member(1),
        U.member(2).
    doAction() ->
        D = sets([1,2,3]);
        stdout.outLine(D.union(sets([1,3,4])).show()).
  }.

  main(_) ->
      checkUnit(testsets).
}
