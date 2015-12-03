sorting{
  compare[t] <~ { less:[t,t]{} }.

  sort:[list[t],compare[t]]=>list[t].
  sort([],_) => [].
  sort([E,..L], O)::split(L,E,O,A,B) => app(sort(A,O),[E,..sort(B,O)]).

  private split:[list[t]+,t+,compare[t],list[t]-,list[t]-]{}.
  split([],_,_,[],[]).
  split([E,..L],K,O,[E,..A],B) :- O.less(E,K), split(L,K,O,A,B).
  split([E,..L],K,O,A,[E,..B]) :- \+O.less(E,K), split(L,K,O,A,B).

  app:[list[t],list[t]]=>list[t].
  app([],X) => X.
  app([E,..A],X) => [E,..app(A,X)].

  cmp[t] <~ { lt:[cmp[t]]{}. ge:[cmp[t]]{} }.

  quick:[list[cmp[t]]]=>list[cmp[t]].
  quick([])=>[].
  quick([E,..L]) =>
      quick({X..(X@lt(E)) in L})<>[E,..quick({X..(X@ge(E)) in L})].
}
