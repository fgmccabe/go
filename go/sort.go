/*
 * Standard quick sorting module 
 */
go.sort{
  sort:[list[t],comparable[t]]=>list[t].
  sort([],_) => [].
  sort([L],_) => [L].
  sort([E,..L],C) => split(E,L,C,[],[]).

  split:[t,list[t],comparable[t],list[t],list[t]]=>list[t].
  split(E,[],C,L1,L2) => sort(L1,C)<>[E,..sort(L2,C)].
  split(E,[D,..L],C,L1,L2)::C.less(E,D) => split(E,L,C,L1,[D,..L2]).
  split(E,[D,..L],C,L1,L2) => split(E,L,C,[D,..L1],L2).
}