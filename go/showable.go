go.showable{
  import go.io.

  /* A small package to support a more optimized display of information */

  dispTree <~ { flatten:[string]=>string }.

  showable <~ { disp:[]=>dispTree. }.

  s:[string]@=dispTree.
  s(S)..{
    flatten(Tail)::append(S,Tail,X) => X.
    show()=>S.
  }.

  l:[list[string]]@=dispTree.
  l(L)..{
    flatten(Tail)::multiApp(L,Tail,X) => X.

    multiApp:[list[list[t]],list[t],list[t]]{}.
    multiApp([],S,S) :-- true.
    multiApp([St,..R],S0,S) :-- append(St,S1,S), multiApp(R,S0,S1).
    show()=>flatten([]).
  }.

  n:[list[dispTree]]@=dispTree.
  n(L)..{
    flatten(Tail) => fltn(L,Tail).

    fltn:[list[dispTree],string]=>string.
    fltn([],Tail) => Tail.
    fltn([E,..M],Tail) => E.flatten(fltn(M,Tail)).
    show()=>flatten([]).
  }.

  show:[showable]=>string.
  show(Tr)=>Tr.disp().flatten("").

  dispOut:[outChannel,dispTree]*.
  dispOut(O,s(Txt)) -> O.outStr(Txt).
  dispOut(O,l(L)) -> E in L *> O.outStr(E).
  dispOut(O,n(L)) -> E in L *> dispOut(O,E).
}