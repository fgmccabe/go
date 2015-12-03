/*
 * module to implement updateable cells
 */

go.cell{
  cell[T] <~ { get:[]=>T. set:[T]* }.
  cell[_] <~ synchronized.

  cell:[T]$=cell[T].
  cell(I)..{
    V:symbol := __term(I).

    get()::__is(V,E) => E.

    set(N) -> __remove(V); V:=__term(N).

    show()::__is(V,E)=>"$"<>E.show().
  }.

  dynamic[T] <~ cell[list[T]].
  dynamic[T] <~ { add:[T]*. delete:[T]* }.

  dynamic:[list[T]]$=dynamic[T].
  dynamic(I) <= cell(I).
  dynamic(I:list[T]):dynamic[T]..{
    add(X) ->
        set(get()<>[X]).

    delete(X) ->
        set(remove(X,get())).

    LL:integer := len(I).
  }.

  remove:[T,list[T]]=>list[T].
  remove(_,[]) => [].
  remove(X,[X,..L]) => remove(X,L).
  remove(X,[A,..L]) => [A,..remove(X,L)].

  len:[list[t]]=>integer.
  len([]) => 0.
  len([_,..L]) => len(L)+1.

  (<>):[list[t],list[t]]=>list[t].
  []<>X=>X.
  [E,..X]<>Y=>[E,..X<>Y].
}