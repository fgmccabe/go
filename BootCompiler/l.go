org.go.sys.cell{
  cell[T] ::= { get:(()=>T), set:((T)*) }.
  cell[_] <~ synchronized[].

  cell(I:T):cell[T]..{
    V := I.

    get() => valof{
               sync{
                 valis V
               }
             }.

    set(N) -> sync{V:=N}.
  }.

  dynamic[T] ::= { add:(T)*, delete:(T)* }.

  dynamic(I) <= cell(I).
  dynamic(I:list[T]):dynamic[T]..{
    add(X) ->
        set(get()<>[X]).

    delete(X) ->
        set(remove(X,get())).

    LL := len(I).
  }.

  len([]) => 0.
  len([_,..L]) => len(L)+1.
  remove(_,[]) => [].
  remove(X,[X,..L]) => remove(X,L).
  remove(X,[A,..L]) => [A,..remove(X,L)].

  []<>X=>X.
  [E,..X]<>Y=>[E,..X<>Y].

}