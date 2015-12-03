app{
/*
  []:list[_]..{
    eof().
    cons(_,_) :- false.
    show() => "[]".
    head() => [].
    tail() => [].
    eq([]).
  }.
*/

  [H:t,..T]:list[t]..{
    eof() :- false.
    cons(H,T).
    show() => "["<>showList([H,..T],"")<>"]".
    head()=>H.
    tail()=>T.
    eq([H,..T]).

    showList([],_)=>[].
    showList([El,..R],Sep) => Sep<>El^0<>showList(R,",").

--    []<>X=>X.
--    [E,..X]<>Y=>[E,..X<>Y].
  }.

  (<>):[t]-((list[t],list[t])=>list[t]).
  []<>X=>X.
  [E,..X]<>Y=>[E,..X<>Y].
}