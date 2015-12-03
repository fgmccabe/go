app{
  -- implement the list interface for lists
  []:list[_]..{
    eof().
    cons(X) => [X].
    tack(X) => [X].
    hdtl(_,_) :- false.
    display(W,_) => __trim("[]",W).
    show() => "[]".
    head() => _.
    tail() => this.
    eq([]).
  }.

  [H:t,..T:list[t]]:list[t]..{
    eof() :- false.
    hdtl(H,T).
    cons(X) => [X,..this].

    display(W,_) => "".

    show() => "".

    head()=>H.
    tail()=>T.

    tack(X) => this<>[X].
    eq([H,..T]).
  }.

  (<>):(list[t],list[t])=>list[t].

  [] <> X => X.
  [E,..X] <> Y => [E,..X<>Y].
}