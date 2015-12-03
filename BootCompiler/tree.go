/*
 * test the type constructor form of a type definition
 */

tree{

  tr[A] ::= empty | node(tr[A],A,tr[A]).

  tree[T] <~ { label:[]=>T }.

  em:[]$=tree[_].
  em..{
    label()=> raise _.
  }.

  nd:[tree[T],T,tree[T]]$=tree[T].
  nd(L,B,R)..{
    label()=>B.
  }.

  walk:[tree[t]]=>list[t].
  walk(nd(L,B,R)) => walk(L)<>[B]<>walk(R).

  (<>):[list[t],list[t]]=>list[t].
  []<>X=>X.
  [E,..X]<>Y=>[E,..X<>Y].

}