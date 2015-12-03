cmp{
  cmp[t] <~ { less:[t]{} }.

  cons:[t,cmp[t]]@=cmp[t].
  cons(hd,tl)..{
    less(cons(H,_)) :- hd<H.
  }.

  nil:[]@=cmp[t].
  nil..{
    less(cons(_,_)).
  }.
}
