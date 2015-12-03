sortmain{
  import sorting.
  import go.io.

  sortIntegers:[list[integer]] => list[integer].
  sortIntegers(L) => sort(L, :compare[integer] .. { less(X,Y) :- X<Y }).

  I:[integer]@=cmp[integer].
  I(K)..{
    lt(I(J)):-K<J.
    ge(I(J)):-K>=J.
    show() => "("<>K.show()<>")".
  }.

  main(_) ->
      stdout.outLine("Sort of [1,3,5,2,0,10] is "<>
		     sortIntegers([1,3,5,2,0,10]).show());
      stdout.outLine("Quick sort of [1,3,5,2,0,10] is "<>
		     quick({I(X)..X in [1,3,5,2,0,10]}).show());
}
  