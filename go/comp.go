comp{
  -- experiments in types
  import go.io.

  comp[] <~ { equal:[S]-(S<~comp[]){} }.

  other[T]<~comp[].
  other[T]<~ {
        more:(()=>other[T])
      }.

  d(X:T):other[T]..{
    equal(this).

    more()=>this.
  }.

  main(_) ->
      X = d('ab');
      Y = d(23);
      stdout.outLine(X.more()^0);
      stdout.outLine(Y.more()^0);
}
