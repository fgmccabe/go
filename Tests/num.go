num{
  import go.unit.
  import go.io.

  numtest:[]@=harness.
  numtest<=harness.
  numtest..{
    doPred() :-
        (34).equal(34).
  }.

  main(_) ->
      checkUnit(numtest).
}