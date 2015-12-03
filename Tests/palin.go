palin{
  import go.io.
  import go.unit.

  private palin:[string]-->string.

  palin([C,..L]) --> [C], palin(L), [C].
  palin([C]) --> [C].
  palin([]) --> [].

  testpalin:[string]@=harness.
  testpalin(_)<=harness.
  testpalin(T)..{
    doAction() ->
        ((palin(X)-->T) ?
           stdout.outLine("Parsed "<>T<>" is "<>X)
       | stdout.outLine(T<>" does not seem to be palindromic")).
  }.

  main([]) -> checkUnit(testpalin("go2002og")).
  main([T]) -> checkUnit(testpalin(T)).
}