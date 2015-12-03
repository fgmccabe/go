trans{
  import go.io.
  import go.unit.

  pred[T]<~{ tst:[T,T]{}}.

  trans:[pred[T]]@=pred[T].
  trans(B)..{
    tst(X,Y) :- B.tst(X,Y).
    tst(X,Y) :- B.tst(X,Z), tst(Z,Y).
  }.

  parent:[]@=pred[symbol].
  parent..{
    tst('sally', 'bob').
    tst('mike', 'bob').
    tst('bob','foo').
  }.

  ancestor:pred[symbol] = trans(parent).

  transtest:[]@=harness.
  transtest<=harness.
  transtest..{
    doPred() :-
        parent.tst('sally','bob'),
        \+parent.tst('foo',_),
        ancestor.tst('sally','foo'),
        ancestor.tst('mike','bob'),
        \+ ancestor.tst('bob','mike').
    doAction() ->
        stdout.outLine("Descendants of sally: "<>{X || ancestor.tst('sally',X)}.show()<>"\n")
  }.

  main(_) ->
      checkUnit(transtest).
}
