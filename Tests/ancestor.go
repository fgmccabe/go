ancestor{
  import go.io.
  import go.unit.

  parent:[symbol,symbol]{}.
  parent('a','b').
  parent('a','c').
  parent('c','d').
  parent('c','e').
  parent('e','f').

  ancestor:[symbol,symbol]{}.
  ancestor(X,Y) :- parent(X,Y).
  ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).

  ancestorTester:[]@=harness.

  ancestorTester <= harness.
  ancestorTester..{
    doAction() -> ancestor('a',X) *> stdout.outLine("A is ancestor of " <> X^).

    errorAction()->
        (_:harness).doAction().		-- this is supposed to fail

    doPred() :- parent('a','b'),        -- define the tests to be applied here
                \+parent('b','a'),
                ancestor('a','f'),
                \+ancestor('f',_).

    show()=> "ancestor test".
  }.
                                        
  main(_) -> checkUnit(ancestorTester).
}
