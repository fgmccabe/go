/*
 * A test of the case action
 */

cases{
  import go.io.
  import go.unit.
  import go.stdparse.

  cases:[integer]@=harness.

  cases(_)<=harness.
  cases(L)..{
    doAction() ->
        I in iota(1,10) *>
        case I in (
         X::X<L ->
             stdout.outLine(X.show()<>" is less than "<>L.show())
       | 5 ->
             stdout.outLine("We have 5")
       | X ->
             stdout.outLine(X.show()<>" is not less than "<>L.show())
        );
        case (12,34) in (
         (12,10) -> stdout.outLine("Duh")
       | (12,34) -> stdout.outLine("Hooray")
       | (_,_) -> stdout.outLine("Boo")
        ).
    
    app:[list[t],list[t]]=>list[t].
    app(X,Y) => case X in (
                 [] => Y
                |[E,..R] => [E,..app(R,Y)]
                ).
    doPred() :-
        app("1234","abc")="1234abc".
  }.

  main([]) ->
      checkUnit(cases(5)).
  main([N]) ->
      checkUnit(cases(naturalOf%%N)).
}.
