/*
 * A test of the conditional expression
 */
ifexp{
  import go.io.
  import go.unit.


  ifFun:[integer]=>symbol.
  ifFun(A)::C=3 => ( A==1 ?
                 'a'
              | A==2?
                 'b'
              | A==C?
                 'c'
             | 'd').

  ifexp:[]@=harness.
  ifexp<=harness.
  ifexp..{
    doAction() ->
        stdout.outLine("ifFun(1)= "<>ifFun(1)^);
        stdout.outLine("ifFun(2)= "<>ifFun(2)^);
        stdout.outLine("ifFun(3)= "<>ifFun(3)^);
        stdout.outLine("ifFun(0)= "<>ifFun(0)^).
  }.

  main(_) ->
      checkUnit(ifexp).
}
            