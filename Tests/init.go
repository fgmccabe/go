/*
 * Test initialization of different kinds
 */
init{
  import go.io.
  import go.unit.

  itype<~{ foo:[]* }.

  iclass:[]@>itype.
  iclass()..{
    X:float := valof{
          Z = 2.0*45.0;
          stdout.outLine("initializating X to "<>Z.show()<>" in "<>this.show());
          valis Z
        }.

    ${
      X := X*23.0;
    }.

    show()=>"iClass".

    foo:[]*.
    foo() ->
        stdout.outLine("X = "<>X.show()).

    ${
      stdout.outLine("X inited to "<>X.show()<>" in "<>this.show());
    }.      
  }.

  testinit:[]@=harness.
  testinit<=harness.
  testinit..{
    doAction() ->
        O = iclass();
        stdout.outLine("$iclass = "<>(O).show());
        O.foo()
  }.

  main(_) ->
      checkUnit(testinit).
}

  