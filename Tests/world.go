/*
 * This acts as a demonstrator for the unit test framework
 */

world{
  import go.io.
  import go.unit.

  worldtest:[]@=harness.

  worldtest<=harness.
  worldtest..{
    doAction()->
        stdout.outLine("hello world").
  }.

  main(_)->
      checkUnit(worldtest).
}