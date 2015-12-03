/*
 * Test out some ideas about threads
 */

threads{
  import go.thread.
  import go.io.
  

  newthread<=thread.
  newthread:thread[]..{
    start() -> stdout.outLine(self^0<>" has forked").
  }.

  main(_) ->
      H = __fork($newthread,true);
      stdout.outLine("Child is "<>H^0).
}