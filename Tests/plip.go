/*
 * Test the multiple processes with a simple delay loop
 */

plip{
  import go.io.
  import go.stdparse.
  import go.unit.

  testplip:[integer]@=harness.
  testplip(_)<=harness.
  testplip(T)..{
    doAction() -> 
        spawn{ plip()};
        spawn{ plop()};
        stdout.outLine("Running for "<>T.show()<>" secs");
        delay(T);
        stdout.outLine("time out").
  }.

  plip:[]*.
  plop:[]*.

  plip() ->
    delay(0.5);
    stdout.outLine("plip");
    plip().

  plop() ->
    delay(1.6);
    stdout.outLine("  plop");
    plop().

  main([T,.._]) ->
      checkUnit(testplip(naturalOf%%T)).
  main([]) ->
      checkUnit(testplip(15)).
}.
