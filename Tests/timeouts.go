/*
 * Test timeouts
 */

timeouts{
  import go.io.
  import go.unit.
  import go.stdparse.
  import go.mbox.

  plip:[dropbox[symbol],mailbox[symbol]]*.
  plip(T,Bx) ->
      delay(rand(1.0));
      T.post('plip');
      plip(T,Bx).

  plop:[mailbox[symbol]]*.
  plop(Bx) ->
      case Bx.nextW(0.5) in (
       'plip' -> stdout.outLine("plip")
      )
      onerror(
       timedout -> stdout.outLine("  plop"));
      plop(Bx).

  timeouts:[float]@=harness.
  timeouts(_)<=harness.
  timeouts(T)..{
    doAction()->
        H = mailbox();
        spawn{ plop(H)};
        spawn{ plip(H.dropbox(),mailbox()) };
        delay(T).
  }.

  main([]) ->
      checkUnit(timeouts(10.0)).
  main([N]) ->
      checkUnit(timeouts(floatOf%%N)).
}.
