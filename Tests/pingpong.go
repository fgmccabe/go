/*
 * simple test of message handling, based on the plipplop program
 */
pingpong{
  import go.io.
  import go.unit.
  import go.stdparse.
  import go.mbox.

  pings ::= ping(dropbox[pings]) | pnger | pong(dropbox[pings]).

  pinger:[mailbox[pings]]*.
  pinger(Mbx) ->
      case Mbx.nextW(0.3) in (
       ping(H) -> 
           stdout.outLine("ping received");
           delay(rand(0.8));
           H.post(pong(Mbx.dropbox()))
     | pnger ->
           stdout.outLine("pnger received")
      )
      onerror(
       timedout -> stdout.outLine("ping timed out")
      );
      pinger(Mbx).

  ponger:[mailbox[pings],integer]*.
  ponger(_,0) -> {}.
  ponger(Mbx,count) ->
      case Mbx.nextW(0.5) in 
      ( pong(Hx) ->
            stdout.outLine("  pong received");
            delay(rand(0.1));
            Hx.post(ping(Mbx.dropbox()));
            Hx.post(pnger)
      )
      onerror(
      timedout -> stdout.outLine("pong timed out"));
      ponger(Mbx,count-1).

  testping:[integer]@>harness.
  testping(_)<=harness.
  testping(count)..{

    doAction() ->
        I = mailbox();
        O = mailbox();
        spawn{ pinger(I) onerror(
         Er -> stdout.outLine("Pinger error: "<>Er.show())
        )};
        I.dropbox().post(ping(O.dropbox())); 
        ponger(O,count)
        onerror(
         Er -> stdout.outLine("Error: "<>Er.show())
        ).
  }.

  main([]) ->
      checkUnit(testping(20)).
  main([C,.._]) ->
      checkUnit(testping(naturalOf%%C)).
}
