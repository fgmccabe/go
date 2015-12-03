onemsg{
  import go.io.
  import go.unit.
  import go.mbox.

  onemsg:[]@=harness.
  onemsg<=harness.
  onemsg..{
    doAction() ->
        H = mailbox();
        T = spawn {echo(H)};
        ping(H.dropbox());
        waitfor(T).

    echo:[mailbox[string]]*.
    echo(Mbx) ->
        case Mbx.next() in (
         Msg -> stdout.outLine(Msg)
        ).

    ping:[dropbox[string]]*.
    ping(T) ->
        T.post("Hello").
  }.

  main(_) ->
      checkUnit(onemsg).
}.