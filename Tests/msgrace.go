msgrace{
  -- threads sending a lot of messages to each other

  import go.io.
  import go.unit.
  import go.stdparse.
  import go.mbox.

  msg::= add(integer) | subtract(integer) .

  plip:[dropbox[msg]]*.
  plip(T) ->
--      delay(rand(0.5));
      ( irand(1000) rem 3 == 0.0 ?
	  T.post(subtract(9))
     |  T.post(add(8)));
      plip(T).

  plop:[integer,integer,mailbox[msg]]*.
  plop((soFar::soFar<0),_,_) -> stdout.outLine("bottomed out").
  plop(soFar,(tgt::soFar>tgt),_) -> stdout.outLine("topped out with "<>soFar.show()).
  plop(soFar,tgt,Bx) ->
      stdout.outLine("current soFar="<>soFar.show());
      case Bx.next() in (
       add(C) -> plop(soFar+C,tgt,Bx)
     | subtract(C) -> plop(soFar-C,tgt,Bx)
      ).

  race:[integer]@=harness.
  race(_)<=harness.
  race(T)..{
    doAction()->
        H = mailbox();
        spawn{ plip(H.dropbox()) };
        waitfor(spawn{ plop(10,T,H)}).
  }.

  main([]) ->
      checkUnit(race(100)).
  main([N]) ->
      checkUnit(race(naturalOf%%N)).
}
