shell{
  import go.io.
  import go.unit.

  shell:[string,string]@=harness.
  shell(_,_)<=harness.
  shell(Cmd,Arg1)..{
    doAction() ->
        stdout.outLine("doing "<>Cmd);
        (_,o,e) = pipeConnect(Cmd,[Arg1],[],utf8Encoding);
        consume(o);
        consume(e).
  }.
    
  consume:[inChannel]*.
  consume(i)::i.eof()->{}.
  consume(i)->stdout.outLine(i.inLine("\n")); consume(i).

  main([]) ->
      checkUnit(shell("/bin/ls",fcwd())).
  main([Cmd,Arg]) ->
      checkUnit(shell(Cmd,Arg)).
}
    