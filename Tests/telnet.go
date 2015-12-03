/* A simple telnet-like program */

telnet{
  import go.io.
  import go.stdparse.
  
  main([H,P]) ->
      tcpConnect(H,naturalOf%%P,i,o,utf8Encoding);
      spawn {reader(i)};
      readKeyStrokes(o);
      stdout.outLine("End").
     
  reader:[inChannel]*.
  reader(i)::i.eof() -> {}.
  reader(i) ->
      stdout.outLine(i.inLine("\n"));
      reader(i).

  readKeyStrokes:[outChannel]*.
  readKeyStrokes(_)::stdin.eof() -> {}.
  readKeyStrokes(o) ->
      o.outCh(stdin.inCh());
      readKeyStrokes(o).
}.