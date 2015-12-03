count{
  import go.io.

  read:[inChannel,integer-]*.
  read(File,Count) ->
      loop(0,Count).

  private loop:[integer,integer-]*.
  loop(C,C)::File.eof() -> {}.
  loop(soFar,C) -> _ = File.inLine("\n");
      loop(soFar+1,C).

  main([F,.._]) ->
      read(openInFile(F,rawEncoding),Count);
      stdout.outLine(Count.show()<>" lines in "<>F).
}

