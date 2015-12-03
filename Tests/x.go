x{
  import go.io.
  import go.xml.
  import go.stream.

  main(Files) ->
      Fl in Files *>
      inStream = inputSequence(openInFile(Fl,utf8Encoding),0);
      { xmlParse(X)-->inStream };
      stdout.outLine(X.show()).
}
      