ultest{
  import go.io.
  import ul.
  import ulex.

  main(Files) ->
      F in Files *>
      stdout.outLine(parser(yyulex(suckIn(F),1)).show()).

  private suckIn:[string]=>string.
  suckIn(Fl) =>
      valof{
        f = openInFile(Fl,unknownEncoding);
        valis f.inText("");           -- This will read the entire file
        f.close();
      }.
}
