/*
 * A module to parse a csv file
 */
csv{
  import go.io.

  csv[]<~{lines:((list[string])-->string)}.
  csv:csv[]..{
    cell(Cont) --> "\"", Str(Cont,`\"), "\"".
    cell([]) --> ",".
    cell([C,..ont]) --> [C],{C!=`,}, cell(ont).
    cell([]),"\n" --> "\n".
    
    line([Cell,..more]) --> cell(Cell)!, line(more).
    line([]) --> "\n".
    line([]) --> eof.
    
    lines([Line,..more]) --> line(Line)!,lines(more).
    lines([]) --> eof.
  }.

  grabCSV(File) => 
      valof{
        f = openInFile(File,unknownEncoding);
        Text = f.inText("");           -- This will read the entire file
        f.close();
        valis csv.lines%%Text.
      }.
}