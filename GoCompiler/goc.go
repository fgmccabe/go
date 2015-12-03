/*
 * Top-level of the Go! in Go! compiler
 */

goc{
  import go.io.
  import tokenizer.
  import parse.
  import errors.
  import opts.
  import abstract.
  import wff.
  import types.
  import terms.
  import typeof.
  import sttypes.

  main(_) ->
      (Path,Opts,Files) = Options();
      processFiles(Path,Opts,Files).
                 
  processFiles:[list[string],list[compOpt],list[string]]*.
  processFiles(_,_,[]) -> {}.
  processFiles(Path,Opts,[Fn,..List]) ->
      process(Path,Opts,Fn);
      processFiles(Path,Opts,List).

  process:[list[string],list[compOpt],string]*.
  process(Path,Opts,Fl) ->
      T = parseFile(Path,Opts,Fl);
      N3=ticks();
      Wt = checkWff(T,Fl);
      (dbgType in Opts ?
	  stdout.outLine("Wff checked in "<>(ticks()-N3).show()<>" secs")
       );
      (wffOnly in Opts ?
         stdout.outLine("wffed is "<>Wt.show())
     | N4 = ticks();
       TypedPkg = typeOfPackage(Wt,standardTypes,Opts);
       (dbgType in Opts ?
	  stdout.outLine("Type checked in "<>(ticks()-N4).show()<>" secs")
       );
       (typeCheckOnly in Opts?
	  stdout.outLine("Program = "<>TypedPkg.show())
       )
      );
      testErrors().

  parseFile:[list[string],list[compOpt],string] => abstract.
  parseFile(Path,Opts,Fl) => 
      valof{
	F = grabURL(Path,Fl,File);
	Now=ticks();
	{(tokens(L,1) --> F)};
	N2 = ticks();
	stdout.outLine(listlen(L).show()<>" tokens found in "<>File<>" in "<>((N2-Now).show())<>" secs");
	(dbgParse in Opts ? showTokens(L,0) | {});
	{(parse(T,File) --> L)};
	N3=ticks();
	stdout.outLine("parsed "<>File<>" in "<>((N3-N2).show())<>" secs");
	(dbgParse in Opts ? stdout.outLine("parse is "<>T.show())|{});
	valis T
      }.


  grabURL:[list[string],string,string]=>string.
  grabURL(Path,Request,Actual)::D in Path,Actual=__mergeURL(D,Request),ffile(Actual) => 
      valof{
        st = openInFile(Actual,utf8Encoding);
        valis st.inText("")                -- This will read the entire file
      }.
  grabURL(Path,Request,_) => 
      valof{
	stdout.outLine("cannot find "<>Request<>" in path "<>Path.show());

	raise error("cannot find "<>Request<>" in path "<>Path.show(),'eFAIL');
	valis Request;
      }.

}
