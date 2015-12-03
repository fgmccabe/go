tdpl{
  import go.io.
  import go.showable.

  import ctypes.
  import dcgparse.
  import canon.

  main(Files) ->
      F in Files *>
      ( Gr = tdplGrammar%%getFile(F,unknownEncoding);
	( R in Gr *>
	  stdout.outLine(show(R)));
	CNF = flatten({ canonRule(r) .. r in Gr});
	stdout.outLine("++ canon form:");
	( R in CNF *>
	  stdout.outLine(show(R));
	)).
}

  