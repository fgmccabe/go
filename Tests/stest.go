stest{
  import go.cell.
  import go.io.

  test(x) ->
      sync(x) {
	action { stdout.outLine("Trying sync test") },false -> {}
      } timeout (now() + 1 ->
		     stdout.outLine("Timed out")).

  testForEver(x) ->
      test(x);
      testForEver(x).

  main(_) ->
      x = $cell(1);
      testForEver(x).

}
