jsontest{
  import go.io.
  import go.unit.
  import go.json.

  jsontest:[list[string]]@=harness.
  jsontest(_)<=harness.
  jsontest(Fls)..{
    doAction() ->
        F in Fls *> (
         J = parseJson%%getFile(F,utf8Encoding);
         stdout.outLine("json is "<>J.show())
	).
  }.

  main([]) ->
      checkUnit(jsontest(["test.json"])).
  main(Fls) ->
      checkUnit(jsontest(Fls)).
}
