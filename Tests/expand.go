expand{
  import go.io.
  import go.unit.
  import go.setlib.


  expandTest:[]@=harness.
  expandTest<=harness.
  expandTest..{
    doPred() :-
	expand("this is a list of words"," ") = ["this","is","a","list","of","words"],
	Sent = "this is a list of words",
	collapse(expand(Sent," ")," ")=Sent.
    doAction() ->
	stdout.outLine(expand("this is a list of words "," ").show());
	stdout.outLine(collapse(expand("this is a list of words "," ")," ")).
  }.

  main(_) ->
      checkUnit(expandTest).
}