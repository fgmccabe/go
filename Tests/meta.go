meta{
  import go.io.
  import go.unit.
  import person.

  metaTest:[]@=harness.
  metaTest<=harness.
  metaTest..{
    doAction() ->
--	S = student("Fred", imperial, noone);
	S = %["23"]%;
	stdout.outLine(__stringOf(X,0,0));
	V = %[X]%;
	stdout.outLine(__stringOf(S,0,0));
	stdout.outLine(S.show());
	stdout.outLine(__stringOf(V,0,0));
	stdout.outLine(V.show());
	X = [23];
	Y = X._meta();
	stdout.outLine(__stringOf(Y,0,0));
	stdout.outLine(Y.show()) .
  }.

  main(_) -> 
      checkUnit(metaTest).

}

	