labels{
  import go.io.
  import go.unit.

  lbT <~ {foo:[]=>string}.

  lbN:[]@=lbT.
  lbN..{
    foo()=>"lbN".
  }.

  lbU:[string]@>lbT.
  lbU(S)..{
    chuck:string = S.

    foo() => chuck.
  }.

  lbTst:[lbT]{}.
  lbTst(X) :-
      X <= lbN.
  
  lbUTst:[lbT,string]{}.
  lbUTst(X,S) :-
      X <= lbU(S).

  labelT:[]$=harness.
  labelT<=harness.
  labelT..{
    doPred() :- lbTst(lbN), \+ lbTst(lbU("U")), lbUTst(lbU("who"),O),
		O=="who".
    doAction() ->
	LL = lbU("who");
	( LL <= Lb *>
	      stdout.outLine(LL.show()<>" <= "<>Lb.show())
	).
  }.

  main(_) ->
      checkUnit(labelT).
}