/*
 * Test some aspects of inner classes
 */
inner{
  import go.io.
  import go.unit.

  -- Basic cell type
  cl[T] <~ { g:[]=>T. p:[T]* }.

  combo[t] <~ { pr:[t]{}. cell:[t]@>cl[t] }.

  -- Basic cell class
  cl:[t] @> cl[t].
  cl(I)..{
    V:t := I.

    g()=>V.

    p(N) -> V:=N
  }.

  dl:[]@>cl[symbol].
  dl()..{
    p(_)->{}.
  }.
  dl()<=cl('a').

  inner:[]@=harness.
  inner<=harness.
  inner..{
    cb:[] @= combo[symbol].
    cb..{
      cell(S)<=cl(S).		-- An inner class defined with a class rule only
      
      pr('a').
    }.

    doAction() ->
	CB = cb;				-- this is stateless
	CC = cb.cell('a');			-- this is stateful
	{ CC.g()=='a' };			-- this should succeed
	CC.p('c');
	stdout.outLine("CC = "<>CC.g().show());
	( {CC.g()=='b'}
	  onerror (
	   E -> stdout.outLine("Test failed as expected, E="<>E.show())
	  )
	).
  }.

  main(_) ->
      checkUnit(inner).
}