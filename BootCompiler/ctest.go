/*
 * Test the different features of the compiler
 * Should eventually offer full coverage
 */
ctest{
  -- import to be tested
 import go.io.

  -- Test package-level programs
  p:[symbol]{}.
  p('a').
  p('b').

  q:[symbol++]{}.			-- also do the lazy version
  q('a'):--true.
  q(X):--p(X).

  f:[]=>symbol.
  f()::p(X)=>X.

  a:[symbol]*.
  a(X) -> (X in ['a','b']?{p(X)}|{q(X)}).

  g:[]-->string.
  g()-->"foo".

  ${
    a('b')
  }.

  clT[T] <~ { p:[T]{}. q:[T]{}. a:[T]*. cell:[symbol]@>clI[symbol] }.

  clI[T] <~ { g:[]=>T. p:[T]* }.

  w:[symbol]=>clI[symbol].
  w(X) => cl().cell(X).

  cl:[] @> clT[symbol].
  cl()..{
    p('a').

    q('b').

    cell(I)..{
      V:symbol:=I.

      g()::q(X) => X.
      g() => (:clI[symbol]..{
		V:symbol := '*'.
		g()=>V.
		p(N)->V:=N.
	      }).g().

      p(N)->V:=N.
    }.

    XX:clI[symbol] = cell('').

    a(U) -> XX.p(U).
  }.

  -- Test the anonymous classes feature

  do[] <~ { do:[]* }.

  runner:[integer]*.
  runner(Mx) ->
      (:do..{
	 jimBo:[]*.

	 I:integer := 0.
	 
	 jimBo()::I==Mx -> {}.
	 jimBo() ->
	     stdout.outLine("I = "<>I.show());
	     I := I+1;
	     jimBo().

	 do()->jimBo().
       }).do().

  main(_) ->
      runner(10).
  
}