/* Test bounded sets */

bounded{
  import go.io.
  import go.unit.

  qq::= p(symbol) | q(symbol).

  boundtest:[]@=harness.
  boundtest<=harness.
  boundtest..{
    doPred() :-
        S = {X .. (X::X<32) in [1,30,-30,45,10,3]},
        1 in S,
        30 in S,
        \+ 45 in S,
        T = {X*X .. X in [1,30,-30,45,10,3]},
        1 in T,
        100 in T,
        A = {(U,V) .. q(U) in [p('a'),q('b')], (V::V<10) in [1,2,12] },
        ('b',2) in A,
        \+ ('a',_) in A.

    doAction() ->
--	stdout.outLine({X .. (X::X<32) in [1,30,-30,45,10,3]}.show());
--        stdout.outLine({(X,Y)..(X::X<32) in [1,30,-30,45,10,3],Y in [`a,`b]}.show());
	stdout.outLine(multi('b',[([([('a',1),('b',2),('c',3)],`A),
				    ([('a',1),('b',4),('c',3)],`B)],'b'),
				  ([([('a',1),('b',2),('c',3)],`A),
				    ([('a',1),('b',4),('c',3)],`B)],'c'),
				  ([([('a',-1),('b',-2),('c',-3)],`C),
				    ([('a',-1),('b',-4),('c',3)],`B)],'b')]).show()).

    multi:[symbol,list[(list[(list[(symbol,integer)],char)],symbol)]]=>
	list[(symbol,char)].
    multi(A,B) =>
	{ (U,C) .. (W,A) in B, (V,C) in W,(U,_) in V }.
	
  }.

  main(_) ->
      checkUnit(boundtest).
}
