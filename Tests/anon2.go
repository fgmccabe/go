/*
 * Testing inner classes with inheritance 
*/
anon2{
  import go.io.
  import go.unit.

  bf <~ { mem:[symbol]{} }.

  beliefs:[list[symbol]]@=bf.
  beliefs(I)..{                          -- A simple belief store
    mem(X) :- X in I
  }.

  ot <~ { bfs:[]=>bf }.			 -- a bfs has a belief store

  outer:[list[symbol]]@>ot.
  outer(X)..{
    b:bf=beliefs(X).
    bfs()=>b.
  }.

  foo <~ { iBelieve:[symbol]{} }.        -- A foo can believe things

  XX <~ { checkMe:[]*. belief:[symbol]{} }. -- An XXX can be checked, and has beliefs
  XX <~ ot.

  inner:[list[symbol]]@>XX.
  inner(X) <= outer(X).			-- inner inherits its belief store
  inner(Bfs) ..{
    belief(B) :- bfs().mem(B).		-- we believe things that are in our belief store

    checkMe() ->			-- check my beliefs
        OO = :foo..{			-- An anonymous class, is a foo
          iBelieve(B) :- belief(B).     -- inherited from outer (i.e., enclosing class)
        };
        ( OO.iBelieve(U) *> stdout.outLine("I Believe: "<>U^)).
  }.

  anon:[]@=harness.
  anon<=harness.                        -- test harness
  anon..{
    doAction() ->                       -- check our beliefs
        II = inner(['A','C','D']);
        II.checkMe().
    doPred() :-
        II = inner(['A','C','D']),     -- what do we believe?
        II.belief('A'),
        \+ II.belief('B').
  }.

  main(_) ->
      checkUnit(anon).
}

    