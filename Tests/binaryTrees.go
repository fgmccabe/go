binaryTrees{
  import go.io.
  import go.unit.
  import go.stdparse.

  -- This benchmark is adapted from 
  -- http://shootout.alioth.debian.org/gp4/benchmark.php?test=binarytrees&lang=yap&id=1

  tree[t] ::= tree(tree[t],t,tree[t]) | nil.

  build:[integer,integer]=>tree[integer].
  build(0,I) => tree(nil,I,nil).
  build(D,I) => tree(build(D-1,I*2-1),I,build(D-1,I*2)).

  check:[tree[integer]]=>integer.
  check(tree(nil,I,_))=>I.
  check(tree(_,I,nil))=>I.
  check(tree(L,I,R))=>check(L)+I-check(R).

  minDepth:integer = 4.

  testTree:[integer]@=harness.
  testTree(_)<=harness.
  testTree(N)..{
    steps:[integer,integer,integer]=>list[integer].
    steps(Ix,Mx,_)::Ix>=Mx => [Ix].
    steps(Ix,Mx,St) => [Ix,..steps(Ix+St,Mx,St)].

    countCheck:[integer,integer,integer,integer]=>integer.
    countCheck(Mx,Mx,Ch,_) => Ch.
    countCheck(Ix,Mx,Ch,Dp) =>
	countCheck(Ix+1,Mx,Ch+check(build(Dp,Ix))+check(build(Dp,-Ix)),Dp).

    doAction() ->
	maxDepth = ( minDepth+2>N ? minDepth | N);
        stretchDepth = maxDepth+1;
        stdout.outLine("stretch tree of depth "<>stretchDepth.show()<>"\t check: "<>
		       check(build(stretchDepth,0)).show());
        longLived = build(maxDepth,0);

	(Depth in steps(minDepth,maxDepth,2) *> (
	  Iterations = bleft(1,maxDepth-Depth+minDepth);
	  
          stdout.outLine((2*Iterations).show()<>" trees of depth "<>Depth.show()<>
			 "\t check: "<>countCheck(1,Iterations+1,0,Depth).show())
	 )
	);
	stdout.outLine("long lived tree of depth "<>maxDepth.show()<>"\t check: "<>
		       check(longLived).show()).
  }.

  main([]) ->
      checkUnit(testTree(4)).
  main([N]) ->
      checkUnit(testTree(naturalOf%%N)).
}