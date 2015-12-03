/*
 * this test a number of aspects of classes, and type inference
 */
tree{
  import go.io.
  import go.unit.

  comp[a] <~ {
    less:[comp[a]]{}.
    eq:[comp[a]]{}
  }.

  tree[a<~comp[a]] <~ {
	insert:[a]=>tree[a]. /* ::sorted(this). insert::sorted(this)=>sorted(this). */ 
	empty:[]{}.
      }.

  em:[]@=tree[comp[_]].
  em..{
    show()=>"*".
    insert(a) => nd(em,a,em).
    empty().
  }.

  nd:[tree[comp[T]],comp[T],tree[comp[T]]]@=tree[comp[T]].
  nd(L,B,R)..{
    show() => "("<>L.show()<>" "<>B.show()<>" "<>R.show()<>")".
    insert(E)::E.eq(B) => this.
    insert(E)::E.less(B) => nd(L.insert(E),B,R).
    insert(E)::\+E.less(B) => nd(L,B,R.insert(E)).
    empty() :- false.
  }.

  II:[a]@=comp[a].
  II(N)..{
    less(II(A)) :- N<A.
    eq(II(N)).
    show()=>"II("<>__stringOf(N,0,0)<>")".
  }.

  testree:[]@=harness.
  testree<=harness.
  testree..{
    doAction() ->
        T0 = em.insert(II(1));
        T1 = T0.insert(II(3));
        T2 = T1.insert(II(2));
        stdout.outLine("T0="<>T0.show());
        stdout.outLine("T1="<>T1.show());
        stdout.outLine("T2="<>T2.show()).
  }.
        
  main(_) ->
      checkUnit(testree).
}
