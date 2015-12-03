/*
 * this test a number of aspects of classes, and type inference
 */
t{
  comp[a] <~ {
    less:[comp[a]]{}.
    eq:[comp[a]]{}
  }.

  tree[x<~comp[x]] <~ {
	insert:[x]=>tree[x]. /* ::sorted(this). insert::sorted(this)=>sorted(this). */ 
      }.

  nd:[tree[comp[T]],comp[T],tree[comp[T]]]@=tree[comp[T]].
  nd(L,B,R)..{
    insert(E)::E.eq(B) => this.
    insert(E)::E.less(B) => nd(L.insert(E),B,R).
    insert(E)::\+E.less(B) => nd(L,B,R.insert(E)).
  }.
}
