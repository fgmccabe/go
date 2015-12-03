show{
  import go.io.
  import bintree.

  tree[T] ::= leaf(T) | node(tree[T],tree[T]).

  sample:tree[symbol] = node(node(leaf('a'),leaf('b')),leaf('c')).

  main(_) ->
      stdout.outLine([sample].show()).
}
