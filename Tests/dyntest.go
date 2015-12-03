/*
 * Test filtered lists
 */
 
dyntest{
  import go.dynamic.
  import go.io.
     
  tree[_A] ::= empty | node(tree[_A],_A,tree[_A]).

--  tree[A] <~ {}.

--  empty:tree[A] .. {}.

--  node(_1:tree[A],_2:A,_3:tree[A]):tree[A]..{}.
  
  main(_) ->
      D = $dynamic([empty,node(empty,'a',empty),node(node(empty,'a',empty),'b',empty),node(empty,'a',node(empty,'b',empty))]);
      stdout.outLine(D.ext().show());
      (D.mem(node(_,Lb,empty)) *> stdout.outLine(Lb.show())).
}
