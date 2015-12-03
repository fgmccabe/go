code{
  import go.io.
  import go.stdparse.
  import tree.

  -- Test the code generation functionality

  test:[]*.
  test() ->
      T = [(1,2),_,(3,X),(4,X)];
      stdout.outLine("T = "<>T.show());
      TT = __term(T);
      stdout.outLine("TT = "<>TT.show());
      {__is(TT,M)};
      stdout.outLine("*TT = "<>M.show());
      OO = $T;
      stdout.outLine("OO = "<>__stringOf(OO,0,0));
      stdout.outLine("*OO = "<>OO.show()).

  emm:[]@=tree[comp[_]].
  emm..{
    show()=>"*".
    insert(a) => nd(em,a,em).
    empty().
  }.

  main(_) ->
      test().
}


