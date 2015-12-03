churn{
  import go.io.
  import go.stdparse.

  -- A simple program to exercise the global memory by creating churn

  XX:list[symbol] := [].

  run:[integer]*.
  run(0)->{}.
  run(C)::C>0->
      XX := { implode(S.show()) .. S in iota(1,C)};
      stdout.outLine("XX is now "<>XX.show());
      run(C-1).

  main([C,.._]) ->
      run(naturalOf%%C).
  main([]) ->
      run(1000).
}
