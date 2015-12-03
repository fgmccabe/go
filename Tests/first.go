first{
  -- The first program to try out

  -- import go.io.

  p:[symbol]{}.
  p('a').
  p('b').

  q:[symbol]{}.
  q('b').

  n:[symbol]{}.
  n(X) :- p(X),q(X).

  main(_) ->
      {n(X)}.
}