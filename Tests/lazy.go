/*
 * A first test of the lazy evaluation feature
 */
lazy{
  import go.io.

  data('a').
  data('b').

  check('b').

  lazy() :- data(X@@check(X)).

  main(_) ->
      { lazy() }.
}.

  