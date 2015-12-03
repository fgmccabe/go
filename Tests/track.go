track{
  import go.io.

  a:[symbol]{}.
  b:[symbol]{}.
  c:[symbol]{}.

  a(X) :- b(X), c(X).

  b('a').
  b('c').

  c('c').

  main(_) ->
      a(X) *> stdout.outLine(X.show()).
}