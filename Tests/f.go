f{
  import go.io.

  s:[symbol,symbol]{}.
  s('a','b').
  s('a','c').
  s('c','d').

  t:[symbol]{}.
  t('d').

  c:[symbol,symbol]{}.
  c(X,Y) :- s(X,Y), s(Y,Z), t(Z).

  a:[list[t],list[t]]=>list[t].
  a([],X) => X.
  a([E,..X],Y) => [E,..a(X,Y)].

  main(_) ->
      c(X,Y) *> 
      ( stdout.outLine("X: "<>X.show());
	stdout.outLine("Y: "<>Y.show())).
}