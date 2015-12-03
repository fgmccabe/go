bagcheck{
  import go.io.

  person:[symbol]{}.
  person('a').
  person('b').
  person('c').
  person('d').
  person('e').

  parent:[symbol,symbol]{}.
  parent('a','b').
  parent('a','c').
  parent(_,'b').
  parent('d','c').

  allPeople:[]=>list[symbol].
  allPeople() => { X || person(X) }.

  parentsOf:[symbol]=>list[symbol].
  parentsOf(X) => { P || parent(P,X) }.

  main(_) ->
      stdout.outLine("Everyone is "<>allPeople().show());
      stdout.outLine("parents of 'b' are "<>parentsOf('b').show());
      stdout.outLine("everyone's parents are "<>{ (X,parentsOf(X)) || person(X) }.show()).
}
  
