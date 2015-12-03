any{
  
  foo[A] ::= bar(A,B).

  check(bar(2,3)).

  funny(XX:A):foo[A]..{
    who()=>bar(XX,B).
  }
}.