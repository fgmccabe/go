-- Simple test of iterator form of grammar rule

iden{
  import go.io.
  import go.unit.

  tokType ::= i(string) | s(string).

  tokens:[list[tokType]]-->string.
  tokens(L) --> (whiteSpace(),token(T))*T^L.

  token:[tokType]-->string.
  token(T) --> identifier(T).
  token(T) --> symb(T).

  identifier:[tokType]-->string.
  identifier(i([C,..L])) --> [C],{idStart(C)}, letter(X) * X ^ L.

  symb:[tokType]-->string.
  symb(s(S)) --> "'", nonQuote(C)*C^S, "'".

  letter:[char]-->string.
  letter(X) --> [X], { idChar(X) }.

  idStart:[char]{}.
  idStart(X) :- __isLetterChar(X).
  idStart(`_).
  
  idChar:[char]{}.
  idChar(X) :- idStart(X).
  idChar(X) :- __isNdChar(X).
  idChar(X) :- __isMnChar(X).
  idChar(X) :- __isMcChar(X).
  idChar(X) :- __isPcChar(X).
  idChar(X) :- __isCfChar(X).

  nonQuote:[char]-->string.
  nonQuote(X) --> [X],{X\=`\'}.

  whiteSpace:[]-->string.
  whiteSpace() --> " ",whiteSpace().
  whiteSpace() --> "".

  identest:[string]@=harness.
  identest(_)<=harness.
  identest(T)..{
    doAction() ->
        ( (tokens(L) --> T) ?
            stdout.outLine("Parsed into "<>L.show())
        | stdout.outLine("could not parse "<>T)
        ).
  }.

  main([T]) -> checkUnit(identest(T)).
  main([]) -> checkUnit(identest("now is \'the time\' to end")).
}





 