parse{
  import go.io.
  import go.unit.
  import go.stdparse.

  private iden:[string]-->string.
  iden([X,..N]) --> [X],{idStart(X)},idChr(C)*C^N.

  private idStart:[char]{}.
  idStart(X) :- __isLetterChar(X).
  idStart(`_).

  private idChr:[char]-->string.
  idChr(X) --> [X],{idChar(X)}.

  private idChar:[char]{}.
  idChar(X) :- idStart(X).
  idChar(X) :- __isNdChar(X).
  idChar(X) :- __isMnChar(X).
  idChar(X) :- __isMcChar(X).
  idChar(X) :- __isPcChar(X).
  idChar(X) :- __isCfChar(X).

  private skip:[]-->string.
  skip()-->" ", skip().
  skip()-->"\t",skip().
  skip()-->"".
  
  tokType ::= id(string) | nm(integer).

  token:[tokType-]-->string.
  
  tokenize:[list[tokType]-]-->string.

  tokenize([])-->eof.
  tokenize([T,..K]) --> token(T),tokenize(K).

  token(T) --> skip(), [C],
      case C in (
       `I --> skip(),iden(I),T=id(I)
     | `N --> skip(),naturalOf(In),T=nm(In)
     | _ --> raise error("Cannot parse: "<>[C],'fail')
      ).

  testparse:[string]@=harness.
  testparse(_)<=harness.
  testparse(Text)..{
    doPred() :-
	( tokenize(List) --> "I twenty2 N234Isixty"),
	id("twenty2") in List,
	id("sixty") in List,
	nm(234) in List,
	\+id("234") in List,
	(id(Id) in List *> Id in ["twenty2","sixty"]).
    doAction() ->
	(( tokenize(List) --> Text) ?
	   stdout.outLine("Parsed "<>Text<>" to\n"<>List.show())
       | stdout.outLine("Could not parse "<>Text)
	).
  }.

  main([]) ->
      checkUnit(testparse("Ininety9 N23")).
  main([Txt]) ->
      checkUnit(testparse(Txt)).
}
