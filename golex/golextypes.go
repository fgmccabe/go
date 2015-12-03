golextypes{
  regexp <~ thing.

  emptyRE:[]@=regexp.
  emptyRE..{
    show() => "<null>"
  }.

  ordRE:[char]@=regexp.
  ordRE(C)..{
    show()::C==`\+ffff; => "<eof>".
    show()=>[`\",C,`\"].
  }.

  orRE:[regexp,regexp]@=regexp.
  orRE(L,R)..{
    show() => "("<> showOr(this)<>")".

    private showOr:[regexp]=>string.
    showOr(orRE(l,r)) => showOr(l)<>"|"<>showOr(r).
    showOr(S) => S.show().
  }.

  catRE:[regexp,regexp]@=regexp.
  catRE(L,R)..{
    show() => "("<> L.show()<>showRight(R)<>")".

    private showRight:[regexp]=>string.
    showRight(catRE(l,r)) => l.show()<>showRight(r).
    showRight(S) => S.show().
  }.

  starRE:[regexp]@=regexp.
  starRE(L)..{
    show() => L.show()<>"*".
  }.

  plusRE:[regexp]@=regexp.
  plusRE(L)..{
    show() => L.show()<>"+".
  }.

  optRE:[regexp]@=regexp.
  optRE(L)..{
    show() => L.show()<>"?".
  }.

  seqRE:[string]@=regexp.
  seqRE(S)..{
    show() => "\""<>S<>"\"".
  }.

  charsRE:[string]@=regexp.
  charsRE(S)..{
    show() => "["<>S<>"]".
  }.

  negCharsRE:[string]@=regexp.
  negCharsRE(S)..{
    show() => "[^"<>S<>"]".
  }.

  periodRE:[]@=regexp.
  periodRE..{
    show() => "."
  }.

  finalRE:[regexp,finalType]@=regexp.
  finalRE(L,F)..{
    show() => L.show()<>"=>"<>F.show().
  }.

  finalType <~ thing.

  state:[string]@=finalType.
  state(St)..{
    show() => " <"<>St<>">".
  }.

  accept:[string]@=finalType.
  accept(St)..{
    show() => "accept "<>St.
  }.

  switch:[string,integer]@=finalType.
  switch(St,Sno)..{
    show() => "switch to <"<>St<>"> start state "<>Sno.show().
  }.

  dfaE ::= dfa(char,integer).

  ndfEdge::=
	  ordTR(char,integer,integer)
	| emptrTR(integer,integer)
	| dotTR(integer,integer)
	| finalTR(integer,finalType).

  dfaEdge ::= dfaE(integer,list[(char,integer)],finalType).

}