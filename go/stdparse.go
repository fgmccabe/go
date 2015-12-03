/*
 * Some standard parsing programs, to be used with the %% operator
 */

go.stdparse{
  /* White space */
  whiteSpace:[char]{}.
  whiteSpace(X)::__isZsChar(X) :-- {}.
  whiteSpace(X)::__isZlChar(X) :-- {}.
  whiteSpace(X)::__isZpChar(X) :-- {}.
  whiteSpace(X)::__isCcChar(X) :-- {}.
  
  skipWhiteSpace:[]-->string.
  skipWhiteSpace() --> [X],{whiteSpace(X)}, skipWhiteSpace().
  skipWhiteSpace() --> "".

  /* Parse a numeric value */

  private str2nat:[string,integer]=>integer.
  str2nat("",I)=>I.
  str2nat([C,..L],I)::__isNdChar(C) => str2nat(L,I*10+__digitCode(C)).
  str2nat(L,_) => raise error("not a numeric string: "<>L,'eFAIL').

  str2integer:[string]=>integer.
  str2integer([])=>0.
  str2integer([C,..L])::whiteSpace(C)=>str2integer(L).
  str2integer([`-,..L])=>-str2nat(L,0).
  str2integer(L)=>str2nat(L,0).
  
  private numberSeq:[integer,integer]-->string.
  numberSeq(I,N) --> ([D],{__isNdChar(D)})!,
      numberSeq(I*10+__digitCode(D),N).
  numberSeq(N,N) --> "".

  private fraction:[float]-->string.
  fraction(F) --> ".", [C],{__isNdChar(C)}, frSeq(100.0,n2float(__digitCode(C))/10.0,F).
  fraction(0.0) --> "".

  private frSeq:[float+,float+,float-]-->string.
  frSeq(X,F,R) --> [C],{__isNdChar(C)}, frSeq(X*10.0,F+n2float(__digitCode(C))/X,R).
  frSeq(_,F,F) --> "".

  private exponent:[integer]-->string.
  exponent(E) --> "E-", numberSeq(0,X), E=10 ** (-X).
  exponent(E) --> "e-", numberSeq(0,X),E=10**(-X).
  exponent(E) --> "E", numberSeq(0,X),E=10**X.
  exponent(E) --> "e", numberSeq(0,X),E=10 ** X.
  exponent(E) --> "E+", numberSeq(0,X),E=10**X.
  exponent(E) --> "e+", numberSeq(0,X),E=10 ** X.
  exponent(1) --> "".
  
  hexDig:[integer]-->string.
  hexDig(H) --> [X],{__isNdChar(X),H=__digitCode(X)}.
  hexDig(10) --> "a".
  hexDig(10) --> "A".
  hexDig(11) --> "b".
  hexDig(11) --> "B".
  hexDig(12) --> "c".
  hexDig(12) --> "C".
  hexDig(13) --> "d".
  hexDig(13) --> "D".
  hexDig(14) --> "e".
  hexDig(14) --> "E".
  hexDig(15) --> "f".
  hexDig(15) --> "F".

  private hexSq:[integer,integer,integer]-->string.
  hexSq(I,N,(C::C>0)) --> hexDig(X),hexSq(I*16+X,N,C-1).
  hexSq(N,N,_) --> "".
  
  hexSeq:[integer,integer]-->string.
  hexSeq(I,C) --> hexSq(0,I,C).

  hexNum:[integer]-->string.
  hexNum(I) --> hexSq(0,I,16).
  
  strChar:[char]-->string.
  strChar(`\a) --> "\\a".
  strChar(`\b) --> "\\b".
  strChar(`\d) --> "\\d".
  strChar(`\e) --> "\\e".
  strChar(`\f) --> "\\f".
  strChar(`\n) --> "\\n".
  strChar(`\r) --> "\\r".
  strChar(`\t) --> "\\t".
  strChar(`\v) --> "\\v".
  strChar(`\') --> "\\\'".
  strChar(`\") --> "\\\"".
  strChar(`\\) --> "\\\\".
  strChar(H) --> "\\+",hexSeq(X,4)! , H=__charOf(X).
  strChar(X) --> [X].

  private numb:[float-]-->string.
  numb(Nm) --> "-", numb(X),Nm=-X.
  numb(Nm) --> [X],{__isNdChar(X)},
      numberSeq(__digitCode(X),I),fraction(F),exponent(E),
      Nm = (n2float(I)+F)*n2float(E).


  /* Parse a natural number */
  naturalOf:[integer-]-->string.
  naturalOf(X) --> skipWhiteSpace(),nat(X).

  /* parse a floating point number */
  floatOf:[float-]-->string.
  floatOf(X) --> skipWhiteSpace(), numb(X).

  private nat:[integer-]-->string.
  nat(X) --> "0x", hexSeq(X,24).
  nat(X) --> "0c", strChar(C), {X=__charCode(C)}.
  nat(I) --> [X],{__isNdChar(X)},numberSeq(__digitCode(X),I).
  
  /* Parse an integer number */
  integerOf:[integer-]-->string.
  integerOf(X) --> skipWhiteSpace(),int(X).

  private int:[integer-]-->string.
  int(X) --> "0x", hexSeq(X,24).
  int(X) --> "0c", strChar(C), {X=__charCode(C)}.
  int(I) --> [X],{__isNdChar(X)},numberSeq(__digitCode(X),I).
  int(I) --> "-", int(J),I=-J.


  ident:[string]-->string.
  ident([C,..L]) --> [C],{idStart(C)}, letter(X) * X ^ L.

  private idStart:[char]{}.
  idStart(X) :- __isLetterChar(X).
  idStart(`_).

  private letter:[char]-->string.
  letter(C) --> [C], { idChar(C) }.
  
  private idChar:[char]{}.
  idChar(X) :- idStart(X).
  idChar(X) :- __isNdChar(X).
  idChar(X) :- __isMnChar(X).
  idChar(X) :- __isMcChar(X).
  idChar(X) :- __isPcChar(X).
  idChar(X) :- __isCfChar(X).

  parseString:[string] --> string.
  parseString(Str) --> skipWhiteSpace(),
    [C], {C in [`\',`\"]},
    quoteSeq(Str,C).
  
  private quoteSeq:[string,char]-->string.
  quoteSeq([],Q) --> [Q].
  quoteSeq([C,..L],Q) --> strChar(C) !, quoteSeq(L,Q).
  quoteSeq([C,..L],Q) --> [C], quoteSeq(L,Q).
}.
