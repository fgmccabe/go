
/* 
 * Go! tokenization module 
 */
tokenizer{
  import go.io.
  import errors.

  tokType <~ {}.

  TokenType ::= tk(tokType,integer).

/* Comment rules */

  whiteSpace:[char]{}.
  whiteSpace(X)::__isZsChar(X) :-- {}.
  whiteSpace(X)::__isZlChar(X) :-- {}.
  whiteSpace(X)::__isZpChar(X) :-- {}.
  whiteSpace(X)::__isCcChar(X) :-- {}.

  comments:[integer,integer]-->string.
  comments(Lno,Lx) --> comment(Lno,Ly)!, comments(Ly,Lx).
  comments(Lno,Lno) --> "".

  comment:[integer,integer]-->string.
  comment(Lno,Lx) --> "-- ",lineComment(Lno,Lx).
  comment(Lno,Lx) --> "--\t",lineComment(Lno,Lx).
  comment(Lno,Lx) --> "/*",bodyComment(Lno,Lx).
  comment(Lno,L1) --> "\n",L1=Lno+1.     -- separator, line character
  comment(Lno,L1) --> [X],{__isZlChar(X)},L1=Lno+1.  -- separator, line character
  comment(Lno,Lno) --> [X],{__isZsChar(X)}.          -- spaces are ignored
  comment(Lno,Lno) --> [X],{__isCcChar(X)}.          -- spaces are ignored

  lineComment:[integer,integer]-->string.
  lineComment(Lno,L1) --> "\n",L1=Lno+1.
  lineComment(Lno,L1) --> [X],{__isZlChar(X)},L1=Lno+1.
  lineComment(Lno,Lx) --> [X],{\+__isZlChar(X)},lineComment(Lno,Lx).

  bodyComment:[integer,integer]-->string.
  bodyComment(Lno,Lno) --> "*/".
  bodyComment(Lno,Lx) --> "\n",bodyComment(Lno+1,Lx).
  bodyComment(Lno,Lx) --> [X],{__isZlChar(X)},bodyComment(Lno+1,Lx).
  bodyComment(Lno,Lx) --> [_],bodyComment(Lno,Lx).
  
  /* Identifier definition */
  idStart:[char]{}.
  idStart(X) :- __isLetterChar(X).
  idStart(`_).

  idChr:[char]-->string.
  idChr(X) --> [X],{idChar(X)}.

  idChar:[char]{}.
  idChar(X) :- idStart(X).
  idChar(X) :- __isNdChar(X).
  idChar(X) :- __isMnChar(X).
  idChar(X) :- __isMcChar(X).
  idChar(X) :- __isPcChar(X).
  idChar(X) :- __isCfChar(X).

  hexDig:[integer]-->string.
  hexDig(C) --> [X],{__isNdChar(X),C=__digitCode(X)}.
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

  hexSeq:[integer,integer]-->string.
  hexSeq(I,N) --> hexDig(X),hexSeq(I*16+X,N).
  hexSeq(N,N) --> "".

  quoteSeq:[string,char]-->string.
  quoteSeq([],Q) --> [Q].
  quoteSeq([C,..L],Q) --> strChar(C) !, quoteSeq(L,Q).
  quoteSeq([C,..L],Q) --> [C], quoteSeq(L,Q).

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
  strChar(C) --> "\\+",hexSeq(0,X),";",C=__charOf(X).
  strChar(C) --> "\\",[C].
  strChar(X) --> [X].

/* Number productions */
  private numberSeq:[integer,integer]-->string.
  numberSeq(I,N) --> [D],{__isNdChar(D)},numberSeq(I*10+__digitCode(D),N).
  numberSeq(N,N) --> "".

  private fraction:[float]-->string.
  fraction(F) --> ".", [C],{__isNdChar(C)}, frSeq(10.0,n2float(__digitCode(C))/10.0,F).

  private frSeq:[float+,float,float]-->string.
  frSeq(X,F,R) --> [C],{__isNdChar(C)}, frSeq(X*10.0,F+n2float(__digitCode(C))/X,R).
  frSeq(_,F,F) --> "".

  exponent:[integer]-->string.
  exponent(E) --> "E-", numberSeq(0,X), E=10 ** (-X).
  exponent(E) --> "e-", numberSeq(0,X),E=10**(-X).
  exponent(E) --> "E", numberSeq(0,X),E=10**X.
  exponent(E) --> "e", numberSeq(0,X),E=10 ** X.
  exponent(1) --> "".

/* Main tokenization grammar */

  tok:[tokType] --> string.
/* Identifier rule */
  tok(ID(Sy)) --> [X],{idStart(X)},idChr(C)*C^N,Sy=implode([X,..N]).

/* Number rules */
  tok(IN(X)) --> "0x",hexSeq(0,X).
  tok(IN(X)) --> "0c",strChar(C),{X=__charCode(C)}.
  tok(Nm) --> [X],{__isNdChar(X)},
      numberSeq(__digitCode(X),I),
      ( fraction(F),exponent(E),
	Nm = FT((n2float(I)+F)*n2float(E))
      | Nm = IN(I)).
        
/* Character rule */
  tok(CH(C)) --> "\`", strChar(C).

/* String rule */
  tok(ST(L)) --> "\"", quoteSeq(L,`\").

/* Symbol rule */
  tok(SY(Sy)) --> "\'", quoteSeq(L,`\'),Sy=implode(L).

/* Special symbols */

  tok(LPAR) --> "(".
  tok(RPAR) --> ")".
  tok(LBRA) --> "[".
  tok(RBRA) --> "]".
  tok(LBRCE) --> "{".
  tok(RBRCE) --> "}".
  
  tok(CONS) --> ",..".          -- This rule has priority over the next
  tok(COMMA) --> ",".
  tok(ID('. ')),[X] --> ".", [X],{whiteSpace(X)}. -- we need to put the white space back, in case its a line feed
  tok(ID('. ')) --> ".", eof.       -- a dot at the end of file also counts

  tok(ID(X)) --> standardTok(X)!.
    
  /* These must be in order. Essentially, if a following definition is longer than
     an earlier definition -- which is a legitimate prefix -- then the 
     longer one will not be found
   */ 
  

  standardTok:[symbol]-->string.
  standardTok('||') --> "||".
  standardTok('|') --> "|".
  standardTok(';') --> ";".
  standardTok('::=') --> "::=".
  standardTok('::') --> "::".
  standardTok(':=') --> ":=".
  standardTok(':--') --> ":--".
  standardTok(':-') --> ":-".
  standardTok(':') --> ":".
  standardTok('..') --> "..".
  standardTok('.=') --> ".=".
  standardTok('.'),[X] --> ".", [X],{idStart(X)}.
  standardTok('-->') --> "-->".
  standardTok('->') --> "->".
  standardTok('-+') --> "-+".
  standardTok('-') --> "-".
  standardTok('~') --> "~".
  standardTok('^') --> "^".
  standardTok('?') --> "?".
  standardTok('!=') --> "!=".
  standardTok('!') --> "!".
  standardTok('=>') --> "=>".
  standardTok('==') --> "==".
  standardTok('=<') --> "=<".
  standardTok('=') --> "=".
  standardTok('!=') --> "\\=".  -- A little fixing here!
  standardTok('\\+') --> "\\+".
  standardTok('\\/') --> "\\/".
  standardTok('\\') --> "\\".
  standardTok('$=') --> "$=".
  standardTok('$') --> "$".
  standardTok('*>') --> "*>".
  standardTok('**') --> "**".
  standardTok('*') --> "*".
  standardTok('/\\') --> "/\\".
  standardTok('/') --> "/".
  standardTok('>=') --> ">=".
  standardTok('>') --> ">".
  standardTok('<=') --> "<=".
  standardTok('<~') --> "<~".
  standardTok('<>') --> "<>".
  standardTok('<') --> "<".
  standardTok('++') --> "++".
  standardTok('+') --> "+".
  standardTok('%=') --> "%=".
  standardTok('%%') --> "%%".
  standardTok('@@') --> "@@".
  standardTok('@>') --> "@>".
  standardTok('@=') --> "@=".
  standardTok('@') --> "@".
  standardTok('#') --> "#".

/* Sequence of tokens grammar */
  
  tokens:[list[TokenType],integer]-->string.
  tokens(Toks,Lno) --> comments(Lno,Ln)!,
      rToks(Toks,Ln).
     
  rToks:[list[TokenType],integer] --> string.
  rToks([tk(Tk,Ln),..oks],Ln) --> 
      tok(Tk)!,
     -- { action{ stdout.outLine("Token : "<>showTok(Tk)<>" @ "<>Ln.show()) }},
      tokens(oks,Ln).
  rToks([tk(EOF,Ln)],Ln) --> eof.

  /* Implementation classes of the different token types */

  ID:[symbol]@=tokType.
  ID(Nm)..{
    show()::Nm=='. ' => ". (dot-space)".
    show() => explode(Nm).
  }.

  IN:[integer]@=tokType.
  IN(N)..{
    show() => N.show().
  }.

  FT:[float]@=tokType.
  FT(N)..{
    show() => N.show()<>"F".
  }.

  ST:[string]@=tokType.
  ST(S)..{
    show() => [`\",..S<>"\""].
  }.

  SY:[symbol]@=tokType.
  SY(Nm)..{
    show() => "'"<>explode(Nm)<>"'".
  }.

  CH:[char]@=tokType.
  CH(C)..{
    show() => [`\`,C].
  }.

  LPAR:[]@=tokType.
  LPAR..{
    show() => "(".
  }.

  RPAR:[]@=tokType.
  RPAR..{
    show() => ")".
  }.

  LBRA:[]@=tokType.
  LBRA..{
    show() => "[".
  }.

  RBRA:[]@=tokType.
  RBRA..{
    show() => "]".
  }.

  LBRCE:[]@=tokType.
  LBRCE..{
    show() => "{".
  }.

  RBRCE:[]@=tokType.
  RBRCE..{
    show() => "}".
  }.

  COMMA:[]@=tokType.
  COMMA..{
    show() => ",".
  }.

  CONS:[]@=tokType.
  CONS..{
    show() => ",..".
  }.

  EOF:[]@=tokType.
  EOF..{
    show() => "<eof>".
  }.

  showTokens:[list[TokenType],integer]*.
  showTokens([],_) -> stdout.outLine([]).
  showTokens([tk(Tk,Ln),..Toks],Ln) -> stdout.outStr(Tk.show()<>" ");
      showTokens(Toks,Ln).
  showTokens([tk(Tk,Ln),..Toks],_) -> 
      stdout.outStr("\n@"<>Ln.show()<>" "<>Tk.show()<>" ");
      showTokens(Toks,Ln).
}.
