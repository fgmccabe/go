/*

  A golex rule file looks like:

  preamble (including type definition of final tokens)

  %%

  rules

  %% 

  extra-definitions


  A rule looks like:

  rule::= 
    <state> regexp => cmd		-- do cmd on a regexp
  | regexp => cmd			-- do cmd on a regexp

  A cmd looks like

  cmd::= skip				-- restart the tokeniser
  | <state>				-- restart the tokeniser in state
  | (final)				-- an expression giving the final token

  A regular expression looks like:

   regexp ::= 
       .				-- any character
     | eof                              -- the end of file
     | [chars]				-- selection of characters
     | [^chars]				-- negative selection of characters
     | "echar*"				-- literal string
     | 'echar*'				-- case-insensitive literal string
     | (regexp)				-- parenthesising
     | regexp|regexp			-- disjunction
     | regexp regexp			-- concatenation of two regexps
     | regexp *
     | regexp +
     | regexp ?				-- optional re

   chars ::= char-char			-- range of characters
     | echar				-- regular or escape char

   echar ::= \escape			-- escape character
     | char				-- regular character

   escape ::= a				-- alarm
     | b				-- backspace
     | d				-- delete
     | e				-- escape
     | f				-- form feed
     | n				-- new line
     | r				-- carriage return
     | t				-- tab
     | v				-- vertical feed
     | oct				-- octal encoded character
     | char				-- other chars are quoted
*/  

genregexp{
  import go.io.
  import golextypes.
  import go.stdparse.
  import go.setlib.

  parseFile:[list[(string,regexp)]]-->string.
  parseFile([("initial",finalRE(ordRE(`\+ffff;),accept("EOF")))]) --> skip(),eof.
  parseFile([(State,RE),..S]) --> 
      parseRule(State,RE),
      skip(),
      parseFile(S).

  parseRule:[string,regexp] --> string.
  parseRule(State,Final) --> 
      parseState(State),
      parseRE(RE),
      skip(),
      parseCmd(RE,State,Final),
      skip().

  private parseState:[string] --> string.
  parseState(S) --> "<",grabChars(S,[],`>).
  parseState("initial") --> "".

  private parseRE:[regexp]-->string.
  parseRE(T) --> skip(), parseL(Left), parseR(Left,T).

  private parseL:[regexp] --> string.
  parseL(periodRE) --> ".".
  parseL(negCharsRE(S1)) --> "[^", grabChars(S,[],`\]), S1=S\/[`\+ffff;].
  parseL(charsRE(S)) --> "[", grabChars(S,[],`\]).
  parseL(S) --> "\'", strChars(Str,[],`\'),S = catIfy(Str).
  parseL(seqRE(S)) --> "\"", strChars(S,[],`\").
  parseL(R) --> "(", parseRE(R), ")".
  parseL(ordRE(`\+ffff;)) --> "eof".

  private parseR:[regexp,regexp]-->string.
  parseR(Left,P) --> "|", parseRE(O), parseR(orRE(Left,O),P).
  parseR(Left,Left),")" --> ")".
  parseR(Left,Left),"-" --> "-".
  parseR(Left,Left),"=" --> "=".
  parseR(Left,Left)," " --> " ".
  parseR(Left,Left),"\t" --> "\t".
  parseR(Left,P) --> "*", parseR(starRE(Left),P).
  parseR(Left,P) --> "+", parseR(plusRE(Left),P).
  parseR(Left,P) --> "?", parseR(optRE(Left),P).
  parseR(Left,Left) --> eof.
  parseR(Left,P) --> parseRE(Right), parseR(catRE(Left,Right),P).


  private parseCmd:[regexp,string,regexp]-->string.
  parseCmd(RE,State,Final) --> "=>", skip(),
      ( "skip", Final = finalRE(RE,state(State))
      | "<", grabChars(S,[],`>), Final = finalRE(RE,state(S))
      | "(", grabExp(S,[`\(], "\)"), Final = finalRE(RE,accept(S))
      | "{", grabExp(S,[`\{], "\}"), Final = finalRE(RE,accept(S))
      | "" , grabExp(S,[],"\n"), Final = finalRE(RE,accept(S))
      ).
  parseCmd(RE,State,finalRE(RE,state(State))) --> "".

  private catIfy:[string]=>regexp.
  catIfy("")=>emptyRE.
  catIfy([C])=>caseInSens(C).
  catIfy([C,..R])=>catRE(caseInSens(C),catIfy(R)).

  private caseInSens:[char]=>regexp.
  caseInSens(X)::isAlphaCh(X) => charsRE([toLower(X),toUpper(X)]).
  caseInSens(X)=>charsRE([X]).

  private isAlphaCh:[char]{}.
  isAlphaCh(C) :- isLowerCh(C).
  isAlphaCh(C) :- isUpperCh(C).

  private isLowerCh:[char]{}.
  isLowerCh(C) :- `a =< C, C=<`z.

  private isUpperCh:[char]{}.
  isUpperCh(C) :- `A =< C, C=<`Z.

  private toLower:[char] => char.
  toLower(C)::isUpperCh(C) => __charOf(__charCode(C)+0x20).
  toLower(C) => C.

  private toUpper:[char] => char.
  toUpper(C)::isLowerCh(C) => __charOf(__charCode(C)-0x20).
  toUpper(C) => C.

  private grabChars:[string,string,char]-->string.
  grabChars(S,R,T) --> [T], S=reverse(R).
  grabChars(S,R,_) --> eof, S=reverse(R).
  grabChars(S,R,T) --> strChar(Fr), "-", strChar(To), grabChars(S,reverse(genChars(Fr,To))<>R,T).
  grabChars(S,R,T) --> strChar(C), grabChars(S,[C,..R],T).

  private genChars:[char,char]=>string.
  genChars(F,T) => { __charOf(C) .. C in iota(__charCode(F),__charCode(T))}.

  private strChars:[string,string,char] --> string.
  strChars(S,R,T) --> [T], S=reverse(R).
  strChars(S,R,_) --> eof, S=reverse(R).
  strChars(S,R,T) --> strChar(C), strChars(S,[C,..R],T).

  private grabExp:[string,string,string] --> string.
  grabExp(S,R,[]) --> "", S=reverse(R).
  grabExp(S,R,[T,..C]) --> [T], grabExp(S,[T,..R],C).
  grabExp(S,R,_) --> eof, S=reverse(R).
  grabExp(S,R,T) --> "\"", expStr(S,[`\",..R],T,`\").
  grabExp(S,R,T) --> "\'", expStr(S,[`\',..R],T,`\').
  grabExp(S,R,T) --> [O],{openParen(O,C)}, grabExp(S,[O,..R],[C,..T]).
  grabExp(S,R,T) --> [C], grabExp(S,[C,..R],T).

  private expStr:[string,string,string,char]-->string.
  expStr(S,R,eT,sT) --> [sT], grabExp(S,[sT,..R],eT).
  expStr(S,R,eT,sT) --> [C], expStr(S,[C,..R],eT,sT).
  expStr(S,R,_,_) --> eof, S=reverse(R).

  private openParen:[char,char]{}.
  openParen(`(,`)) :-- {}.
  openParen(`[,`]) :-- {}.
  openParen(`{,`}) :-- {}.

  private skip:[]-->string.
  skip() --> skipBlanks()!.

  private skipBlanks:[]-->string.
  skipBlanks() --> ([C]::whiteSpace(C)),skipBlanks().
  skipBlanks() --> "--",skipToCr().
  skipBlanks() --> "/*",skipToStarSlash().
  skipBlanks() --> "".

  private skipToCr:[]-->string.
  skipToCr() --> "\n", skipBlanks().
  skipToCr() --> [_], skipToCr().
  skipToCr() --> eof.

  private skipToStarSlash:[]-->string.
  skipToStarSlash() --> "*/", skipBlanks().
  skipToStarSlash() --> [_], skipToStarSlash().
  skipToStarSlash() --> eof.
}