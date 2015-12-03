/*
 * Parse CFG files, using the DCG notation for now
 
 A CFG grammar is based on the TDPL notation ...

 Each rule is of the form:

 nt -> exp

 where exp is one of

 () the empty parse

 "abc..." a string of single character expansions

 . any character

 e1 e2 e3 sequence of expansions

 (e) a parenthesised expansion

 e1/e2/e3 an ordered set of choices

 [a...z] a regexp-style character class expression

 e* greedy repetition, zero or more

 e+ greedy repetition, one or more

 e? optional expansion

*/
dcgparse{
  import go.io.
  import go.stdparse.
  import ctypes.

  -- The main grammar for the language handles expansions
  expansion:[cfg] --> string.
  expansion(E) --> choiceExpansion(E).

  choiceExpansion:[cfg] --> string.
  choiceExpansion(E) --> catStar(E1), ws(), 
      ( "/", ws(), choiceExpansion(E2), 
	E = choice(flatChoice(choice([E1,E2])))
      | E = E1
      ).

  private flatChoice:[cfg]=>list[cfg].
  flatChoice(choice(L)) => flatten({flatChoice(l)..l in L}).
  flatChoice(X) => [X].

  catStar:[cfg] --> string.
  catStar(E) --> starExp(E1), ws(), ( catStar(E2), E = cat([E1,E2])
			      | E = E1).

  starExp:[cfg] --> string.
  starExp(E) --> exp0(E1), ws(), ( "+", E = plus(E1)
				 | "*", E = star(E1)
				 | "?", E = optional(E1)
				 | E = E1
      ).

  exp0:[cfg] --> string.
  exp0(start) --> "^".
  exp0(end) --> "$".
  exp0(empty) --> "(", ws(), ")".
  exp0(E) --> "(", ws(), expansion(E), ws(), ")".
  exp0(str(S)) --> parseString(S).
  exp0(Nt(_,Id)) --> ident(I), {Id=implode(I)}.
  exp0(period) --> "@".
  exp0(chars(S)) --> "[",grabChars(S,[],`\]).
  exp0(negchars(S)) --> "[^",grabChars(S,[],`\]).

  tdplRule:[rule]-->string.
  tdplRule(rule(NT,RHS,S)) --> ws(),
      ident(I), {NT=implode(I)},
      ws(),"->",ws(),expansion(E),ws(), 
      semantic(S), ws(),
      dotSpace(),
      RHS = flatCat(E).

  semantic:[string]-->string.
  semantic(S) --> "{",collectText(S,0),ws(),"}".
  semantic("$$=$1") --> "".

  collectText:[string,integer]-->string.
  collectText([`\',..S],D) -->"'", collectStr(S,D,`\').
  collectText([`\",..S],D) --> "\"", collectStr(S,D,`\").
  collectText([`-,`-,` ,..S],D) --> "-- ",collectStr(S,D,`\n).
  collectText([`\(,..S],D) --> "(", collectText(S,D+1).
  collectText([`\[,..S],D) --> "[", collectText(S,D+1).
  collectText([`\{,..S],D) --> "{", collectText(S,D+1).
  collectText([],0), ")" --> ")".
  collectText([`\),..S],D) --> ")", collectText(S,D-1).
  collectText([],0), "]" --> "]".
  collectText([`\],..S],D) --> "]", collectText(S,D-1).
  collectText([],0), "}" --> "}".
  collectText([`\},..S],D) --> "}", collectText(S,D-1).
  collectText([C,..S],D) --> [C], collectText(S,D).
  collectText([],_) --> eof.

  collectStr:[string,integer,char]-->string.
  collectStr([C,..S],D,C) --> [C], collectText(S,D).
  collectStr([C,..S],D,T) --> [C], collectStr(S,D,T).
  collectStr([],_,_) --> eof.


  private flatCat:[cfg]=>list[cfg].
  flatCat(cat(L)) => flatten({flatCat(l)..l in L}).
  flatCat(empty)=>[].
  flatCat(X) => [X].
  

  tdplGrammar:[list[rule]]-->string.
  tdplGrammar(Rules) --> tdplRule(Rl)*Rl^Rules.

  private dotSpace:[]-->string.
  dotSpace()-->".",[X],{whiteSpace(X)}.
  dotSpace()-->".",eof.
  
  private grabChars:[string,string,char]-->string.

  grabChars(S,R,T) --> [T], S=reverse(R).
  grabChars(S,R,_) --> eof, S=reverse(R).
  grabChars(S,R,T) --> strChar(Fr), "-", strChar(To), grabChars(S,reverse(genChars(Fr,To))<>R,T).
  grabChars(S,R,T) --> strChar(C), grabChars(S,[C,..R],T).

  private genChars:[char,char]=>string.
  genChars(F,T) => { __charOf(C) .. C in iota(__charCode(F),__charCode(T))}.

  private ws:[] --> string.
  ws() --> comment(0,_), ws().
  ws() --> skipWhiteSpace().
  
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
}

 