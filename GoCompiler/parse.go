/*
 * Go! parser module
 * Part of the Go! in Go! compiler
*/
parse{
  import go.io.
  import errors.
  import opts.
  import ops.
  import abstract.
  import tokenizer.

  parse:[abstract,string]-->list[TokenType].
  parse(pTerm,uri) -->
      term(pTerm,2000,implode(uri)),skipTerm().

   -- The main primitive term is term0, handles all non-operator expressions
  term0:[abstract,symbol] --> list[TokenType].
  term0(CHR(S,Loc),U) --> [tk(CH(S),Ln)], Loc=loc(U,Ln,Ln).
  term0(STR(st,Loc),U) --> [tk(ST(S),Ln)], moreStrings(S,st,Ln,Lx)!, 
      Loc=loc(U,Ln,Lx).
  term0(INT(N,Loc),U) --> [tk(IN(N),Ln)],Loc=loc(U,Ln,Ln).
  term0(FLT(N,Loc),U) --> [tk(FT(N),Ln)],Loc=loc(U,Ln,Ln).
  term0(SYM(S,Loc),U) --> [tk(SY(S),Ln)],Loc=loc(U,Ln,Ln).
  term0(T,U) --> term00(L,U), termArgs(L,T,U)! .  -- applicative expressions
  
  moreStrings:[string,string,integer,integer] --> list[TokenType].
  moreStrings(S,st,_,Lx) --> [tk(ST(more),Ln)],
      moreStrings(S<>more,st,Ln,Lx).
  moreStrings(S,S,Ln,Ln) --> [].

  term00:[abstract,symbol] --> list[TokenType].
  term00(IDEN(S,Loc),U) --> [tk(ID(S),Ln)], {\+ isOperator(S)}, Loc=loc(U,Ln,Ln).
  term00(IDEN(S,Loc),U),[tk(RPAR,L0)] --> [tk(ID(S),Ln),tk(RPAR,L0)],Loc=loc(U,Ln,Ln).
  term00(VOID(Loc),U) --> [tk(ID(S),Ln)], {isOperator(S),Loc=loc(U,Ln,Ln),
        action{ reportError("Unexpected operator: ["<>explode(S)<>"]",Loc)} }.
  term00(T,U) --> parenTerm(T,U).
  
  -- parenthesised expressions
  parenTerm:[abstract,symbol] --> list[TokenType].
  parenTerm(IDEN('()',Loc),U) --> [tk(LPAR,Le),tk(RPAR,Lx)],Loc=loc(U,Le,Lx).
  parenTerm(IDEN(Id,Loc),U) --> [tk(LPAR,_),tk(ID(Id),Ln),tk(RPAR,_)],Loc=loc(U,Ln,Ln).
  parenTerm(T,U) --> [tk(LPAR,L1)], term(T,2000,U), checkParen(LPAR,RPAR,U,L1,_).
  parenTerm(IDEN('[]',Loc),U) --> [tk(LBRA,Ln),tk(RBRA,Lx)],Loc=loc(U,Ln,Lx).
  parenTerm(APPLY(IDEN(',..',Loc),[L,R],Loc),U) --> 
      [tk(LBRA,Ln)], term(L,999,U), tList(U,R),Loc=loc(U,Ln,Ln).
  parenTerm(IDEN('{}',Loc),U) --> [tk(LBRCE,Ln),tk(RBRCE,Lx)],Loc=loc(U,Ln,Lx).
  parenTerm(APPLY(IDEN('{}',Loc),[T],Loc),U) -->
      [tk(LBRCE,Le)], 
      term(T,2000,U), checkParen(LBRCE,RBRCE,U,Le,Lx),Loc=loc(U,Le,Lx).

  tList:[symbol,abstract] --> list[TokenType].
  tList(U,IDEN('[]',Loc)) --> [tk(RBRA,Ln)],Loc=loc(U,Ln,Ln).
  tList(U,T) --> [tk(CONS,_)],term(T,999,U),[tk(RBRA,_)].
  tList(U,APPLY(IDEN(',..',nLoc),[L,R],nLoc)) --> 
      [tk(COMMA,_)], term(L,999,U), tList(U,R),nLoc=mergeLoc(L,R).
  tList(U,VOID(Loc)) , [tk(Tok,Ln)] --> [tk(Tok,Ln)],
      Loc=loc(U,Ln,Ln),
      {action{reportError("']', ',' or ',..' expected, got "<>Tok.show(),Loc)}}.
      
--  We have to be a little careful when encountering an identifier,
  termArgs:[abstract,abstract,symbol] --> list[TokenType].
  termArgs(Pref,Term,U) --> [tk(LBRA,_),tk(RBRA,Lx)],
      termArgs(SQUARE(Pref,[],extendLoc(Pref,Lx)),Term,U).
  termArgs(Pref,Term,U) --> [tk(LBRA,Le)],
      term(F,999,U),
      tupleTerm([F],U,Args),
      checkParen(LBRA,RBRA,U,Le,Lx),
      termArgs(SQUARE(Pref,Args,extendLoc(Pref,Lx)),Term,U).
  termArgs(Pref,Term,U) --> [tk(LBRCE,_),tk(RBRCE,Lx)],
      termArgs(BRACE(Pref,[],extendLoc(Pref,Lx)),Term,U).
  termArgs(Pref,Term,U) --> [tk(LBRCE,Ln)], 
      term(F,2000,U), checkParen(LBRCE,RBRCE,U,Ln,Lx),
      termArgs(BRACE(Pref,[F],extendLoc(Pref,Lx)),Term,U).
  termArgs(Pref,Term,U) --> [tk(LPAR,_),tk(RPAR,Lx)],
      termArgs(APPLY(Pref,[],extendLoc(Pref,Lx)),Term,U).
  termArgs(Pref,Term,U) --> [tk(LPAR,Ln)], term(F,999,U),
      tupleTerm([F],U,Args), checkParen(LPAR,RPAR,U,Ln,Lx),
      termArgs(APPLY(Pref,Args,extendLoc(Pref,Lx)),Term,U).
  termArgs(Pref,Term,U) --> [tk(ID('.'),L0),tk(ID(Nm),Ln)], 
      termArgs(APPLY(IDEN('.',loc(U,L0,L0)),
                     [Pref,IDEN(Nm,loc(U,Ln,Ln))],extendLoc(Pref,Ln)),Term,U).
  termArgs(T,T,_)-->[].
      
  tupleTerm:[list[abstract],symbol,list[abstract]]-->list[TokenType].
  tupleTerm(soFar,U,Tlist) --> [tk(COMMA,_)],
      term(El,999,U),
      tupleTerm([El,..soFar],U,Tlist).
  tupleTerm(L,_,rL) --> [],rL=reverse(L).

  checkParen:[tokType,tokType,symbol,integer,integer]-->list[TokenType].
  checkParen(_,rgt,_,_,Lx) --> [tk(rgt,Lx)].
  checkParen(lft,rgt,U,Ln,L2), [tk(Tok,L2)] --> [tk(Tok,L2)],
      { action{ reportError(rgt.show()<>" expected at "<>Tok.show()<>
			    "\n"<>lft.show()<>" at line "<>Ln.show(),loc(U,L2,L2))} }.

  -- operator based expressions
  termLeft:[abstract,integer,integer,symbol]-->list[TokenType].
  termLeft(APPLY(IDEN(Op,Loc),[Right],nLoc),P,Oprior,U) --> 
      [tk(ID(Op),Ln)],
      {isPreOp(Op,Oprior,OrPrior), P>=Oprior},
      term(Right,OrPrior,U),
      nLoc=extendLoc(Right,Ln),
      Loc=loc(U,Ln,Ln).
    
  -- if not an operator, then it must be a primitive at this point
  termLeft(Term,_,0,U) --> term0(Term,U).
  
  -- infix and postfix operator expressions
  termRight:[abstract,integer,integer,integer,abstract,symbol]-->list[TokenType].
  termRight(Left,prior,lprior,aprior,Term,U) --> [tk(ID('.'),L0),tk(ID(Fld),Ln)],
      termRight(APPLY(IDEN('.',loc(U,L0,L0)),
                      [Left,IDEN(Fld,loc(U,Ln,Ln))],extendLoc(Left,Ln)),
                prior,lprior,aprior,Term,U).
  termRight(Left,prior,lprior,aprior,Term,U) --> [tk(ID(Op),Ln)],
      { isPstOp(Op,L,O),O=<prior,L>=lprior },
      ({isInfOp(Op,xL,xO,xR),xO=<prior,xL>=lprior},argLeader() ?  -- we pick the infix route here
         term(Right,xR,U),
         termRight(APPLY(IDEN(Op,loc(U,Ln,Ln)),[Left,Right],mergeLoc(Left,Right)),
                   prior,xO,aprior,Term,U)
     | termRight(APPLY(IDEN(Op,loc(U,Ln,Ln)),[Left],extendLoc(Left,Ln)),
                 prior,O,aprior,Term,U)).
  termRight(Left,prior,lprior,aprior,Term,U) --> [tk(ID(Op),Ln)],
      { isInfOp(Op,L,O,R),O=<prior,L>=lprior },
      term(Right,R,U),
      termRight(APPLY(IDEN(Op,loc(U,Ln,Ln)),[Left,Right],mergeLoc(Left,Right)),
                prior,O,aprior,Term,U).
  termRight(Left,prior,lprior,aprior,Term,U) --> [tk(COMMA,Ln)],
      { isInfOp(',',L,O,R),O=<prior,L>=lprior },
      term(Right,R,U),
      termRight(APPLY(IDEN(',',loc(U,Ln,Ln)),[Left,Right],mergeLoc(Left,Right)),
                prior,O,aprior,Term,U).
  termRight(Left,_,prior,prior,Left,_) --> [].
      
  term:[abstract,integer,symbol] --> list[TokenType].
  term(T,prior,U) -->
      termLeft(Left,prior,Lprior,U)!,
      termRight(Left,prior,Lprior,_,T,U)! .
  
  /* Look ahead one token for something that could be a simple term */
  argLeader:[]-->list[TokenType].
  argLeader(),[tk(Tk,Ln)]  --> [tk(Tk,Ln)], {aL(Tk)!} .

  aL:[tokType]{}.
  aL(ST(_)). aL(IN(_)). aL(FT(_)). aL(SY(_)). aL(LPAR). aL(LBRA). aL(LBRCE).
  aL(ID(N)):- \+ isOperator(N).
  aL(ID(N)) :- isPreOp(N,_,_).

  showCurr:[string] --> list[TokenType].
  showCurr(Msg),[Tk] --> [Tk], {__logmsg(Msg<>Tk.show()<>"\n")}.
  showCurr(_) --> [].
  
  skipTerm:[] --> list[TokenType].
  skipTerm() --> [tk(EOF,_)],eof.
  skipTerm() --> eof.

  isOperator:[symbol]{}.
  isOperator(N) :- (infOp(N,_,_,_)|preOp(N,_,_)|pstOp(N,_,_))! .

  isPreOp:[symbol,integer,integer]{}.
  isPreOp(N,O,R) :- preOp(N,O,R)! .

  isInfOp:[symbol,integer,integer,integer]{}.
  isInfOp(N,L,O,R) :- infOp(N,L,O,R)! .

  isPstOp:[symbol,integer,integer]{}.
  isPstOp(N,L,O) :- pstOp(N,L,O)! .

  extendLoc:[abstract,integer] => fileLoc.
  extendLoc(VOID(Loc),Ln) => Loc.extend(Ln).
  extendLoc(IDEN(_,Loc),Ln) => Loc.extend(Ln).
  extendLoc(SYM(_,Loc),Ln) => Loc.extend(Ln).
  extendLoc(INT(_,Loc),Ln) => Loc.extend(Ln).
  extendLoc(FLT(_,Loc),Ln) => Loc.extend(Ln).
  extendLoc(CHR(_,Loc),Ln) => Loc.extend(Ln).
  extendLoc(STR(_,Loc),Ln) => Loc.extend(Ln).
  extendLoc(APPLY(_,_,Loc),Ln) => Loc.extend(Ln).
  extendLoc(SQUARE(_,_,Loc),Ln) => Loc.extend(Ln).
  extendLoc(BRACE(_,_,Loc),Ln) => Loc.extend(Ln).


  mergeLoc:[abstract,abstract] => fileLoc.
  mergeLoc(L,VOID(Loc)) => extendLoc(L,Loc.to()).
  mergeLoc(L,IDEN(_,Loc)) => extendLoc(L,Loc.to()).
  mergeLoc(L,SYM(_,Loc)) => extendLoc(L,Loc.to()).
  mergeLoc(L,INT(_,Loc)) => extendLoc(L,Loc.to()).
  mergeLoc(L,FLT(_,Loc)) => extendLoc(L,Loc.to()).
  mergeLoc(L,CHR(_,Loc)) => extendLoc(L,Loc.to()).
  mergeLoc(L,STR(_,Loc)) => extendLoc(L,Loc.to()).
  mergeLoc(L,APPLY(_,_,Loc)) => extendLoc(L,Loc.to()).
  mergeLoc(L,SQUARE(_,_,Loc)) => extendLoc(L,Loc.to()).
  mergeLoc(L,BRACE(_,_,Loc)) => extendLoc(L,Loc.to()).
}