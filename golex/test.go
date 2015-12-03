test{
  import go.io.


-- A Test golex file

-- The preamble ...

import go.stdparse.

yyTokType ::= INT(integer) | FLT(float) | ID(string) | EOF.


  yyLexer <~ { nextToken:[]=>yyToken. currentToken:[]=>yyToken. }.
  yyToken <~ { token:[]=>yyTokType. isToken:[yyTokType]{}. line:[]=>integer. pos:[]=>integer. }.

  private yyFinal <~ { final:[string,integer,integer]=>yyToken. }.

  private yyStatus <~ { line:[]=>integer. pos:[]=>integer.
      next:[yyStatus-]=>char. text:[]=>string. }.

  private yyS:[string,integer,integer]@=yyStatus.
  yyS(Text,L,P)..{
    line()=>L.
    pos()=>P.
    next(yyS(R,aL,aP))::[Ch,..R].=Text => valof{
      ( Ch==`\n ?
        aL = L+1
      | aL = L
      );
      aP = P+1;
      valis Ch
    }.
    next(this)::[]=Text => `\+ffff;.

    text() => Text.
  }.

  yyT:[yyTokType,integer,integer]@=yyToken.
  yyT(Tk,L,P)..{
    token()=>Tk.
    isToken(Tk).
    line()=>L.
    pos()=>P.
    show()=>Tk.show()<>"@"<>L.show()<>"/"<>P.show().
  }.

  yytest:[string,integer,integer]@>yyLexer.
  yytest(Text,startLine,startPos)..{
    cToken:yyToken:=yyT(EOF,-1,0).
    currIsValid:logical := false.
    Strm:string:=Text.
    cLine:integer :=startLine.
    cPos:integer:=startPos.
    currentToken()::currIsValid=>cToken.
    currentToken()=>raise error("no token available",'eFAIL').

    nextToken()=>
      valof{
        initStrm = yyS(Strm,cLine,cPos);
        (nT,nS,nL,nP) = yyNextState0(initStrm,[],initStrm,[],yyNoFinal);
        cLine := nL; cPos := nP; Strm := nS; cToken := nT;
        valis nT
      }.

    private yyNextStateX:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextStateX(_,_,yLast,lTok,Final) =>
      (Final.final(reverse(lTok),yLast.line(),yLast.pos()),yLast.text(),yLast.line(),yLast.pos()).

    private yyNextState19:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState19(yStream,_,_,_,yFinal) =>
      yyNextState0(yStream,[],yStream,[],yFinal).  -- restart in initial state

    private yyNextState18:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
     yyNextState18(yStream,yTok,_,_,yFinal) =>
      valof{      -- potential to switch to comment
        yyChar = yStream.next(yyNext);
        valis yyTrans18(yyChar,yyNext,[yyChar,..yTok],yStream,[],yStream,[],yFinal)
      }.

    private yyNextState17:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState17(yStream,_,_,_,yFinal) =>
      yyNextState16(yStream,[],yStream,[],yFinal).  -- restart in comment state

    private yyNextState16:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState16(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans16(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState15:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState15(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans15(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState14:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState14(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans14(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal0)
      }.

    private yyNextState13:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState13(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans13(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState12:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState12(yStream,_,_,_,yFinal) =>
      yyNextState0(yStream,[],yStream,[],yFinal).  -- restart in initial state

    private yyNextState11:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState11(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans11(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal0)
      }.

    private yyNextState10:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState10(yStream,_,_,_,yFinal) =>
      yyNextState16(yStream,[],yStream,[],yFinal).  -- restart in comment state

    private yyNextState9:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState9(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans9(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState8:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState8(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans8(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState7:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState7(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans7(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal1)
      }.

    private yyNextState6:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
     yyNextState6(yStream,yTok,_,_,yFinal) =>
      valof{      -- potential to switch to initial
        yyChar = yStream.next(yyNext);
        valis yyTrans6(yyChar,yyNext,[yyChar,..yTok],yStream,[],yStream,[],yFinal)
      }.

    private yyNextState5:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState5(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans5(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState4:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState4(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans4(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState3:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState3(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans3(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal1)
      }.

    private yyNextState2:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState2(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans2(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal2)
      }.

    private yyNextState1:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState1(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal3). -- new final candidate

    private yyNextState0:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState0(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans0(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.



  private yyTrans18:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans18(`/,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState19(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans18(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState16(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans16:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans16(`*,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState18(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans16(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`+=<X,X=<`\d =>
      yyNextState17(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans16(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`) =>
      yyNextState17(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans16(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState17(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans16(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\v=<X,X=<`\r =>
      yyNextState17(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans16(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\a=<X,X=<`\t =>
      yyNextState17(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans16(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans15:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans15(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState14(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans15(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans14:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans14(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState14(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans14(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans13:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans13(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState14(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans13(`-,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState15(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans13(`+,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState15(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans13(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans11:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans11(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState13(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans11(`E,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState13(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans11(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState11(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans11(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans9:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans9(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState12(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans9(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`\d =>
      yyNextState9(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans9(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState9(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans9(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\v=<X,X=<`\r =>
      yyNextState9(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans9(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\a=<X,X=<`\t =>
      yyNextState9(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans9(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans8:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans8(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState11(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans8(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans7:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans7(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans7(`.,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState8(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans7(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans6:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans6(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState6(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState6(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\b=<X,X=<`\n =>
      yyNextState6(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState0(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans5:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans5(`*,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState10(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans4:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans4(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState7(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`-,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState9(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans3:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans3(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState7(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(`.,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState8(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans2:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans2(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`z =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans0:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans0(`\+ffff;,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState1(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`-,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`z =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`/,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState6(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState6(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\b=<X,X=<`\n =>
      yyNextState6(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).

  }.

  private yyFinal3:[]@=yyFinal.
  yyFinal3..{
    final(_,yyLine,yyPos) => yyT(EOF,yyLine,yyPos).
  }.

  private yyFinal2:[]@=yyFinal.
  yyFinal2..{
    final(yyTok,yyLine,yyPos) => yyT((ID(yyTok)),yyLine,yyPos).
  }.

  private yyFinal1:[]@=yyFinal.
  yyFinal1..{
    final(yyTok,yyLine,yyPos) => yyT((INT(integerOf%%yyTok)),yyLine,yyPos).
  }.

  private yyFinal0:[]@=yyFinal.
  yyFinal0..{
    final(yyTok,yyLine,yyPos) => yyT((FLT(floatOf%%yyTok)),yyLine,yyPos).
  }.

   private yyNoFinal:[]@=yyFinal.
   yyNoFinal..{
     final(yyTok,yyLine,yyPos) => raise error("cannot tokenise: "<>yyTok<>" at line "<>yyLine.show()<>"/"<>yyPos.show(),'eFAIL').
  }.


-- The postamble

  private parseAll:[yyLexer]*.
  parseAll(Lx) ->
    Tok = Lx.nextToken();
    stdout.outLine("Token is "<>Tok.show());
    ( Tok.token()!=EOF?
        parseAll(Lx)
    ).

  main([])::stdin.eof() -> {}.
  main([]) ->
      parseAll(yytest(stdin.inLine("\n"),0,0));
    main([]).
    
  main(L) ->
	(S in L *>
	   f = openInFile(S,unknownEncoding);
	   Text = f.inText("");
	   f.close();
	   parseAll(yytest(Text,0,0))).
	

}.
