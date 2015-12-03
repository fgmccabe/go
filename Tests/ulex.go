ulex{
  import go.io.


/*
 * Lexical rules for the UL parser
 */

import go.stdparse.

yyTokType ::= PRINT | HALT | INT(integer) | ID(string) | EQUAL | SEMI | LPAR | RPAR
            | PLUS | MINUS | STAR | SLASH | EOF.


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

  yyulex:[string,integer,integer]@>yyLexer.
  yyulex(Text,startLine,startPos)..{
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
        (nT,nS,nL,nP) = yyNextState(0,initStrm,[],initStrm,[],yyNoFinal);
        cLine := nL; cPos := nP; Strm := nS; cToken := nT;
        valis nT
      }.

    private yyNextState:[integer,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).

    yyNextState(28,yStream,_,_,_,yFinal) =>
      yyNextState(0,yStream,[],yStream,[],yFinal).  -- restart in initial state

     yyNextState(27,yStream,yTok,_,_,yFinal) =>
      valof{      -- potential to switch to comment
        yyChar = yStream.next(yyNext);
        valis yyTrans27(yyChar,yyNext,[yyChar,..yTok],yStream,[],yStream,[],yFinal)
      }.

    yyNextState(26,yStream,_,_,_,yFinal) =>
      yyNextState(25,yStream,[],yStream,[],yFinal).  -- restart in comment state

    yyNextState(25,yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans25(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    yyNextState(24,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans24(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal0)
      }.

    yyNextState(23,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans23(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal1)
      }.

    yyNextState(22,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans22(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal1)
      }.

    yyNextState(21,yStream,_,_,_,yFinal) =>
      yyNextState(0,yStream,[],yStream,[],yFinal).  -- restart in initial state

    yyNextState(20,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans20(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal1)
      }.

    yyNextState(19,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans19(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal1)
      }.

    yyNextState(18,yStream,_,_,_,yFinal) =>
      yyNextState(25,yStream,[],yStream,[],yFinal).  -- restart in comment state

    yyNextState(17,yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans17(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    yyNextState(16,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans16(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal1)
      }.

    yyNextState(15,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans15(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal1)
      }.

     yyNextState(14,yStream,yTok,_,_,yFinal) =>
      valof{      -- potential to switch to initial
        yyChar = yStream.next(yyNext);
        valis yyTrans14(yyChar,yyNext,[yyChar,..yTok],yStream,[],yStream,[],yFinal)
      }.

    yyNextState(13,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans13(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal2)
      }.

    yyNextState(12,yStream,yTok,_,_,_) =>
      yyNextState(-1,yStream,yTok,yStream,yTok,yyFinal3). -- new final candidate

    yyNextState(11,yStream,yTok,_,_,_) =>
      yyNextState(-1,yStream,yTok,yStream,yTok,yyFinal4). -- new final candidate

    yyNextState(10,yStream,yTok,_,_,_) =>
      yyNextState(-1,yStream,yTok,yStream,yTok,yyFinal5). -- new final candidate

    yyNextState(9,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans9(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal6)
      }.

    yyNextState(8,yStream,yTok,_,_,_) =>
      yyNextState(-1,yStream,yTok,yStream,yTok,yyFinal7). -- new final candidate

    yyNextState(7,yStream,yTok,_,_,_) =>
      yyNextState(-1,yStream,yTok,yStream,yTok,yyFinal8). -- new final candidate

    yyNextState(6,yStream,yTok,_,_,_) =>
      yyNextState(-1,yStream,yTok,yStream,yTok,yyFinal9). -- new final candidate

    yyNextState(5,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans5(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal10)
      }.

    yyNextState(4,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans4(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal1)
      }.

    yyNextState(3,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans3(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal1)
      }.

    yyNextState(2,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans2(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal1)
      }.

    yyNextState(1,yStream,yTok,_,_,_) =>
      yyNextState(-1,yStream,yTok,yStream,yTok,yyFinal11). -- new final candidate

    yyNextState(0,yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans0(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    yyNextState(-1,_,_,yLast,lTok,Final) =>
      (Final.final(reverse(lTok),yLast.line(),yLast.pos()),yLast.text(),yLast.line(),yLast.pos()).



  private yyTrans27:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans27(`/,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(28,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(25,yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans25:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans25(`*,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(27,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans25(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`+=<X,X=<`\d =>
      yyNextState(26,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans25(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`) =>
      yyNextState(26,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans25(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(26,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans25(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\a=<X,X=<`\r =>
      yyNextState(26,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans25(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans24:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans24(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans24(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans24(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans24(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans24(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans23:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans23(`t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(24,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans23(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`u=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans23(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`s =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans23(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans23(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans23(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans23(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans22:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans22(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans22(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans22(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans22(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans22(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans20:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans20(`n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(23,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans20(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`o=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans20(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`m =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans20(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans20(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans20(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans20(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans19:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans19(`t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(22,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans19(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`u=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans19(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`s =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans19(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans19(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans19(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans19(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans17:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans17(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(21,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans17(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`\d =>
      yyNextState(17,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans17(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(17,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans17(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\v=<X,X=<`\r =>
      yyNextState(17,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans17(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\a=<X,X=<`\t =>
      yyNextState(17,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans17(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans16:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans16(`i,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(20,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans16(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`j=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans16(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`h =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans16(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans16(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans16(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans16(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans15:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans15(`l,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(19,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans15(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`m=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans15(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`k =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans15(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans15(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans15(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans15(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans14:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans14(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(14,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans14(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(14,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans14(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\b=<X,X=<`\n =>
      yyNextState(14,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans14(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(0,yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans13:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans13(`*,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(18,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans13(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).





  private yyTrans9:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans9(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState(5,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans9(`-,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(17,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans9(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).





  private yyTrans5:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans5(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState(5,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans4:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans4(`r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(16,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`s=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`q =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans3:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans3(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(15,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`b=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans2:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans2(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans0:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans0(`\+ffff;,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(1,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`h,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(3,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`p,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(4,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState(5,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`*,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(6,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`(,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(7,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`),yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(8,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`-,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(9,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`q=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`i=<X,X=<`o =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`g =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`=,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(10,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`+,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(11,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`;,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(12,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`/,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(13,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(14,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(14,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\b=<X,X=<`\n =>
      yyNextState(14,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).

  }.

  private yyFinal11:[]@=yyFinal.
  yyFinal11..{
    final(_,yyLine,yyPos) => yyT(EOF,yyLine,yyPos).
  }.

  private yyFinal10:[]@=yyFinal.
  yyFinal10..{
    final(yyTok,yyLine,yyPos) => yyT(( INT(integerOf%%yyTok) ),yyLine,yyPos).
  }.

  private yyFinal9:[]@=yyFinal.
  yyFinal9..{
    final(_,yyLine,yyPos) => yyT(( STAR ),yyLine,yyPos).
  }.

  private yyFinal8:[]@=yyFinal.
  yyFinal8..{
    final(_,yyLine,yyPos) => yyT(( LPAR ),yyLine,yyPos).
  }.

  private yyFinal7:[]@=yyFinal.
  yyFinal7..{
    final(_,yyLine,yyPos) => yyT(( RPAR ),yyLine,yyPos).
  }.

  private yyFinal6:[]@=yyFinal.
  yyFinal6..{
    final(_,yyLine,yyPos) => yyT(( MINUS ),yyLine,yyPos).
  }.

  private yyFinal5:[]@=yyFinal.
  yyFinal5..{
    final(_,yyLine,yyPos) => yyT(( EQUAL ),yyLine,yyPos).
  }.

  private yyFinal4:[]@=yyFinal.
  yyFinal4..{
    final(_,yyLine,yyPos) => yyT(( PLUS ),yyLine,yyPos).
  }.

  private yyFinal3:[]@=yyFinal.
  yyFinal3..{
    final(_,yyLine,yyPos) => yyT(( SEMI ),yyLine,yyPos).
  }.

  private yyFinal2:[]@=yyFinal.
  yyFinal2..{
    final(_,yyLine,yyPos) => yyT(( SLASH ),yyLine,yyPos).
  }.

  private yyFinal1:[]@=yyFinal.
  yyFinal1..{
    final(yyTok,yyLine,yyPos) => yyT(( ID(yyTok) ),yyLine,yyPos).
  }.

  private yyFinal0:[]@=yyFinal.
  yyFinal0..{
    final(_,yyLine,yyPos) => yyT(( PRINT ),yyLine,yyPos).
  }.

   private yyNoFinal:[]@=yyFinal.
   yyNoFinal..{
     final(yyTok,yyLine,yyPos) => raise error("cannot tokenise: "<>yyTok<>" at line "<>yyLine.show()<>"/"<>yyPos.show(),'eFAIL').
  }.


  private parseAll:[yyLexer]*.
  parseAll(Lx) ->
    Tok = Lx.nextToken();
    stdout.outLine("Token is "<>Tok.show());
    ( Tok.token()!=EOF?
        parseAll(Lx)
    ).

  private main([])::stdin.eof() -> {}.
  main([]) ->
      parseAll(yyulex(stdin.inLine("\n"),0,0));
    main([]).
    




}.
