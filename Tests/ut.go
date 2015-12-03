../Tests/ut{
  import go.io.




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
        aL = L+1;
        aP = 0
      | aL = L;
        aP = P+1
      );
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

  yy../Tests/ut:[string,integer]@>yyLexer.
  yy../Tests/ut(Text,startLine)..{
    cToken:yyToken:=yyT(EOF,-1,0).
    currIsValid:logical := false.
    Strm:string:=Text.
    cLine:integer :=startLine.
    cPos:integer:=0.
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

    yyNextState(7,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans7(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal0)
      }.

    yyNextState(6,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans6(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal0)
      }.

    yyNextState(5,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans5(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal0)
      }.

    yyNextState(4,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans4(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal0)
      }.

    yyNextState(3,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans3(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal0)
      }.

    yyNextState(2,yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans2(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal0)
      }.

    yyNextState(1,yStream,yTok,_,_,_) =>
      yyNextState(-1,yStream,yTok,yStream,yTok,yyFinal1). -- new final candidate

    yyNextState(0,yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans0(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    yyNextState(-1,_,_,yLast,lTok,Final) =>
      (Final.final(reverse(lTok),yLast.line(),yLast.pos()),yLast.text(),yLast.line(),yLast.pos()).


  private yyTrans7:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans7(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans7(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans7(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans7(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans7(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans6:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans6(`t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(7,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`u=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`s =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans5:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans5(`n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(6,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`o=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`m =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans4:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans4(`i,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(5,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`j=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`h =>
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
  yyTrans3(`r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(4,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`s=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`q =>
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
  yyTrans0(`p,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(3,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`q=<X,X=<`z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`o =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState(2,yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState(-1,yLStrm,yLtok,yLStrm,yLtok,yFinal).

  }.

  private yyFinal1:[]@=yyFinal.
  yyFinal1..{
    final(_,yyLine,yyPos) => yyT(EOF,yyLine,yyPos).
  }.

  private yyFinal0:[]@=yyFinal.
  yyFinal0..{
    final(yyTok,yyLine,yyPos) => yyT(( ID(yyTok) ),yyLine,yyPos).
  }.

   private yyNoFinal:[]@=yyFinal.
   yyNoFinal..{
     final(yyTok,yyLine,yyPos) => raise error("cannot tokenise: "<>yyTok<>" at line "<>yyLine.show()<>"/"<>yyPos.show(),'eFAIL').
  }.




}.
