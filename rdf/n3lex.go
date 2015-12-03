n3lex{
  import go.io.


/*
 * A golex file for the N3 parser
 */

yyTokType ::= BARENAME(string)
          | URI(string)
	  | QNAME(string,string)
	  | INT(integer) | DEC(float) | FLT(float) | STR(string)
	  | PREFIX | A | IS | HAS | OF
	  | LANG(string) | ID(string)
	  | HAT | EQUAL | LESSEQ | GREATERQ 
	  | EOF
          | DOT | COMMA | SEMI 
	  | LPAR | RPAR | LSQ | RSQ | LBRC | RBRC.

-- Import stuff
  import go.hash.
  import go.stdparse.
  import go.io.

  keywords:hash[string,yyTokType] = hash([("a",A),
                                          ("is",IS),("has",HAS),
					  ("of",OF),
					  ("prefix", PREFIX) ],8).


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

  yyn3lex:[string,integer,integer]@>yyLexer.
  yyn3lex(Text,startLine,startPos)..{
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

    private yyNextState64:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState64(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal0). -- new final candidate

    private yyNextState63:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState63(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans63(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState62:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState62(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans62(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState61:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState61(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans61(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState60:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState60(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans60(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState59:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState59(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal1). -- new final candidate

    private yyNextState58:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState58(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans58(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal2)
      }.

    private yyNextState57:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState57(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans57(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState56:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState56(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans56(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState55:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState55(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal3). -- new final candidate

    private yyNextState54:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState54(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal4). -- new final candidate

    private yyNextState53:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState53(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans53(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState52:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState52(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans52(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal2)
      }.

    private yyNextState51:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState51(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans51(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState50:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState50(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans50(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState49:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState49(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal5). -- new final candidate

    private yyNextState48:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState48(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans48(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal6)
      }.

    private yyNextState47:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState47(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal7). -- new final candidate

    private yyNextState46:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState46(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans46(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState45:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState45(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal8). -- new final candidate

    private yyNextState44:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState44(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans44(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal9)
      }.

    private yyNextState43:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState43(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans43(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState42:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState42(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans42(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal10)
      }.

    private yyNextState41:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState41(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal11). -- new final candidate

    private yyNextState40:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState40(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal12). -- new final candidate

    private yyNextState39:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState39(yStream,_,_,_,yFinal) =>
      yyNextState0(yStream,[],yStream,[],yFinal).  -- restart in initial state

    private yyNextState38:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
     yyNextState38(yStream,yTok,_,_,yFinal) =>
      valof{      -- potential to switch to initial
        yyChar = yStream.next(yyNext);
        valis yyTrans38(yyChar,yyNext,[yyChar,..yTok],yStream,[],yStream,[],yFinal)
      }.

    private yyNextState37:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
     yyNextState37(yStream,yTok,_,_,yFinal) =>
      valof{      -- potential to switch to initial
        yyChar = yStream.next(yyNext);
        valis yyTrans37(yyChar,yyNext,[yyChar,..yTok],yStream,[],yStream,[],yFinal)
      }.

    private yyNextState36:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
     yyNextState36(yStream,yTok,_,_,yFinal) =>
      valof{      -- potential to switch to initial
        yyChar = yStream.next(yyNext);
        valis yyTrans36(yyChar,yyNext,[yyChar,..yTok],yStream,[],yStream,[],yFinal)
      }.

    private yyNextState35:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
     yyNextState35(yStream,yTok,_,_,yFinal) =>
      valof{      -- potential to switch to initial
        yyChar = yStream.next(yyNext);
        valis yyTrans35(yyChar,yyNext,[yyChar,..yTok],yStream,[],yStream,[],yFinal)
      }.

    private yyNextState34:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState34(yStream,_,_,_,yFinal) =>
      yyNextState0(yStream,[],yStream,[],yFinal).  -- restart in initial state

    private yyNextState33:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState33(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal13). -- new final candidate

    private yyNextState32:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState32(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans32(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState31:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState31(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans31(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal14)
      }.

    private yyNextState30:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState30(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans30(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState29:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState29(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans29(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal15)
      }.

    private yyNextState28:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState28(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans28(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal16)
      }.

    private yyNextState27:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState27(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans27(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal6)
      }.

    private yyNextState26:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState26(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans26(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal6)
      }.

    private yyNextState25:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
     yyNextState25(yStream,yTok,_,_,yFinal) =>
      valof{      -- potential to switch to initial
        yyChar = yStream.next(yyNext);
        valis yyTrans25(yyChar,yyNext,[yyChar,..yTok],yStream,[],yStream,[],yFinal)
      }.

    private yyNextState24:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState24(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans24(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState23:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState23(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans23(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState22:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState22(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal17). -- new final candidate

    private yyNextState21:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState21(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal18). -- new final candidate

    private yyNextState20:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState20(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans20(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal10)
      }.

    private yyNextState19:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState19(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal19). -- new final candidate

    private yyNextState18:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState18(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal20). -- new final candidate

    private yyNextState17:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState17(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal21). -- new final candidate

    private yyNextState16:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState16(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal22). -- new final candidate

    private yyNextState15:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState15(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal23). -- new final candidate

    private yyNextState14:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState14(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans14(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal24)
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
    yyNextState12(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal25). -- new final candidate

    private yyNextState11:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState11(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal26). -- new final candidate

    private yyNextState10:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState10(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans10(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

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
        valis yyTrans7(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal14)
      }.

    private yyNextState6:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState6(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans6(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal6)
      }.

    private yyNextState5:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState5(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans5(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal6)
      }.

    private yyNextState4:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState4(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans4(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal6)
      }.

    private yyNextState3:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState3(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans3(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal6)
      }.

    private yyNextState2:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState2(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans2(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal6)
      }.

    private yyNextState1:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState1(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal27). -- new final candidate

    private yyNextState0:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState0(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans0(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.



  private yyTrans63:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans63(`x,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState64(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans63(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans62:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans62(`i,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState63(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans62(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans61:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans61(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`f =>
      yyNextState61(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans61(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`F =>
      yyNextState61(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans61(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState61(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans61(`;,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState24(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans61(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans60:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans60(`f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState62(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans60(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans58:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans58(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState30(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans58(`E,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState30(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans58(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState52(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans58(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans57:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans57(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`f =>
      yyNextState61(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans57(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`F =>
      yyNextState61(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans57(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState61(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans57(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans56:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans56(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState60(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans56(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).




  private yyTrans53:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans53(`s,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState59(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans53(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans52:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans52(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState30(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans52(`E,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState30(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans52(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState58(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans52(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans51:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans51(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState58(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans51(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans50:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans50(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState49(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans50(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans48:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans48(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`z =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans48(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans48(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans48(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans48(`:,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans48(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans46:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans46(`+,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState57(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans46(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`,=<X,X=<`\d =>
      yyNextState24(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans46(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`* =>
      yyNextState24(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans46(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState24(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans46(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\a=<X,X=<`\r =>
      yyNextState24(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans46(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans44:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans44(`>,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState45(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans44(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`?=<X,X=<`\d =>
      yyNextState43(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans44(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`= =>
      yyNextState43(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans44(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState43(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans44(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\a=<X,X=<`\r =>
      yyNextState43(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans44(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans43:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans43(`>,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState45(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`?=<X,X=<`\d =>
      yyNextState43(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`= =>
      yyNextState43(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState43(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\a=<X,X=<`\r =>
      yyNextState43(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans42:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans42(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`z =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans42(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans42(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans42(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans42(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).





  private yyTrans38:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans38(`r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState56(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans38(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState0(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans37:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans37(`f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState55(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState0(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans36:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans36(`s,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState54(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans36(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState0(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans35:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans35(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState53(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans35(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState0(yLStrm,yLtok,yLStrm,yLtok,yFinal).




  private yyTrans32:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans32(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState52(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans32(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans31:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans31(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState30(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans31(`E,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState30(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans31(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState7(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans31(`.,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState51(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans31(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans30:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans30(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState49(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans30(`-,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState50(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans30(`+,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState50(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans30(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans29:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans29(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`z =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans29(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans29(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans29(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans29(`:,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans29(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans28:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans28(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`z =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans28(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans28(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans28(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans28(`:,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans28(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans27:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans27(`s,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState48(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`t=<X,X=<`z =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`r =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(`:,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans26:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans26(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`z =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans26(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans26(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans26(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans26(`:,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans26(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans25:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans25(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState25(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans25(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState25(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans25(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\b=<X,X=<`\n =>
      yyNextState25(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans25(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState0(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans24:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans24(`\\,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState46(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans24(`\",yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState47(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans24(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`]=<X,X=<`\d =>
      yyNextState24(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans24(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`#=<X,X=<`[ =>
      yyNextState24(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans24(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`! =>
      yyNextState24(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans24(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState24(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans24(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\a=<X,X=<`\r =>
      yyNextState24(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans24(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans23:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans23(`=,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState44(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans23(`>,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState45(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans23(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`?=<X,X=<`\d =>
      yyNextState43(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans23(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`< =>
      yyNextState43(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans23(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState43(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans23(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\a=<X,X=<`\r =>
      yyNextState43(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans23(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).




  private yyTrans20:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans20(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`z =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans20(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans20(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans20(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).







  private yyTrans14:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans14(`>,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState41(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans14(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans13:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans13(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState31(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans13(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).




  private yyTrans10:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans10(`^,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState40(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans10(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans9:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans9(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState39(yStrm,yTok,yLStrm,yLtok,yFinal).
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
  yyTrans8(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState33(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans8(`h,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState35(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans8(`i,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState36(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans8(`o,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState37(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans8(`p,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans8(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`q=<X,X=<`z =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans8(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`j=<X,X=<`n =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans8(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`b=<X,X=<`g =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans8(`],yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans8(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`(=<X,X=<`[ =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans8(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans8(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans8(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans7:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans7(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState30(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans7(`E,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState30(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans7(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState31(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans7(`.,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState32(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans7(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans6:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans6(`f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState29(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`g=<X,X=<`z =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`e =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(`:,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans5:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans5(`s,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState28(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`t=<X,X=<`z =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`r =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`:,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans4:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans4(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState27(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`b=<X,X=<`z =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`:,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans3:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans3(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`z =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(`:,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans2:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans2(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`z =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(`:,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans0:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans0(`\+ffff;,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState1(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`h,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`i,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`o,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState6(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState7(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`@,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState8(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`#,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState9(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`^,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState10(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`(,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState11(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`),yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState12(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`p=<X,X=<`z =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`j=<X,X=<`n =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`b=<X,X=<`g =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`=,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState14(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`-,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState13(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`+,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState13(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`[,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState15(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`],yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState16(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`{,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState17(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`},yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState18(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`;,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState19(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`:,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`,,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState21(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`.,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState22(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`<,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState23(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`\",yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState24(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState25(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState25(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\b=<X,X=<`\n =>
      yyNextState25(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).

  }.

  private yyFinal27:[]@=yyFinal.
  yyFinal27..{
    final(_,yyLine,yyPos) => yyT(EOF,yyLine,yyPos).
  }.

  private yyFinal26:[]@=yyFinal.
  yyFinal26..{
    final(_,yyLine,yyPos) => yyT(LPAR
,yyLine,yyPos).
  }.

  private yyFinal25:[]@=yyFinal.
  yyFinal25..{
    final(_,yyLine,yyPos) => yyT(RPAR
,yyLine,yyPos).
  }.

  private yyFinal24:[]@=yyFinal.
  yyFinal24..{
    final(_,yyLine,yyPos) => yyT(EQUAL
,yyLine,yyPos).
  }.

  private yyFinal23:[]@=yyFinal.
  yyFinal23..{
    final(_,yyLine,yyPos) => yyT(LSQ
,yyLine,yyPos).
  }.

  private yyFinal22:[]@=yyFinal.
  yyFinal22..{
    final(_,yyLine,yyPos) => yyT(RSQ
,yyLine,yyPos).
  }.

  private yyFinal21:[]@=yyFinal.
  yyFinal21..{
    final(_,yyLine,yyPos) => yyT(LBRC
,yyLine,yyPos).
  }.

  private yyFinal20:[]@=yyFinal.
  yyFinal20..{
    final(_,yyLine,yyPos) => yyT(RBRC
,yyLine,yyPos).
  }.

  private yyFinal19:[]@=yyFinal.
  yyFinal19..{
    final(_,yyLine,yyPos) => yyT(SEMI
,yyLine,yyPos).
  }.

  private yyFinal18:[]@=yyFinal.
  yyFinal18..{
    final(_,yyLine,yyPos) => yyT(COMMA
,yyLine,yyPos).
  }.

  private yyFinal17:[]@=yyFinal.
  yyFinal17..{
    final(_,yyLine,yyPos) => yyT(DOT
,yyLine,yyPos).
  }.

  private yyFinal16:[]@=yyFinal.
  yyFinal16..{
    final(_,yyLine,yyPos) => yyT((IS),yyLine,yyPos).
  }.

  private yyFinal15:[]@=yyFinal.
  yyFinal15..{
    final(_,yyLine,yyPos) => yyT((OF),yyLine,yyPos).
  }.

  private yyFinal14:[]@=yyFinal.
  yyFinal14..{
    final(yyTok,yyLine,yyPos) => yyT(INT(integerOf%%yyTok)
,yyLine,yyPos).
  }.

  private yyFinal13:[]@=yyFinal.
  yyFinal13..{
    final(_,yyLine,yyPos) => yyT(A
,yyLine,yyPos).
  }.

  private yyFinal12:[]@=yyFinal.
  yyFinal12..{
    final(_,yyLine,yyPos) => yyT(HAT
,yyLine,yyPos).
  }.

  private yyFinal11:[]@=yyFinal.
  yyFinal11..{
    final(_,yyLine,yyPos) => yyT(GREATERQ
,yyLine,yyPos).
  }.

  private yyFinal10:[]@=yyFinal.
  yyFinal10..{
    final(yyTok,yyLine,yyPos) => yyT((qname(yyTok)),yyLine,yyPos).
  }.

  private yyFinal9:[]@=yyFinal.
  yyFinal9..{
    final(_,yyLine,yyPos) => yyT(LESSEQ
,yyLine,yyPos).
  }.

  private yyFinal8:[]@=yyFinal.
  yyFinal8..{
    final(yyTok,yyLine,yyPos) => yyT(explicitURI(yyTok)   -- Looking for an explicit URI
,yyLine,yyPos).
  }.

  private yyFinal7:[]@=yyFinal.
  yyFinal7..{
    final(yyTok,yyLine,yyPos) => yyT(STR(tok2str(yyTok))
,yyLine,yyPos).
  }.

  private yyFinal6:[]@=yyFinal.
  yyFinal6..{
    final(yyTok,yyLine,yyPos) => yyT((
		       keywords.present(yyTok,Tok) ? Tok | 
		       BARENAME(yyTok)),yyLine,yyPos).
  }.

  private yyFinal5:[]@=yyFinal.
  yyFinal5..{
    final(yyTok,yyLine,yyPos) => yyT(FLT(floatOf%%yyTok)
,yyLine,yyPos).
  }.

  private yyFinal4:[]@=yyFinal.
  yyFinal4..{
    final(_,yyLine,yyPos) => yyT(IS
,yyLine,yyPos).
  }.

  private yyFinal3:[]@=yyFinal.
  yyFinal3..{
    final(_,yyLine,yyPos) => yyT(OF
,yyLine,yyPos).
  }.

  private yyFinal2:[]@=yyFinal.
  yyFinal2..{
    final(yyTok,yyLine,yyPos) => yyT(DEC(floatOf%%yyTok)
,yyLine,yyPos).
  }.

  private yyFinal1:[]@=yyFinal.
  yyFinal1..{
    final(_,yyLine,yyPos) => yyT(HAS
,yyLine,yyPos).
  }.

  private yyFinal0:[]@=yyFinal.
  yyFinal0..{
    final(_,yyLine,yyPos) => yyT(PREFIX
,yyLine,yyPos).
  }.

   private yyNoFinal:[]@=yyFinal.
   yyNoFinal..{
     final(yyTok,yyLine,yyPos) => raise error("cannot tokenise: "<>yyTok<>" at line "<>yyLine.show()<>"/"<>yyPos.show(),'eFAIL').
  }.


  private tok2str:[string]=>string.

  tok2str([`",..X]) => dequoteStr(X,`").

  private dequoteStr:[string,char]=>string.

  dequoteStr([C],C)=>[].
  dequoteStr([],_) =>[].
  dequoteStr([`\\,`a,..L],C) => [`\a,..dequoteStr(L,C)].
  dequoteStr([`\\,`b,..L],C) => [`\b,..dequoteStr(L,C)].
  dequoteStr([`\\,`d,..L],C) => [`\d,..dequoteStr(L,C)].
  dequoteStr([`\\,`e,..L],C) => [`\e,..dequoteStr(L,C)].
  dequoteStr([`\\,`f,..L],C) => [`\f,..dequoteStr(L,C)].
  dequoteStr([`\\,`n,..L],C) => [`\n,..dequoteStr(L,C)].
  dequoteStr([`\\,`r,..L],C) => [`\r,..dequoteStr(L,C)].
  dequoteStr([`\\,`t,..L],C) => [`\t,..dequoteStr(L,C)].
  dequoteStr([`\\,`v,..L],C) => [`\v,..dequoteStr(L,C)].
  dequoteStr([`\\,`+,..L],C) => 
     valof{
       extractUnicode(L,U,rL);
       valis [__charOf(hexNum%%U),..dequoteStr(rL,C)]
     }.
  dequoteStr([`\\,X,..L],C) => [X,..dequoteStr(L,C)].
  dequoteStr([X,..L],C) => [X,..dequoteStr(L,C)].

  private extractUnicode:[string,string-,string-]*.
  extractUnicode([],[],[])->{}.
  extractUnicode([`;,..L],[],L)->{}.
  extractUnicode([C,..L],[C,..U],rL) -> extractUnicode(L,U,rL).

  private tok2int:[string]=>integer.
  tok2int(L)=>integerOf%%L.

  private tok2float:[string]=>float.
  tok2float(L)=>floatOf%%L.

  private tok2sym:[string]=>symbol.
  tok2sym([`',..L])=>implode(dequoteStr(L,`')).

  private tok2chr:[string]=>char.
  tok2chr([``,..L]) => dequoteStr(L,`\+ffff;).head().

  private qname:[string]=>yyTokType.
  qname(N)::append(F,[`:,..B],N) => QNAME(F,B).

  private explicitURI:[string]=>yyTokType.
  explicitURI([`<,..U]) :: append(F,">",U) => URI(F).

  private n3tokens:[yyLexer]=>list[yyToken].
  n3tokens(Lx) =>
     valof{
       tK = Lx.nextToken();
       (tK.token()=EOF ?
         valis []
       | valis [tK,..n3tokens(Lx)]
       )
     }.

  tokenizeFile:[string]=>list[yyToken].
  tokenizeFile(F) => n3tokens(yyn3lex(getFile(F,utf8Encoding),1,1)).



}.
