gpglex{
  import go.io.


/*
 * Lexical rules for the gpg parser
 */
import go.stdparse.

/*
 * Define the standard token forms
 */

yyTokType ::= TOKEN | LEFT | RIGHT | NONASSOC | PREC | EXPECT | START 
	  | ID(string)
          | INT(integer)
          | CENTBRACE(string)
          | BRACE(string)
          | COLON | BAR | SEMI | MARK | EOF .


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

  yygpglex:[string,integer,integer]@>yyLexer.
  yygpglex(Text,startLine,startPos)..{
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

    private yyNextState60:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState60(yStream,_,_,_,yFinal) =>
      yyNextState0(yStream,[],yStream,[],yFinal).  -- restart in initial state

    private yyNextState59:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
     yyNextState59(yStream,yTok,_,_,yFinal) =>
      valof{      -- potential to switch to comment
        yyChar = yStream.next(yyNext);
        valis yyTrans59(yyChar,yyNext,[yyChar,..yTok],yStream,[],yStream,[],yFinal)
      }.

    private yyNextState58:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState58(yStream,_,_,_,yFinal) =>
      yyNextState57(yStream,[],yStream,[],yFinal).  -- restart in comment state

    private yyNextState57:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState57(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans57(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState56:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState56(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal0). -- new final candidate

    private yyNextState55:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState55(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans55(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState54:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState54(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans54(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState53:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState53(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal1). -- new final candidate

    private yyNextState52:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState52(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal2). -- new final candidate

    private yyNextState51:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState51(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal3). -- new final candidate

    private yyNextState50:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState50(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal4). -- new final candidate

    private yyNextState49:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState49(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans49(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState48:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState48(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans48(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState47:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState47(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans47(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState46:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState46(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans46(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState45:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState45(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans45(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState44:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState44(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal5). -- new final candidate

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
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal6). -- new final candidate

    private yyNextState41:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState41(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans41(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState40:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState40(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal7). -- new final candidate

    private yyNextState39:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState39(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans39(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState38:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState38(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans38(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState37:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState37(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans37(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState36:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState36(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans36(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState35:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState35(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans35(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState34:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState34(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans34(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState33:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState33(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans33(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState32:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState32(yStream,_,_,_,yFinal) =>
      yyNextState0(yStream,[],yStream,[],yFinal).  -- restart in initial state

    private yyNextState31:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState31(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans31(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
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
    yyNextState29(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans29(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState28:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState28(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans28(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState27:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState27(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans27(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState26:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState26(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans26(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState25:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState25(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans25(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
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
    yyNextState23(yStream,_,_,_,yFinal) =>
      yyNextState57(yStream,[],yStream,[],yFinal).  -- restart in comment state

    private yyNextState22:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState22(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal8). -- new final candidate

    private yyNextState21:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState21(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans21(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState20:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState20(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans20(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState19:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState19(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal9). -- new final candidate

    private yyNextState18:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState18(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans18(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState17:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState17(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans17(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

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
    yyNextState14(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans14(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
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
    yyNextState12(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans12(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState11:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
     yyNextState11(yStream,yTok,_,_,yFinal) =>
      valof{      -- potential to switch to initial
        yyChar = yStream.next(yyNext);
        valis yyTrans11(yyChar,yyNext,[yyChar,..yTok],yStream,[],yStream,[],yFinal)
      }.

    private yyNextState10:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState10(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal10). -- new final candidate

    private yyNextState9:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState9(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans9(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState8:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState8(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal11). -- new final candidate

    private yyNextState7:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState7(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal12). -- new final candidate

    private yyNextState6:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState6(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans6(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
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
        valis yyTrans3(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal13)
      }.

    private yyNextState2:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState2(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans2(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal14)
      }.

    private yyNextState1:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState1(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal15). -- new final candidate

    private yyNextState0:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState0(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans0(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.



  private yyTrans59:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans59(`/,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState60(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans59(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState57(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans57:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans57(`*,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState59(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans57(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`+=<X,X=<`\d =>
      yyNextState58(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans57(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`) =>
      yyNextState58(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans57(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState58(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans57(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\a=<X,X=<`\r =>
      yyNextState58(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans57(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans55:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans55(`c,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState56(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans55(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans54:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans54(`o,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState55(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans54(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).






  private yyTrans49:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans49(`s,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState54(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans49(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans48:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans48(`t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState53(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans48(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans47:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans47(`n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState52(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans47(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans46:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans46(`t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState51(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans46(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans45:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans45(`t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState50(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans45(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans43:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans43(`s,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState49(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans41:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans41(`c,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState48(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans41(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans39:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans39(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState47(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans39(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans38:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans38(`r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState46(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans38(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans37:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans37(`h,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState45(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans36:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans36(`c,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState44(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans36(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans35:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans35(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState43(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans35(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans34:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans34(`t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans34(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans33:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans33(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState41(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans33(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans31:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans31(`},yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState40(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans31(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`~=<X,X=<`\d =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans31(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`| =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans31(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans31(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\a=<X,X=<`\r =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans31(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans30:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans30(`k,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState39(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans30(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans29:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans29(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans29(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans28:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans28(`g,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState37(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans28(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans27:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans27(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState36(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans26:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans26(`n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState35(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans26(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans25:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans25(`f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans25(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans24:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans24(`p,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState33(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans24(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).




  private yyTrans21:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans21(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState32(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans21(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`\d =>
      yyNextState21(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans21(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState21(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans21(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\v=<X,X=<`\r =>
      yyNextState21(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans21(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\a=<X,X=<`\t =>
      yyNextState21(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans21(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans20:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans20(`%,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState31(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans20(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`&=<X,X=<`\d =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans20(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`$ =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans20(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans20(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\a=<X,X=<`\r =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans20(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans18:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans18(`o,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState30(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans18(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans17:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans17(`t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState29(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans17(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans16:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans16(`i,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState28(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans16(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans15:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans15(`r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState27(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans15(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans14:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans14(`o,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans14(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans13:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans13(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState25(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans13(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans12:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans12(`x,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState24(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans12(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans11:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans11(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState11(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans11(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState11(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans11(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\b=<X,X=<`\n =>
      yyNextState11(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans11(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState0(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans9:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans9(`*,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState23(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans9(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).




  private yyTrans6:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans6(`},yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState22(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`~=<X,X=<`\d =>
      yyNextState6(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`| =>
      yyNextState6(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState6(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\a=<X,X=<`\r =>
      yyNextState6(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans5:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans5(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`-,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState21(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans4:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans4(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState12(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`l,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState13(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState14(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`p,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState15(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState16(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`s,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState17(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState18(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`%,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState19(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`{,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans3:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans3(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`0=<X,X=<`9 =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
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
  yyTrans0(`%,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`-,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`z =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`{,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState6(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`;,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState7(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`:,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState8(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`/,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState9(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`|,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState10(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState11(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState11(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`\b=<X,X=<`\n =>
      yyNextState11(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).

  }.

  private yyFinal15:[]@=yyFinal.
  yyFinal15..{
    final(_,yyLine,yyPos) => yyT(EOF,yyLine,yyPos).
  }.

  private yyFinal14:[]@=yyFinal.
  yyFinal14..{
    final(yyTok,yyLine,yyPos) => yyT(( ID(yyTok) ),yyLine,yyPos).
  }.

  private yyFinal13:[]@=yyFinal.
  yyFinal13..{
    final(yyTok,yyLine,yyPos) => yyT(( INT(integerOf%%yyTok) ),yyLine,yyPos).
  }.

  private yyFinal12:[]@=yyFinal.
  yyFinal12..{
    final(_,yyLine,yyPos) => yyT(( SEMI ),yyLine,yyPos).
  }.

  private yyFinal11:[]@=yyFinal.
  yyFinal11..{
    final(_,yyLine,yyPos) => yyT(( COLON ),yyLine,yyPos).
  }.

  private yyFinal10:[]@=yyFinal.
  yyFinal10..{
    final(_,yyLine,yyPos) => yyT(( BAR ),yyLine,yyPos).
  }.

  private yyFinal9:[]@=yyFinal.
  yyFinal9..{
    final(_,yyLine,yyPos) => yyT(( MARK ),yyLine,yyPos).
  }.

  private yyFinal8:[]@=yyFinal.
  yyFinal8..{
    final(yyTok,yyLine,yyPos) => yyT(( BRACE(trim(yyTok,1)) ),yyLine,yyPos).
  }.

  private yyFinal7:[]@=yyFinal.
  yyFinal7..{
    final(yyTok,yyLine,yyPos) => yyT(( CENTBRACE(trim(yyTok,2)) ),yyLine,yyPos).
  }.

  private yyFinal6:[]@=yyFinal.
  yyFinal6..{
    final(_,yyLine,yyPos) => yyT(( LEFT ),yyLine,yyPos).
  }.

  private yyFinal5:[]@=yyFinal.
  yyFinal5..{
    final(_,yyLine,yyPos) => yyT(( PREC ),yyLine,yyPos).
  }.

  private yyFinal4:[]@=yyFinal.
  yyFinal4..{
    final(_,yyLine,yyPos) => yyT(( RIGHT ),yyLine,yyPos).
  }.

  private yyFinal3:[]@=yyFinal.
  yyFinal3..{
    final(_,yyLine,yyPos) => yyT(( START ),yyLine,yyPos).
  }.

  private yyFinal2:[]@=yyFinal.
  yyFinal2..{
    final(_,yyLine,yyPos) => yyT(( TOKEN ),yyLine,yyPos).
  }.

  private yyFinal1:[]@=yyFinal.
  yyFinal1..{
    final(_,yyLine,yyPos) => yyT(( EXPECT ),yyLine,yyPos).
  }.

  private yyFinal0:[]@=yyFinal.
  yyFinal0..{
    final(_,yyLine,yyPos) => yyT(( NONASSOC ),yyLine,yyPos).
  }.

   private yyNoFinal:[]@=yyFinal.
   yyNoFinal..{
     final(yyTok,yyLine,yyPos) => raise error("cannot tokenise: "<>yyTok<>" at line "<>yyLine.show()<>"/"<>yyPos.show(),'eFAIL').
  }.

  yyTokenizeAll:[string]=>list[yyToken].
  yyTokenizeAll(Text) => yyTokReader(yygpglex(Text,1,1)).

  private yyTokReader:[yyLexer]=>list[yyToken].
  yyTokReader(Lx) => ( Tok=Lx.nextToken(), Tok.token()!=EOF ?
                       [Tok,..yyTokReader(Lx)]
                     | []).



private trim:[string,integer]=>string.
trim(S,C) => front(drop(S,C),listlen(S)-2*C).

}.
