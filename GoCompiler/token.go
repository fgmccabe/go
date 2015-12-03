token{
  import go.io.


/*
 * The standard tokenizer, expressed as a golex file
 */

import go.stdparse.

yyTokType ::= ID(symbol) | 
	  IN(integer) | 
	  FT(float) |
	  ST(string) |
	  SY(symbol) |
	  CH(char) |
	  LPAR |
	  RPAR |
	  LBRA | 
	  RBRA |
	  LBRCE |
	  RBRCE |
	  COMMA |
	  CONS |
          TERM |
	  EOF.


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

  yytoken:[string,integer,integer]@>yyLexer.
  yytoken(Text,startLine,startPos)..{
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

    private yyNextState105:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState105(yStream,_,_,_,yFinal) =>
      yyNextState0(yStream,[],yStream,[],yFinal).  -- restart in initial state

    private yyNextState104:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
     yyNextState104(yStream,yTok,_,_,yFinal) =>
      valof{      -- potential to switch to comment
        yyChar = yStream.next(yyNext);
        valis yyTrans104(yyChar,yyNext,[yyChar,..yTok],yStream,[],yStream,[],yFinal)
      }.

    private yyNextState103:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState103(yStream,_,_,_,yFinal) =>
      yyNextState102(yStream,[],yStream,[],yFinal).  -- restart in comment state

    private yyNextState102:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState102(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans102(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState101:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState101(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans101(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState100:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState100(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans100(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal0)
      }.

    private yyNextState99:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState99(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans99(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState98:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState98(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans98(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState97:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState97(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans97(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState96:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState96(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans96(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState95:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState95(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans95(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState94:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState94(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans94(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState93:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState93(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans93(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState92:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState92(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal1). -- new final candidate

    private yyNextState91:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState91(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal2). -- new final candidate

    private yyNextState90:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState90(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal3). -- new final candidate

    private yyNextState89:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState89(yStream,_,_,_,yFinal) =>
      yyNextState0(yStream,[],yStream,[],yFinal).  -- restart in initial state

    private yyNextState88:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState88(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans88(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal4)
      }.

    private yyNextState87:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState87(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans87(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState86:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState86(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans86(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState85:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState85(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans85(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState84:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState84(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans84(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal0)
      }.

    private yyNextState83:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState83(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans83(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal5)
      }.

    private yyNextState82:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState82(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans82(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState81:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState81(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal6). -- new final candidate

    private yyNextState80:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState80(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal7). -- new final candidate

    private yyNextState79:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState79(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans79(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState78:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState78(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal8). -- new final candidate

    private yyNextState77:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState77(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal9). -- new final candidate

    private yyNextState76:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState76(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal10). -- new final candidate

    private yyNextState75:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState75(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal11). -- new final candidate

    private yyNextState74:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState74(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal12). -- new final candidate

    private yyNextState73:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState73(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal13). -- new final candidate

    private yyNextState72:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState72(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal14). -- new final candidate

    private yyNextState71:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState71(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal15). -- new final candidate

    private yyNextState70:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState70(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal16). -- new final candidate

    private yyNextState69:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState69(yStream,_,_,_,yFinal) =>
      yyNextState102(yStream,[],yStream,[],yFinal).  -- restart in comment state

    private yyNextState68:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState68(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans68(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal17)
      }.

    private yyNextState67:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState67(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal18). -- new final candidate

    private yyNextState66:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState66(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal19). -- new final candidate

    private yyNextState65:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState65(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal17). -- new final candidate

    private yyNextState64:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState64(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans64(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState63:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState63(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans63(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal20)
      }.

    private yyNextState62:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState62(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal21). -- new final candidate

    private yyNextState61:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState61(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans61(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal22)
      }.

    private yyNextState60:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState60(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal23). -- new final candidate

    private yyNextState59:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState59(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal24). -- new final candidate

    private yyNextState58:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState58(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal25). -- new final candidate

    private yyNextState57:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState57(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal26). -- new final candidate

    private yyNextState56:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState56(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal27). -- new final candidate

    private yyNextState55:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState55(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal28). -- new final candidate

    private yyNextState54:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState54(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal29). -- new final candidate

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
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal30). -- new final candidate

    private yyNextState51:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState51(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal31). -- new final candidate

    private yyNextState50:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState50(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal32). -- new final candidate

    private yyNextState49:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState49(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal33). -- new final candidate

    private yyNextState48:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState48(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal34). -- new final candidate

    private yyNextState47:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState47(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal35). -- new final candidate

    private yyNextState46:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState46(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal36). -- new final candidate

    private yyNextState45:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState45(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal37). -- new final candidate

    private yyNextState44:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState44(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal11). -- new final candidate

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
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal38). -- new final candidate

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
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal39). -- new final candidate

    private yyNextState39:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState39(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans39(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState38:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState38(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans38(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal6)
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
     yyNextState35(yStream,yTok,_,_,yFinal) =>
      valof{      -- potential to switch to initial
        yyChar = yStream.next(yyNext);
        valis yyTrans35(yyChar,yyNext,[yyChar,..yTok],yStream,[],yStream,[],yFinal)
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
    yyNextState33(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans33(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal40)
      }.

    private yyNextState32:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState32(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans32(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal41)
      }.

    private yyNextState31:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState31(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal42). -- new final candidate

    private yyNextState30:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState30(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans30(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal43)
      }.

    private yyNextState29:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState29(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans29(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal44)
      }.

    private yyNextState28:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState28(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans28(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal45)
      }.

    private yyNextState27:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState27(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans27(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal46)
      }.

    private yyNextState26:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState26(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans26(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal47)
      }.

    private yyNextState25:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState25(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans25(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal48)
      }.

    private yyNextState24:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState24(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal49). -- new final candidate

    private yyNextState23:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState23(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal50). -- new final candidate

    private yyNextState22:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState22(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal51). -- new final candidate

    private yyNextState21:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState21(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal52). -- new final candidate

    private yyNextState20:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState20(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal53). -- new final candidate

    private yyNextState19:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState19(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans19(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal54)
      }.

    private yyNextState18:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState18(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans18(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal55)
      }.

    private yyNextState17:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState17(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans17(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal56)
      }.

    private yyNextState16:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState16(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal57). -- new final candidate

    private yyNextState15:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState15(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal58). -- new final candidate

    private yyNextState14:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState14(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans14(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal59)
      }.

    private yyNextState13:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState13(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal60). -- new final candidate

    private yyNextState12:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState12(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans12(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.

    private yyNextState11:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState11(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans11(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal61)
      }.

    private yyNextState10:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState10(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal62). -- new final candidate

    private yyNextState9:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState9(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans9(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal63)
      }.

    private yyNextState8:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState8(yStream,yTok,_,_,_) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans8(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal64)
      }.

    private yyNextState7:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState7(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal65). -- new final candidate

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
        valis yyTrans2(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yStream,yTok,yyFinal66)
      }.

    private yyNextState1:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState1(yStream,yTok,_,_,_) =>
      yyNextStateX(yStream,yTok,yStream,yTok,yyFinal67). -- new final candidate

    private yyNextState0:[yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
    yyNextState0(yStream,yTok,yLast,lTok,yFinal) =>
      valof{
        yyChar = yStream.next(yyNext);
        valis yyTrans0(yyChar,yyNext,[yyChar,..yTok],yStream,yTok,yLast,lTok,yFinal)
      }.



  private yyTrans104:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans104(`/,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState105(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans104(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState102(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans102:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans102(`*,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState104(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`+=<X,X=<`\d =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(`!,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(`\",yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(`#,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(`$,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(`%,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(`&,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(`',yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(`(,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(`),yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(`\v,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(`\f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(`\a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(`\b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(`\t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState103(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans102(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans101:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans101(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans101(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans101(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans101(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans101(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans101(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans101(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans101(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans101(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans101(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans101(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans100:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans100(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans100(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans100(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans100(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans100(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans100(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans100(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans100(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans100(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans100(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans100(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans99:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans99(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`c,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`d,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`A,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`B,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`C,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`D,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`E,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`F,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(`;,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans99(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans98:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans98(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`c,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`d,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`A,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`B,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`C,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`D,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`E,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`F,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(`;,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans98(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans97:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans97(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`c,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`d,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`A,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`B,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`C,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`D,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`E,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`F,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(`;,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans97(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans96:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans96(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`c,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`d,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`A,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`B,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`C,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`D,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`E,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`F,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(`;,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans96(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans95:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans95(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans95(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans95(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans95(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans95(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans95(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans95(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans95(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans95(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans95(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState100(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans95(`-,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState101(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans95(`+,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState101(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans95(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans94:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans94(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`c,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`d,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`A,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`B,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`C,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`D,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`E,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`F,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState99(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans94(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans93:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans93(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`c,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`d,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`A,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`B,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`C,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`D,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`E,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`F,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState98(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans93(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).






  private yyTrans88:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans88(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState89(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans88(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`\d =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans88(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans88(`\v,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans88(`\f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans88(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans88(`\a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans88(`\b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans88(`\t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans88(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans87:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans87(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState89(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans87(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`\d =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans87(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans87(`\v,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans87(`\f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans87(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans87(`\a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans87(`\b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans87(`\t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans87(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans86:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans86(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`c,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`d,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`A,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`B,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`C,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`D,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`E,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`F,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState97(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans86(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans85:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans85(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`c,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`d,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`A,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`B,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`C,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`D,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`E,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`F,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState96(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans85(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans84:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans84(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState95(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans84(`E,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState95(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans84(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans84(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans84(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans84(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans84(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans84(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans84(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans84(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans84(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans84(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans84(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans83:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans83(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`c,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`d,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`A,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`B,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`C,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`D,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`E,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`F,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans83(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans82:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans82(`+,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState94(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans82(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`,=<X,X=<`\d =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans82(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`* =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans82(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans82(`\a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans82(`\b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans82(`\t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans82(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans82(`\v,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans82(`\f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans82(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans82(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).




  private yyTrans79:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans79(`+,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState93(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans79(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`,=<X,X=<`\d =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans79(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`* =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans79(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans79(`\a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans79(`\b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans79(`\t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans79(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans79(`\v,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans79(`\f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans79(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans79(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).












  private yyTrans68:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans68(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState68(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans68(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState68(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans68(`\b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState68(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans68(`\t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState68(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans68(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState68(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans68(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).





  private yyTrans64:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans64(`.,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState92(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans64(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans63:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans63(`=,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState91(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans63(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans61:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans61(`-,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState90(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans61(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).









  private yyTrans53:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans53(`>,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState88(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans53(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState89(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans53(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`?=<X,X=<`\d =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans53(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`= =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans53(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans53(`\v,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans53(`\f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans53(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans53(`\a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans53(`\b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans53(`\t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState87(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans53(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).











  private yyTrans43:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans43(`+,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState86(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`,=<X,X=<`\d =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`* =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(`\a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(`\b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(`\t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(`\v,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(`\f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans43(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans41:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans41(`+,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState85(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans41(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`,=<X,X=<`\d =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans41(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`* =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans41(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans41(`\a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans41(`\b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans41(`\t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans41(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans41(`\v,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans41(`\f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans41(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans41(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans39:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans39(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans39(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans39(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans39(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans39(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans39(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans39(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans39(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans39(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans39(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState84(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans39(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans38:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans38(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans38(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans38(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans38(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans38(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans38(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans38(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans38(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans38(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans38(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans38(`.,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState39(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans38(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans37:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans37(`a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`c,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`d,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`A,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`B,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`C,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`D,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`E,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`F,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState83(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans37(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans36:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans36(`\\,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState82(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans36(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`]=<X,X=<`\d =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans36(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`#=<X,X=<`[ =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans36(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans36(`!,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans36(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans36(`\a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans36(`\b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans36(`\t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans36(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans36(`\v,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans36(`\f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans36(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState81(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans36(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans35:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans35(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState35(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans35(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState35(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans35(`\b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState35(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans35(`\t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState35(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans35(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState35(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans35(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextState0(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans34:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans34(`\\,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState79(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans34(`\",yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState80(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans34(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`]=<X,X=<`\d =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans34(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`#=<X,X=<`[ =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans34(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans34(`!,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans34(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans34(`\a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans34(`\b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans34(`\t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans34(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans34(`\v,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans34(`\f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans34(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans34(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans33:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans33(`|,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState78(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans33(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans32:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans32(`=,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState75(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans32(`+,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState76(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans32(`/,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState77(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans32(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans30:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans30(`=,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState74(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans30(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans29:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans29(`~,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState71(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans29(`=,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState72(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans29(`>,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState73(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans29(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans28:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans28(`*,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState69(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans28(`\\,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState70(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans28(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans27:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans27(`\+ffff;,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState65(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(`=,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState66(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(`.,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState67(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState68(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState68(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(`\b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState68(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(`\t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState68(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState68(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans27(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans26:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans26(`.,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState64(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans26(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans25:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans25(`-,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState61(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans25(`=,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState62(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans25(`:,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState63(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans25(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).







  private yyTrans19:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans19(`+,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState60(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans19(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans18:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans18(`=,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState56(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans18(`.,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState57(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans18(`<,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState58(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans18(`>,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState59(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans18(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans17:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans17(`-,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState53(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans17(`+,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState54(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans17(`>,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState55(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans17(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).




  private yyTrans14:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans14(`*,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState51(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans14(`>,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState52(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans14(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans12:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans12(`%,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState49(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans12(`=,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState50(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans12(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans11:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans11(`=,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState48(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans11(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans9:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans9(`@,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState45(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans9(`=,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState46(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans9(`>,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState47(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans9(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans8:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans8(`=,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState44(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans8(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans6:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans6(`\\,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState43(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`]=<X,X=<`\d =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::` =<X,X=<`[ =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(`\a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(`\b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(`\t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(`\v,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(`\f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState42(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans6(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans5:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans5(`',yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState40(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`\\,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState41(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`]=<X,X=<`\d =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`(=<X,X=<`[ =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`!,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`\",yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`#,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`$,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`%,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`&,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`\e,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`\a,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`\b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`\t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`\v,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`\f,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans5(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans4:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans4(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(`.,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState39(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans4(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).


  private yyTrans3:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans3(`c,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState36(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(`x,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState37(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState38(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans3(`.,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState39(yStrm,yTok,yLStrm,yLtok,yFinal).
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
  yyTrans2(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans2(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).



  private yyTrans0:[char,yyStatus,string,yyStatus,string,yyStatus,string,yyFinal]=>
          (yyToken,string,integer,integer).
  yyTrans0(`\+ffff;,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState1(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`0,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState3(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`1,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`2,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`3,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`4,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`5,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`6,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`7,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`8,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`9,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState4(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`',yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState5(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(``,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState6(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`~,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState7(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`!,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState8(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`@,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState9(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`#,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState10(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`$,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState11(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`%,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState12(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`^,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState13(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`*,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState14(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`(,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState15(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`),yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState16(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`-,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState17(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`a=<X,X=<`z =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`_,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(X,yStrm,yTok,_,_,yLStrm,yLtok,yFinal)::`A=<X,X=<`Z =>
      yyNextState2(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`=,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState18(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`+,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState19(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`[,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState20(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`],yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState21(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`{,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState22(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`},yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState23(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`;,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState24(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`:,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState25(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`,,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState26(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`.,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState27(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`/,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState28(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`<,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState29(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`>,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState30(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`?,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState31(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`\\,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState32(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`|,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState33(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`\",yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState34(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(` ,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState35(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`\r,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState35(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`\b,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState35(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`\t,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState35(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(`\n,yStrm,yTok,_,_,yLStrm,yLtok,yFinal) =>
      yyNextState35(yStrm,yTok,yLStrm,yLtok,yFinal).
  yyTrans0(_,_,_,_,_,yLStrm,yLtok,yFinal)=>
      yyNextStateX(yLStrm,yLtok,yLStrm,yLtok,yFinal).

  }.

  private yyFinal67:[]@=yyFinal.
  yyFinal67..{
    final(_,yyLine,yyPos) => yyT(EOF,yyLine,yyPos).
  }.

  private yyFinal66:[]@=yyFinal.
  yyFinal66..{
    final(yyTok,yyLine,yyPos) => yyT((ID(implode(yyTok))),yyLine,yyPos).
  }.

  private yyFinal65:[]@=yyFinal.
  yyFinal65..{
    final(_,yyLine,yyPos) => yyT(( ID('~') ),yyLine,yyPos).
  }.

  private yyFinal64:[]@=yyFinal.
  yyFinal64..{
    final(_,yyLine,yyPos) => yyT(( ID('!') ),yyLine,yyPos).
  }.

  private yyFinal63:[]@=yyFinal.
  yyFinal63..{
    final(_,yyLine,yyPos) => yyT(( ID('@') ),yyLine,yyPos).
  }.

  private yyFinal62:[]@=yyFinal.
  yyFinal62..{
    final(_,yyLine,yyPos) => yyT(( ID('#') ),yyLine,yyPos).
  }.

  private yyFinal61:[]@=yyFinal.
  yyFinal61..{
    final(_,yyLine,yyPos) => yyT(( ID('$') ),yyLine,yyPos).
  }.

  private yyFinal60:[]@=yyFinal.
  yyFinal60..{
    final(_,yyLine,yyPos) => yyT(( ID('^') ),yyLine,yyPos).
  }.

  private yyFinal59:[]@=yyFinal.
  yyFinal59..{
    final(_,yyLine,yyPos) => yyT(( ID('*') ),yyLine,yyPos).
  }.

  private yyFinal58:[]@=yyFinal.
  yyFinal58..{
    final(_,yyLine,yyPos) => yyT(( LPAR ),yyLine,yyPos).
  }.

  private yyFinal57:[]@=yyFinal.
  yyFinal57..{
    final(_,yyLine,yyPos) => yyT(( RPAR ),yyLine,yyPos).
  }.

  private yyFinal56:[]@=yyFinal.
  yyFinal56..{
    final(_,yyLine,yyPos) => yyT(( ID('-') ),yyLine,yyPos).
  }.

  private yyFinal55:[]@=yyFinal.
  yyFinal55..{
    final(_,yyLine,yyPos) => yyT(( ID('=') ),yyLine,yyPos).
  }.

  private yyFinal54:[]@=yyFinal.
  yyFinal54..{
    final(_,yyLine,yyPos) => yyT(( ID('+') ),yyLine,yyPos).
  }.

  private yyFinal53:[]@=yyFinal.
  yyFinal53..{
    final(_,yyLine,yyPos) => yyT(( LBRA ),yyLine,yyPos).
  }.

  private yyFinal52:[]@=yyFinal.
  yyFinal52..{
    final(_,yyLine,yyPos) => yyT(( RBRA ),yyLine,yyPos).
  }.

  private yyFinal51:[]@=yyFinal.
  yyFinal51..{
    final(_,yyLine,yyPos) => yyT(( LBRCE),yyLine,yyPos).
  }.

  private yyFinal50:[]@=yyFinal.
  yyFinal50..{
    final(_,yyLine,yyPos) => yyT(( RBRCE ),yyLine,yyPos).
  }.

  private yyFinal49:[]@=yyFinal.
  yyFinal49..{
    final(_,yyLine,yyPos) => yyT(( ID(';') ),yyLine,yyPos).
  }.

  private yyFinal48:[]@=yyFinal.
  yyFinal48..{
    final(_,yyLine,yyPos) => yyT(( ID(':') ),yyLine,yyPos).
  }.

  private yyFinal47:[]@=yyFinal.
  yyFinal47..{
    final(_,yyLine,yyPos) => yyT(( COMMA ),yyLine,yyPos).
  }.

  private yyFinal46:[]@=yyFinal.
  yyFinal46..{
    final(_,yyLine,yyPos) => yyT(( ID('.') ),yyLine,yyPos).
  }.

  private yyFinal45:[]@=yyFinal.
  yyFinal45..{
    final(_,yyLine,yyPos) => yyT(( ID('/') ),yyLine,yyPos).
  }.

  private yyFinal44:[]@=yyFinal.
  yyFinal44..{
    final(_,yyLine,yyPos) => yyT(( ID('<') ),yyLine,yyPos).
  }.

  private yyFinal43:[]@=yyFinal.
  yyFinal43..{
    final(_,yyLine,yyPos) => yyT(( ID('>') ),yyLine,yyPos).
  }.

  private yyFinal42:[]@=yyFinal.
  yyFinal42..{
    final(_,yyLine,yyPos) => yyT(( ID('?') ),yyLine,yyPos).
  }.

  private yyFinal41:[]@=yyFinal.
  yyFinal41..{
    final(_,yyLine,yyPos) => yyT(( ID('\\') ),yyLine,yyPos).
  }.

  private yyFinal40:[]@=yyFinal.
  yyFinal40..{
    final(_,yyLine,yyPos) => yyT(( ID('|') ),yyLine,yyPos).
  }.

  private yyFinal39:[]@=yyFinal.
  yyFinal39..{
    final(yyTok,yyLine,yyPos) => yyT((SY(tok2sym(yyTok))),yyLine,yyPos).
  }.

  private yyFinal38:[]@=yyFinal.
  yyFinal38..{
    final(yyTok,yyLine,yyPos) => yyT(( CH(tok2chr(yyTok)) ),yyLine,yyPos).
  }.

  private yyFinal37:[]@=yyFinal.
  yyFinal37..{
    final(_,yyLine,yyPos) => yyT(( ID('@@') ),yyLine,yyPos).
  }.

  private yyFinal36:[]@=yyFinal.
  yyFinal36..{
    final(_,yyLine,yyPos) => yyT(( ID('@=') ),yyLine,yyPos).
  }.

  private yyFinal35:[]@=yyFinal.
  yyFinal35..{
    final(_,yyLine,yyPos) => yyT(( ID('@>') ),yyLine,yyPos).
  }.

  private yyFinal34:[]@=yyFinal.
  yyFinal34..{
    final(_,yyLine,yyPos) => yyT(( ID('$=') ),yyLine,yyPos).
  }.

  private yyFinal33:[]@=yyFinal.
  yyFinal33..{
    final(_,yyLine,yyPos) => yyT(( ID('%%') ),yyLine,yyPos).
  }.

  private yyFinal32:[]@=yyFinal.
  yyFinal32..{
    final(_,yyLine,yyPos) => yyT(( ID('%=') ),yyLine,yyPos).
  }.

  private yyFinal31:[]@=yyFinal.
  yyFinal31..{
    final(_,yyLine,yyPos) => yyT(( ID('**') ),yyLine,yyPos).
  }.

  private yyFinal30:[]@=yyFinal.
  yyFinal30..{
    final(_,yyLine,yyPos) => yyT(( ID('*>') ),yyLine,yyPos).
  }.

  private yyFinal29:[]@=yyFinal.
  yyFinal29..{
    final(_,yyLine,yyPos) => yyT(( ID('-+') ),yyLine,yyPos).
  }.

  private yyFinal28:[]@=yyFinal.
  yyFinal28..{
    final(_,yyLine,yyPos) => yyT(( ID('->') ),yyLine,yyPos).
  }.

  private yyFinal27:[]@=yyFinal.
  yyFinal27..{
    final(_,yyLine,yyPos) => yyT(( ID('==') ),yyLine,yyPos).
  }.

  private yyFinal26:[]@=yyFinal.
  yyFinal26..{
    final(_,yyLine,yyPos) => yyT(( ID('=.') ),yyLine,yyPos).
  }.

  private yyFinal25:[]@=yyFinal.
  yyFinal25..{
    final(_,yyLine,yyPos) => yyT(( ID('=<') ),yyLine,yyPos).
  }.

  private yyFinal24:[]@=yyFinal.
  yyFinal24..{
    final(_,yyLine,yyPos) => yyT(( ID('=>') ),yyLine,yyPos).
  }.

  private yyFinal23:[]@=yyFinal.
  yyFinal23..{
    final(_,yyLine,yyPos) => yyT(( ID('++') ),yyLine,yyPos).
  }.

  private yyFinal22:[]@=yyFinal.
  yyFinal22..{
    final(_,yyLine,yyPos) => yyT(( ID(':-') ),yyLine,yyPos).
  }.

  private yyFinal21:[]@=yyFinal.
  yyFinal21..{
    final(_,yyLine,yyPos) => yyT(( ID(':=') ),yyLine,yyPos).
  }.

  private yyFinal20:[]@=yyFinal.
  yyFinal20..{
    final(_,yyLine,yyPos) => yyT(( ID('::') ),yyLine,yyPos).
  }.

  private yyFinal19:[]@=yyFinal.
  yyFinal19..{
    final(_,yyLine,yyPos) => yyT(( ID('.=') ),yyLine,yyPos).
  }.

  private yyFinal18:[]@=yyFinal.
  yyFinal18..{
    final(_,yyLine,yyPos) => yyT(( ID('..') ),yyLine,yyPos).
  }.

  private yyFinal17:[]@=yyFinal.
  yyFinal17..{
    final(_,yyLine,yyPos) => yyT(( TERM ),yyLine,yyPos).
  }.

  private yyFinal16:[]@=yyFinal.
  yyFinal16..{
    final(_,yyLine,yyPos) => yyT(( ID('/\\') ),yyLine,yyPos).
  }.

  private yyFinal15:[]@=yyFinal.
  yyFinal15..{
    final(_,yyLine,yyPos) => yyT(( ID('<~') ),yyLine,yyPos).
  }.

  private yyFinal14:[]@=yyFinal.
  yyFinal14..{
    final(_,yyLine,yyPos) => yyT(( ID('<=') ),yyLine,yyPos).
  }.

  private yyFinal13:[]@=yyFinal.
  yyFinal13..{
    final(_,yyLine,yyPos) => yyT(( ID('<>') ),yyLine,yyPos).
  }.

  private yyFinal12:[]@=yyFinal.
  yyFinal12..{
    final(_,yyLine,yyPos) => yyT(( ID('>=') ),yyLine,yyPos).
  }.

  private yyFinal11:[]@=yyFinal.
  yyFinal11..{
    final(_,yyLine,yyPos) => yyT(( ID('!=') ),yyLine,yyPos).
  }.

  private yyFinal10:[]@=yyFinal.
  yyFinal10..{
    final(_,yyLine,yyPos) => yyT(( ID('\\+') ),yyLine,yyPos).
  }.

  private yyFinal9:[]@=yyFinal.
  yyFinal9..{
    final(_,yyLine,yyPos) => yyT(( ID('\\/') ),yyLine,yyPos).
  }.

  private yyFinal8:[]@=yyFinal.
  yyFinal8..{
    final(_,yyLine,yyPos) => yyT(( ID('||') ),yyLine,yyPos).
  }.

  private yyFinal7:[]@=yyFinal.
  yyFinal7..{
    final(yyTok,yyLine,yyPos) => yyT((ST(tok2str(yyTok))),yyLine,yyPos).
  }.

  private yyFinal6:[]@=yyFinal.
  yyFinal6..{
    final(yyTok,yyLine,yyPos) => yyT((IN(tok2int(yyTok))),yyLine,yyPos).
  }.

  private yyFinal5:[]@=yyFinal.
  yyFinal5..{
    final(yyTok,yyLine,yyPos) => yyT(( IN(tok2int(yyTok))),yyLine,yyPos).
  }.

  private yyFinal4:[]@=yyFinal.
  yyFinal4..{
    final(_,yyLine,yyPos) => yyT(( ID('-->') ),yyLine,yyPos).
  }.

  private yyFinal3:[]@=yyFinal.
  yyFinal3..{
    final(_,yyLine,yyPos) => yyT(( ID(':--') ),yyLine,yyPos).
  }.

  private yyFinal2:[]@=yyFinal.
  yyFinal2..{
    final(_,yyLine,yyPos) => yyT(( ID('::=') ),yyLine,yyPos).
  }.

  private yyFinal1:[]@=yyFinal.
  yyFinal1..{
    final(_,yyLine,yyPos) => yyT(( CONS ),yyLine,yyPos).
  }.

  private yyFinal0:[]@=yyFinal.
  yyFinal0..{
    final(yyTok,yyLine,yyPos) => yyT((FT(tok2float(yyTok))),yyLine,yyPos).
  }.

   private yyNoFinal:[]@=yyFinal.
   yyNoFinal..{
     final(yyTok,yyLine,yyPos) => raise error("cannot tokenise: "<>yyTok<>" at line "<>yyLine.show()<>"/"<>yyPos.show(),'eFAIL').
  }.

  yyTokenizeAll:[string]=>list[yyToken].
  yyTokenizeAll(Text) => yyTokReader(yytoken(Text,1,1)).

  private yyTokReader:[yyLexer]=>list[yyToken].
  yyTokReader(Lx) => ( Tok=Lx.nextToken(), Tok.token()!=EOF ?
                       [Tok,..yyTokReader(Lx)]
                     | []).



-- The postamble

  main(L) ->
	(S in L *>
	   Text  = getFile(S,unknownEncoding);
           Now = ticks();
	   Tokens = yyTokenizeAll(Text);
           Then = ticks();
	   stdout.outLine("parsed "<>listlen(Tokens).show()<>
                          " tokens in file "<>S<>" in "<>(Then-Now).show()<>
                          " seconds")).

	
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


}.
