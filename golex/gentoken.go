gentoken{
  -- Generate the tokenizer according to the DFA produced
  import go.io.
  import go.setlib.
  import go.sort.
  import golextypes.


  genToken:[string,list[(integer,list[dfaE],list[finalType])],
	    string,string,string,logical]=>string.
  genToken(Name,Dfas,Sigma,Preamble,PostAmble,trace) =>
      valof{
	Finals = genFinals(Dfas,0,[]);
	Sts = genStates(Dfas,Sigma,Finals,trace);
	Trans = genTransFuns(Dfas);
	FinalClasses = finalClasses(Finals);
	Front = Name<>"{\n"
	"  import go.io.\n\n";

	GoLexTps = 
	"  yyLexer <~ { nextToken:[]=>yyToken. currentToken:[]=>yyToken. }.\n"
	"  yyToken <~ { token:[]=>yyTokType. isToken:[yyTokType]{}. line:[]=>integer. pos:[]=>integer. }.\n\n"

	"  private yyFinal <~ { final:[string,integer,integer]=>yyToken. }.\n\n"
	"  private yyStatus <~ { line:[]=>integer. pos:[]=>integer.\n"
	"      next:[yyStatus-]=>char. text:[]=>string. }.\n\n"
	"  private yyS:[string,integer,integer]@=yyStatus.\n"
	"  yyS(Text,L,P)..{\n"
	"    line()=>L.\n"
	"    pos()=>P.\n"<>
        ( trace ?
	    "    next(yyS(R,aL,aP)) => valof{\n"
	    "      Ch = nextCh(Text,R,L,aP);\n"
	    "      stderr.outLine(\"Char :\"<>Ch.show()<>\" at \"<>aP.show());\n"
	    "      valis Ch\n"
	    "    }.\n"
	|   "    next(yyS(R,aL,aP)) => nextCh(Text,R,aL,aP).\n\n"
	) <>
	"    nextCh:[string,string-,integer-,integer-]=>char.\n"
	"    nextCh([],Text,L,P) => `\\+ffff;.\n"
	"    nextCh([`\\n,..R],R,L1,P1) :: L1=L+1, P1=P+1 => `\\n.\n"
	"    nextCh([Ch,..R],R,L,P1) :: P1=P+1 => Ch.\n\n"
	"    text() => Text.\n"
	"  }.\n\n"
	"  yyT:[yyTokType,integer,integer]@=yyToken.\n"
	"  yyT(Tk,L,P)..{\n"
	"    token()=>Tk.\n"
	"    isToken(Tk).\n"
	"    line()=>L.\n"
	"    pos()=>P.\n"
	"    show()=>Tk.show()<>\"@\"<>L.show()<>\"/\"<>P.show().\n"
	"  }.\n\n"

	"  yy"<>stripDots(Name)<>":[string,integer,integer]@>yyLexer.\n"
	"  yy"<>stripDots(Name)<>"(Text,startLine,startPos)..{\n"
	"    cToken:yyToken:=yyT(EOF,-1,0).\n"
	"    currIsValid:logical := false.\n"
	"    Strm:string:=Text.\n"
	"    cLine:integer :=startLine.\n"
	"    cPos:integer:=startPos.\n"
	"    currentToken()::currIsValid=>cToken.\n"
	"    currentToken()=>raise error(\"no token available\",'eFAIL').\n\n"
	"    nextToken()=>\n"
	"      valof{\n"
	"        initStrm = yyS(Strm,cLine,cPos);\n"
	"        (nT,nS,nL,nP) = yyNextState0(initStrm,[],initStrm,[],yyNoFinal);\n"
	"        cLine := nL; cPos := nP; Strm := nS; cToken := nT;\n"
	"        valis nT\n"
	"      }.\n";


	Build = [Front,Preamble,GoLexTps]<>[
	     "    private yyNextStateX:[yyStatus,string,yyStatus,string,yyFinal]=>\n"
	     "          (yyToken,string,integer,integer).\n"
	     "    yyNextStateX(_,_,yLast,lTok,Final) =>\n"
	     "      (Final.final(reverse(lTok),yLast.line(),yLast.pos()),yLast.text(),yLast.line(),yLast.pos()).\n"]<>Sts<>
	Trans<>
	["  }.\n"]<>
	FinalClasses<>
	["   private yyNoFinal:[]@=yyFinal.\n"
	 "   yyNoFinal..{\n"
	 "     final(yyTok,yyLine,yyPos) => raise error(\"cannot tokenise: \"<>yyTok<>\" at line \"<>yyLine.show()<>\"/\"<>yyPos.show(),'eFAIL').\n"
	 "  }.\n\n"
         "  yyTokenizeAll:[string]=>list[yyToken].\n"
	 "  yyTokenizeAll(Text) => yyTokReader(yy"<>stripDots(Name)<>"(Text,1,1)).\n\n"
	 "  private yyTokReader:[yyLexer]=>list[yyToken].\n"
	 "  yyTokReader(Lx) => ( Tok=Lx.nextToken(), Tok.token()!=EOF ?\n"
	 "                       [Tok,..yyTokReader(Lx)]\n"
	 "                     | []).\n\n",
	 PostAmble,
	 "\n}."];
	valis collapse(Build,"\n")
      }.


  private genStates:[list[(integer,list[dfaE],list[finalType])],
		     string,list[(string,string)],logical]=>list[string].
  genStates(Sts,Sigma,Finals,trace) => 
      {genState(S,D,F,Sigma,Finals,trace)..(S,D,F) in Sts}.

  private genState:[integer,list[dfaE],list[finalType],
		    string,list[(string,string)],logical]=>string.

  genState(S,[],[switch(In,initial)],_,_,_trace) =>
      "    private yyNextState"<>S.show()<>":[yyStatus,string,yyStatus,string,yyFinal]=>\n"
	     "          (yyToken,string,integer,integer).\n"
      "    yyNextState"<>S.show()<>"(yStream,_,_,_,yFinal) =>\n"
      "      yyNextState"<>initial.show()<>"(yStream,[],yStream,[],yFinal).  -- restart in "<>In<>" state\n".
  genState(S,[],F,_,Finals,trace) =>
      "    private yyNextState"<>S.show()<>":[yyStatus,string,yyStatus,string,yyFinal]=>\n"
	     "          (yyToken,string,integer,integer).\n"
      "    yyNextState"<>S.show()<>"(yStream,yTok,_,_,_) =>\n"
      "      yyNextStateX(yStream,yTok,yStream,yTok,"<>finalText(Finals,F,trace)<>"). -- new final candidate\n".
  genState(S,_,[switch(In,_)],_,_,_) =>
      "    private yyNextState"<>S.show()<>":[yyStatus,string,yyStatus,string,yyFinal]=>\n"
	     "          (yyToken,string,integer,integer).\n"
      "     yyNextState"<>S.show()<>"(yStream,yTok,_,_,yFinal) =>\n"
      "      valof{      -- potential to switch to "<>In<>"\n"
      "        yyChar = yStream.next(yyNext);\n"
      "        valis yyTrans"<>S.show()<>"(yyChar,yyNext,[yyChar,..yTok],"
      "yStream,[],yStream,[],yFinal)\n"
      "      }.\n".
  genState(S,_,[],_,_,_) => 
      "    private yyNextState"<>S.show()<>":[yyStatus,string,yyStatus,string,yyFinal]=>\n"
	     "          (yyToken,string,integer,integer).\n"
      "    yyNextState"<>S.show()<>"(yStream,yTok,yLast,lTok,yFinal) =>\n"
      "      valof{\n"
      "        yyChar = yStream.next(yyNext);\n"
      "        valis yyTrans"<>S.show()<>"(yyChar,yyNext,[yyChar,..yTok],"
      "yStream,yTok,yLast,lTok,yFinal)\n"
      "      }.\n".
  genState(S,_,[accept(F)],_,Finals,trace) => 
      valof{
	Final = finalText(Finals,[accept(F)],trace);
	valis 
	"    private yyNextState"<>S.show()<>":[yyStatus,string,yyStatus,string,yyFinal]=>\n"
	     "          (yyToken,string,integer,integer).\n"
	"    yyNextState"<>S.show()<>"(yStream,yTok,_,_,_) =>\n"
	"      valof{\n"
	"        yyChar = yStream.next(yyNext);\n"
	"        valis yyTrans"<>S.show()<>"(yyChar,yyNext,[yyChar,..yTok],"
      "yStream,yTok,yStream,yTok,"<>Final<>")\n"
	"      }.\n"
      }.

  private finalText:[list[(string,string)],list[finalType],logical]=>string.
  finalText(_,[],_)=>"".
  finalText(Finals,[accept(F)],_trace)::(F,Name) in Finals => Name.

  private genFinals:[list[(integer,list[dfaE],list[finalType])],integer,
		     list[(string,string)]] =>
      list[(string,string)].
  genFinals([],_,F)=>F.
  genFinals([(_,_,[accept(Final)]),..Dfas],C,F)::(Final,_) in F =>
      genFinals(Dfas,C,F).
  genFinals([(_,_,[accept(Final)]),..Dfas],C,F) =>
      genFinals(Dfas,C+1,[(Final,"yyFinal"<>C.show()),..F]).
  genFinals([_,..Dfas],C,F) => genFinals(Dfas,C,F).

  private finalClasses:[list[(string,string)]]=>list[string].
  finalClasses(L) => {finalClass(Name,Txt)..(Txt,Name) in L}.

  private finalClass:[string,string]=>string.
  finalClass(Name,Text) =>
      valof{
	yT = (present("yyTok",Text)?"yyTok"|"_");
	valis "  private "<>Name<>":[]@=yyFinal.\n"<>
	"  "<>Name<>"..{\n"
	"    final("<>yT<>",yyLine,yyPos) => yyT("<>Text<>",yyLine,yyPos).\n"
	"  }.\n"
      }.
      
  genCharCase:[integer,list[dfaE],list[char],string]=>string.
  genCharCase(_,L,Sigma,_)::covers(L,Sigma,S) => 
      S.show().
  genCharCase(Sno,_,_,Deflt) =>
      "yyTrans"<>Sno.show()<>"(yyChar,"<>Deflt<>")".

  private covers:[list[dfaE],list[char],integer+]{}.
  covers(L,Sigma,S) :-
      [dfa(_,S),.._]=L,
      ( C in Sigma *>
	dfa(C,S) in L).

  genTransFuns:[list[(integer,list[dfaE],list[finalType])]] => list[string].
  genTransFuns(Tbl) => { genTransFun(St,Df,DefaultState(Fn)) .. (St,Df,Fn) in Tbl}.

  private genTransFun:[integer,list[dfaE],string] => string.
  genTransFun(_,[],_) => "".
  genTransFun(St,D,Dflt) => "\n  private yyTrans"<>St.show()<>
			    ":[char,yyStatus,string,yyStatus,"
			    "string,yyStatus,string,yyFinal]=>\n"
			    "          (yyToken,string,integer,integer).\n"<>
			    genTransCases("  yyTrans"<>St.show(),genRanges(mergeCharSets(D,[])),Dflt).

  private DefaultState:[list[finalType]]=>string.
  DefaultState(Finals)::switch(_,S) in Finals => S.show().
  DefaultState(_) => "X".

  private genTransCases:[string,list[(integer,list[(char,char)])],string] =>
      string.
  genTransCases(Pre,Cases,Deflt) => 
      collapse({genTransCase(Pre,Tg,C1,C2)..
		 (Tg,Cs) in Cases,(C1,C2) in Cs}<>
	       [Pre<>"(_,_,_,_,_,yLStrm,yLtok,yFinal)=>\n"
		"      yyNextState"<>Deflt<>
		"(yLStrm,yLtok,yLStrm,yLtok,yFinal).\n"],"").

  private genTransCase:[string,integer,char,char]=>string.
  genTransCase(Pre,Tg,C,C) => Pre<>"("<>C.show()<>",yStrm,yTok,_,_,"
			      "yLStrm,yLtok,yFinal) =>\n"<>
			      "      yyNextState"<>Tg.show()<>
			      "(yStrm,yTok,yLStrm,yLtok,yFinal).\n".
  genTransCase(Pre,Tg,C,Ch)::
	  __charCode(Ch)-__charCode(C)<10 => Pre<>"("<>C.show()<>",yStrm,yTok,_,_,"
				     "yLStrm,yLtok,yFinal) =>\n"<>
				     "      yyNextState"<>Tg.show()<>
				     "(yStrm,yTok,yLStrm,yLtok,yFinal).\n"<>
				     genTransCase(Pre,Tg,__succChar(C,1),Ch).
  genTransCase(Pre,Tg,Cl,Ch) => Pre<>"(X,yStrm,yTok,_,_,"
				"yLStrm,yLtok,yFinal)::"<>
				Cl.show()<>"=<X,X=<"<>Ch.show()<>" =>\n"
				"      yyNextState"<>Tg.show()<>
				"(yStrm,yTok,yLStrm,yLtok,yFinal).\n".

  private genRanges:[list[(integer,list[char])]]=>
      list[(integer,list[(char,char)])].
  genRanges([])=>[].
  genRanges([(Tg,Cs),..Rs]) => 
      [(Tg,genRange(sort(Cs,charComp),[])),..genRanges(Rs)].

  charComp:[]@=comparable[char].
  charComp..{
    less(X,Y) :- X<Y.
    equal(X,X).
  }.

  private genRange:[list[char],list[(char,char)]]=>list[(char,char)].
  genRange([],R)=>R.
  genRange([C,..Cs],R)::(A,B) in R, adjacent(C,A) =>
      genRange(Cs,replace(A,(C,B),R)).
  genRange([C,..Cs],R)::(A,B) in R, adjacent(B,C) =>
      genRange(Cs,replace(A,(A,C),R)).
  genRange([C,..Cs],R) =>
      genRange(Cs,[(C,C),..R]).

  private adjacent:[char+,char+]{}.
  adjacent(A,B) :- __charCode(A)+1=__charCode(B).
  
  private mergeCharSets:[list[dfaE],list[(integer,list[char])]] => 
      list[(integer,list[char])].

  mergeCharSets([],S) => S.
  mergeCharSets([dfa(C,T),..D],S)::(T,Cs) in S =>
      mergeCharSets(D,replace(T,(T,[C]\/Cs),S)).
  mergeCharSets([dfa(C,T),..D],S) => mergeCharSets(D,[(T,[C]),..S]).

  private replace:[t,(t,u),list[(t,u)]]=>list[(t,u)].
  replace(_,U,[]) => [U].
  replace(T,U,[(T,_),..L])=>[U,..L].
  replace(T,U,[I,..L]) => [I,..replace(T,U,L)].

  private present:[list[t],list[t]]{}.
  present(F,T) :- append(F,_,T).
  present(F,[_,..T]) :- present(F,T).

  private stripDots:[string]=>string.
  stripDots(N)=>{C..(C::C!=`.) in N}.
}