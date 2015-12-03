gpg.genparser{
  import go.io.
  import gpg.gpgTypes.
  import go.setlib.

  genParser:[logical,string,string,string,
	     list[(integer,list[item],list[actionT])],list[(integer,symbol,integer)],
	     list[rule],list[symbol],list[symbol],list[(symbol,string)],
	     string]=>string.
  genParser(trace,preamble,postamble,source,actions,gotos,
	    rules,terms,nonterms,tokdata,yyType) =>
      valof{
	eStates = errorStates(actions);

	valis 
	"/* Parser generated automatically from \""<>source<>"\"*/\n"<>
	source<>"{\n"<>
	(trace ?"  import go.io.\n"|"")<>
	preamble<>
	"\n  private __item::= __t(yyToken) | __n("<>yyType<>").\n\n"
	
	"  failed:[]@=exception.\n"
	"  failed..{\n"
	"    cause() => \"failed\".\n"
	"    code() => 'failed'.\n"
	"    show() => \"failed\".\n"
	"   }.\n\n"

	"  doParse <~ { parse:[integer,yyToken,list[integer],list[__item]]=>"<>yyType<>". }.\n"
	
	"  parser:[yyLexer]=>"<>yyType<>".\n"
	"  parser(lexer) => \n"
	"     (:doParse..{\n"
	"    yyErrCount:integer := 0.\n\n"
	
	"    parse(Sno,Tok,Stack,Vstack) => do(Sno,Tok,Stack,Vstack) onerror (\n"
	"      failed => valof{\n"
	<>(trace?
        "         stdout.outLine(\"Detected parse error\");\n"
	  |"")<>
	"         yyErrCount := yyErrCount+1;\n"
	<>( eStates\=[]?
	      "         valis popErrorStates(Sno,Tok,Stack,Vstack);\n"
	 |  "         raise error(\"unrecoverable parse error\",'parse');\n"
	    "         valis _\n"
	)<>
        "      }).\n"

	"    do:[integer,yyToken,list[integer],list[__item]]=>"<>yyType<>".\n"
	<>genStates(actions,gotos,rules,terms,nonterms,tokdata,trace)<>
	(eStates\=[] ?
	   "\n    popErrorStates:[integer,yyToken,list[integer],list[__item]]=>"<>yyType<>".\n"
	   "    popErrorStates(Sno,Tok,Stack,Vstack)::Sno in "<>eStates.show()<>" => parse(Sno,Tok,Stack,Vstack).\n"
	   "    popErrorStates(_,Tok,Stack,Vstack) => \n"
	   "      valof{\n"
	   "        [__nxSno,..__Stack] = Stack;\n"
	   <>(trace?
		"        stderr.outLine(\"Now in state \"<>__nxSno.show());\n"
	    | "")<>
	   "        valis popErrorStates(__nxSno,Tok,__Stack,Vstack.tail())\n"
	   "      }.\n\n"
       | "")<>
	scanForToken(trace)<>
	"  }).parse(0,lexer.nextToken(),[],[]).\n"
	<>genGotos(gotos,nonterms)<>
	(trace ? 
	   "private __showStack:[list[integer],list[__item]]*.\n"
	   "__showStack([],[])->{}.\n"
	   "__showStack([0],[])->{}.\n"
	   "__showStack([Sno,..St],[V,..VSt])->\n"
	   "   stderr.outLine(\"State: \"<>Sno.show()<>\" = \"<>V.show());\n"
	   "   __showStack(St,VSt).\n"
       | "")<>
	postamble<>
	"}\n"
      }.

  private errorStates:[list[(integer,list[item],list[actionT])]]=>list[integer].
  errorStates(Actions) =>
      setof({Sno..((Sno,_,Acts)::recoverError(_,_,_) in Acts) in Actions}).


  private genStates:[list[(integer,list[item],list[actionT])],
		     list[(integer,symbol,integer)],
		     list[rule],
		     list[symbol],list[symbol],
		     list[(symbol,string)],logical]=>string.
  genStates([],_,_,_,_,_,_)=>"".
  genStates([(Sno,_,[reduceBy('__default__',Rno)]),..States],G,R,T,nT,TD,Tr)=>
      "    do("<>Sno.show()<>",Tok,Stack,Vstack) => "<>
      genReduction(Sno,Rno,R,TD,nT,Tr)<>
      ".\n"<>
      genStates(States,G,R,T,nT,TD,Tr).
  genStates([(Sno,_,Acts),..States],G,R,T,nT,TD,Tr)=>
      ( (A in Acts *> A=shiftOn(_,_,_)) ? -- all shifts
	  "    do("<>Sno.show()<>",Tok,Stack,Vstack) => \n"<>
	  "       case Tok.token() in (\n"
	  <>genShiftActs(Sno,Acts,T,TD,"        ",Tr)<>
	  ( Tr ?
	      "      | _ => \n"
	      "          valof{ stderr.outLine(\"error detected@ \"<>Tok.show());\n"
	      "                valis _;\n"
	      "          raise failed}\n"
	  | "      | _ => raise failed\n")<>
	  "     ).\n"
      | "    do("<>Sno.show()<>",Tok,Stack,Vstack) =>\n"<>
	"      case Tok.token() in (\n"<>
	valof{
	  A = genActions(Sno,Acts,G,R,nT,TD,"        ",Tr,nPr);
	  B = genReductions(Sno,Acts,R,nT,TD,nPr,Tr); -- To force the order of evaluation
	  E = genErrorRecovery(Sno,Acts,Tr);
	  valis A<>B<>E}<>
	"\n    ).\n"
      )<>genStates(States,G,R,T,nT,TD,Tr).
  
  private genReduction:[integer,integer,list[rule],list[(symbol,string)],
			list[symbol],logical]=>string.
  genReduction(Sno,Rno,R,TD,nT,Tr) => 
      ( rule(Rno,A,B,_,AA,_) in R ?
	  valof{
	    K = listlen(B);

	    case K in (
	     0 -> 
		 mtchStack = "";
		 nxVstack = "[__n("<>substVars(AA,"")<>"),..Vstack]";
		 nxStack = "["<>Sno.show()<>",..Stack]";
		 nxSno = "goto"<>A.show()<>"("<>Sno.show()<>")"

	    | 1 ->
		 mtchStack = "      ["<>extractVarRefs(K,B,AA,TD,nT,"")<>",..__Vstack] .= Vstack;\n";
		 nxVstack = "[__n("<>substVars(AA,"")<>"),..__Vstack]";
		 nxStack = "Stack";
		 nxSno = "goto"<>A.show()<>"(Stack.head())";
	    | _ ->
		 mtchStack = "      ["<>extractVarRefs(K,reverse(B),AA,TD,nT,"")<>",..__Vstack] .= Vstack;\n";
		 nxVstack = "[__n("<>substVars(AA,"")<>"),..__Vstack]";
		 nxStack = "drop(Stack,"<>(K-1).show()<>")";
		 nxSno = "goto"<>A.show()<>"("<>nxStack<>".head())";
	    );

	    ( Tr ?
		valis "valof{\n"	
		"      __nxSno = "<>nxSno<>";\n"<>
		"      stderr.outLine(\"Reduce by rule "<>Rno.show()<>"\");\n"
		"      stderr.outLine(\"Stack = \"<>Stack.show());\n"
		"      stderr.outLine(\"Dropping \"<>front(Vstack,"<>K.show()<>").show()<>\" from Vstack\");\n"
		<>mtchStack<>
		"      stderr.outLine(\"Adding \"<>"<>substVars(AA,"")<>".show()<>\" to Vstack\");\n"
		"      stderr.outLine(\"Goto state \"<>__nxSno.show());\n"
		"      stderr.outLine(\"Stack now = \"<>"<>nxStack<>".show());\n"
		"      valis do(__nxSno,Tok,"<>nxStack<>","<>nxVstack<>")\n"
		"    }"
	    | mtchStack!=[] ?
		valis "valof{\n"<>mtchStack<>
		"      valis do("<>nxSno<>",Tok,"<>nxStack<>","<>nxVstack<>")\n"
		"    }"
	    | valis " do("<>nxSno<>",Tok,"<>nxStack<>","<>nxVstack<>")"
	    )
	  }

      | raise error("problem in genparser",'eFAIL')
      ).

  private genReductions:[integer,list[actionT],
		      list[rule],
		      list[symbol],
		      list[(symbol,string)],string,logical]=>string.
  genReductions(_,[],_,_,_,_,_) => "".
  genReductions(Sno,[reduceBy(a,Rno),..Acts],R,nT,TD,Pr,Tr)=>
      Pr<>matchTok(a,TD,"_")<>" =>"<>
      genReduction(Sno,Rno,R,TD,nT,Tr)<>
      genReductions(Sno,Acts,R,nT,TD,"\n      | ",Tr).
  genReductions(Sno,[_,..Acts],R,nT,TD,Pr,Tr)=>
      genReductions(Sno,Acts,R,nT,TD,Pr,Tr).

  private stripStack:[integer]=>string.
  stripStack(0) => "".
  stripStack(N) => "_,"<>stripStack(N-1).
	
  private genShiftActs:[integer,list[actionT],
			list[symbol],
			list[(symbol,string)],string,logical]=>string.
  genShiftActs(_,[],_,_,_,_) => "".
  genShiftActs(Sno,[shiftOn(a,S,_),..Acts],T,TD,Pre,Tr) =>
      Pre<>matchTok(a,TD,"_")<>" => \n"<>
      ( Tr?
	  "       valof{\n"
	  "          stderr.outLine(\"Shift "<>a.show()<>" goto state "<>S.show()<>"\");\n"
	  "          __nxTok = lexer.nextToken();\n"
	  "          stderr.outLine(\"Next token is \"<>__nxTok.show());\n"
	  "          stderr.outLine(\"Stack now = \"<>["<>Sno.show()<>",..Stack].show());\n"

	  "          valis do("<>S.show()<>",__nxTok,["<>Sno.show()<>",..Stack],[__t(Tok),..Vstack])\n"
	  "      }\n"

       | "          do("<>S.show()<>",lexer.nextToken(),["<>Sno.show()<>",..Stack],[__t(Tok),..Vstack])\n"
      )<>
      genShiftActs(Sno,Acts,T,TD,"      | ",Tr).

  private extractVarRefs:[integer,list[symbol],
			  string,
			  list[(symbol,string)],
			  list[symbol],string]=>string.
  extractVarRefs(_,[],_,_,_,_) => "".
  extractVarRefs(K,[L,..RL],AA,TD,nT,Pre)::L in nT =>
      valof{
	{ append([`$,..K.show()],_,Kx)}; -- stick a variable on the end
	( append(_,Kx,AA) ? 
	    valis Pre<>"__n(__V"<>K.show()<>")"
	| valis Pre<>"_")
      }<>extractVarRefs(K-1,RL,AA,TD,nT,",").
  extractVarRefs(K,['error',..RL],AA,TD,nT,Pre) =>
      valof{
	{ append([`$,..K.show()],_,Kx)}; -- stick a variable on the end
	( append(_,Kx,AA) ?
	    valis Pre<>"__t(__V"<>K.show()<>")"
	 | valis Pre<>"_"
	)}
      <>extractVarRefs(K-1,RL,AA,TD,nT,",").
  extractVarRefs(K,[L,..RL],AA,TD,nT,Pre) =>
      valof{
	{ append([`$,..K.show()],_,Kx)}; -- stick a variable on the end
	 ( append(_,Kx,AA) ? 
	    Vx = "__V"<>K.show();
	    valis Pre<>"__t(__T"<>K.show()<>"@isToken("<>matchTok(L,TD,Vx)<>"))"
	| valis Pre<>"_")
      }<>extractVarRefs(K-1,RL,AA,TD,nT,",").

  private matchTok:[symbol,list[(symbol,string)],string]=>string.
  matchTok('__default__',_,_) => "_".
  matchTok(a,TD,Rep) :: (a,Txt) in TD => substVars(Txt,Rep).
  matchTok(a,_,_) => raise error("missing token info for "<>a.show(),'eFAIL').
      
  private substVars:[string,string]=>string.
  substVars([],_) => "".
  substVars([`$,`$,..Txt],V) => V<>substVars(Txt,V).
  substVars([`$,N,`.,`y,`y,`L,`i,`n,`e,..Txt],V) =>
      "__T"<>[N]<>".line()"<>substVars(Txt,V).
  substVars([`$,N,`.,`y,`y,`F,`i,`l,`e,..Txt],V) =>
      "__T"<>[N]<>".file()"<>substVars(Txt,V).
  substVars([`$,N,..Txt],V) =>
      "__V"<>[N]<>substVars(Txt,V).
  substVars([C,..Txt],V) => [C,..substVars(Txt,V)].
  

  private genGotos:[list[(integer,symbol,integer)],list[symbol]]=>string.
  genGotos(_,[]) => "".
  genGotos(G,[N,..nT]) => genGto(N,G)<>genGotos(G,nT).

  private genGto:[symbol,list[(integer,symbol,integer)]]=>string.
  genGto(N,G)=>
      "\ngoto"<>explode(N)<>":[integer]=>integer.\n"<>
      genGs(G,N,explode(N)).

  private genGs:[list[(integer,symbol,integer)],symbol,string]=>string.
  genGs([],_,_)=>"".
  genGs([(Sno,N,Nx),..G],N,Nm) => 
      "goto"<>Nm<>"("<>Sno.show()<>") => "<>Nx.show()<>".\n"<>genGs(G,N,Nm).
  genGs([_,..G],N,Nm) => genGs(G,N,Nm).

  private genActions:[integer,list[actionT],
		      list[(integer,symbol,integer)],
		      list[rule],
		      list[symbol],
		      list[(symbol,string)],string,logical,string-]=>string.
  genActions(_,[],_,_,_,_,Pre,_,Pre) => "".

  genActions(Sno,[accept(a),..Acts],G,R,nT,TD,Pr,Tr,nPr)=>
      Pr<>matchTok(a,TD,"_")<>" =>"
      " ( yyErrCount>0 ?\n"
      "                 raise error(yyErrCount.show()<>\" parse errors detected\",'eFAIL')\n"
      "               | "<>
      ( rule(_,'?',[A],_,_,_) in R, A in nT ?
	  "valof{\n"
	  "                 [__n(__Vx),.._] .= Vstack;\n"<>
	  (Tr ?
	     "                 stderr.outLine(\"accept \"<>__Vx.show());\n"
	 | "")<>
	  "                 valis __Vx;\n"
	  "               }\n"
    |   "valof{\n"
	"                   [__t(__Vx),.._] .= Vstack;\n"
	"                   valis __Vx\n"
	"                 }\n"
      )<>
      "               )\n"<>
      genActions(Sno,Acts,G,R,nT,TD,"    | ",Tr,nPr).

  genActions(Sno,[shiftOn(a,S,_),..Acts],G,R,nT,TD,Pr,Tr,nPr)=>
      Pr<>matchTok(a,TD,"_")<>" =>"<>
      (Tr?
	 "valof{\n"
	 "          stderr.outLine(\"Shift "<>a.show()<>", goto state "<>S.show()<>"\");\n"
	 "          __nxTok = lexer.nextToken();\n"
	 "          stderr.outLine(\"Next token is \"<>__nxTok.show());\n"
	 "         valis do("<>S.show()<>",__nxTok,["<>Sno.show()<>",..Stack],[__t(Tok),..Vstack])\n"
	 "        }\n"
    |  "         do("<>S.show()<>",lexer.nextToken(),["<>Sno.show()<>",..Stack],[__t(Tok),..Vstack])\n"
      )<>
      genActions(Sno,Acts,G,R,nT,TD,"      | ",Tr,nPr).
  genActions(Sno,[_,..Acts],G,R,nT,TD,Pr,Tr,nPr)=>
      genActions(Sno,Acts,G,R,nT,TD,Pr,Tr,nPr).
  genActions(Sno,[disabled(_),..Acts],G,R,nT,TD,Pr,Tr,nPr)=>
      genActions(Sno,Acts,G,R,nT,TD,Pr,Tr,nPr).

  genErrorRecovery:[integer,list[actionT],logical]=>string.
      
  genErrorRecovery(Sno,Acts,Tr)::recoverError([H,.._],Nx,_) in Acts =>
      "\n      | _ => valof{\n"<>
      ( Tr ?
	  "        stderr.outLine(\"In error recovery mode\\n\");\n"
      | "")<>
      "        scanForToken("<>H.show()<>");\n"<>
      (Tr ?
	 "        stderr.outLine(\"Current state is "<>Nx.show()<>"\");\n"
     | "")<>
      "        valis do("<>Nx.show()<>",lexer.currentToken(),["<>Sno.show()<>",..Stack],Vstack)\n"
      "      }\n".
  genErrorRecovery(_,_,_) => "".

  scanForToken:[logical]=>string.
  scanForToken(true) =>
      "\n  scanForToken:[yyTokType]*.\n"
      "  scanForToken(Tok)->\n"
      "     nxT = lexer.nextToken();\n"
      "     stderr.outLine(\"Moving past token \"<>nxT.show());\n"
      "     ( nxT.token()\\=Tok ? scanForToken(Tok)).\n\n".
  scanForToken(false) =>
      "\n  scanForToken:[yyTokType]*.\n"
      "  scanForToken(Tok)->\n"
      "     nxT = lexer.nextToken();\n"
      "     ( nxT.token()\\=Tok ? scanForToken(Tok)).\n\n".

      
}