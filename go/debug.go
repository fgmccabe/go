/*
  A module to support debugging of Go! program
  (c) 2001-2008 F.G. McCabe
 
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Contact: Francis McCabe <frankmccabe@mac.com>
 */
go.debug{
  import go.io.
  import go.stdparse.
  import go.pp.
  import go.dynamic.

  currDebugger:debugger := 
      ((`g,_) in __command_opts() ? consDebug | nullDebugger).

  consDebug:debugger = consoleDebug().

  current:[]=>debugger.
  current()=>currDebugger.			-- the current debugger

  debugger <~ {
	line:[string,integer,integer,integer]*.
	break:[symbol]*.

	evaluate:[symbol,list[thing],symbol,list[(symbol,thing)]]*.
	value:[symbol,thing,symbol,list[(symbol,thing)]]*.
	prove:[symbol+,list[thing]+,symbol+,list[(symbol,thing)]+]{}.
	succ:[symbol+,list[thing]+,symbol+,list[(symbol,thing)]+]{}.
	call:[symbol,list[thing],symbol,list[(symbol,thing)]]*.
	return:[symbol,symbol,list[(symbol,thing)]]*.
	parse:[symbol+,list[thing],list[thing],symbol,list[(symbol,thing)]]{}.
	parsed:[symbol+,list[thing],symbol,list[(symbol,thing)]]{}.
	next:[thing,list[thing],symbol,list[(symbol,thing)]]{}.
	token:[thing+,list[thing],symbol,list[(symbol,thing)]]{}.

	rule:[symbol,integer,list[(symbol,thing)]]*.
	xrule:[symbol,integer]*.

	asgn:[symbol,thing]*.
	vlis:[thing]*.
	trigger:[symbol,thing]*.
      }.

  nullDebugger:[]@=debugger.
  nullDebugger..{
    evaluate(_,_,_,_) -> {}.
    value(_,_,_,_) -> {}.
    prove(_,_,_,_).
    succ(_,_,_,_).
    parse(_,_,_,_,_).
    parsed(_,_,_,_).
    next(_,_,_,_).
    token(_,_,_,_).
    call(_,_,_,_) -> {}.
    return(_,_,_) -> {}.
    line(_,_,_,_)->{}.
    vlis(_)->{}.
    asgn(_,_)->{}.
    trigger(_,_)->{}.
    rule(_,_,_)->{}.
    xrule(_,_)->{}.
    break(Ky)-> currDebugger:=consDebug; consDebug.break(Ky).
  }.

  consoleDebug:[]@>debugger.
  consoleDebug()..{
    debugOut:outChannel = stderr.
    debugIn:inChannel = stdin.

    tracing:logical := true.
    useShow:logical := true.

    private shw:[thing]=>string.
    shw(X)::useShow => X.show().
    shw(X) => __stringOf(X,-1024,0).

    private dispThread:[]=>pP.
    dispThread() => pSeq([pStr("["),pStr(__thread().show()),pStr("]")]).

    prove(Prd,Args,Ky,Vars) :: tracing :--
	( action{
	    debugOut.outLine(pSeq([dispThread(),pStr(" prove "),
				   dispCall(Prd,Args)]).show());
	    wayt(Ky,Vars,Prd,Args)}
	| until(Ky,Vars),
	  action{
	    debugOut.outLine(pSeq([dispThread(),pStr(" failing "),
				   dispCall(Prd,Args)]).show());
	  },
	  fail
	).
    prove(_,_,_,_) :-- true.

    succ(Prd,Args,Ky,Vars)::until(Ky,Vars) :--
	( action{
	    debugOut.outLine(pSeq([dispThread(),pStr(" proved "),
				   dispCall(Prd,Args)]).show())}
	| action{
	    debugOut.outLine(pSeq([dispThread(),pStr(" retry "),
				   dispCall(Prd,Args)]).show())
	},
	  fail
	).
    succ(_,_,_,_) :-- true.

    line(File,Ln,S,E)::\+stepping,checkLine(File,Ln) ->
	stepping := true;
	tracing := true;
	line(File,Ln,S,E).
    line(File,Ln,_,_)::tracing -> 
	debugOut.outLine(pSeq([dispThread(),pStr(" line "),
			       pStr(File),pStr(":"),pInt(Ln)]).show()).
    line(_,_,_,_) -> {}.

    evaluate(Fun,Args,Ky,Vars)::tracing -> 
	debugOut.outLine(pSeq([dispThread(),pStr(" evaluate "),
			       dispCall(Fun,Args)]).show());
	wayt(Ky,Vars,Fun,Args).
    evaluate(_,_,_,_)->{}.

    value(Name,Value,Ky,Vars) -> 
	(until(Ky,Vars) ?
	   debugOut.outLine(pSeq([dispThread(),pSym(Name),pStr(" = "),
				  pStr(shw(Value))]).show())).

    parse(Gr,Strm,Args,Ky,Vars)::tracing :--
	( action{
	    debugOut.outLine(pSeq([dispThread(),pStr(" parse "),
				   pStr(upto(Strm,5).show()),pStr(" using "),
				   dispCall(Gr,Args)]).show());
	    wayt(Ky,Vars,Gr,Args)
	  }
	| action{
	    debugOut.outLine(pSeq([dispThread(),pStr(" could not parse "),
				   pStr(upto(Strm,5).show()),pStr(" using "),
				   dispCall(Gr,Args)]).show())
	},
	  fail
	).
    parse(_,_,_,_,_):--true.

    parsed(Gr,Rem,Ky,Vars)::until(Ky,Vars) :--
	( action{
	    debugOut.outLine(pSeq([dispThread(),pStr(" parsed "),
				   dispCall(Gr,Args),
				   pStr(" "),pInt(listlen(Rem)),
				   pStr(" remaining")]).show())}
	| action{
	    debugOut.outLine(pSeq([dispThread(),pStr(" retry parse of "),
				   dispCall(Gr,Args)]).show())},
	  fail
	).
    parsed(_,_,_,_) :-- true.

    next(Tk,Strm,Ky,Vars)::tracing :--
	( action{
	    debugOut.outLine(pSeq([dispThread(),pStr(" next token from "),
				   pStr(upto(Strm,5).show())]).show());
	    wayt(Ky,Vars,'token',[Tk])
	  }
	| fail
	).

    token(Tk,_,Ky,Vars)::until(Ky,Vars) :--
	( action{
	    debugOut.outLine(pSeq([dispThread(),pStr(" next token is "),
				   pStr(Tk.show())]).show())}
	| action{
	    debugOut.outLine(pSeq([dispThread(),pStr(" backtracking over token "),pStr(Tk.show())]).show())},
	  fail
	).
    token(_,_,_,_) :-- true.

    call(Pr,Args,Ky,Vars)::tracing -> 
	debugOut.outLine(pSeq([dispThread(),pStr(" call "),
				   dispCall(Pr,Args)]).show());
	wayt(Ky,Vars,Pr,Args).
    call(_,_,_,_)->{}.

    return(Pr,Ky,Vars)::until(Ky,Vars) ->
	debugOut.outLine(pSeq([dispThread(),pStr(" return "),pSym(Pr)]).show()).
    return(_,_,_)->{}.

    vlis(Value)::tracing ->
	debugOut.outLine(pSeq([dispThread(),pStr(" valis "),
			       pStr(Value.show())]).show()).
    vlis(_)->{}.

    asgn(Var,Value)::tracing ->
	debugOut.outLine(pSeq([dispThread(),pSym(Var),pStr(" := "),
			       pStr(Value.show())]).show()).
    asgn(_,_)->{}.

    trigger(Var,Value)::tracing ->
	debugOut.outLine(pSeq([dispThread(),pSym(Var),pStr(" triggered with "),
			       pStr(Value.show())]).show()).
    trigger(_,_)->{}.

    rule(Nm,No,_)::tracing ->
	debugOut.outLine(pSeq([dispThread(),pStr(" rule "),
			       pSym(Nm),pStr(" "),pInt(No)]).show()).
    rule(_,_,_) -> {}.

    xrule(Nm,No)::tracing ->
	debugOut.outLine(pSeq([dispThread(),pStr(" leave "),
			       pSym(Nm),pStr(" "),pInt(No)]).show()).
    xrule(_,_) -> {}.

    stepping:logical := true.

    break(_)-> stepping := true; tracing:=true.

    skipUntil:symbol := ''.

    -- until is true if either we have reached the end of a skipped section
    -- or we are already tracing
    until:[symbol,list[(symbol,thing)]]{}.
    until(skipUntil,_) :--
	action{
	  tracing := true;
	  stepping := true;
	  skipUntil := ''}.
    until(_,_):--tracing.
    
    wayt:[symbol,list[(symbol,thing)],symbol,list[thing]]*.
    wayt(_,_,_,_)::\+tracing -> {}.
    wayt(Ky,Vars,Pr,Args)::stepping ->
	debugOut.outStr("\n[");
	debugOut.outStr(__thread().show());
	debugOut.outStr("] (go.Debug)? ");
	input = debugIn.inLine("\n");
--	stdout.outLine("Debug command line: "<>input);
	case input in (
	 [`n] -> skipUntil := Ky; stepping := false; tracing := false
       | [`s] -> {}
       | [`q] -> exit(1)
       | [`c] -> stepping:=false; tracing:=false
       | [`t] -> stepping:=false; tracing:=true
       | [`i] -> __ins_debug()
       | [`v] -> showVars(Vars); wayt(Ky,Vars,Pr,Args)
       | [`v,..L] -> (skipWhiteSpace(),ident(Nm)-->L ? showVar(Nm,Vars)); 
	     wayt(Ky,Vars,Pr,Args)
       | [`b,..Ln] -> parseBreakInfo(Ln); wayt(Ky,Vars,Pr,Args)
       | [`B,..Ln] -> parseBreakInfo(Ln); wayt(Ky,Vars,Pr,Args)
       | [`x] -> showCall(Pr,Args); wayt(Ky,Vars,Pr,Args)
       | [`0] -> debugOut.outLine("Program = "<>Pr.show());  wayt(Ky,Vars,Pr,Args)
       | [`1] -> showArg(1,"st",Args); wayt(Ky,Vars,Pr,Args)
       | [`2] -> showArg(2,"nd",Args); wayt(Ky,Vars,Pr,Args)
       | [`3] -> showArg(3,"rd",Args); wayt(Ky,Vars,Pr,Args)
       | [`4] -> showArg(4,"th",Args); wayt(Ky,Vars,Pr,Args)
       | [`5] -> showArg(5,"th",Args); wayt(Ky,Vars,Pr,Args)
       | [`6] -> showArg(6,"th",Args); wayt(Ky,Vars,Pr,Args)
       | [`7] -> showArg(7,"th",Args); wayt(Ky,Vars,Pr,Args)
       | [`8] -> showArg(8,"th",Args); wayt(Ky,Vars,Pr,Args)
       | [`9] -> showArg(9,"th",Args); wayt(Ky,Vars,Pr,Args)

       | [] -> {}
       | _ -> debugOut.outLine("`n = step over");
	     debugOut.outLine("`s = step in");
	     debugOut.outLine("`\n = step in");
	     debugOut.outLine("`q = quit");
	     debugOut.outLine("`c = continue mode");
	     debugOut.outLine("`t = trace mode");
             debugOut.outLine("`b file:line = set break point");
	     debugOut.outLine("`v = show all known variables");
	     wayt(Ky,Vars,Pr,Args)
	).
    wayt(_,_,_,_) -> {}.

    parseBreakInfo:[string]*.
    parseBreakInfo(In) ->
	debugOut.outLine("Break command: "<>In);
	( (skipWhiteSpace(),ident(Nm) --> In) 
	  ? debugOut.outLine("Setting break point at program "<>Nm);
	    setBreakOnEntry(implode(Nm))
	| (skipWhiteSpace(),parseString(Fl),":",naturalOf(Ln) --> In)
	  ? debugOut.outLine("Setting break point at "<>Fl<>"/"<>Ln.show());
	    setBreakOnLine(Fl,Ln)
	| {}).

    lineBreaks:dynamic[(string,integer)] = dynamic([]).
    setBreakOnLine:[string,integer]*.
    setBreakOnLine(Fl,Ln) :: lineBreaks.mem((Fl,Ln)) -> {}.
    setBreakOnLine(Fl,Ln) -> lineBreaks.add((Fl,Ln)).

    clearBreakInfo:[string]*.
    clearBreakInfo(In) ->
	( (skipWhiteSpace(),"*" --> In) 
	  ? debugOut.outLine("Clear all break points ");
	    removeAllBreaks()
	| (skipWhiteSpace(),ident(Nm) --> In) 
	  ? debugOut.outLine("Clear break point at program "<>Nm);
	    removeBreakOnEntry(implode(Nm))
	| (skipWhiteSpace(),parseString(Fl),":",naturalOf(Ln) --> In)
	  ? debugOut.outLine("Clear break point at "<>Fl<>"/"<>Ln.show());
	    removeBreakOnLine(Fl,Ln)
	| {}).

    removeBreakOnLine:[string,integer]*.
    removeBreakOnLine(Fl,Ln) -> lineBreaks.del((Fl,Ln)).

    removeAllBreaks:[]*.
    removeAllBreaks() -> 
	lineBreaks.delall(_);
	progBreaks.delall(_).

    checkLine:[string,integer]{}.
    checkLine(Fl,Ln):-lineBreaks.mem((Fl,Ln)).

    progBreaks:dynamic[symbol] = dynamic([]).
    setBreakOnEntry:[symbol]*.
    setBreakOnEntry(Pr)::progBreaks.mem(Pr) -> {}.
    setBreakOnEntry(Pr) -> progBreaks.add(Pr).

    removeBreakOnEntry:[symbol]*.
    removeBreakOnEntry(Pr) -> progBreaks.del(Pr).

    checkEntry:[symbol]{}.
    checkEntry(Pr):-progBreaks.mem(Pr).

    showCall:[symbol,list[thing]]*.
    showCall(Pr,[]) ->
	debugOut.outStr(explode(Pr));
	debugOut.outLine("()").
    showCall(Pr,[Arg]) ->
	debugOut.outStr(explode(Pr));
	debugOut.outStr("(");
	debugOut.outStr(Arg.show());
	debugOut.outLine(")").
    showCall(Pr,[Arg,..Args]) ->
	debugOut.outStr(explode(Pr));
	debugOut.outStr("(");
	debugOut.outStr(Arg.show());
	(A in Args *>
	 debugOut.outStr(",");
	 debugOut.outStr(A.show()));
	debugOut.outLine(")").

    dispCall:[symbol,list[thing]]=>pP.
    dispCall(Pr,[]) => pSeq([pSym(Pr),pStr("()")]).
    dispCall(Pr,Args) => pSeq([pSym(Pr),pStr("("),
			       pTwine({pStr(a.show())..a in Args},pStr(", ")),
			       pStr(")")]).


    showArg:[integer,string,list[thing]]*.
    showArg(N,Suff,Args) :: N>0 , N=<listlen(Args) ->
	debugOut.outStr(N.show());
	debugOut.outStr(Suff);
	debugOut.outStr(" arg is ");
	debugOut.outLine(nth(Args,N).show()).
    showArg(N,Suff,Args) ->
	debugOut.outLine("no "<>N.show()<>Suff<>" argument in "<>Args.show()).

    showVar:[string,list[(symbol,thing)]]*.
    showVar(Vr,Vars)::nonvar(Vars) ->
	V = implode(trim(Vr));
	( (V,Val) in Vars ?
	    debugOut.outLine(Vr<>" = "<>Val.show())
	| debugOut.outLine("Do not know "<>Vr)
	).
    showVar(_,_) -> {}.

    showVars:[list[(symbol,thing)]]*.
    showVars(Vars)::nonvar(Vars) ->
	debugOut.outLine("Variables in rule:");
	((V,Val) in Vars *>
	 debugOut.outLine(V.show()<>" = "<>Val.show())).
    showVars(_) -> {}.

    trim:[string]=>string.
    trim([` ,..V]) => trim(V).
    trim([`\t,..V]) => trim(V).
    trim(V)::append(F," ",V) => trim(F).
    trim(V)::append(F,"\n",V) => trim(F).
    trim(V) => V.

    upto:[list[t],integer]=>list[t].
    upto(L,0)=>L.
    upto([],_)=>[].
    upto([E,..L],C) => [E,..upto(L,C-1)].
  }.
}
