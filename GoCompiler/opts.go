/*
 * Process options
 */
opts{
  import go.stdparse.
  import go.io.
  import go.dynamic.
  import misc.

  fileLoc <~ {
	extend:[integer]=>fileLoc.
	url:[]=>string.
	from:[]=>integer.
	to:[]=>integer.
      }.

  noLoc:[]@=fileLoc.
  noLoc..{
    show()=>"noLoc".
    extend(_) => raise error("not possible",'eFAIL').
    url()=>raise error("not possible",'eFAIL').
    from()=> raise error("not possible",'eFAIL').
    to()=> raise error("not possible",'eFAIL').
  }.
  loc:[symbol,integer,integer]@>fileLoc.
  loc(Fl,Fr,To)..{
    show()::Fr==To => explode(Fl)<>"["<>Fr.show()<>"]".
    show()=>explode(Fl)<>"["<>Fr.show()<>"-"<>To.show()<>"]".
    url()=>explode(Fl).
    extend(Nt) => loc(Fl,Fr,Nt).
    from() => Fr.
    to() => To.
  }.

  compOpt ::= noStdlib | noCodeGen | genGOF | noMeta
            | genProfile
	    | wffOnly | typeCheckOnly
            | bkPt(symbol) | noDbg(symbol) | dbg(symbol) | dbgVar(list[symbol])
	    | version(symbol) 
            | dbgComp | dbgType | dbgCanon | dbgParse | dbgDeps
            | codeFile(string)
	    | classPath(list[string]).

  Options:[]=>(list[string],list[compOpt],list[string]).
  Options() :: opts(tail(__command_line()),
                    [fcwd()<>"/",getenv('GO_DIR',"/opt/go")<>"/"],
                    Opts,Path,Files) => (Path,adjustedOpts(Path,Opts),Files).

  typeOpts <~ {
	isStatefull:[]{}.
	isTopLevel:[]{}.
	inPkg:[]=>symbol.
	option:[compOpt]{}.
	options:[]=>list[compOpt].
	markStatefull:[logical]=>typeOpts.
      }.

  typeOpts:[logical,logical,symbol,list[compOpt]]@>typeOpts.
  typeOpts(St,Top,Pkg,Opts)..{
    isStatefull():-St.
    isTopLevel():-Top.
    inPkg()=>Pkg.
    option(O) :- O in Opts.
    options()=>Opts.
    markStatefull(S)=>typeOpts(S,false,Pkg,Opts).
  }.

  subOpts:[logical,typeOpts]=>typeOpts.
  subOpts(stateful,Opts) =>
      typeOpts(stateful,false,Opts.inPkg(),Opts.options()).

  private adjustedOpts:[list[string],list[compOpt]]=>list[compOpt].
  adjustedOpts(Path,Opts)::\+version(_) in Opts => [classPath(Path),..Opts].
  adjustedOpts(Path,Opts) => [classPath(Path),version('*'),..Opts].
       
  private options:dynamic[compOpt] = dynamic(valof{ (_,O,_) = Options(); valis O}).

  isOption:[compOpt]{}.
  isOption(Opt) :- options.mem(Opt).

  private opts:[list[string],list[string],list[compOpt],list[string],list[string]]{}.
  opts([[`-,..cla],..Line],P,Opts,Path,Files) :-
      selectOpt(cla,Line,P,Opts,Path,Files).
  opts(Files,Path,[],Path,Files).

  private selectOpt:[string,list[string],list[string],list[compOpt],
                     list[string],list[string]]{}.
  selectOpt("p",Line,P,[genProfile,..Opts],Path,Files) :-
      opts(Line,P,Opts,Path,Files).
  selectOpt("P",[Dir,..Line],P,Opts,Path,Files) :-
      opts(Line,[Dir,..P],Opts,Path,Files).
  selectOpt("nostdlib",Line,P,[noStdlib,..Opts],Path,Files) :-
      opts(Line,P,Opts,Path,Files).
  selectOpt("o",[Fl,..Line],P,[codeFile(Fl),..Opts],Path,Files) :-
      opts(Line,P,Opts,Path,Files).
  selectOpt("b",[Ln,..Line],P,[bkPt(Pt),..Opts],Path,Files) :-
      Pt = implode(Ln),
      opts(Line,P,Opts,Path,Files).
  selectOpt("gb",[Ln,..Line],P,[dbg(Pt),..Opts],Path,Files) :-
      Pt = implode(Ln),
      opts(Line,P,Opts,Path,Files).
  selectOpt("g",Line,P,[dbg('*'),..Opts],Path,Files) :-
      opts(Line,P,Opts,Path,Files).
  selectOpt("ve",[Vr,..Line],P,[version(Vers),..Opts],Path,Files) :-
      Vers = implode(Vr),
      opts(Line,P,Opts,Path,Files).
  selectOpt("dx",Line,P,[noCodeGen,..Opts],Path,Files) :-
      opts(Line,P,Opts,Path,Files).
  selectOpt("dw",Line,P,[wffOnly,..Opts],Path,Files) :-
      opts(Line,P,Opts,Path,Files).
  selectOpt("dt",Line,P,[typeCheckOnly,..Opts],Path,Files) :-
      opts(Line,P,Opts,Path,Files).
  selectOpt("dP",Line,P,[dbgParse,..Opts],Path,Files) :-
      opts(Line,P,Opts,Path,Files).
  selectOpt("dE",Line,P,[dbgDeps,..Opts],Path,Files) :-
      opts(Line,P,Opts,Path,Files).
  selectOpt("dT",Line,P,[dbgType,..Opts],Path,Files) :-
      opts(Line,P,Opts,Path,Files).

  selectOpt(Opt,Line,P,[],P,Line) :-
      action{
        stdout.outLine("Unknown option: "<>Opt);
        stdout.outLine("-P <dir>  -- add <dir> to class path");
        stdout.outLine("-gb <p>   -- enable symbolic debugging of p");
        stdout.outLine("-b <p>    -- set break point on entry to p");
        stdout.outLine("-g        -- enable symbolic debugging of all programs");
        stdout.outLine("-p        -- enable profiling");
        stdout.outLine("-o file   -- send output to file");
        stdout.outLine("-ve <v>   -- set version to v");
        stdout.outLine("-b <p>    -- set break point on entry to p")
      }.

  private tail:[list[t]]=>list[t].
  tail([_,..L])=>L.

  private debug:[do,compOpt]*.
  debug(D,O)::isOption(O) ->
      D.do().
  debug(_,_)->{}.
}.
