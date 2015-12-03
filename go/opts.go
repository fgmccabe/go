/*
 * Module to process command line options
 */
go.opts{
  import go.stdparse.
  import go.io.

  option <~ {
	letterCode:[]=>char.
	stringCode:[]=>string.
	hasArgument:[]{}.
      }.

  -- This is a default option specification
  option:[char,string,logical]@=option.
  option(L,S,A)..{
    letterCode()=>L.			-- for a -<chr> option

    stringCode()=>S.			-- for a --name=<value> option

    hasArgument() :- A.
  }.

  optionSet <~ {
	help:[] => string.
	letterOption:[char,option]{}.
	longOption:[string,option]{}.
      }.

  private opts:[list[string],optionSet,list[(option,string)]]=>
      (list[(option,string)],list[string]).
  opts([],_,Opts)=>(Opts,[]).
  opts([[`-,`-,..cla],..Line],Options,Opts)::
	  append(OptNm,[`=,..V],cla), Options.longOption(OptNm,O) =>
      ( O.hasArgument()?
	  opts(Line,Options,[(O,V),..Opts])
      | raise error("Option --"<>OptNm<>" does not take an argument",'eINVAL')
      ).
  opts([[`-,`-,..cla],..Line],Options,Opts):: Options.longOption(cla,O) =>
      opts(Line,Options,[(O,cla),..Opts]).
  opts([[`-,`-,..cla],.._],_,_) =>
      raise error("Option --"<>cla<>" not recognized",'eNOTFND').
  opts([[`-,C],..Line],Options,Opts):: Options.letterOption(C,O) =>
      ( O.hasArgument() ?
	  ( [A,..R].=Line ?
	      opts(R,Options,[(O,A),..Opts])
	  | raise error("Option -"<>[C]<>" must have an argument",'eINVAL')
	  )
      | opts(Line,Options,[(O,[C]),..Opts])
      ).
  opts([[`-,C],.._],_,_) =>
      raise error("Option -"<>[C]<>" not recognized",'eNOTFND').
  opts(Line,_,Opts) => (Opts,Line).

  options:[list[string],optionSet]=>(list[(option,string)],list[string]).
  options(Line,Options) => 
      ( opts(Line,Options,[])
	onerror (
	 error(Bec,_) =>
	     valof{
	       stderr.outLine(Bec);
	       stderr.outLine(Options.help());
	       valis ([],[])
	     }
	)
      ) .

  commandLineOptions:[optionSet]=>(list[(option,string)],list[string]).
  commandLineOptions(Options) => options(tail(__command_line()),Options).

  private tail:[list[t]]=>list[t].
  tail([_,..L])=>L.

}
  
  