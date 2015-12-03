golex{
  import go.io.
  import go.opts.
  import golextypes.
  import genregexp.
  import gendfa.
  import gentoken.

  sigma:string = [`\+ffff;,.."abcdefghijklmnopqrstuvwxyz"
		  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		  "0123456789'`~!@#$%^&*()-_=+[]{};:,./<>?\\|\" \a\b\d\e\f\n\r\t\v"].

  
  private verboseOption:[]@=option.
  verboseOption <= option(`v,"verbose",false).

  private traceOption:[]@=option.
  traceOption <= option(`t,"trace",false).

  private timeOption:[]@=option.
  timeOption <= option(`T,"time",false).

  private pkgOption:[]@=option.
  pkgOption <= option(`p,"package",true).

  private golexOpts:[]@=optionSet.
  golexOpts..{
    help() => "-t enable trace in lexer\n"
	      "-v verbose lexer generation\n"
	      "-T enable timer flags\n"
	      "-p <package> use <package> for generated lexer".

    letterOption(`t,traceOption).
    letterOption(`T,timeOption).
    letterOption(`v,verboseOption).
    letterOption(`p,pkgOption).

    longOption("trace",traceOption).
    longOption("verbose",verboseOption).
    longOption("package",pkgOption).
  }.

  main(Args) ->
      (Opts,Files) = options(Args,golexOpts);
      verbose = ((verboseOption,_) in Opts ? true | false);
      trace = ((traceOption,_) in Opts ? true | false);
      timer = ((timeOption,_) in Opts ? true | false);
      start = now();
      (file in Files *>
       pkgName = ((pkgOption,P) in Opts ? P |
		   append(pk,".glx",file) ? pk | file);
       Text = getFile(file,unknownEncoding);
       (Pre,R) = split(Text);

       ( R!=[] ?
	   (T,Post) = split(R);
	   { parseFile(REs) --> T }
       | { parseFile(REs) --> R };
	 Post = ""
       );

       afterParse = now();

       (timer?
	  stdout.outLine("generated RE in "<>(afterParse-start).show()));
       ( verbose ?
	   ((State,RE) in REs *>
	    stdout.outLine(State<>": "<>RE.show()))
       );
       
       DfaStates = genDfa(REs,sigma);

       afterDFA = now();
       
       (timer?
	  stdout.outLine("generated DFE in "<>(afterDFA-afterParse).show()));

       ( verbose ?
	   showDfaStates(DfaStates);
	   stdout.outLine("")
       );
       
       out = openOutFile((append(pk,".glx",file) ? pk | file)<>".go",
			 utf8Encoding);
       
       out.outLine(genToken(pkgName,DfaStates,sigma,Pre,Post,trace));
       out.close();
       (timer?
	  stdout.outLine("generated output in "<>(now()-afterDFA).show());
	  stdout.outLine("total time is "<>(now()-start).show()));
      ).

  private split:[string]=>(string,string).
  split(T) => splt(T,[]).

  private splt:[string,string]=>(string,string).
  splt([],L) => (reverse(L),"").
  splt([`%,`%,`\n,..R],L) => (reverse(L),R).
  splt([C,..R],L) => splt(R,[C,..L]).

	
}