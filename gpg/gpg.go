gpg{
  -- An LALR parser generator for Go!, written in Go!
  import go.io.
  import go.opts.
  import gpg.gpgTypes.
  import gpg.grab.
  import gpg.gpgrules.
  import gpg.genrules.
  import gpglex.
  import gpg.gensets.
  import gpg.lr0.
  import gpg.lalr.
  import gpg.genparser.

  private suffixName:[string,string]=>string.
  suffixName(Nm,_)::`. in Nm => Nm.
  suffixName(Nm,S) => Nm<>S.

  private verbose:[]@=option.
  verbose <= option(`v,"verbose",false).

  private trace:[]@=option.
  trace <= option(`t,"trace",false).

  private lexer:[]@=option.
  lexer <= option(`l,"lexer",true).

  private type:[]@=option.
  type <= option(`y,"type",true).

  private gpgOpts:[]@=optionSet.
  gpgOpts..{
    help()=> "-t enable trace in parser\n"
	     "-v verbose parser generation\n"
	     "-l <lexer> use specific lexer\n"
	     "-y <type> use specific type".

    letterOption(`t,trace).
    letterOption(`v,verbose).
    letterOption(`l,lexer).
    letterOption(`y,type).

    longOption("trace",trace).
    longOption("verbose",verbose).
    longOption("lexer",lexer).
    longOption("type",type).
  }.

  private gpg:[string,list[(option,string)]]*.
  gpg(File,Opts) ->
      (pre,data,post) = grabData(File);

      aTree = parser(yygpglex(pre<>"%%"<>data<>"%%",countLines(pre,0)+1,listlen(pre)));
      (T,nT,Prs,Td,Rules,Start,Preamble)=extractRuleset(aTree);
      ( (verbose,_) in Opts ?
	  stdout.outLine("Terms = "<>T.show());
	  stdout.outLine("nonTerms = "<>nT.show());
	  stdout.outLine("Priorities = "<>Prs.show());
	  stdout.outLine("Token Data = "<>Td.show());
	  stdout.outLine("Rules");
	  ( R in Rules*>
	    stdout.outLine(R.show())
	  );
	  stdout.outLine("Start symbol = "<>Start.show());
	  stdout.outLine("Preamble is "<>Preamble)
      );
      N = nullable(nT,Rules);
      F = first(T,nT,Rules,N);

      ( (verbose,_) in Opts ?
	  stderr.outLine("Nullable non-terminals: "<>N.show());
	  stderr.outLine("First symbols: "<>F.show())
      );

      (Trans,G) = lr0states(Rules,Start);

/*      stdout.outLine("Trans = ");
      ( (Sno,I) in Trans *>
	stdout.outLine("ste: "<>Sno.show()<>":"<>I.show())
      );

      stdout.outLine("G = "<>G.show());
*/

      XX = lookAheadK(Trans,G,Rules,F,N);
--      stdout.outLine("XX = ");
--      showStates(XX,G,nT);

      Acts = actions(XX,G,nT,['error',..T],Prs,Rules,F,N);

      ( (verbose,_) in Opts ?
	  showActions(Acts,stderr);
	  (R in Rules *>
	     stderr.outLine(R.show())
	  )
      );

      Stem = analyseFileName(File);
      rulesFile = openOutFile(Stem<>".go",utf8Encoding);
      Pkg = ((_,Pk).=chopOn(Stem,`/),Pk!=""?Pk|Stem);

      rulesFile.outStr(genParser(((trace,_) in Opts?true|false),Preamble,
				 post,Pkg,Acts,G,
				 [rule(0,'?',[Start],[],[],nA(0)),..Rules],
				 T,nT,Td,
				 ((type,Tp) in Opts ? Tp | "yyType")));
      rulesFile.outLine("");
      rulesFile.close().
  
  private analyseFileName:[string]=>string.
  analyseFileName(Fn) :: (Stem,_)= chopOn(Fn,`.) => Stem.

  chopOn:[string,char]=>(string,string).
  chopOn(T,C)::append(F,[C,..B],T),\+C in B => (F,B).
  chopOn(T,_)=>(T,"").

  private countLines:[string,integer]=>integer.
  countLines([],L)=>L.
  countLines([`\n,..T],L) => countLines(T,L+1).
  countLines([_,..T],L) => countLines(T,L).
      
  main(Args) ->
      (O,Files) = options(Args,gpgOpts);
      ( F in Files *>
	gpg(F,O)).
}
