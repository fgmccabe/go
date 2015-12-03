/*
 * Dependency analysis of theta environments 
 */
depends{
  import abstract.
  import errors.
  import misc.
  import opts.
  import terms.
  import go.do.
  import go.io.

  thetaDepend:[list[abstract],typeOpts]=>
      list[list[(abstract,visibility,abstract)]].
  
  thetaDepend(Th,Opts)::thDepend(Th,[],[],[],[],Imp,Defs) => 
      valof{
	Gps = analyseTheta(Defs,Opts);
	( Opts.option(dbgDeps)?
	    stdout.outLine(showGroups(Gps)));
	(Imp==[] ? valis Gps  | valis [Imp]<>Gps)
      }.

  private thDepend:[list[abstract]+,
		  list[(abstract,visibility,abstract)]+,
		  list[(abstract,visibility,abstract)]+,
		  list[(abstract,visibility,abstract)]+,
		  list[(abstract,visibility,abstract)]+,
		  list[(abstract,visibility,abstract)]-,
		  list[(abstract,visibility,abstract)]-]{}.

  thDepend([APPLY(IDEN('import',L0),[Pk],Lc),..Th],Dfs,Types,Inits,I,Imp,Defs) :--
      thDepend(Th,Dfs,Types,Inits,[(IDEN(implode(makePkgName(Pk)),Lc),privAte,
				     APPLY(IDEN('import',L0),[Pk],Lc)),..I],Imp,Defs).
  thDepend([APPLY(IDEN('<~',L0),[Id,APPLY(IDEN('private',_),[R],_)],_),..Th],
	   Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Tps),
      thDepend(Th,Dfs,[(Id,privAte,APPLY(IDEN('<~',L0),[Id,R],L0)),..Tps],Inits,I,Imp,Defs).
  thDepend([APPLY(IDEN('<~',L0),[Id,R],Lx),..Th],
	   Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Tps),
      thDepend(Th,Dfs,[(Id,pUblic,APPLY(IDEN('<~',L0),[Id,R],Lx)),..Tps],Inits,I,Imp,Defs).
  thDepend([APPLY(IDEN(':',L0),[Id,APPLY(IDEN('private',_),[R],_)],_),..Th],
	   Dfs,Tps,Inits,I,Imp,Defs):--
      thDepend(Th,Dfs,[(Id,privAte,APPLY(IDEN(':',L0),[Id,R],L0)),..Tps],Inits,I,Imp,Defs).
  thDepend([APPLY(IDEN(':',L0),[Id,R],Lx),..Th],
	   Dfs,Tps,Inits,I,Imp,Defs):--
      thDepend(Th,Dfs,[(Id,pUblic,APPLY(IDEN(':',L0),[Id,R],Lx)),..Tps],Inits,I,Imp,Defs).

  thDepend([APPLY(IDEN('=',L0),[Id,APPLY(IDEN('private',_),[R],_)],_),..Th],
	   Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Dfs),
      thDepend(Th,[(Id,privAte,APPLY(IDEN('=',L0),[Id,R],L0)),..Dfs],Tps,Inits,I,Imp,Defs).
  thDepend([APPLY(IDEN('=',L0),[Id,R],Lx),..Th],Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Dfs),
      thDepend(Th,[(Id,pUblic,APPLY(IDEN('=',L0),[Id,R],Lx)),..Dfs],Tps,Inits,I,Imp,Defs).

  thDepend([APPLY(IDEN(':=',L0),[Id,APPLY(IDEN('private',_),[R],_)],_),..Th],
	   Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Dfs),
      thDepend(Th,[(Id,privAte,APPLY(IDEN(':=',L0),[Id,R],L0)),..Dfs],Tps,Inits,I,Imp,Defs).
  thDepend([APPLY(IDEN(':=',L0),[Id,R],Lx),..Th],Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Dfs),
      thDepend(Th,[(Id,pUblic,APPLY(IDEN(':=',L0),[Id,R],Lx)),..Dfs],Tps,Inits,I,Imp,Defs).

  thDepend([APPLY(IDEN('=>',L0),[Id,APPLY(IDEN('private',_),[R],_)],_),..Th],
	   Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Dfs),
      thDepend(Th,[(Id,privAte,APPLY(IDEN('=>',L0),[Id,R],L0)),..Dfs],Tps,Inits,I,Imp,Defs).
  thDepend([APPLY(IDEN('=>',L0),[Id,R],Lx),..Th],Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Dfs),
      thDepend(Th,[(Id,pUblic,APPLY(IDEN('=>',L0),[Id,R],Lx)),..Dfs],Tps,Inits,I,Imp,Defs).

  thDepend([APPLY(IDEN(':-',L0),[Id,APPLY(IDEN('private',_),[R],_)],_),..Th],
	   Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Dfs),
      thDepend(Th,[(Id,privAte,APPLY(IDEN(':-',L0),[Id,R],L0)),..Dfs],Tps,Inits,I,Imp,Defs).
  thDepend([APPLY(IDEN(':-',L0),[Id,R],Lx),..Th],Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Dfs),
      thDepend(Th,[(Id,pUblic,APPLY(IDEN(':-',L0),[Id,R],Lx)),..Dfs],Tps,Inits,I,Imp,Defs).

  thDepend([APPLY(IDEN(':--',L0),[Id,APPLY(IDEN('private',_),[R],_)],_),..Th],
	   Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Dfs),
      thDepend(Th,[(Id,privAte,APPLY(IDEN(':--',L0),[Id,R],L0)),..Dfs],Tps,Inits,I,Imp,Defs).
  thDepend([APPLY(IDEN(':--',L0),[Id,R],Lx),..Th],Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Dfs),
      thDepend(Th,[(Id,pUblic,APPLY(IDEN(':--',L0),[Id,R],Lx)),..Dfs],Tps,Inits,I,Imp,Defs).

  thDepend([APPLY(IDEN('-->',L0),[Id,APPLY(IDEN('private',_),[R],_)],_),..Th],
	   Dfs,Tps,Inits,I,Imp,Defs):--
      thDepend(Th,[(Id,privAte,APPLY(IDEN('-->',L0),[Id,R],L0)),..Dfs],Tps,Inits,I,Imp,Defs).
  thDepend([APPLY(IDEN('-->',L0),[Id,R],Lx),..Th],Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Dfs),
      thDepend(Th,[(Id,pUblic,APPLY(IDEN('-->',L0),[Id,R],Lx)),..Dfs],Tps,Inits,I,Imp,Defs).

  thDepend([APPLY(IDEN('->',L0),[Id,APPLY(IDEN('private',_),[R],_)],_),..Th],
	   Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Dfs),
      thDepend(Th,[(Id,privAte,APPLY(IDEN('->',L0),[Id,R],L0)),..Dfs],Tps,Inits,I,Imp,Defs).
  thDepend([APPLY(IDEN('->',L0),[Id,R],Lx),..Th],Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Dfs),
      thDepend(Th,[(Id,pUblic,APPLY(IDEN('->',L0),[Id,R],Lx)),..Dfs],Tps,Inits,I,Imp,Defs).

  thDepend([APPLY(IDEN('<$',L0),[Id,APPLY(IDEN('private',_),[R],_)],_),..Th],
	   Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Dfs),
      thDepend(Th,[(Id,privAte,APPLY(IDEN('<$',L0),[Id,R],L0)),..Dfs],Tps,Inits,I,Imp,Defs).
  thDepend([APPLY(IDEN('<$',L0),[Id,R],Lx),..Th],Dfs,Tps,Inits,I,Imp,Defs):--
      checkName(Id,Dfs),
      thDepend(Th,[(Id,pUblic,APPLY(IDEN('<$',L0),[Id,R],Lx)),..Dfs],Tps,Inits,I,Imp,Defs).

  thDepend([APPLY(IDEN('$',L0),[In],Lx),..Th],Dfs,Tps,Inits,I,Imp,Defs):--
      thDepend(Th,Dfs,Tps,[(IDEN('$',L0),privAte,APPLY(IDEN('$',L0),[In],Lx)),..Inits],I,Imp,Defs).

  thDepend([],Types,Dfs,Inits,I,I,Defs):--Defs=Types<>Inits<>Dfs.


  private makePkgName:[abstract]=>string.
  makePkgName(IDEN(Nm,_))=>explode(Nm).
  makePkgName(APPLY(IDEN('.',_),[L,R],_))=>makePkgName(L)<>[`.,..makePkgName(R)].

  private checkName:[abstract,list[(abstract,visibility,abstract)]]{}.
  checkName(IDEN(Nm,Lc),Defs) :-
      ( (IDEN(Nm,L0),_,_) in Defs ?
	  action{ reportError("multiple definitions of "<>explode(Nm)<>
			      ", also at "<>L0.show(),Lc) }
      | true).

  private analyseTheta:[list[(abstract,visibility,abstract)],typeOpts]=>
      list[list[(abstract,visibility,abstract)]].
  analyseTheta(Defs,Opts) => 
      (:ev[list[list[(abstract,visibility,abstract)]]]..{

	 ev()=> valof{
		  defs := Defs;
		  processDefs();
		  valis reverse(groups)
		}.

	 stk:list[(abstract,integer,visibility,abstract)] := [].
	 defs:list[(abstract,visibility,abstract)] := [].
	 groups:list[list[(abstract,visibility,abstract)]]:=[].

	 processDefs:[]*.
	 processDefs()::defs=[] -> {}.
	 processDefs()::[(Id,_,_),.._].=defs ->
	     _ = analyseDef(Id,[]);
	     processDefs().

	 analyseDef:[abstract,list[symbol]]=>integer.
	 analyseDef(Id,eX)=>
	     valof{
	       ( Opts.option(dbgDeps)?
		   stdout.outLine("Analysing dependencies of "<>Id.show()));
	       low = listlen(stk)+1;
	       (D,S) = filterDefs(defs,stk,[],low,Id);
	       stk:=S;
	       defs:=D;			-- reset the list of definitions
	      
	       point = minPoints(low,{analyse(P,low,eX)..(_,_,_,P) in S});

	       ( Opts.option(dbgDeps)?
		   stdout.outLine("Minpoint of analysing dependencies of "<>Id.show()<>" is "<>point.show()));
	       
	       (low==point ?
		  (SS,G) = makeGroup(point,stk,[]);
		  stk := SS;
		  groups := [G,..groups];
		  ( Opts.option(dbgDeps)?
		      stdout.outLine("New group "<>showGroup(G)))
	      | {});
	       valis point
	     }.
	 
	 -- Seed the stack with the selected definitions
	 filterDefs:[list[(abstract,visibility,abstract)],
		     list[(abstract,integer,visibility,abstract)],
		     list[(abstract,visibility,abstract)],integer,abstract]=>
	     (list[(abstract,visibility,abstract)],
	      list[(abstract,integer,visibility,abstract)]).
	 filterDefs([],S,D,_,_)=>(D,S).
	 filterDefs([(IDEN(Nm,Lc),M,E),..D],S,Df,low,IDEN(Nm,L0)) =>
	     filterDefs(D,[(IDEN(Nm,Lc),low,M,E),..S],Df,low,IDEN(Nm,L0)).
	 filterDefs([(Id,M,E),..D],S,Df,low,Nm)=>
	     filterDefs(D,S,[(Id,M,E),..Df],low,Nm).
	 
	 inDefs:[symbol+]{}.
	 inDefs(Nm) :- (IDEN(Nm,_),_,_) in defs.

	 inStack:[symbol,list[(abstract,integer,visibility,abstract)],integer]{}.
	 inStack(Nm,[(IDEN(Nm,_),L,_,_),.._],L):-- true.
	 inStack(Nm,[_,..Stk],L) :-- inStack(Nm,Stk,L).
	 
	 analyse:[abstract,integer,list[symbol]]=>integer.
	 analyse(IDEN(Nm,_),low,eX)::Nm in eX => low. -- excluded name
	 analyse(IDEN(Nm,_),_,_)::inStack(Nm,stk,L) => L.
	 analyse(IDEN(Nm,Lc),_,eX)::inDefs(Nm) => analyseDef(IDEN(Nm,Lc),eX).
	 analyse(IDEN(_,_),low,_) => low.
	 analyse(INT(_,_),low,_)=>low.
	 analyse(FLT(_,_),low,_)=>low.
	 analyse(SYM(_,_),low,_)=>low.
	 analyse(CHR(_,_),low,_)=>low.
	 analyse(STR(_,_),low,_)=>low.
	 analyse(APPLY(IDEN('..',_),[Lbl,APPLY(IDEN('{}',Lc),Th,L0)],_),low,eX) =>
	     valof{
	       eXX = extendExcl(Th,eX);
	       valis minPoint(analyse(Lbl,low,eXX),
			      analyse(APPLY(IDEN('{}',Lc),Th,L0),low,eXX))
	     }.
/*	 analyse(APPLY(IDEN('=>',_),[P,R],_),low,eX) =>
	     minPoint(analyse(P,low,eX),analyse(R,low,eX)).
	 analyse(APPLY(IDEN(':-',_),[P,R],_),low,eX) =>
	     minPoint(analyse(P,low,eX),analyse(R,low,eX)).
	 analyse(APPLY(IDEN(':--',_),[P,R],_),low,eX) =>
	     minPoint(analyse(P,low,eX),analyse(R,low,eX)).
	 analyse(APPLY(IDEN('->',_),[P,R],_),low,eX) =>
	     minPoint(analyse(P,low,eX),analyse(R,low,eX)).
	 analyse(APPLY(IDEN('-->',_),[P,R],_),low,eX) =>
	     minPoint(analyse(P,low,eX),analyse(R,low,eX)).
*/
	 analyse(APPLY(IDEN('.',_),[P,IDEN(_,_)],_),low,eX) => analyse(P,low,eX).
/*	 analyse(APPLY(IDEN('.',_),[P,R],_),low,eX) =>
	     minPoint(analyse(P,low,eX),analyse(R,low,eX)).
	 analyse(APPLY(IDEN(':',_),[P,R],_),low,eX) =>
	     minPoint(analyse(P,low,eX),analyse(R,low,eX)).
*/
	 analyse(APPLY(L,R,_),low,eX) =>
	     minPoint(analyse(L,low,eX),minPoints(low,{analyse(A,low,eX)..A in R})).
	 analyse(BRACE(L,R,_),low,eX) =>
	     minPoint(analyse(L,low,eX),minPoints(low,{analyse(A,low,eX)..A in R})).
	 analyse(SQUARE(L,R,_),low,eX) =>
	     minPoint(analyse(L,low,eX),minPoints(low,{analyse(A,low,eX)..A in R})).
	 analyse(TPL(L,_),low,eX) => 
	     minPoints(low,{analyse(A,low,eX)..A in L}).
	 analyse(X,_,_)=> raise error("Problem in analysing dependencies of: "<>X.show(),
				      'eFAIL').
	 
	 extendExcl:[list[abstract],list[symbol]]=>list[symbol].
	 extendExcl([],eX)=>eX.
	 extendExcl([APPLY(IDEN(_,_),[IDEN(N,_),_],_),..Th],eX) =>
	     (N in eX ?
		extendExcl(Th,eX)
	    | extendExcl(Th,[N,..eX])).
	 extendExcl([_,..Th],eX) => extendExcl(Th,eX).
	 
	 -- construct a group from the top region of the stack
	 makeGroup:[integer,list[(abstract,integer,visibility,abstract)],
		    list[(abstract,visibility,abstract)]]=>
	     (list[(abstract,integer,visibility,abstract)],
	      list[(abstract,visibility,abstract)]).
	 makeGroup(point,[(NN,Lw,M,E),..S],St)::Lw>=point =>
	     makeGroup(point,S,[(NN,M,E),..St]).
	 makeGroup(_,S,St)=>(S,St).
       }).ev().
      
  private minPoint:[integer,integer]=>integer.
  minPoint(X,Y)::X=<Y => X.
  minPoint(X,0) => X.
  minPoint(_,Y) => Y.

  private minPoints:[integer,list[integer]]=>integer.
  minPoints(L,[])=>L.
  minPoints(L,[I,..M])=>minPoints(minPoint(L,I),M).
  
  showGroup:[list[(abstract,visibility,abstract)]]=>string.
  showGroup([]) => [].
  showGroup([(Nm,V,_),..L]) => V.show()<>" "<>display(Nm)<>", "<>showGroup(L).
  
  showGroups:[list[list[(abstract,visibility,abstract)]]]=>string.
  showGroups([]) => [].
  showGroups([Gp,..L]) => "{"<>showGroup(Gp)<>"}\n"<>showGroups(L).
}.
