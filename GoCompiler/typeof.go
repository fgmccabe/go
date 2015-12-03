/*
  Module to perform type inference and type check
  It also constructs an attributed tree from the original parse tree

  (c) 2004-2008 F.G. McCabe
 
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
typeof{
  import go.io.
  import go.setlib.
  import go.do.
  import abstract.
  import macro.
  import misc.
  import types.
  import errors.
  import depends.
  import findvars.
  import subtype.
  import keywords.
  import terms.
  import opts.
  import parsetype.
  import imports.

  typeOfPackage:[abstract,dict,list[compOpt]]=>pkgTree.
  typeOfPackage(BRACE(P,Defs,Lc),Env,Opts) => 
      valof{
	Pkg = packageReference(P);
	(theta,imports) = thetaEnv(Pkg,Defs,emptyDict,Env,typeOpts(true,true,Pkg,Opts));

	pType = faceType(fields({vS(Nm,K,Nd.tpe()) .. (Nm,K,Nd) in theta}));
	valis pkg(Pkg,imports,pType,{Pr..(_,_,Pr) in theta},Lc)
      }.

  packageReference:[abstract]=>symbol.

  packageReference(IDEN(Pk,_))=>Pk.
  packageReference(APPLY(IDEN('.',_),[P,IDEN(F,_)],_)) =>
      implode(explode(packageReference(P))<>"."<>explode(F)).
  packageReference(T) => raise error(T.show()<>" not a valid package name",'fail').
      

  -- Analyse the type of a function and expression
  typeOfFunction:[list[abstract],typeTree,dict,typeOpts]=>
      list[ruleTree].
  typeOfFunction(Eqns,fTp,Env,Opts)=>
      {typeOfEqn(E,fTp,findVarsInEqn(E,Env),Env,Opts)..E in Eqns}.

  typeOfEqn:[abstract,typeTree,dict,dict,typeOpts]=>ruleTree.
  typeOfEqn(APPLY(IDEN('=>',_),[APPLY(IDEN('::',_),[APPLY(_,H,_),G],_),
				R],eqLc),fTp,Q,Env,Opts) =>
      valof{
	(funType(tA,tR),tQ) = freshenQ(fTp,[]);
	sEnv = Env.pushDict(Q);
	Args = typeOfModedParams(H,tA,sEnv,Opts);
	Guard = checkGoal(G,sEnv,Opts);
	Val = typeOf(R,sEnv,Opts);

	(
	 subType(Val.tpe(),tR,sEnv)
	 onerror(
	  error(Bec,'fail') ->
	      reportError("type of returned value: "<>Val.tpe().show()<>
			  " not consistent with required type: "<>tR.show()<>
			  "\nbecause "<>Bec,eqLc)
	 ));

	( vS(_,_,tV) in tQ *>
	  ( \+tV.isvar() ?
	      reportError("Not permitted to instantiate type variable "<>
			  tV.show()<>
			  " of program type: "<>fTp.show(),eqLc)
	  )
	);

	valis eqn(Q,Args,Guard,Val,eqLc)
      }.
  typeOfEqn(APPLY(IDEN('=>',_),[APPLY(_,H,_),R],eqLc),fTp,Q,Env,Opts) =>
      valof{
	(funType(tA,tR),tQ) = freshenQ(fTp,[]);
	sEnv = Env.pushDict(Q);
	Args = typeOfModedParams(H,tA,sEnv,Opts);
	Val = typeOf(R,sEnv,Opts);

	(
	 subType(Val.tpe(),tR,sEnv)
	 onerror(
	  error(Bec,'fail') ->
	      reportError("type of returned value: "<>Val.tpe().show()<>
			  " not consistent with required type: "<>tR.show()<>
			  "\nbecause "<>Bec,eqLc)
	 ));

	( vS(_,_,tV) in tQ *>
	  ( \+tV.isvar() ?
	      reportError("Not permitted to instantiate type variable "<>
			  tV.show()<>
			  " of program type: "<>fTp.show(),eqLc)
	  )
	);

	valis eqn(Q,Args,trueGl(eqLc),Val,eqLc)
      }.

  typeOf:[abstract,dict,typeOpts]=>termTree.
    
  typeOf(VOID(_),_,_) => vde.
  typeOf(CHR(C,Lc),_,_) => chr(C,Lc).
  typeOf(SYM(Sy,Lc),_,_) => sym(Sy,Lc).
  typeOf(STR(Str,Lc),_,_) => str(Str,Lc).
  typeOf(INT(I,Lc),_,_) => intgr(I,Lc).
  typeOf(FLT(F,Lc),_,_) => flot(F,Lc).
  typeOf(IDEN('[]',Lc),_,_) => enu('[]',lsType(typeVar(vrBind('_',topType))),Lc).
  typeOf(IDEN(Nm,Lc),Env,_)::Env.isbound(Nm,varBind,iTp) => 
      case freshen(iTp).deRef() in (
       enuType(T) => enu(Nm,T,Lc)
     | T => iden(Nm,T,Lc)
      ).
  typeOf(IDEN(Nm,Lc),_,_) => 
      valof{
	reportError(explode(Nm)<>" not declared",Lc);
	valis iden(Nm,typeVar(vrBind('_',topType)),Lc)
      }.

  typeOf(APPLY(IDEN('.',_),[Rc,IDEN(Fld,_)],Lc),Env,Opts) => 
      valof{
	rcNd = typeOfObject(Rc,Env,Opts);

	( M = fieldsOf(typeInterface(rcNd.tpe().deRef(),Env));
	  (M.isbound(Fld,_,fTp) ?
	     valis dot(rcNd,Fld,freshen(fTp),Lc)
	 | reportError(explode(Fld)<>" not a field of "<>rcNd.show(),Lc) ;
	   valis dot(rcNd,Fld,typeVar(vrBind('_',topType)),Lc)
	  )
	) onerror (
	 error(Bec,'fail') -> reportError(rcNd.show()<>" not known to be "
					  "consistent with "<>explode(Fld)<>
					  "\nbecause "<>Bec,Lc);
	     valis dot(rcNd,Fld,typeVar(vrBind('_',topType)),Lc)
	)
      }.
  typeOf(APPLY(IDEN('#',_),[Pkg,IDEN(F,_)],Lc),Env,_) =>
      valof{
	( pkgName = packageReference(Pkg);
	  pTp = fieldsOf(freshen(lookupPackage(pkgName,Env)));
	  ( pTp.isbound(F,varBind,Tp) ?
	      fTp = freshen(Tp);
	      case fTp in (
	       enuType(eTp) -> valis pkrf(pkgName,enu(F,eTp,Lc),Lc)
	     | _ -> valis pkrf(pkgName,iden(F,fTp,Lc),Lc)
	      )
	  | reportError(explode(F)<>" not defined in package "<>Pkg.show(),Lc);
	    valis pkrf(pkgName,iden(F,typeVar(vrBind('_',topType)),Lc),Lc)
	  )
	) onerror (
	 error(_,'fail') -> 
	     reportError("package "<>Pkg.show()<>" not a valid package name",Lc);
	     valis pkrf(pkgName,iden(F,typeVar(vrBind('_',topType)),Lc),Lc)
       | error(_,_) ->
	     reportError("package "<>Pkg.show()<>" not imported",Lc);
	     valis pkrf(pkgName,iden(F,typeVar(vrBind('_',topType)),Lc),Lc)
	)
      }.
  typeOf(APPLY(IDEN('..',Lc),[APPLY(IDEN(':',_),[S,T],_),
			      APPLY(IDEN('{}',_),Defs,Lc1)],Lc2),Env,Opts) =>
      valof{
	Id = genNew("anon");
	(nQ,aT) = realType(T,Opts.inPkg(),Env);

	( \+nQ.empty() ?
	    reportError("new type variables: "<>nQ.show()<>" in inappropriate "
		      "context",Lc)
	);
	nCons = APPLY(IDEN(Id,Lc),[],Lc);
	subEnv = Env.push(Id,varBind,sconType([],aT));
	(Opts.option(dbgType)?
	   stdout.outLine("Computing type of anonymous class: "<>nCons.show()));
	Cls = typeOfClass(Id,[APPLY(IDEN('<=',Lc),[nCons,S],Lc),
			      APPLY(IDEN('..',Lc),[nCons,
						   APPLY(IDEN('{}',Lc),
							 Defs,Lc1)],
				    Lc2)],anOnymous,Lc,sconType([],aT),subEnv,Opts);
	valis anon(Cls,aT,Lc);
      }.
  typeOf(APPLY(IDEN('..',Lc),[APPLY(IDEN(':',_),[T],_),
			      APPLY(IDEN('{}',_),Defs,Lc1)],Lc2),Env,Opts) =>
      valof{
	Id = genNew("anon");
	(nQ,aT) = realType(T,Opts.inPkg(),Env);

	( \+nQ.empty() ?
	   reportError("new type variables: "<>nQ.show()<>" in inappropriate "
		       "context",Lc)
	| {});
	nCons = APPLY(IDEN(Id,Lc),[],Lc);
	sEnv = Env.push(Id,varBind,sconType([],aT));

	(Opts.option(dbgType)?
	   stdout.outLine("Computing type of anonymous class: "<>nCons.show()));

	Cls = typeOfClass(Id,[APPLY(IDEN('<=',Lc),[nCons,IDEN('thing',Lc)],Lc),
			      APPLY(IDEN('..',Lc),[nCons,
						   APPLY(IDEN('{}',Lc),
							 Defs,Lc1)],
				    Lc2)],anOnymous,Lc,sconType([],aT),sEnv,Opts);
	valis anon(Cls,aT,Lc);
      }.
  typeOf(APPLY(IDEN('..',Lc),[S,APPLY(IDEN('{}',_),Defs,Lc1)],Lc2),Env,Opts) =>
      valof{
	Id = genNew("anon");
	Spr = typeOf(S,Env,Opts);

	nCons = APPLY(IDEN(Id,Lc),[],Lc);
	sEnv = Env.push(Id,varBind,sconType([],Spr.tpe()));
	Cls = typeOfClass(Id,[APPLY(IDEN('<=',Lc),[nCons,IDEN('thing',Lc)],Lc),
			      APPLY(IDEN('..',Lc),[nCons,
						   APPLY(IDEN('{}',Lc),
							 Defs,Lc1)],
				    Lc2)],anOnymous,Lc,sconType([],aT),sEnv,Opts);
	valis anon(Cls,aT,Lc);
      }.
  typeOf(APPLY(IDEN('|',Lc),[APPLY(IDEN('?',_),[G,A],_),B],_),Env,Opts) =>
      valof{
	a = typeOf(A,Env,Opts);
	b = typeOf(B,Env,Opts);

	( valis ifEx(checkGoal(G,Env,Opts),a,b,lub(a.tpe(),b.tpe(),Env),Lc)
	  onerror (
	   error(Bec,_) ->
	       reportError("type mismatch between arms of conditional"
			   "\nbecause "<>Bec,Lc);
	       valis vde
	  )
	)
      }.
  typeOf(APPLY(IDEN('case',Lc),[APPLY(IDEN('in',_),[Exp,Cases],_)],_),Env,Opts) =>
      valof{
	Ex = typeOf(Exp,Env,Opts);
	Tp = typeVar(vrBind('_',topType));
	valis csEx(Ex,eqnCases(listIfy(Cases,'|'),Tp,Ex.tpe(),Env,Opts,rT),rT,Lc)
      }.
  typeOf(APPLY(IDEN('{}',Lc),[APPLY(IDEN('..',_),[E,B],_)],_),Env,Opts) =>
      valof{
	Q = findVarsInRule(E,VOID(Lc),B,Env);

	sEnv = Env.pushDict(Q);
	body = { 
	  valof{
	    bseList = typeOf(S,Env,Opts);	-- collect the stream
	    Ptn = typeOf(P,sEnv,Opts);
	    ( 
	     sameType(bseList.tpe(),lsType(Ptn.tpe()),Env)
	     onerror (
	      error(Bec,'fail') ->
		  reportError("mismatch in type of pattern: "<>Ptn.show()<>
			      " and base list "<>bseList.show()<>"\nbecause "<>
			      Bec,Lc)
	     )
	    );
	    valis (Ptn,bseList)
	  } .. APPLY(IDEN('in',_),[P,S],_) in listIfy(B,',') };
	valis bnd(Q,typeOf(E,sEnv,Opts),body,Lc)
      }.

  typeOf(APPLY(IDEN('{}',Lc),[APPLY(IDEN('||',_),[E,G],_)],_),Env,Opts) =>
      valof{
	Q = findVarsInRule(E,G,VOID(Lc),Env);

	sEnv = Env.pushDict(Q);
	valis bag(Q,typeOf(E,sEnv,Opts),checkGoal(G,sEnv,Opts),Lc)
      }.
  typeOf(APPLY(IDEN('-',Lc),[INT(I,_)],_),_,_) => intgr(-I,Lc).

  typeOf(APPLY(IDEN('-',Lc),[FLT(F,_)],_),_,_) => flot(-F,Lc).

  typeOf(APPLY(IDEN('-',Lc),[N],_),Env,Opts) =>
      valof{
	nN = typeOf(N,Env,Opts);
	nTp = nN.tpe();

	( subType(nTp,uType('go.stdlib','number',[]),Env)
	  onerror (
	   error(_,_) ->
	       reportError(nN.show()<>" should be a number",Lc)
	  )
	);
	valis app(iden('-',funType([(nTp,inpMode),(nTp,inpMode)],nTp),Lc),
		  [intgr(0,Lc),nN],nTp,Lc)
      }.
  typeOf(APPLY(IDEN(':',Lc),[I,T],_),Env,Opts)=>
      valof{
	iD = typeOf(I,Env,Opts);
	(nQ,rT) = realType(T,Opts.inPkg(),Env);
	( \+nQ.empty() ?
	    reportError("new type variables: "<>nQ.show()<>" in inappropriate "
			"context",Lc)
	);
	( sameType(iD.tpe(),rT,Env)
	  onerror(
	   error(Bec,_) ->
		 reportError("type of "<>iD.show()<>" not consistent with "
			     "asserted type: "<>rT.show()<>"\nbecause "<>Bec,Lc)
	  )
	);
	valis iD
      }.
  typeOf(APPLY(IDEN('::',Lc),[E,G],_),Env,Opts) =>
      valof{
	Exp = typeOf(E,Env,Opts);
	valis grd(Exp,checkGoal(G,Env,Opts),Lc)
      }.
  typeOf(APPLY(IDEN('@',Lc),[A,APPLY(IDEN(B,Lca),bA,Lcb)],Lcc),Env,Opts) =>
      typeOf(APPLY(IDEN('::',Lc),[A,APPLY(APPLY(IDEN('.',Lc),[A,IDEN(B,Lca)],Lcb),
					  bA,Lcc)],Lc),Env,Opts).
  typeOf(APPLY(IDEN('@@',Lc),[L,G],_),Env,Opts) =>
      lzy(typeOf(L,Env,Opts),checkGoal(G,Env,Opts),Lc).
  typeOf(APPLY(IDEN('%%',Lc),[G,APPLY(IDEN('~',_),[L,R],_)],_),Env,Opts) =>
      valof{
	g = typeOf(G,Env,Opts);
	lft = typeOf(L,Env,Opts);
	rgt = typeOf(R,Env,Opts);
	aT = typeVar(vrBind('_',topType)); 	-- constrain the type variable
	( g.tpe() = gramType([(cA,cMode)],fT)?
	    ( case cMode in (
	       inpMode -> subType(aT,cA,Env)
	     | superMode -> subType(aT,cA,Env)
	     | outMode -> sameType(aT,cA,Env)
	     | biMode -> sameType(aT,cA,Env)
	      );
	      ( sameType(lft.tpe(),fT,Env)
		onerror(
		 error(Bec,_) ->
		     reportError("Stream "<>lft.show()<>
				 " not consistent with "<>g.show()<>
				 "\nbecause "<>Bec,Lc)
		)
	      );
	      
	      ( sameType(rgt.tpe(),fT,Env)
		onerror(
		 error(Bec,_) ->
		     reportError("Stream "<>rgt.show()<>
				 " not consistent with "<>g.show()<>
				 "\nbecause "<>Bec,Lc)
		)
	      );
	      valis ntEx(g,aT,lft,rgt,Lc)
	    )
	| reportError(g.show()<>" is not a single-argument grammar",Lc);
	      valis vde
	)
      }.
  typeOf(APPLY(IDEN('%%',Lc),[G,R],_),Env,Opts) =>
      valof{
	g = typeOf(G,Env,Opts);
	rgt = typeOf(R,Env,Opts);
	aT = typeVar(vrBind('_',topType));
	case g.tpe() in (
	 gramType([(cA,cMode)],fT) ->
	     case cMode in (		-- constrain the type variable
	      inpMode -> subType(aT,cA,Env)
	    | superMode -> subType(aT,cA,Env)
	    | outMode -> sameType(aT,cA,Env)
	    | biMode -> sameType(aT,cA,Env)
	     );
	     ( sameType(rgt.tpe(),fT,Env)
	       onerror(
		error(Bec,_) ->
		    reportError("Stream "<>rgt.show()<>
				" not consistent with "<>g.show()<>
				"\nbecause "<>Bec,Lc)
	       )
	     );

	     valis ntEx(g,aT,rgt,vde,Lc)
       | _ -> reportError(g.show()<>" is not a single-argument grammar",Lc);
	     valis vde
	)
      }.

  typeOf(BRACE(IDEN('spawn',Lc),[G],_),Env,Opts) =>
      valof{
	Q = findVarsInRule(VOID(Lc),VOID(Lc),G,Env);
	valis spwn(Q,checkAction(G,voidType,Env.pushDict(Q),Opts),Lc)
      }.
  typeOf(BRACE(IDEN('valof',Lc),[G],_),Env,Opts) =>
      valof{
	Q = findVarsInRule(VOID(Lc),VOID(Lc),G,Env);
	vT = typeVar(vrBind('_',topType));
	valis vlof(Q,checkAction(G,vT,Env.pushDict(Q),Opts),vT,Lc)
      }.
  typeOf(APPLY(IDEN('onerror',_),[A,B],Lc),Env,Opts) =>
      valof{
	lft = typeOf(A,Env,Opts);

	errProg = eqnCases(listIfy(B,'|'),lft.tpe(),
			   uType('go.stdlib','exception',[]),Env,Opts,oT);
	( sameType(lft.tpe(),oT,Env)
	  onerror (
	   error(Bec,'fail') ->
	       reportError("type of caught expression: "<>lft.show()<>
			   " not consistent with error handler"
			   "\nbecause "<>Bec,Lc)
	  )
	);
	valis errEx(lft,errProg,Lc)
      }.
  typeOf(APPLY(IDEN('raise',_),[E],Lc),Env,Opts) =>
      valof{
	lft = typeOf(E,Env,Opts);
	( subType(lft.tpe(),uType('*','exception',[]),Env);
	  valis exc(lft,Lc)
	)
	onerror(
	 error(Bec,'eFAIL') ->
	     reportError("exception expression: "<>
			 lft.show()<>":"<>lft.tpe().show()<>
			 " not consistent with exception type\nbecause "<>Bec,Lc);
	     valis vde
	)
      }.
  typeOf(APPLY(F,A,Lc),Env,Opts) => 
      valof{
	f = typeOf(F,Env,Opts);
	case f.tpe().deRef() in (
	 funType(cA,fT) -> 
	     ( listlen(cA)==listlen(A) ?
		 valis app(f,typeOfFunArgs(A,cA,Env,Opts),fT,Lc)
	     | reportError("number of arguments "<>listlen(A).show()<>
			   " supplied to call to "<>f.show()<>
			   " differnt to that expected: "<>listlen(cA).show(),Lc);
	       valis vde
	     )
       | conType(cA,fT) ->
	     ( listlen(cA)==listlen(A) ?
		 (F = IDEN(Nm,_);
		  valis con(Nm,typeOfArgs(A,cA,Env,Opts),fT,Lc))
	     | reportError("number of arguments "<>listlen(A).show()<>
			   " supplied to constructor "<>f.show()<>
			   " differnt to that expected: "<>listlen(cA).show(),Lc);
	       valis vde
	     )
       | sconType(cA,fT) -> 
	     ( listlen(cA)==listlen(A) ?
		 (F = IDEN(Nm,_);
		  valis scon(Nm,typeOfFunArgs(A,cA,Env,Opts),fT,Lc))
	     | reportError("number of arguments "<>listlen(A).show()<>
			   " supplied to call to "<>f.show()<>
			   " differnt to that expected: "<>listlen(cA).show(),Lc);
	       valis vde
	     )
       | _ -> 
	     reportError(f.show()<>" is not a function or constructor, its type is "<>
			 f.tpe().show(),Lc);
	     valis vde
	)
      }.
  typeOf(X,_,_) =>
      valof{
	reportError("cannot give type to expression: "<>X.show(),X.loc());
	valis vde
      }.

  typeCheck <~ { check:[typeTree,fileLoc]* }.

  typeOfPtn:[abstract,dict,typeOpts,typeCheck]=>termTree.
    
  typeOfPtn(VOID(_),_,_,_) => vde.
  typeOfPtn(CHR(C,Lc),_,_,Chk) =>
      valof{
	Ptn = chr(C,Lc);
	Chk.check(Ptn.tpe().deRef(),Lc);
	valis Ptn
      }.

  typeOfPtn(SYM(Sy,Lc),_,_,Chk) =>
      valof{
	Ptn = sym(Sy,Lc);
	Chk.check(Ptn.tpe().deRef(),Lc);
	valis Ptn
      }.
  typeOfPtn(STR(Str,Lc),_,_,Chk) =>
      valof{
	Ptn = str(Str,Lc);
	Chk.check(Ptn.tpe().deRef(),Lc);
	valis Ptn
      }.

  typeOfPtn(INT(I,Lc),_,_,Chk) =>
      valof{
	Ptn = intgr(I,Lc);
	Chk.check(Ptn.tpe().deRef(),Lc);
	valis Ptn
      }.

  typeOfPtn(FLT(F,Lc),_,_,Chk) =>
      valof{
	Ptn = flot(F,Lc);
	Chk.check(Ptn.tpe().deRef(),Lc);
	valis Ptn
      }.

  typeOfPtn(IDEN('[]',Lc),_,_,Chk) =>
      valof{
	Ptn = enu('[]',lsType(typeVar(vrBind('_',topType))),Lc);
	Chk.check(Ptn.tpe().deRef(),Lc);
	valis Ptn
      }.

  typeOfPtn(IDEN(Nm,Lc),Env,_,Chk)::Env.isbound(Nm,varBind,iTp) => 
      valof{
	Ptn = ( case freshen(iTp).deRef() in (
		 enuType(T) => enu(Nm,T,Lc)
	       | T => iden(Nm,T,Lc)
		));
	Chk.check(Ptn.tpe().deRef(),Lc);
	valis Ptn
      }.

  typeOfPtn(IDEN(Nm,Lc),_,_,_) => 
      valof{
	reportError(explode(Nm)<>" not declared",Lc);
	valis iden(Nm,typeVar(vrBind('_',topType)),Lc)
      }.

  typeOfPtn(APPLY(IDEN('#',_),_,Lc),_,_,_) =>
      valof{
	reportError("package references not permitted as patterns",Lc);
	valis vde
      }.

  typeOfPtn(APPLY(IDEN(':',Lc),[I,T],_),Env,Opts,Chk)=>
      valof{
	iD = typeOfPtn(I,Env,Opts,Chk);
	(nQ,rT) = realType(T,Opts.inPkg(),Env);
	( \+nQ.empty() ?
	    reportError("new type variables: "<>nQ.show()<>" in inappropriate "
			"context",Lc)
	);
	( sameType(iD.tpe(),rT,Env)
	  onerror(
	   error(Bec,_) ->
		 reportError("type of "<>iD.show()<>" not consistent with "
			     "asserted type: "<>rT.show()<>"\nbecause "<>Bec,Lc)
	  )
	);
	valis iD
      }.
  typeOfPtn(APPLY(IDEN('::',Lc),[E,G],_),Env,Opts,Chk) =>
      valof{
	Exp = typeOfPtn(E,Env,Opts,Chk);
	valis grd(Exp,checkGoal(G,Env,Opts),Lc)
      }.
  typeOfPtn(APPLY(IDEN('@',Lc),[A,APPLY(IDEN(B,Lca),bA,Lcb)],Lcc),Env,Opts,Chk) =>
      typeOfPtn(APPLY(IDEN('::',Lc),[A,APPLY(APPLY(IDEN('.',Lc),[A,IDEN(B,Lca)],Lcb),
					     bA,Lcc)],Lc),Env,Opts,Chk).
  typeOfPtn(APPLY(IDEN('@@',Lc),[L,G],_),Env,Opts,Chk) =>
      lzy(typeOfPtn(L,Env,Opts,Chk),checkGoal(G,Env,Opts),Lc).

  typeOfPtn(APPLY(F,A,Lc),Env,Opts,Chk) => 
      valof{
	f = typeOf(F,Env,Opts);
	case f.tpe().deRef() in (
	 conType(cA,fT) ->
	     ( listlen(cA)==listlen(A) ?
		 F = IDEN(Nm,_);
		 Tp = con(Nm,typeOfModedParams(A,{(aT,biMode)..aT in cA},Env,Opts),fT,Lc);
		 Chk.check(Tp.tpe().deRef(),Lc)
	     | reportError("number of arguments "<>listlen(A).show()<>
			   " supplied to constructor "<>f.show()<>
			   " differnt to that expected: "<>listlen(cA).show(),Lc);
	       valis vde
	     )
       | _ -> 
	     reportError(f.show()<>" not permitted as a pattern"<>f.tpe().show(),Lc);
	     valis vde
	)
      }.
  typeOfPtn(X,_,_,_) =>
      valof{
	reportError("cannot give type to pattern: "<>X.show(),X.loc());
	valis vde
      }.


  lsType:[typeTree]=>typeTree.
  lsType(T)=>uType('go.stdlib','list',[T]).

  eqnCases:[list[abstract],typeTree,typeTree,dict,typeOpts,typeTree-]=> list[ruleTree].
  eqnCases([],T,_,_,_,T)=>[].
  eqnCases([APPLY(IDEN('=>',_),[P,E],eLc),..Cases],iT,pT,Env,Opts,oT) =>
      valof{
	Q = findVarsInRule(APPLY(P,[E],eLc),VOID(eLc),VOID(eLc),Env);
	sEnv = Env.pushDict(Q);
	eVal = typeOf(E,sEnv,Opts);
	ePtn = typeOfPtn(P,sEnv,Opts,argChecker(pT,Env,Opts,biMode));

	( ot = lub(eVal.tpe(),iT,Env);
	  ( grd(Pt,Gr,_)=ePtn ?
	      valis [eqn(Q,[Pt],Gr,eVal,eLc),..
		     eqnCases(Cases,ot,pT,Env,Opts,oT)]
	  | valis [eqn(Q,[ePtn],trueGl(eLc),eVal,eLc),..
		   eqnCases(Cases,ot,pT,Env,Opts,oT)]
	  )
	  onerror(
	   error(Bec,'eFAIL') ->
	       reportError("type returned by case rule "<>eVal.show()<>
			   ":"<>eVal.tpe().show()<>" not consistent with "
			   "expected type: "<>iT.show()<>"\nbecause "<>Bec,eLc);
	       valis eqnCases(Cases,iT,pT,Env,Opts,oT)
	  )
	)
      }.

  typeOfObject:[abstract,dict,typeOpts]=>termTree.
  typeOfObject(IDEN(Nm,Lc),Env,_)::Env.isbound(Nm,inheritBind,iTp) =>
      case iTp.deRef() in (
       enuType(T) => enu(Nm,T,Lc)
     | T => iden(Nm,T,Lc)
      ).
  typeOfObject(O,Env,Opts) => typeOf(O,Env,Opts).

  private argChecker:[typeTree,dict,typeOpts,flowMode]@>typeCheck.
  argChecker(Tp,Env,Opts,Md)..{
    check(sconType(cA,_),Lc) ->
	reportError(listlen(cA).show()<>" constructor,"<>
		    "\nnot allowed as an argument",Lc).
    check(fT,Lc)::isProgramType(fT) ->
	reportError("program name not allowed as an argument",Lc).
    check(T,Lc) -> 
	( case Md in (
	   inpMode ->
	       subType(T,Tp,Env)
	 | superMode ->
	       subType(T,Tp,Env)
	 | outMode ->
	       sameType(T,Tp,Env)
	 | biMode ->
	       sameType(T,Tp,Env)
	  )
	  onerror(
	   error(Bec,'fail') ->
	       reportError("Argument inconsistent with expected type: "<>Tp.show()<>
			   "\nbecause "<>Bec,Lc)
	  )
	).
  }.

  typeOfArgs:[list[abstract],list[typeTree],dict,typeOpts]=>list[termTree].
  typeOfArgs([],[],_,_)=>[].
  typeOfArgs([A,..rgs],[T,..ps],Env,Opts) =>
      [typeOf(A,Env,Opts),..typeOfArgs(rgs,ps,Env,Opts)].

  typeOfFunArgs:[list[abstract],list[(typeTree,flowMode)], dict,typeOpts]=>list[termTree].
  typeOfFunArgs([],_,_,_)=>[].
  typeOfFunArgs([A,..rgs],[(aT,aMd),..aTypes],Env,Opts) =>
      [typeOfPtn(A,Env,Opts,argChecker(aT,Env,Opts,aMd))
       ,..typeOfFunArgs(rgs,aTypes,Env,Opts)].

  private paramChecker:[typeTree,dict,typeOpts,flowMode]@>typeCheck.
  paramChecker(Tp,Env,Opts,Md)..{
    check(sconType(_,_),Lc) ->
	reportError(" constructor not allowed as an argument",Lc).
    check(fT,Lc)::isProgramType(fT) ->
	reportError("program not allowed as an argument",Lc).
    check(T,Lc) -> 
	( case Md in (
	   inpMode ->
	       sameType(T,Tp,Env)
	 | superMode ->
	       sameType(T,Tp,Env)
	 | outMode ->
	       subType(T,Tp,Env)
	 | biMode ->
	       sameType(T,Tp,Env)
	  )
	  onerror(
	   error(Bec,'fail') ->
	       reportError("Argument inconsistent with expected type: "<>
			   Tp.show()<>"\nbecause "<>Bec,Lc)
	  )
	).
  }.

  typeOfModedParams:[list[abstract],list[(typeTree,flowMode)], dict,typeOpts]=>list[termTree].
  typeOfModedParams([],_,_,_)=>[].
  typeOfModedParams([A,..rgs],[(aT,aMd),..aTypes],Env,Opts) =>
      [typeOfPtn(A,Env,Opts,paramChecker(aT,Env,Opts,aMd)),..
       typeOfModedParams(rgs,aTypes,Env,Opts)].

  -- Analyse the type of a relation and relational query
  typeOfPredicate:[list[abstract],typeTree,dict,typeOpts]=>
      list[ruleTree].
  typeOfPredicate(Clses,fTp,Env,Opts)=>
      {typeOfClse(E,fTp,findVarsInClause(E,Env),Env,Opts)..E in Clses}.

  typeOfClse:[abstract,typeTree,dict,dict,typeOpts]=>ruleTree.
  typeOfClse(APPLY(IDEN(':-',_),[APPLY(_,H,_),G],clLc),clTp,Q,Env,Opts) =>
      valof{
	(predType(tA),tQ) = freshenQ(clTp,[]);
	(Opts.option(dbgType)?
	   stdout.outLine("predicate type is "<>predType(tA).show());
	   stdout.outLine("checking parameters: "<>H.show()));

	sEnv = Env.pushDict(Q);
	Args = typeOfModedParams(H,tA,sEnv,Opts);
	(Opts.option(dbgType)?
	   stdout.outLine("local vars types = "<>Q.show());
	   stdout.outLine("checking goal: "<>G.show()));

	Goal = checkGoal(G,sEnv,Opts);

	( vS(_,_,tV) in tQ *>
	  (\+tV.isvar() ?
	      reportError("Not permitted to instantiate type variable "<>
			  tV.show()<>
			 " of program type: "<>clTp.show(),clLc)
	  )
	);

	(Opts.option(dbgType)?
	   stdout.outLine("type checked clause"));
	valis cls(Q,Args,Goal,clLc)
      }.

  typeOfStrongPred:[list[abstract],typeTree,dict,typeOpts]=> list[ruleTree].
  typeOfStrongPred(Clses,fTp,Env,Opts)=>
      {typeOfStrongClse(E,fTp,findVarsInClause(E,Env),Env,Opts)..E in Clses}.

  typeOfStrongClse:[abstract,typeTree,dict,dict,typeOpts]=>ruleTree.
  typeOfStrongClse(APPLY(IDEN(':--',_),[APPLY(IDEN('::',_),[APPLY(_,H,_),G],_),
					Gl],clLc),clTp,Q,Env,Opts) =>
      valof{
	(predType(tA),tQ) = freshenQ(clTp,[]);
	sEnv = Env.pushDict(Q);
	Args = typeOfModedParams(H,tA,sEnv,Opts);
	Guard = checkGoal(G,sEnv,Opts);
	Goal = checkGoal(Gl,sEnv,Opts);

	( vS(_,_,tV) in tQ *>
	  ( \+tV.isvar() ?
	      reportError("Not permitted to instantiate type variable "<>
			  tV.show()<>
			  " of program type: "<>clTp.show(),clLc)
	  )
	);

	valis scls(Q,Args,Guard,Goal,clLc)
      }.
  typeOfStrongClse(APPLY(IDEN(':--',_),[APPLY(_,H,_),
					Gl],clLc),clTp,Q,Env,Opts) =>
      valof{
	(predType(tA),tQ) = freshenQ(clTp,[]);
	sEnv = Env.pushDict(Q);
	Args = typeOfModedParams(H,tA,sEnv,Opts);
	Goal = checkGoal(Gl,sEnv,Opts);
	
	( vS(_,_,tV) in tQ *>
	  ( \+tV.isvar() ?
	      reportError("Not permitted to instantiate type variable "<>
			  tV.show()<>
			  " of program type: "<>clTp.show(),clLc)
	  )
	);

	valis scls(Q,Args,trueGl(clLc),Goal,clLc)
      }.



  checkGoal:[abstract,dict,typeOpts]=>goalTree.
  checkGoal(APPLY(IDEN(',',_),[L,R],gLc),Env,Opts) =>
      cnjGl({checkGoal(G,Env,Opts)..G in listIfy(L,',')<>listIfy(R,',')},gLc).
  checkGoal(IDEN('true',Lc),_,_)=>trueGl(Lc).
  checkGoal(IDEN('false',Lc),_,_)=>falseGl(Lc).
  checkGoal(IDEN(Nm,Lc),Env,Opts) =>
      checkGoal(APPLY(IDEN('=',Lc),[IDEN(Nm,Lc),IDEN('true',Lc)],Lc),Env,Opts).
  checkGoal(APPLY(IDEN('=',_),[L,R],gLc),Env,Opts) =>
      valof{
	lhs = typeOf(L,Env,Opts);
	rhs = typeOf(R,Env,Opts);
	(
	 sameType(lhs.tpe(),rhs.tpe(),Env)
	 onerror(
	  error(Bec,'fail') ->
	      reportError("type of "<>lhs.showTerm()<>" not "
			  "consistent with type of "<>rhs.showTerm()<>
			  "\nbecause "<>Bec,gLc)
	 )
	);
	valis eqGl(lhs,rhs,gLc)
      }.
  checkGoal(APPLY(IDEN('.=',_),[L,R],gLc),Env,Opts) =>
      valof{
	lhs = typeOf(L,Env,Opts);
	(Opts.option(dbgType)?
	   stdout.outLine("lhs of .= "<>lhs.show()));
	rhs = typeOf(R,Env,Opts);
	(Opts.option(dbgType)?
	   stdout.outLine("rhs of .= "<>rhs.show()));
	(
	 sameType(lhs.tpe(),rhs.tpe(),Env)
	 onerror(
	  error(Bec,'fail') ->
	      reportError("type of "<>lhs.showTerm()<>" not "
			  "consistent with type of "<>rhs.showTerm()<>
			  "\nbecause "<>Bec,gLc)
	 )
	);
	valis mtcGl(lhs,rhs,gLc)
      }.
  checkGoal(APPLY(IDEN('<=',_),[L,R],gLc),Env,Opts) =>
      valof{
	lhs = typeOf(L,Env,Opts);
	rhs = typeOf(R,Env,Opts);
	(
	 subType(lhs.tpe(),rhs.tpe(),Env)
	 onerror(
	  error(Bec,'fail') ->
	      reportError("type of "<>lhs.showTerm()<>" not "
			  "consistent with type of "<>rhs.showTerm()<>
			  "\nbecause "<>Bec,gLc)
	 )
	);
	valis lblGl(lhs,rhs,gLc)
      }.
  checkGoal(APPLY(IDEN('in',_),[L,R],gLc),Env,Opts) =>
      valof{
	rhs = typeOf(R,Env,Opts);
	(Opts.option(dbgType)?
	   stdout.outLine("rhs of in: "<>rhs.show()));

	lhs = typeOf(L,Env,Opts);
	(
	 sameType(uType('go.stdlib','list',[lhs.tpe()]),rhs.tpe(),Env)
	 onerror(
	  error(Bec,'fail') ->
	      reportError("type of "<>lhs.showTerm()<>" not "
			  "consistent with type of "<>rhs.showTerm()<>
			  "\nbecause "<>Bec,gLc)
	 )
	);
	valis inGl(lhs,rhs,gLc)
      }.
  checkGoal(APPLY(IDEN('@@',_),[L,R],gLc),Env,Opts) =>
      lzyGl(typeOf(L,Env,Opts),checkGoal(R,Env,Opts),gLc).
  checkGoal(APPLY(IDEN('|',_),[APPLY(IDEN('?',_),[T,A],_),B],glLc),Env,Opts) =>
      valof{
	tGoal = checkGoal(T,Env,Opts);
	aGoal = checkGoal(A,Env,Opts);
	
	valis ifGl(tGoal,aGoal, checkGoal(B,Env,Opts),glLc)
      }.
  checkGoal(APPLY(IDEN('|',_),[A,B],glLc),Env,Opts) =>
      dsjGl(checkGoal(A,Env,Opts),checkGoal(B,Env,Opts),glLc).
  checkGoal(APPLY(IDEN('*>',_),[A,B],glLc),Env,Opts) =>
      frllGl(checkGoal(A,Env,Opts),checkGoal(B,Env,Opts),glLc).
  checkGoal(APPLY(IDEN('!',_),[A],glLc),Env,Opts) =>
      oneGl(checkGoal(A,Env,Opts),glLc).
  checkGoal(APPLY(IDEN('\\+',_),[A],glLc),Env,Opts) =>
      negGl(checkGoal(A,Env,Opts),glLc).
  checkGoal(APPLY(IDEN('onerror',_),[A,B],glLc),Env,Opts) =>
      valof{
	lft = checkGoal(A,Env,Opts);

	errProg = clsCases(listIfy(B,'|'),uType('go.stdlib','exception',[]),
			   Env,Opts);
	valis errGl(lft,errProg,glLc)
      }.
  checkGoal(APPLY(IDEN('raise',_),[E],gLc),Env,Opts) =>
      valof{
	Ex = typeOf(E,Env,Opts);
	( subType(Ex.tpe(),uType('*','exception',[]),Env)
	  onerror (
	   error(Bec,'fail') ->
	       reportError("Expression: "<>Ex.showTerm()<>" not consistent "
			   "with required type: exception"
			   "\nbecause "<>Bec,gLc)
	  )
	);
	valis exGl(Ex,gLc)
      }.
  checkGoal(APPLY(IDEN('-->',_),[L,APPLY(IDEN('~',_),[S,R],_)],glLc),Env,Opts) =>
      valof{
	stm = typeOf(S,Env,Opts);
	rgt = typeOf(R,Env,Opts);

	( subType(stm.tpe(),uType('go.stdlib','list',[topType]),Env)
	  onerror(
	   error(Bec,'fail') ->
	       reportError("parsed stream: "<>stm.showTerm()<>
			   " not consistent with the list type "
			   "\nbecause "<>Bec,stm.loc())
	  )
	);
	( sameType(stm.tpe(),rgt.tpe(),Env)
	  onerror(
	   error(Bec,'fail') ->
	       reportError("parsed stream: "<>stm.showTerm()<>
			   " not consistent with remainder stream type "<>
			   rgt.showTerm()<>
			   "\nbecause "<>Bec,stm.loc())
	  )
	);

	lft = grammarBody(L,stm.tpe(),Env,Opts);
	valis ntGl(lft,stm,rgt,glLc)
      }.
  checkGoal(BRACE(IDEN('action',_),[G],Lc),Env,Opts) =>
      actGl(checkAction(G,uType('go.stdlib','logical',[]),Env,Opts),Lc).
  checkGoal(APPLY(P,A,glLc),Env,Opts) =>
      valof{
	p = typeOf(P,Env,Opts);
	case p.tpe() in (
	 predType(cA) ->
	     ( listlen(cA)!=listlen(A) ?
		 reportError("number of arguments "<>listlen(A).show()<>
			     " not consistent with expected arity: "<>
			     listlen(cA).show(),glLc)
	     );
	     ags = typeOfFunArgs(A,cA,Env,Opts);
	       
	     valis prdGl(p,ags,glLc)
	| _ ->
	     reportError("non-predicate: "<>p.showTerm()<>
			 " in relation condition: "<>APPLY(P,A,glLc).show(),glLc);
	     valis falseGl(glLc)
	)
      }.
  

  clsCases:[list[abstract],typeTree,dict,typeOpts]=> list[ruleTree].
  clsCases([],_,_,_)=>[].
  clsCases([APPLY(IDEN(':-',_),[P,E],eLc),..Cases],pT,Env,Opts) =>
      valof{
	Q = findVarsInRule(APPLY(P,[E],eLc),VOID(eLc),VOID(eLc),Env);
	sEnv = Env.pushDict(Q);
	eGoal = checkGoal(E,sEnv,Opts);
	ePtn = typeOf(P,sEnv,Opts);

	( sameType(ePtn.tpe(),pT,Env);
	  ( grd(Pt,Gr,_)=ePtn ?
	      valis [scls(Q,[Pt],Gr,eGoal,eLc),..
		     clsCases(Cases,pT,Env,Opts)]
	  | valis [scls(Q,[ePtn],trueGl(eLc),eGoal,eLc),..
		   clsCases(Cases,pT,Env,Opts)]
	  )
	  onerror(
	     error(Bec,'fail') ->
	       reportError("type of: "<>ePtn.showTerm()<>
			   " not consistent with "<>pT.show()<>
			   "\nbecause "<>Bec,eLc);
	       valis clsCases(Cases,pT,Env,Opts)
	  )
	)
      }.

  -- Analyse the type of an action procedure and action
  typeOfProcedure:[list[abstract],typeTree,dict,typeOpts]=>list[ruleTree].
  typeOfProcedure(Rules,aTp,Env,Opts)=>
      {typeOfActRule(E,aTp,findVarsInActionRule(E,Env),Env,Opts)..E in Rules}.

  typeOfActRule:[abstract,typeTree,dict,dict,typeOpts]=>ruleTree.
  typeOfActRule(APPLY(IDEN('->',_),[APPLY(IDEN('::',_),
					  [APPLY(_,H,_),G],_),A],clLc),
		aTp,Q,Env,Opts) =>
      valof{
	(actType(tA),tQ) = freshenQ(aTp,[]);
	sEnv = Env.pushDict(Q);
	Args = typeOfModedParams(H,tA,sEnv,Opts);
	Goal = checkGoal(G,sEnv,Opts);
	Body = checkAction(A,aTp,sEnv,Opts);

	( vS(_,_,tV) in tQ *>
	  ( \+tV.isvar() ?
	      reportError("Not permitted to instantiate type variable "<>
			  tV.show()<>
			  " of program type: "<>aTp.show(),clLc)
	  )
	);

	valis acRl(Q,Args,Goal,Body,clLc)
      }.
  typeOfActRule(APPLY(IDEN('->',_),[APPLY(_,H,_),A],clLc),aTp,Q,Env,Opts) =>
      valof{
	(actType(tA),tQ) = freshenQ(aTp,[]);

	sEnv = Env.pushDict(Q);
	Args = typeOfModedParams(H,tA,sEnv,Opts);
	Body = checkAction(A,voidType,sEnv,Opts);

	( vS(_,_,tV) in tQ *>
	  ( \+tV.isvar() ?
	      reportError("Not permitted to instantiate type variable "<>
			  tV.show()<>
			  " of program type: "<>aTp.show(),clLc)
	  )
	);
	valis acRl(Q,Args,trueGl(clLc),Body,clLc)
      }.

  checkAction:[abstract,typeTree,dict,typeOpts]=>actTree.
  checkAction(IDEN('{}',Lc),_,_,_)=>noopAc(Lc).
  checkAction(APPLY(IDEN(';',_),[A,B],aLc),eTp,Env,Opts) =>
      seqAc({checkAction(S,eTp,Env,Opts)..S in listIfy(A,';')<>listIfy(B,';')},aLc).
  checkAction(APPLY(IDEN(';',_),[A],_),eTp,Env,Opts) =>
      checkAction(A,eTp,Env,Opts).
  checkAction(APPLY(IDEN('!',_),[G],aLc),_,Env,Opts) =>
      glAc(checkGoal(G,Env,Opts),aLc).
  checkAction(APPLY(IDEN('{}',_),[G],Lc),_,Env,Opts) =>
      glAc(checkGoal(G,Env,Opts),Lc).
  checkAction(APPLY(IDEN('\\+',nLc),[G],aLc),_,Env,Opts) =>
      glAc(checkGoal(APPLY(IDEN('\\+',nLc),[G],aLc),Env,Opts),aLc).
  checkAction(APPLY(IDEN('=',eLc),A,aLc),_,Env,Opts) =>
      glAc(checkGoal(APPLY(IDEN('=',eLc),A,aLc),Env,Opts),aLc).
  checkAction(APPLY(IDEN('.=',eLc),A,aLc),_,Env,Opts) =>
      glAc(checkGoal(APPLY(IDEN('.=',eLc),A,aLc),Env,Opts),aLc).
  checkAction(APPLY(IDEN(':=',_),[IDEN(Nm,iLc),Vl],aLc),_,Env,Opts) =>
      valof{
	Vr = typeOf(IDEN(Nm,iLc),Env,Opts);
	Val = typeOf(Vl,Env,Opts);
	(
	 subType(Val.tpe(),Vr.tpe(),Env)
	 onerror(
	  error(Bec,'fail') ->
	      reportError("expression: "<>Val.showTerm()<>
			  " not consistent with "<>Vr.showTerm()<>
			  "\nbecause "<>Bec,aLc)
	 )
	);
	valis asgAc(Nm,Val,aLc)
      }.
  checkAction(APPLY(IDEN('valis',_),_,aLc),voidType,_,_) =>
      valof{
	reportError("valis action not permitted here",aLc);
	valis glAc(trueGl(aLc),aLc)
      }.
  checkAction(APPLY(IDEN('valis',_),[E],aLc),vTp,Env,Opts) =>
      valof{
	Val = typeOf(E,Env,Opts);
	( subType(Val.tpe(),vTp,Env)
	  onerror(
	   error(Bec,'fail') ->
	       reportError("expression: "<>Val.showTerm()<>
			   " not consistent with "<>vTp.show()<>
			  "\nbecause "<>Bec,aLc)
	  )
	);
	valis vlisAc(Val,aLc)
      }.
  checkAction(APPLY(IDEN('istrue',_),_,aLc),voidType,_,_) =>
      valof{
	reportError("istrue action not permitted here",aLc);
	valis glAc(trueGl(aLc),aLc)
      }.
  checkAction(APPLY(IDEN('istrue',_),[G],aLc),vTp,Env,Opts) =>
      valof{
	Gl = checkGoal(G,Env,Opts);
	( subType(uType('go.stdlib','logical',[]),vTp,Env)
	  onerror(
	   error(Bec,'fail') ->
	       reportError(Gl.show()<>
			   " not consistent with "<>vTp.show()<>
			   "\nbecause "<>Bec,aLc)
	  )
	);
	valis istrueAc(Gl,aLc)
      }.
  checkAction(APPLY(IDEN('case',_),[APPLY(IDEN('in',_),[E,C],_)],Lc),Tp,Env,Opts) =>
      valof{
	Ex = typeOf(E,Env,Opts);
	Cs = { actCase(Rl,Ex.tpe(),Tp,Env,Opts) .. Rl in listIfy(C,'|')};
	valis csAc(Ex,Cs,Lc)
      }.
  checkAction(APPLY(IDEN('onerror',_),[A,B],Lc),Tp,Env,Opts) =>
      valof{
	Act = checkAction(A,Tp,Env,Opts);
	Err = { actCase(Rl,uType('go.stdlib','exception',[]),
			Tp,Env,Opts) .. Rl in listIfy(B,'|')};
	valis errAc(Act,Err,Lc)
      }.
  checkAction(APPLY(IDEN('raise',_),[E],Lc),_,Env,Opts) =>
      valof{
	Ex = typeOf(E,Env,Opts);
	( subType(Ex.tpe(),uType('*','exception',[]),Env)
	  onerror(
	   error(Bec,'fail') ->
	       reportError("exception expression "<>Ex.showTerm()<>
			   " not consistent with exception type\nbecause "<>Bec,Lc)
	  )
	);
	valis exAc(Ex,Lc)
      }.
  checkAction(APPLY(IDEN('|',_),[APPLY(IDEN('?',_),[T,A],_),B],Lc),Tp,Env,Opts)=>
      valof{
	tst = checkGoal(T,Env,Opts);
	lft = checkAction(A,Tp,Env,Opts);
	rgt = checkAction(B,Tp,Env,Opts);
	valis ifAc(tst,lft,rgt,Lc)
      }.
  checkAction(APPLY(IDEN('?',_),[T,A],Lc),Tp,Env,Opts)=>
      valof{
	tst = checkGoal(T,Env,Opts);
	lft = checkAction(A,Tp,Env,Opts);
	valis ifAc(tst,lft,noopAc(Lc),Lc)
      }.
  checkAction(BRACE(IDEN('spawn',_),[A],Lc),_,Env,Opts)=>
      valof{
	Q = findVarsInRule(VOID(Lc),VOID(Lc),A,Env);
	valis spwnAc(Q,checkAction(A,voidType,Env.pushDict(Q),Opts),Lc)
      }.
  checkAction(BRACE(APPLY(IDEN('sync',_),[O],_),[A],Lc),Tp,Env,Opts)=>
      syncAc(typeOf(O,Env,Opts),
	     { actCase(Rl,voidType,Tp,Env,Opts)..Rl in listIfy(A,'|')},Lc).

  checkAction(APPLY(IDEN('timeout',_),[L,APPLY(IDEN('->',_),[T,A],_)],Lc),
	      Tp,Env,Opts)=>
      valof{
	oL = checkAction(L,Tp,Env,Opts);
	oA = checkAction(A,Tp,Env,Opts);
	Tm = typeOf(T,Env,Opts);
	( subType(Tm.tpe(),uType('go.stdlib','number',[]),Env)
	  onerror(
	   error(Bec,'fail') ->
	       reportError("timeout value: "<>Tm.show()<>" inconsistent"
			   "\nbecause "<>Bec,Lc)
	  )
	);
	valis tmeAc(oL,Tm,oA,Lc)
      }.
  checkAction(APPLY(P,A,Lc),_,Env,Opts)=>
      valof{
	p = typeOf(P,Env,Opts);
	( actType(cA).=p.tpe() ?
	    ( listlen(cA)=listlen(A) ?
		ags = typeOfFunArgs(A,cA,Env,Opts);
		valis actAc(p,ags,Lc)
	    | reportError("incorrect number of arguments: "<>listlen(A).show()<>
			  " to procedure: "<>p.showTerm(),Lc);
	      valis noopAc(Lc)
	    )
	| reportError("non-action procedure: "<>p.showTerm()<>
		      " in action call",Lc);
	  valis noopAc(Lc)
	)
      }.

  actCase:[abstract,typeTree,typeTree,dict,typeOpts]=>ruleTree.
  actCase(APPLY(IDEN('->',_),[H,B],rLc),pTp,vTp,Env,Opts) =>
      valof{
	Q = findVarsInRule(H,VOID(noLoc),B,Env);
	sEnv = Env.pushDict(Q);
	P = typeOf(H,sEnv,Opts);
	( sameType(P.tpe(),pTp,Env)
	  onerror(
	   error(Bec,'fail') ->
	       reportError("case pattern: "<>P.showTerm()<>" not consistent with "<>
			   pTp.show()<>"\nbecause "<>Bec,rLc)
	  )
	);
	Body = checkAction(B,vTp,Env,Opts);
	( grd(cPtn,cGrd,_)=P ?
	    valis acRl(Q,[cPtn],cGrd,Body,rLc)
	| valis acRl(Q,[P],trueGl(rLc),Body,rLc)
	)
      }.

  typeOfGrammar:[list[abstract],typeTree,dict,typeOpts]=>list[ruleTree].
  typeOfGrammar(Clses,fTp,Env,Opts)=>
      {typeOfGrammarRule(E,fTp,findVarsInGrammarRule(E,Env),Env,Opts)..E in Clses}.

  typeOfGrammarRule:[abstract,typeTree,dict,dict,typeOpts]=>ruleTree.
  typeOfGrammarRule(APPLY(IDEN('-->',_),[APPLY(IDEN(',',_),[APPLY(_,H,_),P],_),
					 B],gLc),gTp,Q,Env,Opts) =>
      valof{
	(gramType(tA,sT),tQ) = freshenQ(gTp,[]);
	sEnv = Env.pushDict(Q);
	Args = typeOfModedParams(H,tA,sEnv,Opts);

	aHead = grammarBody(P,sT,Env,Opts);
	body = grammarBody(B,sT,Env,Opts);

	( vS(_,_,tV) in tQ *>
	  ( \+tV.isvar() ?
	      reportError("Not permitted to instantiate type variable "<>
			  tV.show()<>
			  " of program type: "<>gTp.show(),gLc)
	  )
	);

	valis grRl(Q,Args,aHead,body,gLc)
      }.
  typeOfGrammarRule(APPLY(IDEN('-->',_),[APPLY(_,H,_),B],gLc),gTp,Q,Env,Opts) =>
      valof{
	(gramType(tA,sT),tQ) = freshenQ(gTp,[]);
	sEnv = Env.pushDict(Q);
	Args = typeOfModedParams(H,tA,sEnv,Opts);
	body = grammarBody(B,sT,Env,Opts);

	( vS(_,_,tV) in tQ *>
	  ( \+tV.isvar() ?
	      reportError("Not permitted to instantiate type variable "<>
			  tV.show()<>
			  " of program type: "<>gTp.show(),gLc)
	  )
	);

	valis grRl(Q,Args,seqGr([],gTp,gLc),body,gLc)
      }.

  grammarBody:[abstract,typeTree,dict,typeOpts]=>gramTree.
  grammarBody(APPLY(IDEN(',',_),[L,R],gLc),sT,Env,Opts) =>
      seqGr({grammarBody(G,sT,Env,Opts)..G in listIfy(L,',')<>listIfy(R,',')},
	    sT,gLc).
  grammarBody(IDEN('[]',Lc),sT,_,_) => seqGr([],sT,Lc).
  grammarBody(IDEN('eof',Lc),sT,_,_) => eofGr(sT,Lc).
  grammarBody(IDEN(N,Lc),sT,_,_) =>
      valof{
	reportError("identifier "<>explode(N)<>" not permitted here",Lc);
	valis seqGr([],sT,Lc)
      }.
  grammarBody(STR(S,Lc),sT,Env,_) =>
      valof{
	( subType(uType('go.stdlib','list',[uType('go.stdlib','char',[])]),sT,Env)
	  onerror(
	   error(Bec,'fail')->
	       reportError("string: "<>S<>" not permitted in this grammar"
			   "\nbecause "<>Bec,Lc)
	  )
	);
	valis strGr(S,Lc)
      }.
  grammarBody(APPLY(IDEN('|',_),[APPLY(IDEN('?',_),[T,L],_),R],Lc),sT,Env,Opts)=>
      ifGr(grammarBody(T,sT,Env,Opts),
	   grammarBody(L,sT,Env,Opts),
	   grammarBody(R,sT,Env,Opts),sT,Lc).
  grammarBody(APPLY(IDEN('|',_),[L,R],Lc),sT,Env,Opts)=>
      dsjGr(grammarBody(L,sT,Env,Opts),
	    grammarBody(R,sT,Env,Opts),sT,Lc).
  grammarBody(APPLY(IDEN('{}',Lc),[G],_),sT,Env,Opts) =>
      glGr(checkGoal(G,Env,Opts),sT,Lc).
  grammarBody(APPLY(IDEN(',..',_),[H,T],Lc),sT,Env,Opts) =>
      seqGr({terminalGrammar(Tm,sT,Env,Opts)..Tm in [H,..listIfy(T,',..')]},sT,Lc).
  grammarBody(APPLY(IDEN('onerror',_),[A,E],Lc),sT,Env,Opts) =>
      valof{
	lft = grammarBody(A,sT,Env,Opts);

	errProg = {grmCase(Rl,sT,uType('go.stdlib','exception',[]),
			   Env,Opts) .. Rl in listIfy(E,'|')};
	valis errGr(lft,errProg,Lc)
      }.
  grammarBody(APPLY(IDEN('raise',_),[E],Lc),sT,Env,Opts) =>
      valof{
	lft = typeOf(E,Env,Opts);
	( subType(lft.tpe(),uType('*','exeption',[]),Env)
	  onerror(
	   error(Bec,'eFAIL') ->
	       reportError("exception expression: "<>
			   lft.show()<>":"<>lft.tpe().show()<>
			   " not consistent with exception type\nbecause "<>Bec,Lc)
	  )
	);
	valis exGr(lft,sT,Lc)
      }.
  grammarBody(APPLY(IDEN('!',_),[G],Lc),sT,Env,Opts) =>
      oneGr(grammarBody(G,sT,Env,Opts),Lc).
  grammarBody(APPLY(IDEN('{}',_),[G],Lc),sT,Env,Opts) =>
      glGr(checkGoal(G,Env,Opts),sT,Lc).
  grammarBody(APPLY(IDEN('\\+',_),[G],Lc),sT,Env,Opts) =>
      negGr(grammarBody(G,sT,Env,Opts),Lc).
  grammarBody(APPLY(IDEN('*',_),[N,APPLY(IDEN('^',_),[P,Lst],_)],Lc),sT,Env,Opts)=>
      valof{
	Q = findVarsInGrammarRule(APPLY(IDEN('-->',noLoc),[P,N],noLoc),Env);
	sEnv = Env.pushDict(Q);
	l = typeOf(Lst,Env,Opts);	-- note, evaluated in parent environment
	aT = newVr();
	( sameType(uType('go.stdlib','list',[aT]),l.tpe(),Env)
	  onerror(
	   error(Bec,'fail') ->
	       reportError("expecting a list type, got "<>l.tpe().show()<>
			   "\nbecause "<>Bec,Lc)
	  )
	);
	ptn = typeOf(P,sEnv,Opts);
	( subType(ptn.tpe(),l.tpe(),sEnv)
	  onerror(
	   error(Bec,'fail') ->
	       reportError("type of iterated element: "<>ptn.showTerm()<>
			   " not consistent with expected type: "<>aT.show()<>
			   "\nbecause "<>Bec,Lc)
	  )
	);
	body = grammarBody(N,sT,sEnv,Opts);
	valis itrGr(Q,body,ptn,l,sT,Lc)
      }.
  grammarBody(APPLY(N,A,Lc),sT,Env,Opts) =>
      valof{
	nt = typeOf(N,Env,Opts);

	( gramType(cA,cT).=nt.tpe().deRef() ?
	    ( listlen(cA)=listlen(A)?
		args = typeOfFunArgs(A,cA,Env,Opts);
		( subType(sT,cT,Env)
		  onerror(
		   error(Bec,'fail') ->
		       reportError("grammar non-terminal: "<>nt.showTerm()<>
				   " not consistent with expected stream type: "<>
				   sT.show()<>
				   "\nbecause "<>Bec,Lc)
		  )
		);
		valis ntGr(nt,args,sT,Lc)
	    | reportError("number of arguments "<>listlen(A).show()<>
			  " supplied to call to "<>nt.show()<>
			  " differnt to that expected: "<>listlen(cA).show(),Lc);
	      valis seqGr([],sT,Lc)
	    )
	| reportError(nt.showTerm()<>" is not a grammar",Lc);
	  valis seqGr([],sT,Lc)
	)
      }.


  private grmCase:[abstract,typeTree,typeTree,dict,typeOpts]=>ruleTree.
  grmCase(APPLY(IDEN('-->',_),[P,E],eLc),sT,pT,Env,Opts) =>
      valof{
	Q = findVarsInGrammarRule(APPLY(IDEN('-->',eLc),[P,E],eLc),Env);
	sEnv = Env.pushDict(Q);
	ePtn = typeOf(P,sEnv,Opts);

	( sameType(ePtn.tpe(),pT,Env)
	  onerror(
	   error(Bec,'eFAIL') ->
	       reportError("type of handler pattern: "<>ePtn.show()<>
			   ":"<>ePtn.tpe().show()<>
			   " not consistent with "<>pT.show()<>
			   "\nbecause "<>Bec,eLc)
	  )
	);

	eBody = grammarBody(E,sT,sEnv,Opts);
	valis grRl(Q,[ePtn],seqGr([],sT,eLc),eBody,eLc)
      }.


  private terminalGrammar:[abstract,typeTree,dict,typeOpts]=>gramTree.
  terminalGrammar(T,sT,Env,Opts) =>
      valof{
	tm = typeOf(T,Env,Opts);
	( subType(tm.tpe(),sT,Env)
	  onerror(
	   error(Bec,'fail')->
	       reportError("terminal "<>T.show()<>" not consistent with "
			   " grammar rule type "<>sT.show()<>
			   "\nbecause "<>Bec,tm.loc())
	  )
	);
	valis tmGr(tm,sT,tm.loc())
      }.

  thetaEnv:[symbol,list[abstract],dict, dict,typeOpts]=>(list[(symbol,typeBinding,progTree)],list[(symbol,symbol)]).
  thetaEnv(Pkg,Th,Methods,Ev,Opts)=>
      (:ev[(list[(symbol,typeBinding,progTree)], list[(symbol,symbol)])]..{
	 tEnv:dict := Ev.push('main',varBind,actType([(uType('go.stdlib','list',[uType('go.stdlib','list',[uType('go.stdlib','char',[])])]),biMode)])).

	 thePrograms:list[(symbol,typeBinding,progTree)] := [].
	 imports:list[(symbol,symbol)] := [].
	 declared:list[(symbol,fileLoc)] := [('main',noLoc)].
	 privateTypes:list[symbol] := [].
	 privateVals:list[symbol] := [].
	 privateCons:list[symbol] := [].

	-- Sort out top-level stuff

	-- Phase one extracts type names.
	 tempEnv:[list[(abstract,visibility,abstract)]]=>list[vSpec].
	 tempEnv([]) => [].
	 tempEnv([(_,_,APPLY(IDEN('<~',_),[IDEN(Nm,_),TPL(tRules,_)],_)),..Gp])::
		 APPLY(IDEN('<~',_),[SQUARE(_,tA,_),_],_) in tRules =>
	     [vS(Nm,tvarBind,uType(Pkg,Nm,{newVr().._ in tA})),..tempEnv(Gp)].
	 tempEnv([_,..G]) => tempEnv(G).
	
	 phaseI:[list[(abstract,visibility,abstract)],dict]*.
	 phaseI(Gp,tmpEnv) ->
	     ((_,Vis,El) in Gp *>
	      ( Opts.option(dbgType)?
		  stdout.outLine("PhaseI for "<>El.show()));
	      case El in (
	       APPLY(IDEN(':',_),[IDEN(Nm,iLc),Tp],_) -> -- Variable declaration
		   ( Opts.option(dbgType)?
		       stdout.outLine(explode(Nm)<>" has a variable decl"));
		   ( (nQ,rTp) = realType(Tp,Pkg,tmpEnv);
		     tEnv := tEnv.push(Nm,varBind,foldupType(nQ,rTp));
		     declared := [(Nm,iLc),..declared]
		   ) onerror (
		    error(Bec,'fail') ->
			reportError("Could not determine the type of "<>Nm.show()<>
				    ":"<>display(Tp)<>"\nbecause "<>Bec,iLc)
		   );
		   ( Vis=privAte?
		       privateVals := privateVals\/[Nm] )
		  
	     | APPLY(IDEN('<~',_),[IDEN(Nm,tLc),TPL(tpRules,_)],_):: -- Type declaration
		       APPLY(IDEN('<~',_),[SQUARE(_,tA,_),_],_) in tpRules ->
		   (Opts.option(dbgType)?
		      stdout.outLine("Processing type "<>explode(Nm)));
		   declared := [(Nm,tLc),..declared];
		   (:do..{

		      do() -> checkTpRules(tpRules). 

		      checkTpRules:[list[abstract]]*.
		      checkTpRules(tpRls) ->
			  (tpRl in tpRls *> check(tpRl));
			  tEnv := tEnv.push(Nm,typeBind,
					    foldupType(fields(polyQ),
						       typeDef(uType(Pkg,Nm,
								     {V .. vS(_,_,V) in polyQ}),
							       [faceType(fields(iFace)),..
								supers])));
			  (Opts.option(dbgType)?
			     stdout.outLine("Type declared "<>explode(Nm))).


		      iFace:list[vSpec] := [].
		      supers:list[typeTree] := [].
		      tpArgs:list[abstract] = tA.
		      polyQ:list[vSpec] = polyVars(tA,[]).
		      
		      polyVars:[list[abstract],list[vSpec]]=>list[vSpec].
		      polyVars([],E)=>E.
		      polyVars([IDEN(tV,vLc),..tArgs],E)::vS(tV,_,_) in E =>
			  valof{
			    reportError("repeated occurrence of type variable "<>
					tV.show()<>" in type template ",vLc);
			    valis polyVars(tArgs,E)
			  }.
		      polyVars([IDEN(tV,_),..tArgs],E) =>
			  polyVars(tArgs,[vS(tV,tvarBind,newVr()),..E]).
		      
		      check:[abstract]*.
		      check(APPLY(IDEN('<~',_),[SQUARE(tN,tA,tL),APPLY(IDEN('{}',_),mT,_)],_))->
			  ( listlen(tpArgs)!=listlen(tA)?
			      reportError("inconsistent number of type arguments in type "<>
					  SQUARE(tN,tA,tL).show()<>
					  " "<>listlen(tpArgs).show()<>" expected",tL)
			  );
			  M = macroList(mT,
					macroEnv(tA,
						 {IDEN(tV,noLoc)..vS(tV,_,_) in polyQ},[]));
			  ( APPLY(IDEN(':',_),[IDEN(Fld,fLc),Tp],_) in M *>
			    stdout.outLine("Parsing type "<>Tp.show());
			    xTp = parseType(Tp,Pkg,tmpEnv,fields(polyQ));
			    ( vS(Fld,_,oTp) in iFace ?
				( sameType(oTp,xTp,tmpEnv)
				  onerror (
				   error(Bec,'fail') ->
				       reportError("type of field: "<>
						   Fld.show()<>":"<>xTp.show()<>
						   " not consistent with other declarations"
						   " of same field\nbecause "<>Bec,fLc)
				  )
				)
			    | isProgramType(xTp) ?
				iFace := [vS(Fld,varBind,xTp),..iFace]
			    | reportError("Type of "<>Fld.show()<>" should be a program"
					  " type, not "<>xTp.show(),fLc)
			    )
			  ).
		      check(APPLY(IDEN('<~',_),[SQUARE(tN,tA,tL),sT],fLc)) ->
			  ( listlen(tpArgs)!=listlen(tA)?
			      reportError("inconsistent number of type arguments in type "<>
					  SQUARE(tN,tA,tL).show()<>
					  " "<>listlen(tpArgs).show()<>" expected",tL)
			  );
			  sTp = macro(sT,
				      macroEnv(tA,
					       {IDEN(tV,noLoc)..vS(tV,_,_) in polyQ},[]));
			  stdout.outLine("Parsing super type "<>sTp.show()<>" in "<>tmpEnv.disp(2).flatten(""));
			  spTp = parseType(sTp,Pkg,tmpEnv,fields(polyQ));
			  ( iTp = typeInterface(spTp.deRef(),tEnv);
			    ( fieldsOf(iTp).isbound(Fld,Mode,fTp) *>
			      ( vS(Fld,_,oTp) in iFace ?
				  ( sameType(oTp,fTp,tmpEnv)
				    onerror (
				     error(Bec,'fail') ->
					 reportError("type of field: "<>
						     Fld.show()<>":"<>fTp.show()<>
						     " not consistent with other declarations"
						     " of same field\nbecause "<>Bec,fLc)
				    )
				  )
			      | iFace := [vS(Fld,Mode,fTp),..iFace]
			      )
			    );
			    supers := [spTp,..supers];
			    
			    ( uType(sPk,sNm,sA) in typeSupers(spTp,tEnv) *>
			      ( uType(sPk,sNm,ssA) in supers ?
				  ( sameType(uType(sPk,sNm,sA),uType(sPk,sNm,ssA),tEnv)
				    onerror (
				     error(Bec,'fail') ->
					 reportError("multiply inherited type: "<>
						     uType(sPk,sNm,sA).show()<>
						     " not consistent with other"
						     "inherited version "<>
						     uType(sPk,sNm,ssA).show()<>
						     "\nbecause "<>Bec,fLc)
				    )
				  )
			      | supers := [uType(sPk,sNm,sA),..supers]
			      )
			    )
			  ) onerror(
			   error(Bec,'fail') ->
			       reportError(Bec,tL)
			  ).
		      
		    }).do()
	     | APPLY(IDEN('<$',_),[IDEN(Nm,Lc),_],_) ->
		   declared := [(Nm,Lc),..declared];
		   (Vis==privAte ?
		      privateVals := privateVals\/[Nm]
		   )
	     | APPLY(IDEN('import',_),[imPkg],Lc) ->
		   (
		    (iPkg,iVers,iDefs,iType) = importPkg(Opts,packageReference(imPkg));
		    imports := [(iPkg,iVers),..imports];
		    tEnv := tEnv.pushDict(iDefs).pushDict(iType).push(iPkg,pkgBind,faceType(iType));
		   ) onerror (
		    error(Bec,'fail') ->
			reportError("could not import "<>iPkg.show()<>
				    "\nbecause "<>Bec,Lc)
		   )
		   
	     | APPLY(_,[IDEN(Nm,_),.._],_) ->
		   ( Vis=privAte?
		       privateVals := privateVals\/[Nm])
	     | APPLY(IDEN('$',_),_,_) -> {}
	      )
	     ).
	 
	 -- Handle standard imports
	 phaseII:[]*.
	 phaseII()::Opts.isTopLevel() ->
	     ( \+ Opts.option(noStdlib), \+ ('go.stdlib',_) in imports ?
		 (iPkg,iVers,iDefs,iType) = importPkg(Opts,'go.stdlib');
		 imports := [(iPkg,iVers),..imports];
		 tEnv := tEnv.pushDict(iDefs).pushDict(iType).push(iPkg,pkgBind,faceType(iType));
	     );
	     ( Opts.option(dbg(_)), \+ ('go.debug',_) in imports ?
		 (iPkg,iVers,iDefs,iType) = importPkg(Opts,'go.debug');
		 imports := [(iPkg,iVers),..imports];
		 tEnv := tEnv.pushDict(iDefs).pushDict(iType).push(iPkg,pkgBind,faceType(iType));
	     ).
	 phaseII()->{}.
	 
	 phaseIII:[list[(abstract,visibility,abstract)],dict] => list[(symbol,typeBinding,progTree)].
	 phaseIII([],_) => [].
	 phaseIII([(IDEN(_,_),_,El),..Gp],sE) =>
	     case El in (
	      APPLY(IDEN('<~',Lc),[IDEN(Id,_),_],_) =>
		  valof{
		    (Opts.option(dbgType)?
		       stdout.outLine("PhaseIII type "<>explode(Id)));
		    iTp = sE.lookup(Id,typeBind);
		    valis [(Id,typeBind,tDef(Id,(Id in privateTypes ? privAte | pUblic),
					     iTp,Lc)),..phaseIII(Gp,sE)]
		  }
	    | APPLY(IDEN('<~',Lc),[SQUARE(IDEN(Id,_),_,_),_],_) =>
		  valof{
		    (Opts.option(dbgType)?
		       stdout.outLine("PhaseIII type "<>explode(Id)));
		    iTp = sE.lookup(Id,typeBind);
		    valis [(Id,typeBind,tDef(Id,(Id in privateTypes ? privAte | pUblic),
					     iTp,Lc)),..phaseIII(Gp,sE)]
		  }
	    | APPLY(IDEN(':',_),_,_) => phaseIII(Gp,sE)
	    | APPLY(IDEN('=',_),[IDEN(Id,_),R],Lc) =>
		  valof{
		    (Opts.option(dbgType)?
		       stdout.outLine("PhaseIII constant "<>explode(Id)));
		    ( \+Opts.isStatefull() ?
			reportError("variable "<>Id.show()<>" not permitted in a stateless "
				    "class",Lc)
		    | \+((Id,_) in declared), \+Methods.isbound(Id,_,_) ?
			reportError("constant "<>Id.show()<>" not declared",Lc)
		    );
		    ( sE.isbound(Id,varBind,vType) ?
			Const = ( findVarsInTerm(R,sE).empty() ?
				    typeOf(R,sE,Opts)
				| typeOf(BRACE(IDEN('valof',Lc),[APPLY(IDEN('valis',Lc),[R],Lc)],Lc),
					 sE,Opts)
			);
			( subType(Const.tpe(),vType,sE)
			  onerror(
			   error(Bec,'fail') ->
			       reportError("expression "<>Const.showTerm()<>
					   "not consistent with declared type "<>
					   vType.show()<>"\nbecause "<>Bec,Lc)
			  )
			);
			valis [(Id,varBind,cDef(Id,(Id in privateVals?privAte|pUblic),
						Const,vType,Lc)),..phaseIII(Gp,sE)]
		     | reportError(Id.show()<>" not declared",Lc);
		      valis phaseIII(Gp,sE)
		    )
		  }
	    | APPLY(IDEN(':=',_),[IDEN(Id,Lc),R],_) =>
		  valof{
		    (Opts.option(dbgType)?
		       stdout.outLine("PhaseIII assignable "<>explode(Id)));
		    ( \+Opts.isStatefull() ?
			reportError("variable "<>Id.show()<>" not permitted in a stateless "
				    "class",Lc)
		    | \+((Id,_) in declared), \+(Methods.isbound(Id,_,_)) ?
			reportError("variable "<>Id.show()<>" not declared",Lc)
		    );
		    ( sE.isbound(Id,varBind,vType) ?
			Variable = ( findVarsInTerm(R,sE).empty() ?
				       typeOf(R,sE,Opts)
				   | typeOf(BRACE(IDEN('valof',Lc),[APPLY(IDEN('valis',Lc),[R],Lc)],Lc),
					    sE,Opts)
			);
			( subType(Variable.tpe(),vType,sE)
			  onerror(
			   error(Bec,'fail') ->
			       reportError("expression "<>Variable.showTerm()<>
					   "not consistent with declared type "<>
					   vType.show()<>"\nbecause "<>Bec,Lc)
			  )
			);
			valis [(Id,varBind,vDef(Id,(Id in privateVals?privAte|pUblic),
						Variable,vType,Lc)),..phaseIII(Gp,sE)]
		    | reportError(Id.show()<>" not declared",Lc);
		      valis phaseIII(Gp,sE)
		    )
		  }
	    | APPLY(IDEN('=>',_),[IDEN(Id,Lc),TPL(Eqns,_)],_) =>
		  valof{
		    (Opts.option(dbgType)?
		       stdout.outLine("PhaseIII function "<>explode(Id)));
		    ( \+((Id,_) in declared), \+Methods.isbound(Id,_,_) ?
			reportError("function "<>Id.show()<>" not declared",Lc);
			stdout.outLine("Declared = "<>declared.show());
			stdout.outLine("Methods = "<>Methods.show())
		    );
		    ( sE.isbound(Id,varBind,vType) ?
			Eqs = typeOfFunction(Eqns,vType,sE,Opts);
		      
			valis [(Id,varBind,fnDf(Id,(Id in privateVals?privAte|pUblic),
						Eqs,vType,Lc)),..phaseIII(Gp,sE)]
		     | reportError(Id.show()<>" not declared",Lc);
		      valis phaseIII(Gp,sE)
		    )
		  }
	    | APPLY(IDEN(':-',_),[IDEN(Id,Lc),TPL(Clses,_)],_) =>
		  valof{
		    (Opts.option(dbgType)?
		       stdout.outLine("PhaseIII relation "<>explode(Id)));
		    ( \+((Id,_) in declared), \+Methods.isbound(Id,_,_) ?
			reportError("relation "<>Id.show()<>" not declared",Lc)
		    );
		    ( sE.isbound(Id,varBind,vType) ?
			Cls = typeOfPredicate(Clses,vType,sE,Opts);
			
			valis [(Id,varBind,rDf(Id,(Id in privateVals?privAte|pUblic),
					       Cls,vType,Lc)),..phaseIII(Gp,sE)]
		    | reportError("predicate "<>Id.show()<>" not declared in "<>sE.disp(3).flatten(""),Lc);
		      valis phaseIII(Gp,sE)
		    )
		  }
	    | APPLY(IDEN(':--',_),[IDEN(Id,Lc),TPL(Clses,_)],_) =>
		  valof{
		    (Opts.option(dbgType)?
		       stdout.outLine("PhaseIII strong relation "<>explode(Id)));
		    ( \+((Id,_) in declared), \+Methods.isbound(Id,_,_) ?
			reportError("relation "<>Id.show()<>" not declared",Lc)
		    );
		    ( sE.isbound(Id,varBind,vType) ?
		      Cls = typeOfStrongPred(Clses,vType,sE,Opts);
		      
		      valis [(Id,varBind,srDf(Id,(Id in privateVals?privAte|pUblic),
					      Cls,vType,Lc)),..phaseIII(Gp,sE)]
		    | reportError("strong predicate "<>Id.show()<>" not declared in "<>sE.disp(3).flatten(""),Lc);
		      valis phaseIII(Gp,sE)
		    )
		  }
	    | APPLY(IDEN('->',_),[IDEN(Id,Lc),TPL(Rules,_)],_) =>
		  valof{
		    (Opts.option(dbgType)?
		       stdout.outLine("PhaseIII procedure "<>explode(Id)));
		    ( \+((Id,_) in declared), \+Methods.isbound(Id,_,_) ?
			reportError("procedure "<>Id.show()<>" not declared",Lc)
		    );
		    ( sE.isbound(Id,varBind,vType) ?
		      Cls = typeOfProcedure(Rules,vType,sE,Opts);
		      
		      valis [(Id,varBind,acDf(Id,(Id in privateVals?privAte|pUblic),
					      Cls,vType,Lc)),..phaseIII(Gp,sE)]
		    | reportError(Id.show()<>" not declared",Lc);
		      valis phaseIII(Gp,sE)
		    )
		  }
	    | APPLY(IDEN('-->',_),[IDEN(Id,Lc),TPL(Rules,_)],_) =>
		  valof{
		    (Opts.option(dbgType)?
		       stdout.outLine("PhaseIII grammarn "<>explode(Id)));
		    ( \+((Id,_) in declared), \+Methods.isbound(Id,_,_) ?
			reportError("grammar "<>Id.show()<>" not declared",Lc)
		    );
		    ( sE.isbound(Id,varBind,vType) ?
		      Cls = typeOfGrammar(Rules,vType,sE,Opts);
		      
		      valis [(Id,varBind,grDf(Id,(Id in privateVals?privAte|pUblic),
					      Cls,vType,Lc)),..phaseIII(Gp,sE)]
		    | reportError(Id.show()<>" not declared",Lc);
		      valis phaseIII(Gp,sE)
		    )
		  }
	    | APPLY(IDEN('<$',_),[IDEN(Id,Lc),TPL(Rules,_)],_) =>
		  valof{
		    (Opts.option(dbgType)?
		       stdout.outLine("PhaseIII for class "<>explode(Id)));
		    ( \+((Id,_) in declared), \+Methods.isbound(Id,_,_) ?
			reportError("class "<>Id.show()<>" not declared",Lc)
		    );
		    ( sE.isbound(Id,varBind,vType) ?
			Cls = typeOfClass(Id,Rules,
					  (Id in privateVals?privAte|pUblic),
					  Lc,vType,sE,Opts);
			valis [(Id,varBind,Cls),..phaseIII(Gp,sE)]
		    | reportError(Id.show()<>" not declared",Lc);
		      valis phaseIII(Gp,sE)
		    )
		  }
	    | APPLY(IDEN('$',Lc),[A],_) =>
		  valof{
		    (Opts.option(dbgType)?
		       stdout.outLine("PhaseIII static"));
		    ( \+Opts.isStatefull() ?
			reportError("static initializer not permitted in a stateless "
				    "class",Lc)
		    );
		    
		    Q = findVarsInTerm(A,sE);
		    Init = checkAction(A,voidType,sE.pushDict(Q),Opts);
		    valis [('$',varBind,iDef(Init,Lc)),..phaseIII(Gp,sE)]
		  }
	    | APPLY(IDEN('import',_),_,_) =>
		  phaseIII(Gp,sE)
	    | X => valof{
		     reportError("Cannot handle definition "<>X.show(),X.loc());
		     valis phaseIII(Gp,sE)
	    }
	     ).
	 
	 reQuantPhase:[list[(symbol,typeBinding,progTree)]]*.
	 reQuantPhase([]) -> {}.
	 reQuantPhase([(Nm,Kind,Prg),..st]) ->
	     (Prg.vis()=pUblic ?
		(P in privateTypes *>
		 ( isReferenced(Pkg,P,[],Prg.tpe()) ?
		     reportWarning(Nm.show()<>" references private type "<>P.show(),Prg.loc())
		 )
		)
	     );
	     thePrograms := [(Nm,Kind,Prg),..thePrograms];
	     --	    thePrograms := [(Nm,Kind,reQuantify(Prg,sE)),..thePrograms];
	     reQuantPhase(st).
	 
	 checkDeclaredPhase:[]*.
	 checkDeclaredPhase() ->
	     (Nm,Lc) in declared *>
	     (\+(Nm,_,_) in thePrograms,(Nm!='main'|\+Opts.isTopLevel())?
		reportWarning(Nm.show()<>" was declared but not defined",Lc)).
	 
	 ev()=>
	     valof{
	       ( gp in thetaDepend(Th,Opts) *>
		 (Opts.option(dbgType)?
		    stdout.outLine("Processing group: "<>showGroup(gp));
		    stdout.outLine("Temp env is "<>tempEnv(gp).show()));
		 tmpDict = tEnv.pushList(tempEnv(gp));
		 phaseI(gp,tmpDict);
		 phaseII();
		 (Opts.option(dbgType)?
		    stdout.outLine("Phase III for "<>showGroup(gp));
		    stdout.outLine("Dict prior to PhaseIII: "<>tEnv.disp(2).flatten("")));
		 st = phaseIII(gp,tEnv.pushDict(Methods));
		 (Opts.option(dbgType)?
		    stdout.outLine("PhaseIII ends "<>showGroup(gp)));
		 reQuantPhase(st)
	       );
	       valis (thePrograms,imports)
	     }.
       }).ev().


  typeOfClass:[symbol,list[abstract],visibility,fileLoc,typeTree,dict,typeOpts]=> progTree.
  typeOfClass(clName,Prog,Vis,clLc,ClTp,Env,xOpts) :: 
	  (thType,nQ) = freshenQ(ClTp,[]),
	  extThisType(thType,thisType,statefull),
	  qEnv = Env.pushList(nQ) =>
      (:ev[progTree]..{

	 ev()=> valof{
		  (Opts.option(dbgType)?
		     stdout.outLine("computing type of class: "<>explode(clName));
		     stdout.outLine("thisType = "<>thisType.show())
		  );
		  
		  doClass();
		  valis clDf(clName,Vis,classRules,thisType,clLc)
		}.

	 supers:list[vSpec] := [].
	 Methods:list[vSpec] := [].
	 classRules:list[ruleTree] := [].
	 duplicates:list[(symbol,fileLoc)] := [].
	 Opts:typeOpts = xOpts.markStatefull(statefull).

	 doClass:[]*.
	 doClass() ->
	     El in Prog *>
	     case El in (
	      APPLY(IDEN('<=',Lc),[Lbl,Supr],_) -> 
		  (Opts.option(dbgType)?
		     stdout.outLine("class body rule: "<>El.show()));
		  
		  Q = findVarsInClassRule(El,qEnv);
		  sEnv = qEnv.pushDict(Q);
		  SpNme = implode("."<>
				  (IDEN(S,_)=Supr ?
				     explode(S)
				 | APPLY(IDEN(S,_),_,_)=Supr ?
				     explode(S)
				 | raise error("we have a problem houston",
					       'internal')
				  ));
		  ( vS(SpNme,_,_) in supers ?
		      reportError("can only inherit "<>Supr.show()<>
				  " once from a given class ",Lc)
		  );
		  super = typeOf(Supr,sEnv,Opts);
		  ( sconType(_,_).=super.tpe(), \+statefull ?
		      reportError("a statefree class may not inherit from a "
				  "stateful super class: "<>super.show(),Lc)
		  );
		  supers := [vS(SpNme,varBind,super.tpe()),..supers];
		  
		  Label = typeOf(Lbl,sEnv,Opts);
		  
		  ( sameType(Label.tpe(),thisType,sEnv)
		    onerror(
		     error(Bec,'fail') ->
			 reportError("type of label: "<>Label.show()<>
				     " not consistent with type"
				     " declared for class: "<>thisType.show()<>
				     "\nbecause "<>Bec,Lc)
		    )
		  );
		  ( subType(thisType,super.tpe(),sEnv)
		    onerror(
		     error(Bec,'fail') ->
			 reportError("class: "<>Label.show()<>
				     " not consistent with super "<>super.show()<>
				     "\nbecause "<>Bec,Lc)
		    )
		  );
		  (Opts.option(dbgType)?
		     stdout.outLine("figuring out supers in class"));
		  ( spFlds = fieldsOf(typeInterface(super.tpe().deRef(),sEnv));
		    (Opts.option(dbgType)?
		       stdout.outLine("fields of super "<>super.show()<>" are "<>spFlds.show()));

		    ( spFlds.isbound(Fld,Mode,fTp) *>
		      ( vS(Fld,_,mTp) in Methods ?
			  duplicates := [(Fld,Lc),..duplicates];
			  ( sameType(fTp,mTp,sEnv)
			    onerror(
			     error(Bec,'fail') ->
				 reportError("inherited definition of field "<>
					     explode(Fld)<>":"<>fTp.show()<>
					     " not consistent with field type "<>
					     mTp.show()<>" resulting from "
					     "other class rules"
					     "\nbecause "<>Bec,Lc)
			    )
			  )
		      | Methods := [vS(Fld,Mode,fTp),..Methods];
		      )
		    );
		    classRules:=[clRule(Q,Label,super,Lc),..classRules]
		  )
		  onerror(
		   error(Bec,'fail') ->
		       reportError("problem in type associated with super "<>
				   super.show()<>
				   "\nbecause "<>Bec,Lc)
		  )
	    | APPLY(IDEN('..',Lc),[Lbl,APPLY(IDEN('{}',_),Defs,_)],_) ->
		  (Opts.option(dbgType)?
		     stdout.outLine("type of class body: "<>Defs.show()));
		  
		  Q = findVarsInClassBody(El,qEnv.pushList(Methods));
		  (Opts.option(dbgType)?
		     stdout.outLine("class body variables: "<>Q.show()));
		  sEnv = qEnv.pushDict(Q).push('this',varBind,thisType);
		  
		  Label = typeOf(Lbl,sEnv,Opts);
		  
		  ( sameType(Label.tpe(),thisType,sEnv)
		    onerror(
		     error(Bec,'fail') ->
			 reportError("type of label: "<>Label.show()<>
				     " not consistent with type"
				     " declared for class: "<>thisType.show()<>
				     "\nbecause "<>Bec,Lc)
		    )
		  );
		  
		  ( spFlds = fieldsOf(typeInterface(thisType.deRef(),sEnv));
		    (Opts.option(dbgType)?
		       stdout.outLine("fields of thisType: "<>spFlds.show());
		       stdout.outLine("Current methods = "<>Methods.show()));
		    
		    ( spFlds.isbound(Fld,Mode,fTp) *>
		      ( vS(Fld,Mode,mTp) in Methods ?
			  ( subType(mTp,fTp,sEnv)
			    onerror (
			     error(Bec,'fail') ->
				 reportError("inconsistency with assumed "
					     "type for method "<>Fld.show()<>
					     "\nbecause "<>Bec,Lc)
			    )
			  )
		      | Methods := [vS(Fld,Mode,fTp),..Methods]
		      )
		    );

		    (Opts.option(dbgType)?
		       stdout.outLine("class Thet: "<>Defs.show());
		       stdout.outLine("Current methods = "<>Methods.show()));

		    
		    (theta,Imprts) = thetaEnv(Opts.inPkg(),
					      Defs,fields(Methods),
					      sEnv.pushList(supers),
					      subOpts(statefull,Opts));
		    ( Imprts!=[] ?
			reportError("imports not permitted inside class body",Lc)
		    );
		    
		    ( spFlds.isbound(Fld,_,_) *>
		      ( \+ (_,_,@defines(Fld)) in theta,
			\+ ( vS(_,_,spTp) in supers,
			     fieldsOf(typeInterface(spTp.deRef(),sEnv)).isbound(Fld,_,_))?
			  reportError("no implementation given for "<>Fld.show(),Lc)
		      )
		    );
		    ((Fld,fLc) in duplicates *>
		     ( \+(Fld,_,_) in theta ?
			 reportWarning("multiply inherited method "<>Fld.show()<>
				       "\nonly one will be used",fLc)
		     )
		    );
		    classRules := [clBdy(Q,Label,{Pr..(_,_,Pr) in theta},Lc),..classRules]
		  )
		  onerror (
		   error(_,'fail') ->
		       reportError("no type interface associated with class "
				   "type "<>thisType.show(),Lc)
		  );
	     ).
       }).ev().

  private extThisType:[typeTree,typeTree-,logical-]{}.
  extThisType(conType(_,rT),rT,false).
  extThisType(sconType(_,rT),rT,true).
  extThisType(enuType(rT),rT,false).
}
