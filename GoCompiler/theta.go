theta{
  tempEnv:[list[(abstract,visibility,abstract)]]=>list[vSpec].
  tempEnv([]) => [].
  tempEnv([(_,_,APPLY(IDEN('<~',_),[IDEN(Nm,_),TPL(tRules,_)],_)),..Gp])::
	  APPLY(IDEN('<~',_),[SQUARE(_,tA,_),_],_) in tRules =>
      [vS(Nm,tvarBind,uType(Pkg,Nm,{newVr().._ in tA})),..tempEnv(Gp)].
  tempEnv([_,..G]) => tempEnv(G).

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

  -- phaseI collects pre-binding information for all the elements in a group
  phaseI:[list[(abstract,visibility,abstract)],
	  list[(symbol,fileLoc)],
	  list[symbol],
	  list[symbol],
	  list[(symbol,symbol)],
	  dict]=>
      (list[(symbol,fileLoc)],
       list[symbol],
       list[symbol],
       list[(symbol,symbol)],
       dict).
  phaseI([],tmpEnv,declared,privateVals,privateTypes,imports,tEnv) => 
      (declared,privateVals,privateTypes,imports,tEnv).
  -- Variable declaration
  phaseI([(_,Vis,APPLY(IDEN(':',_),[IDEN(Nm,iLc),Tp],_)),..Gps],
	 tmpEnv,declared,pVals,pTypes,imports,tEnv) =>
      valof{
	( rTp = rlType(Tp,Pkg,tmpEnv);
	  valis phaseI(Gps,tmpEnv,
		       [(Nm,iLc),..declared],
		       (Vis=privAte?pVals\/[Nm]
		       |pVals),
		       pTypes,
		       imports,
		       tEnv.push(Nm,varBind,rTp))
	) onerror (
	 error(Bec,'fail') ->
	     reportError("Could not determine the type of "<>Nm.show()<>
			 ":"<>display(Tp)<>"\nbecause "<>Bec,iLc);
	     valis phaseI(Gps,tmpEnv,declared,pVals,pTypes,imports,tEnv)
	)
      }.
  -- Type declaration
  phaseI([(_,Vis,APPLY(IDEN('<~',_),[IDEN(Nm,iLc),TPL(tpRules,_)],_)),..Gps],
	 tmpEnv,declared,pVals,pTypes,imports,tEnv) :: 
	  APPLY(IDEN('<~',_),[SQUARE(_,tA,_),_],_) in tpRules =>
      phaseI(Gps,tmpEnv,[(Nm,iLc),..declared],
	     pVals,
	     (Vis=privAte?pTypes\/[Nm]
	     |pTypes),
	     imports,
	     checkTpRules(tpRules,Pkg,Nm,polyVars(tA,[]),tmpEnv,tEnv)).

  -- class definition
  phaseI([(_,Vis,APPLY(IDEN('<$',_),[IDEN(Nm,Lc),_],_)),..Gps],
	 tmpEnv,declared,pVals,pTypes,imports,tEnv) =>
      phaseI(Gps,tmpEnv,[(Nm,Lc),..declared],(Vis==privAte ?
						pVals\/[Nm] |
					      pVals),
	     pTypes,imports,tEnv).
  -- package import
  phaseI([(_,Vis,APPLY(IDEN('import',_),[imPkg],Lc)),..Gps],
	 tmpEnv,declared,pVals,pTypes,imports,tEnv) =>
      valof{
	(
	 (iPkg,iVers,iDefs,iType) = importPkg(Opts,packageReference(imPkg));
	 valis phaseI(Gps,tmpEnv,declared,pVals,pTypes,
		      [(iPkg,iVers),..imports],
		      tEnv.pushDict(iDefs).pushDict(iType).push(iPkg,pkgBind,faceType(iType)))
	) onerror (
	 error(Bec,'fail') ->
	     reportError("could not import "<>iPkg.show()<>
			 "\nbecause "<>Bec,Lc);
	     valis phaseI(Gps,tmpEnv,declared,pVals,pTypes,imports,tEnv)
	)
      }.

  -- program definition
  phaseI([(_,Vis,APPLY(_,[IDEN(Nm,_),.._],_)),..Gps],
	 tmpEnv,declared, privateVals,pTypes,imports,tEnv) =>
      phaseI(Gps,tmpEnv,declared,(Vis==privAte ?
				    privateVals\/[Nm] |
				  privateVals),pTypes,imports,tEnv).
  -- We skip others
  phaseI([(_,Vis,_),..Gps],
	 tmpEnv,declared, privateVals,pTypes,imports,tEnv) =>
      phaseI(Gps,tmpEnv,declared,privateVals,pTypes,imports,tEnv).


  checkTpRules:[list[abstract],symbol,symbol,list[vSpec],dict,dict] => 
      (list[typeTree],dict).
  checkTpRules(tpRls,Pkg,Nm,polyQ,tmpEnv,tEnv) =>
      valof{
	(iFace,supers) = checkTypeDef(tpRls,tpArgs,polyQ,[],[],Pkg,tmpEnv);
	valis tEnv.push(Nm,typeBind,
			foldupType(fields(polyQ),
				   typeDef(uType(Pkg,Nm,
						 {V .. vS(_,_,V) in polyQ}),
					   [faceType(fields(iFace)),..supers])))
      }.

  checkTypeDef:[abstract,list[abstract],list[vSpec],
		list[vSpec],list[typeTree],
		symbol,dict]=>(list[vSpec],list[typeTree]).
  checkTypeDef(APPLY(IDEN('<~',_),[SQUARE(tN,tA,tL),
				   APPLY(IDEN('{}',_),mT,_)],_),
	       tpArgs,polyQ,iFace,supers,Pkg,tmpEnv) => 
      valof{
	( listlen(tpArgs)!=listlen(tA)?
	    reportError("inconsistent number of type arguments in type "<>
			SQUARE(tN,tA,tL).show()<>
			" "<>listlen(tpArgs).show()<>" expected",tL)
	);
	M = macroList(mT,
		      macroEnv(tA,
			       {IDEN(tV,noLoc)..vS(tV,_,_) in polyQ},[]));

	valis (checkTypeInterface(M,polyQ,iFace,Pkg,tmpEnv),supers)
      }.
  checkTypeDef(APPLY(IDEN('<~',_),[SQUARE(tN,tA,tL),sT],fLc),
	       tpArgs,polyQ,iFace,supers,Pkg,tmpEnv) =>
      valof{
	( listlen(tpArgs)!=listlen(tA)?
	    reportError("inconsistent number of type arguments in type "<>
			SQUARE(tN,tA,tL).show()<>
			" "<>listlen(tpArgs).show()<>" expected",tL)
	);
	sTp = macro(sT,
		    macroEnv(tA,
			     {IDEN(tV,noLoc)..vS(tV,_,_) in polyQ},[]));
	spTp = parseType(sTp,Pkg,tmpEnv,fields(polyQ));
	( iTp = typeInterface(spTp.deRef(),tEnv);
	  niFace = mergeFields(fieldsOf(iTp).ext(),iFace,tmpEnv);

	  nSupers = [spTp,..mergeSupers(typeSupers(spTp,tEnv),supers,tEnv)];
	  valis (niFace,nSupers)
	);
	valis (iFace,supers)
      }.


  private mergeFields:[list[vSpec],list[vSpec],dict]=>list[vSpec].
  mergeFields([],iFace,_) => iFace.
  mergeFields([vS(Fld,Mode,fTp),..M],iFace,tmpEnv)::
	  vS(Fld,_,oTp) in iFace =>
      valof{
	sameType(oTp,fTp,tmpEnv)
	onerror (
	 error(Bec,'fail') ->
	     reportError("type of field: "<>
			 Fld.show()<>":"<>fTp.show()<>
			 " not consistent with other declarations"
			 " of same field\nbecause "<>Bec,fLc)
	);
	valis mergeFields(M,iFace,tmpEnv)
      }.
  mergeFields([vS(Fld,Mode,fTp),..M],iFace,tmpEnv) =>
      mergeFields(M,[vS(Fld,Mode,fTp),..iFace],tmpEnv).

  private mergeSupers:[list[typeTree],list[typeTree],dict]=>list[typeTree].
  mergeSupers([],supers,_)=>supers.
  mergeSupers([uType(sPk,sNm,sA),..Sprs],supers,tEnv) =>
      valof{
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
	    );
	    valis mergeSupers(Sprs,supers,tEnv)
	| valis mergeSupers(Sprs,[uType(sPk,sNm,sA),..supers],tEnv)
	)
      }.
  
  private checkTypeInterface:[list[abstract],
			      list[vSpec],list[vSpec],symbol,dict]=>
      list[vSpec].
  checkTypeInterface([],_,iFace,_,_) => iFace.
  checkTypeInterface([APPLY(IDEN(':',_),[IDEN(Fld,fLc),Tp],_),..M],
		     polyQ,iFace,Pkg,tmpEnv) =>
      valof{
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
	    );
	    valis checkTypeInterface(M,polyQ,iFace,Pkg,tmpEnv)
	| isProgramType(xTp) ?
	    valis checkTypeInterface(M,polyQ,[vS(Fld,varBind,xTp),..iFace],
				     Pkg,tmpEnv)
	| reportError("Type of "<>Fld.show()<>" should be a program"
		      " type, not "<>xTp.show(),fLc);
	    valis checkTypeInterface(M,polyQ,iFace,Pkg,tmpEnv)
	)
      }.

  -- phaseII handles imports of stdlib and debug
  phaseIIa:[typeOpts,list[(symbol,symbol)],dict] =>
      (list[(symbol,symbol)],dict).
  phaseIIa(Opts,imports,tEnv)::Opts.isTopLevel(), \+ Opts.option(noStdlib),
	  \+ ('go.stdlib',_) in imports =>
      valof{
	(iPkg,iVers,iDefs,iType) = importPkg(Opts,'go.stdlib');
	valis phaseIIb(Opts,[(iPkg,iVers),..imports],
		       tEnv.pushDict(iDefs).pushDict(iType).push(iPkg,pkgBind,faceType(iType)))
      }.
  phaseIIa(Opts,imports,tEnv) => phaseIIb(Opts,imports,tEnv).

  phaseIIb:[list[compOpt],list[(symbol,symbol)],dict] =>
      (list[(symbol,symbol)],dict).
  phaseIIb(Opts,imports,tEnv)::Opts.isTopLevel(), 
	  Opts.option(dbg(_)), \+ ('go.debug',_) in imports =>
      valof{
	(iPkg,iVers,iDefs,iType) = importPkg(Opts,'go.debug');
	valis ([(iPkg,iVers),..imports],
	       tEnv.pushDict(iDefs).pushDict(iType).
	       push(iPkg,pkgBind,faceType(iType)))
      }.
  phaseIIb(Opts,imports,tEnv) => (imports,tEnv).

	 thePrograms:list[(symbol,typeBinding,progTree)] := [].
	 privateTypes:list[symbol] := [].
	 privateVals:list[symbol] := [].



  -- Do the type analysis on individual members of the group
  phaseIII:[list[(abstract,visibility,abstract)],
	    list[symbol],list[symbol],list[(symbol,fileLoc)],
	    dict,dict,typeOpts] => 
      list[(symbol,typeBinding,progTree)].
  phaseIII([],_,_,_,_,_,_) => [].
  phaseIII([(_,_,APPLY(IDEN('<~',Lc),[IDEN(Id,_),_],_)),..Gp],
	   pVals,pTypes,decl,sE,M,Opts) =>
      [(Id,typeBind,tDef(Id,(Id in pTypes ? privAte | pUblic),
			 sE.lookup(Id,typeBind),Lc)),..
       phaseIII(Gp,pVals,pTypes,decl,sE,M,Opts)].

  phaseIII([(_,_,APPLY(IDEN('<~',Lc),[SQUARE(IDEN(Id,_),_,_),_],_)),..Gp],
	   pVals,pTypes,decl,sE,M,Opts) =>
      [(Id,typeBind,tDef(Id,(Id in privateTypes ? privAte | pUblic),
			 sE.lookup(Id,typeBind),Lc)),..
       phaseIII(Gp,pVals,pTypes,decl,sE,M,Opts)].
  phaseIII([(_,_,APPLY(IDEN(':',_),_,_)),..Gp],
	   pVals,pTypes,decl,sE,Opts) =>
      phaseIII(Gp,pVals,pTypes,decl,sE,M,Opts).

  phaseIII([(_,_,APPLY(IDEN('=',_),[IDEN(Id,_),R],Lc)),..Gp],
	   pVals,pTypes,declared,sE,Methods,Opts) =>
      valof{
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
	    valis [(Id,varBind,cDef(Id,(Id in pVals?privAte|pUblic),
				    Const,vType,Lc)),..
		   phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)]
	| reportError(Id.show()<>" not declared",Lc);
	  valis phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)
	)
      }.
  phaseIII([(_,_,APPLY(IDEN(':=',_),[IDEN(Id,_),R],Lc)),..Gp],
	   pVals,pTypes,declared,sE,Methods,Opts) =>
      valof{
	( \+Opts.isStatefull() ?
	    reportError("variable "<>Id.show()<>" not permitted in a stateless "
			"class",Lc)
	| \+((Id,_) in declared), \+Methods.isbound(Id,_,_) ?
	    reportError("constant "<>Id.show()<>" not declared",Lc)
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
	    valis [(Id,varBind,vDef(Id,(Id in pVals?privAte|pUblic),
				    Variable,vType,Lc)),..
		   phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)]
	| reportError(Id.show()<>" not declared",Lc);
	  valis phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)
	)
      }.

  phaseIII([(_,_,APPLY(IDEN('=>',_),[IDEN(Id,Lc),TPL(Eqns,_)],_)),..Gp],
	   pVals,pTypes,declared,sE,Methods,Opts) =>
      valof{
	( \+((Id,_) in declared), \+Methods.isbound(Id,_,_) ?
	    reportError("function "<>Id.show()<>" not declared",Lc)
	);
	( sE.isbound(Id,varBind,vType) ?
	    Eqs = typeOfFunction(Eqns,vType,sE,Opts);
		 
	    valis [(Id,varBind,fnDf(Id,(Id in pVals?privAte|pUblic),
				    Eqs,vType,Lc)),..
		   phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)]
	| reportError(Id.show()<>" not declared",Lc);
	  valis phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)
	)
      }.
  phaseIII([(_,_,APPLY(IDEN(':-',_),[IDEN(Id,Lc),TPL(Clses,_)],_)),..Gp],
	   pVals,pTypes,declared,sE,Methods,Opts) =>
      valof{
	( \+((Id,_) in declared), \+Methods.isbound(Id,_,_) ?
	    reportError("relation "<>Id.show()<>" not declared",Lc)
	);
	( sE.isbound(Id,varBind,vType) ?
	    Cls = typeOfPredicate(Clses,vType,sE,Opts);
		 
	    valis [(Id,varBind,rDf(Id,(Id in pVals?privAte|pUblic),
				   Cls,vType,Lc)),..
		   phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)]
	| reportError(Id.show()<>" not declared",Lc);
	  valis phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)
	)
      }.
  phaseIII([(_,_,APPLY(IDEN(':--',_),[IDEN(Id,Lc),TPL(Clses,_)],_)),..Gp],
	   pVals,pTypes,declared,sE,Methods,Opts) =>
      valof{
	( \+((Id,_) in declared), \+Methods.isbound(Id,_,_) ?
	    reportError("relation "<>Id.show()<>" not declared",Lc)
	);
	( sE.isbound(Id,varBind,vType) ?
	    Cls = typeOfStrongPred(Clses,vType,sE,Opts);
	    
	    valis [(Id,varBind,srDf(Id,(Id in pVals?privAte|pUblic),
				    Cls,vType,Lc)),..
		   phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)]
	| reportError(Id.show()<>" not declared",Lc);
	  valis phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)
	)
      }.

  phaseIII([(_,_,APPLY(IDEN('->',_),[IDEN(Id,Lc),TPL(Rules,_)],_)),..Gp],
	   pVals,pTypes,declared,sE,Methods,Opts) =>
      valof{
	( \+((Id,_) in declared), \+Methods.isbound(Id,_,_) ?
	    reportError("procedure "<>Id.show()<>" not declared",Lc)
	);
	( sE.isbound(Id,varBind,vType) ?
	    Cls = typeOfProcedure(Rules,vType,sE,Opts);
		 
	    valis [(Id,varBind,acDf(Id,(Id in privateVals?privAte|pUblic),
				    Cls,vType,Lc)),..
		   phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)]
	| reportError(Id.show()<>" not declared",Lc);
	  valis phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)
	)
      }.

  phaseIII([(_,_,APPLY(IDEN('-->',_),[IDEN(Id,Lc),TPL(Rules,_)],_)),..Gp],
	   pVals,pTypes,declared,sE,Methods,Opts) =>
      valof{
	( \+((Id,_) in declared), \+Methods.isbound(Id,_,_) ?
	    reportError("grammar "<>Id.show()<>" not declared",Lc)
	);
	     ( sE.isbound(Id,varBind,vType) ?
		 Cls = typeOfGrammar(Rules,vType,sE,Opts);
		 
		 valis [(Id,varBind,grDf(Id,(Id in pVals?privAte|pUblic),
					 Cls,vType,Lc)),..
		   phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)]
	| reportError(Id.show()<>" not declared",Lc);
	  valis phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)
	)
      }.

  phaseIII([(_,_,APPLY(IDEN('<$',_),[IDEN(Id,Lc),TPL(Rules,_)],_)),..Gp],
	   pVals,pTypes,declared,sE,Methods,Opts) =>
      valof{
	( \+((Id,_) in declared), \+Methods.isbound(Id,_,_) ?
	    reportError("class "<>Id.show()<>" not declared",Lc)
	);
	( sE.isbound(Id,varBind,vType) ?
	    Cls = typeOfClass(Id,Rules,
			      (Id in pVals?privAte|pUblic),
			      Lc,vType,sE,Opts);
	    valis [(Id,varBind,Cls),..
		   phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)]
	| reportError(Id.show()<>" not declared",Lc);
	  valis phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)
	)
      }.
  phaseIII([(_,_,APPLY(IDEN('$',Lc),[A],_)),..Gp],
	   pVals,pTypes,declared,sE,Methods,Opts) =>
      valof{
	( \+Opts.isStatefull() ?
	    reportError("static initializer not permitted in a stateless "
			"class",Lc)
	);
	     
	Q = findVarsInTerm(A,sE);
	Init = checkAction(A,voidType,sE.pushDict(Q),Opts);
	valis [('$',varBind,iDef(Init,Lc)),..
	       phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)]
      }.
  phaseIII([(_,_,APPLY(IDEN('import',_),_,_)),..Gp],
	   pVals,pTypes,declared,sE,Methods,Opts) =>
      phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts).
  phaseIII([(_,_,X),..Gp],pVals,pTypes,declared,sE,Methods,Opts) =>
      valof{
	reportError("Cannot handle definition "<>X.show(),X.loc());
	valis phaseIII(Gp,pVals,pTypes,declared,sE,Methods,Opts)
      }.

  reQuantPhase:[list[(symbol,typeBinding,progTree)],list[symbol]]*.
  reQuantPhase([],_) -> {}.
  reQuantPhase([(Nm,Kind,Prg),..st],pTypes) ->
      (Prg.vis()=pUblic ?
	 (P in pTypes *>
	  ( isReferenced(Pkg,P,[],Prg.tpe()) ?
	      
	      reportWarning(Nm.show()<>" references private type "<>P.show(),Prg.loc())
	  )
	 )
      );
      reQuantPhase(st,pTypes).

  thetaEnv:[symbol,list[abstract],dict, dict,typeOpts]=>(list[(symbol,typeBinding,progTree)],list[(symbol,symbol)]).
  thetaEnv(Pkg,Th,Methods,Ev,Opts)::Opts.isTopLevel()=>
      gpsEnv(thetaDepend(Th),[],[],Ev.push('main',varBind,actType([(uType('go.stdlib','list',[uType('go.stdlib','list',[uType('go.stdlib','char',[])])]),biMode)])),Methods,Opts).
  thetaEnv(Pkg,Th,Methods,Ev,Opts)=>
      gpsEnv(thetaDepend(Th),[],[],Ev,Methods,Opts).

  gpsEnv([],thePrograms,imports,_,_,_) => (thePrograms,imports).
  gpsEnv([Gp,..Gps],thePrograms,imports,Env,Methods,Opts) =>
      valof{
	(declared,pVals,pTypes,gpImports,gpEnv) = phaseI(Gp,tempEnv(Gp),[],pVals,pTypes,imports,Env);
	(gImports,tEnv) = phaseIIa(Opts,gpImports,gpEnv);
	gpProgs = phaseIII(Gp,pVals,pTypes,declared,tEnv,Methods,Opts);
	valis gpsEnv(Gps,gpProgs,gImports,tEnv,Methods,Opts)
      }.
}