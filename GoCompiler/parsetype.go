parsetype{
  -- Parse an abstract syntax term into a type expression
  import abstract.
  import types.
  import sttypes.
  import findvars.
  import errors.
  import subtype.
  import go.io.

  parseType:[abstract,symbol,dict,dict]=>typeTree.
  parseType(IDEN('_',_),_,_,_)=>typeVar(vrBind('_',topType)).
  parseType(IDEN('top',_),_,_,_)=>topType.
  parseType(IDEN('void',_),_,_,_)=>voidType.
  parseType(IDEN('string',_),_,_,_)=>uType('go.stdlib','list',
					   [uType('go.stdlib','char',[])]).
  parseType(IDEN('{}',_),_,_,_)=>faceType(fields([])).
  parseType(IDEN(Nm,_),_,_,Bound)::Bound.isbound(Nm,tvarBind,Tp) => Tp.
  parseType(IDEN(Nm,Lc),_,_,Bound)::Bound.isbound(Nm,typeBind,Tp) =>
      valof{
	(typeDef(uType(Pk,Nm,A),_).=stripForAlls(Tp) ?
	   ( listlen(A)==0 ?
	       valis uType(Pk,Nm,[])
	   | reportError("Type: "<>Tp.show()<>" has arity: "<>listlen(A).show(),Lc);
	     valis uType(Pk,Nm,[])
	   )
       | raise error("badly defined type: "<>Tp.show(),'eINTERNAL')
	)
      }.
  parseType(IDEN(Nm,_),_,Env,_)::Env.isbound(Nm,tvarBind,Tp) => Tp.
  parseType(IDEN(Nm,Lc),_,Env,_)::Env.isbound(Nm,typeBind,Tp) =>
      valof{
	(typeDef(uType(Pk,Nm,A),_).=stripForAlls(Tp) ?
	   ( listlen(A)==0 ?
	       valis uType(Pk,Nm,[])
	   | reportError("Type: "<>Tp.show()<>" has arity: "<>listlen(A).show(),Lc);
	     valis uType(Pk,Nm,[])
	   )
       | raise error("badly defined type: "<>Tp.show(),'eINTERNAL')
	)
      }.
  parseType(IDEN(Nm,_),Pk,_,_) => uType(Pk,Nm,[]).
  parseType(APPLY(IDEN('$=',_),[L,R],_),Pk,Env,Bound) => 
      valof{
	conArgs = parseTypeTuple(L,Pk,Env,Bound);
	( conArgs=[]?
	    valis enuType(parseType(R,Pk,Env,Bound))
	| valis  conType(conArgs,parseType(R,Pk,Env,Bound)))
      }.
  parseType(APPLY(IDEN('@=',_),[L,R],_),Pk,Env,Bound) => 
      valof{
	conArgs = parseTypeTuple(L,Pk,Env,Bound);
	( conArgs=[]?
	    valis enuType(parseType(R,Pk,Env,Bound))
	| valis  conType(conArgs,parseType(R,Pk,Env,Bound)))
      }.
  parseType(APPLY(IDEN('@>',_),[TPL(L,_),R],_),Pk,Env,Bound) => 
      sconType(parseArgTypes(L,Pk,Env,Bound,inpMode),parseType(R,Pk,Env,Bound)).
  parseType(APPLY(IDEN('=>',_),[TPL(L,_),R],_),Pk,Env,Bound) => 
      funType(parseArgTypes(L,Pk,Env,Bound,inpMode),parseType(R,Pk,Env,Bound)).
  parseType(APPLY(IDEN('-->',_),[TPL(L,_),R],_),Pk,Env,Bound) => 
      gramType(parseArgTypes(L,Pk,Env,Bound,inpMode),parseType(R,Pk,Env,Bound)).
  parseType(APPLY(IDEN('*',_),[TPL(L,_)],_),Pk,Env,Bound) => 
      actType(parseArgTypes(L,Pk,Env,Bound,inpMode)).
  parseType(BRACE(TPL(L,_),[],_),Pk,Env,Bound) => 
      predType(parseArgTypes(L,Pk,Env,Bound,biMode)).
  parseType(APPLY(IDEN('<~',Lc),[L,R],_),Pk,Env,Bound) => 
      valof{				 -- A constrained type
	rT = parseType(R,Pk,Env,Bound);
	lT = parseType(L,Pk,Env,Bound);

	( typeVar(lvT).=lT ?
	    lvT.setUBound(rT)
	| reportError("cannot constrain non-variable type: "<>lT.show(),Lc) 
	);
	valis lT
      }.
  parseType(APPLY(IDEN('{}',_),L,_),Pk,Env,Bound) => 
      faceType(fields({vS(Fld,varBind,parseType(mT,Pk,Env,Bound)) .. 
		       APPLY(IDEN(':',_),[IDEN(Fld,_),mT],_) in L})).
  parseType(APPLY(IDEN('-',_),[Q,Tp],_),Pk,Env,Bound) =>
      valof{
	QQ = pickTvars(Q);
	pTp = parseType(Tp,Pk,Env,Bound.pushList(QQ));
	valis rollupType(Q,pTp,Pk,QQ,Env,Bound)
      }.
  parseType(APPLY(IDEN(',',_),[L,R],_),Pk,Env,Bound) => 
      uType('go.stdlib',',',[parseType(L,Pk,Env,Bound),
			     parseType(R,Pk,Env,Bound)]).
  parseType(APPLY(IDEN('#',Lc),[P,SQUARE(IDEN(Nm,_),A,_)],_),Pk,Env,Bound) =>
      valof{
	pkgName = packageReference(P);
	tA = { parseType(a,Pk,Env,Bound)..a in A};
	( Env.isbound(pkgName,pkgBind,pkgTp) ?
	    Imps = fieldsOf(freshen(pkgTp));
	    (Imps.isbound(Nm,typeBind,aTp) ?
	       typeDef(uType(_,_,uA),_).=stripForAlls(aTp);
	       ( listlen(uA)!=listlen(tA) ?
		   reportError(explode(Nm)<>" type has "<>listlen(uA).show()
			       <>" parameters in package "<>
			       P.show()<>", "<>
			       listlen(tA).show()<>" parameters given",Lc)
	       | {} );
	   | reportError(explode(pkgName)<>" not seem to export the "<>
			 explode(Nm)<>" type",Lc)
	    )
	| reportError(explode(pkgName)<>" not seem to have been imported",Lc)
	);
	valis uType(pkgName,Nm,tA)
      }.
  parseType(APPLY(IDEN('#',Lc),[P,IDEN(Nm,_)],_),_,Env,_) =>
      valof{
	pkgName = packageReference(P);
	( Env.isbound(pkgName,pkgBind,pkgTp) ?
	    Imps = fieldsOf(freshen(pkgTp));
	    (Imps.isbound(Nm,typeBind,aTp) ?
	       typeDef(uType(_,_,uA),_).=stripForAlls(aTp);
	       ( listlen(uA)!=0 ?
		   reportError(explode(Nm)<>" in package "<>
			       P.show()<>" is polymorphic ",Lc)
	       | {} );
	   | reportError(explode(pkgName)<>" not seem to export the "<>
			 explode(Nm)<>" type",Lc)
	    )
	| reportError(explode(pkgName)<>" not seem to have been imported",Lc)
	);
	valis uType(pkgName,Nm,[])
      }.
  parseType(SQUARE(IDEN(Tn,L0),A,L1),Pk,Env,Bound) => 
      valof{
	tA = { parseType(a,Pk,Env,Bound)..a in A};
	( Env.isbound(Tn,typeBind,rwTp) ?
	    typeDef(uType(P,_,aA),_) .= freshen(rwTp);
	    ( sameType(uType(P,Tn,tA),uType(P,Tn,aA),Env);
	      valis uType(P,Tn,tA)
	    ) onerror (
	   error(Bec,'fail') -> 
	       reportError("Cannot parse type expression: "<>SQUARE(IDEN(Tn,L0),A,L1).show()<>
			   "\nbecause "<>Bec,L1);
	       valis uType(Pk,Tn,tA)
	  )
	| Env.isbound(Tn,tvarBind,_) ?
	    valis uType(Pk,Tn,tA)
	| reportError("type "<>explode(Tn)<>" not declared in "<>Env.show(),L0);
	  valis uType(Pk,Tn,tA)
	)
      }.
  parseType(T,_,_,_) => 
      valof{
	reportError("cannot parse this type expression: "<>T.show(),T.loc());
	valis voidType
      }.

  private parseTypeList:[abstract,symbol,dict,dict]=>list[typeTree].
  parseTypeList(APPLY(IDEN(',..',_),[L,R],_),Pk,Env,Bound) =>
      parseTypeList(L,Pk,Env,Bound)<>parseTypeList(R,Pk,Env,Bound).
  parseTypeList(IDEN('[]',_),_,_,_) =>[].
  parseTypeList(Tp,Pk,Env,Bound) => [parseType(Tp,Pk,Env,Bound)].

  private parseTypeTuple:[abstract,symbol,dict,dict]=>list[typeTree].
  parseTypeTuple(TPL(L,_),Pk,Env,Bound) => { parseType(t,Pk,Env,Bound) .. t in L}.

  private parseArgTypes:[list[abstract],symbol,dict,dict,flowMode]=>list[(typeTree,flowMode)].
  parseArgTypes(L,Pk,Env,Bound,defltMode) =>
      { parseArgType(Tp,Pk,Env,Bound,defltMode) .. Tp in L}.
  
  private parseArgType:[abstract,symbol,dict,dict,flowMode]=>(typeTree,flowMode).

  parseArgType(APPLY(IDEN('+',_),[Tp],_),Pk,Env,Bound,_) => (parseType(Tp,Pk,Env,Bound),inpMode).
  parseArgType(APPLY(IDEN('-',_),[Tp],_),Pk,Env,Bound,_) => (parseType(Tp,Pk,Env,Bound),outMode).
  parseArgType(APPLY(IDEN('-+',_),[Tp],_),Pk,Env,Bound,_) => (parseType(Tp,Pk,Env,Bound),biMode).
  parseArgType(Tp,Pk,Env,Bound,Deflt) => (parseType(Tp,Pk,Env,Bound),Deflt).

  private pickTvars:[abstract]=>list[vSpec].
  pickTvars(IDEN('[]',_))=>[].
  pickTvars(IDEN(Vr,_))=>[vS(Vr,tvarBind,undef(Vr))].
  pickTvars(APPLY(IDEN(',..',_),[L,R],_))=> pickTvars(L)<>pickTvars(R).
  pickTvars(APPLY(IDEN('<~',_),[L,_],_))=>pickTvars(L).
  pickTvars(T) => valof{
		    reportError("Invalid bound var expression: "<>T.show(),T.loc());
		    valis []
		  }.

  private rollupType:[abstract,typeTree,symbol, list[vSpec],dict,dict]=>typeTree.
  rollupType(IDEN('[]',_),Tp,_,_,_,_)=>Tp.
  rollupType(IDEN(Vr,_),Tp,_,QQ,_,_) :: vS(Vr,tvarBind,_) in QQ => 
      allType(Vr,topType,Tp).
  rollupType(APPLY(IDEN('<~',_),[IDEN(Vr,_),bTp],_),Tp,Pk,QQ,Env,Bound) :: 
	  vS(Vr,tvarBind,_) in QQ =>
      allType(Vr,parseType(bTp,Pk,Env,Bound.pushList(QQ)),Tp).
  rollupType(APPLY(IDEN(',..',_),[L,R],_),Tp,Pk,QQ,Env,Bound) =>
      rollupType(R,rollupType(L,Tp,Pk,QQ,Env,Bound),Pk,QQ,Env,Bound).

  realType:[abstract,symbol,dict]=>(dict,typeTree).
  realType(Tp,Pkg,Env) => 
      valof{
	nQ = findVarsInType(Tp,Env);
	valis (nQ,parseType(Tp,Pkg,Env,nQ))
      }.

  rlType:[abstract,symbol,dict]=>typeTree.
  rlType(Tp,Pkg,Env) => 
      valof{
	nQ = findVarsInType(Tp,Env);
	valis foldupType(nQ,parseType(Tp,Pkg,Env,nQ))
      }.

  packageReference:[abstract]=>symbol.
  packageReference(T) => implode(flatten(pkReference(T))).

  pkReference:[abstract]=>list[string].
  pkReference(IDEN(Nm,_)) => [explode(Nm)].
  pkReference(APPLY(IDEN('.',_),[L,IDEN(Fld,_)],_)) => 
      pkReference(L)<>[".",explode(Fld)].

  private flatten:[list[list[s]]] => list[s].

  flatten([]) => [].
  flatten([S,..L]) => fltn(S,L).
  
  private fltn:[list[t],list[list[t]]] => list[t].

  fltn([],L) => flatten(L).
  fltn([C,..S],L) => [C,..fltn(S,L)].
}
