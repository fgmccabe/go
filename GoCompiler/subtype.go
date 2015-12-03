subtype{
  -- Implement the subtype and same type relations

  import types.
  import errors.
  import misc.

  -- These 'functions' do not return values normally.
  -- Also they raise an exception rather than print messages

  subType:[typeTree,typeTree,dict]*.
  subType(X,Y,Env) ->
      Creset = types#current(); -- The # is not strictly necessary
      ((sb(X.deRef(),Y.deRef(),Env);types#clearReset(Creset))
       onerror(
	E -> types#reset(Creset);
	    raise E
       )
      ).

  private sb:[typeTree,typeTree,dict]*.
  sb(typeVar(U@isVar()),typeVar(V@isVar()),Env) -> 
      sm(typeVar(U),typeVar(V),Env).
  sb(typeVar(U@isVar()),V,Env) -> 
      case V in (
       topType -> {}
     | voidType -> raise error(U.show()<>" not consistent with void",'fail')
     | allType(bV,bG,bT) ->
	   sb(typeVar(U),bT,Env.push(bV,tvarBind,bG))
     | _ -> nUx = glb(U.upper(),V,Env);
	   ( sb(U.lower(),nUx,Env);
	     U.setUBound(nUx) ) onerror (
	    error(Bec,'fail') ->
		raise error(U.show()<>" not consistent with "<>V.show()<>
			    "\nbecause "<>Bec,'fail')
	   )
      ).
  sb(U,typeVar(V@isVar()),Env) -> 
      case U in (
       voidType -> {}
     | topType -> raise error(U.show()<>" not consistent with top",'fail')
     | allType(bV,bG,bT) ->
	   sb(bT,typeVar(V),Env.push(bV,tvarBind,bG))
     | _ -> nG = lub(U,V.lower(),Env);
	   (sb(nG,V.upper(),Env);
	    V.setLBound(nG)) onerror (
	    error(Bec,'fail') ->
		raise error(U.show()<>" not consistent with "<>V.show()<>
			    "\nbecause "<>Bec,'fail')
	   )
      ).
  sb(voidType,_,_) -> {}.
  sb(_,topType,_) -> {}.
  sb(undef(U),undef(U),_) -> {}.	-- must be the same bound variable
  sb(uType(Pk,U,L),uType(Pk,U,M),Env) ->
      ( listlen(L)=listlen(M) ? sbTypeList(L,M,Env) | 
	raise error("arity of "<>uType(Pk,U,L).show()<>"/"<>listlen(L).show()<>
		    " not equal to arity of "<>uType(Pk,U,M).show()<>"/"<>listlen(M).show(),
		    'fail')
      ).
  sb(uType(P,U,L),uType(Q,V,M),Env) ->
      { Env.isbound(U,typeBind,uT) };
      typeDef(D,F) = freshen(uT);
      sm(uType(P,U,L),D,Env);
      ( uType(Q,V,sL) in F ?
	  sb(uType(Q,V,sL),uType(Q,V,M),Env)
      | raise error(explode(U)<>" is not a subtype of "<>explode(V),'fail')
      ).

  sb(uType(Pk,U,L),faceType(M),Env) ->
      { Env.isbound(U,typeBind,vT) };
      typeDef(D,F) = freshen(vT);
      sm(uType(Pk,U,L),D,Env);
      ( faceType(sL) in F ?
	  sb(faceType(sL),faceType(M),Env)
      | raise error(explode(U)<>" is not a subtype of "<>faceType(M).show(),'fail')
      ).
  sb(faceType(L),faceType(M),Env) ->
      M.isbound(F,_,fT) *>
      ( L.isbound(F,_,lT) ? sb(lT.deRef(),fT.deRef(),Env)
      | raise error(explode(F)<>" not implemented by "<> faceType(L).show(),'fail')).
  sb(funType(uA,uR),funType(vA,vR),Env) ->
      sbArgs(uA,vA,Env); sb(uR.deRef(),vR.deRef(),Env).
  sb(predType(uA),predType(vA),Env) ->
      sbArgs(uA,vA,Env).
  sb(actType(uA),actType(vA),Env) ->
      sbArgs(uA,vA,Env).
  sb(gramType(uA,uS),gramType(vA,S),Env) ->
      sbArgs(uA,vA,Env);
      sb(uS.deRef(),S.deRef(),Env).
  sb(U,V,_) ->
      raise error(U.show()<>" not consistent with "<>V.show(),'fail').

  sbArgs:[list[(typeTree,flowMode)],list[(typeTree,flowMode)],dict]*.
  sbArgs([],[],_)->{}.
  sbArgs([(lT,inpMode),..L],[(rT,inpMode),..R],Env) ->
      sb(rT.deRef(),lT.deRef(),Env);
      sbArgs(L,R,Env).
  sbArgs([(lT,biMode),..L],[(rT,inpMode),..R],Env) ->
      sb(rT.deRef(),lT.deRef(),Env);
      sbArgs(L,R,Env).
  sbArgs([(lT,outMode),.._],[(rT,inpMode),.._],_) ->
      raise error("output mode type argment: "<>lT.show()<>" not consistent with "
		  "input mode type argument: "<>rT.show(),'fail').
  sbArgs([(lT,outMode),..L],[(rT,outMode),..R],Env) ->
      sb(lT.deRef(),rT.deRef(),Env);
      sbArgs(L,R,Env).
  sbArgs([(lT,biMode),..L],[(rT,outMode),..R],Env) ->
      sb(lT.deRef(),rT.deRef(),Env);
      sbArgs(L,R,Env).
  sbArgs([(lT,inpMode),.._],[(rT,outMode),.._],_) ->
      raise error("input mode type argment: "<>lT.show()<>" not consistent with "
		  "output mode type argument: "<>rT.show(),'fail').
  sbArgs([(lT,inpMode),..L],[(rT,biMode),..R],Env) ->
      sb(rT.deRef(),lT.deRef(),Env);
      sbArgs(L,R,Env).
  sbArgs([(lT,biMode),..L],[(rT,biMode),..R],Env) ->
      sm(rT,lT,Env);
      sbArgs(L,R,Env).
  sbArgs([(lT,outMode),..L],[(rT,biMode),..R],Env) ->
      sb(lT.deRef(),rT.deRef(),Env);
      sbArgs(L,R,Env).

  private sbTypeList:[list[typeTree],list[typeTree],dict]*.
  sbTypeList([],[],_) -> {}.
  sbTypeList([u,..U],[v,..V],Env) -> sb(u.deRef(),v.deRef(),Env); 
      sbTypeList(U,V,Env).
  sbTypeList(U,V,_) -> raise error("not of elements in "<>U.show()<>
				     " different to that in "<>V.show(),'fail').

  -- Type unification
  sameType:[typeTree,typeTree,dict]*.
  sameType(X,Y,Env) ->
      Creset = types#current(); -- The # is not strictly necessary
      ((sm(X.deRef(),Y.deRef(),Env);types#clearReset(Creset))
       onerror(
	E -> types#reset(Creset);
	    raise E
       )
      ).

  private sm:[typeTree,typeTree,dict]*.
  sm(typeVar(U@isVar()),typeVar(V@isVar()),_) -> 
      ( U==V ? {} | U.bind(typeVar(V))).
  sm(typeVar(U@isVar()),V,_) -> 
      ( V.occIn(U) ? 
	  raise error(U.show()<>" not consistent with "<>
		      V.show()<> "\nbecause "<>U.show()<>
		      " occurs in "<>V.show(), 'fail') 
      | U.bind(V)).
  sm(U,typeVar(V@isVar()),_) -> 
      ( U.occIn(V) ? 
	  raise error(U.show()<>" not consistent with "<>
		      V.show()<> "\nbecause "<>V.show()<>
		      " occurs in "<>U.show(),'fail') 
      | V.bind(U)).
  sm(voidType,voidType,_) -> {}.
  sm(topType,topType,_)->{}.
  sm(undef(A),undef(A),_)->{}.
  sm(conType(uA,uT),conType(vA,vT),Env) ->
      sm(uT.deRef(),vT.deRef(),Env);
      smList(uA,vA,Env).
  sm(sconType(uA,uR),sconType(vA,vR),Env) ->
      sm(uR.deRef(),vR.deRef(),Env);
      smArgs(uA,vA,Env).
  sm(funType(uA,uR),funType(vA,vR),Env) ->
      sm(uR.deRef(),vR.deRef(),Env);
      smArgs(uA,vA,Env).
  sm(actType(uA),actType(vA),Env) ->
      smArgs(uA,vA,Env).
  sm(predType(uA),predType(vA),Env) ->
      smArgs(uA,vA,Env).
  sm(gramType(uA,uR),gramType(vA,vR),Env) ->
      sm(uR.deRef(),vR.deRef(),Env);
      smArgs(uA,vA,Env).
  sm(faceType(uM),faceType(vM),Env) ->
      uM.isbound(M,_,um) *>
      (vM.isbound(M,_,vm) ? 
	 sm(um.deRef(),vm.deRef(),Env)
     | raise error(M.show()<>" not implemented in "<>faceType(vM).show(),'fail')
      );
      (vM.isbound(M,_,_) *> uM.isbound(M,_,_) ? {}
     | raise error(M.show()<>" not implemented in "<>faceType(uM).show(),'fail')
      ).
  sm(uType(Pk,Nm,uA),uType(Pk,Nm,vA),Env) ->
      smList(uA,vA,Env).
  sm(allType(uN,uG,uT),allType(vN,vG,vT),Env) ->
      nEnv = Env.push(uN,tvarBind,undef(vN));
      sm(uG,vG,nEnv);
      sm(uT,vT,nEnv).
  sm(U,V,_) ->
      raise error(U.show()<>" not consistent with "<>V.show(),'fail').

  private smList:[list[typeTree],list[typeTree],dict]*.
  smList([],[],_)->{}.
  smList([u,..U],[v,..V],Env) -> sm(u.deRef(),v.deRef(),Env);
      smList(U,V,Env).

  private smArgs:[list[(typeTree,flowMode)],list[(typeTree,flowMode)],dict]*.
  smArgs([],[],_)->{}.
  smArgs([(u,_),..U],[(v,_),..V],Env)->
      sm(u.deRef(),v.deRef(),Env);
      smArgs(U,V,Env).
  
  -- Least upper bound of two type expressions
  lub:[typeTree,typeTree,dict] => typeTree.
  lub(XX,YY,Env) => 
      valof{
	CR = types#current();
	( valis lb(XX.deRef(),YY.deRef(),Env);
	  types#clearReset(CR)
	) onerror(
	 E -> types#reset(CR); raise E
	)
      }.

  private lb:[typeTree,typeTree,dict]=>typeTree.
  lb(typeVar(U@isVar()),typeVar(V@isVar()),Env) =>
      valof{
	(subType(U.upper(),V.lower(),Env); valis typeVar(V))
	onerror(
	 _ -> ( subType(V.upper(),U.lower(),Env); valis typeVar(U) )
	     onerror (
	      _ -> ( sameType(typeVar(U),typeVar(V),Env); valis typeVar(U) )
	     )
	)
      }.
  lb(typeVar(U@isVar()),V,Env) => 
      valof{
	( subType(U.upper(),V,Env); valis V)
	onerror(
	 _ -> 
	     sameType(typeVar(U),V,Env);
	     valis typeVar(U)
	)
      }.
  lb(U,typeVar(V@isVar()),Env) =>
      valof{
	( subType(V.upper(),U,Env); valis U)
	onerror(
	 _ -> sameType(U,typeVar(V),Env);
	     valis U
	)
      }.
  lb(voidType,V,_)=>V.
  lb(U,voidType,_)=>U.
  lb(topType,_,_)=>topType.
  lb(_,topType,_)=>topType.
  lb(conType(uA,uR),conType(vA,vR),Env)=>
      conType(lbList(uA,vA,Env),lb(uR.deRef(),vR.deRef(),Env)).
  lb(sconType(uA,uR),sconType(vA,vR),Env)=>
      sconType(lbArgs(uA,vA,Env),lb(uR.deRef(),vR.deRef(),Env)).
  lb(funType(uA,uR),funType(vA,vR),Env)=>
      funType(lbArgs(uA,vA,Env),lb(uR.deRef(),vR.deRef(),Env)).
  lb(predType(uA),predType(vA),Env)=>predType(lbArgs(uA,vA,Env)).
  lb(actType(uA),actType(vA),Env)=>actType(lbArgs(uA,vA,Env)).
  lb(gramType(uA,uR),gramType(vA,vR),Env)=>
      gramType(lbArgs(uA,vA,Env),lb(uR.deRef(),vR.deRef(),Env)).
  lb(uType(P,N,U),uType(P,N,V),Env) => uType(P,N,lbList(U,V,Env)).
  lb(uType(P,N,U),uType(Q,M,V),Env) => 	
      (Env.isbound(M,typeBind,mT),	 -- we need to find out if one 
       typeDef(vT,vI) = mT.deRef(),	 -- is a subtype of the other
       uType(P,N,nV) in vI,
       action{sameType(uType(Q,M,V),vT,Env); valis true}?
	 uType(P,N,lbList(U,nV,Env))
     | Env.isbound(N,typeBind,nT),
       typeDef(uT,uI) = nT.deRef(),
       action{sameType(uType(P,N,U),uT,Env); valis true},
       uType(Q,M,mV) in uI?
	 uType(Q,M,lbList(V,mV,Env))
     | raise error(uType(P,N,U).show()<>" not consistent with "<>uType(Q,M,V).show()<>
		   "\nbecause neither is a sub-type of the other",'fail')
      ).
  lb(uType(P,N,U),faceType(vM),Env) =>
      valof{
	( subType(typeInterface(uType(P,N,U),Env),faceType(vM),Env);
	  valis faceType(vM)
	) onerror (error(_,'fail') -> valis topType)
      }.
  lb(faceType(vM),uType(P,N,U),Env) =>
      valof{
	( subType(typeInterface(uType(P,N,U),Env),faceType(vM),Env);
	  valis faceType(vM)
	) onerror (error(_,'fail') -> valis topType)
      }.
  lb(faceType(uM),faceType(vM),Env) =>
      faceType(fields(lbMethods(uM.ext(),vM.ext(),Env))) onerror (
       error(Bec,'fail') => raise error(faceType(uM).show()<>
					" not consistent with "<>faceType(vM).show()<>
					"\nbecause "<>Bec,'fail')
      ).
  lb(U,V,_) =>
      raise error(U.show()<> " not consistent with "<>V.show(),'fail').

  private lbMethods:[list[vSpec], list[vSpec],dict]=> list[vSpec].
  lbMethods([],V,_) => V.
  lbMethods([vS(uN,uB,uT),..U],V,Env) :: append(Vf,[vS(uN,uB,vT),..Vb],V) =>
      [vS(uN,uB,lb(uT.deRef(),vT.deRef(),Env)),..lbMethods(U,Vf<>Vb,Env)].
  lbMethods([u,..U],V,Env) => [u,..lbMethods(U,V,Env)].

  private lbList:[list[typeTree],list[typeTree],dict]=>list[typeTree].
  lbList([],[],_)=>[].
  lbList([u,..U],[v,..V],Env)=>[lb(u.deRef(),v.deRef(),Env),..lbList(U,V,Env)].
	 
  private lbArgs:[list[(typeTree,flowMode)],list[(typeTree,flowMode)],dict]=>
      list[(typeTree,flowMode)].
  lbArgs([],[],_)=>[].
  lbArgs([(u,inpMode),..U],[(v,inpMode),..V],Env)=>
      [(glb(u.deRef(),v.deRef(),Env),inpMode),..lbArgs(U,V,Env)].
  lbArgs([(u,biMode),..U],[(v,inpMode),..V],Env)=>
      [(lub(u.deRef(),v.deRef(),Env),inpMode),..lbArgs(U,V,Env)].
  lbArgs([(u,inpMode),..U],[(v,biMode),..V],Env)=>
      [(lub(u.deRef(),v.deRef(),Env),inpMode),..lbArgs(U,V,Env)].
  lbArgs([(u,outMode),..U],[(v,outMode),..V],Env)=>
      [(lub(u.deRef(),v.deRef(),Env),outMode),..lbArgs(U,V,Env)].
  lbArgs([(u,biMode),..U],[(v,outMode),..V],Env)=>
      [(lub(u.deRef(),v.deRef(),Env),outMode),..lbArgs(U,V,Env)].
  lbArgs([(u,outMode),..U],[(v,biMode),..V],Env)=>
      [(lub(u.deRef(),v.deRef(),Env),outMode),..lbArgs(U,V,Env)].
  lbArgs([(u,inpMode),.._],[(v,outMode),.._],_) =>
      raise error(u.show()<>" not compatible with "<>v.show(),'fail').
  lbArgs([(u,outMode),.._],[(v,inpMode),.._],_) =>
      raise error(u.show()<>" not compatible with "<>v.show(),'fail').
  lbArgs([],[_,.._],_) => raise error("different arities",'fail').
  lbArgs([_,.._],[],_) => raise error("different arities",'fail').

  -- Greatest lower bound of two type expressions
  glb:[typeTree,typeTree,dict] => typeTree.
  glb(XX,YY,Env) => 
      valof{
	CR = types#current();
	( valis gb(XX.deRef(),YY.deRef(),Env);
	  types#clearReset(CR)
	) onerror(
	 E -> types#reset(CR); raise E
	)
      }.

  private gb:[typeTree,typeTree,dict]=>typeTree.
  gb(typeVar(U@isVar()),typeVar(V@isVar()),Env) =>
      valof{
	(subType(V.upper(),U.lower(),Env); valis typeVar(V))
	onerror(
	 _ -> ( subType(U.upper(),V.lower(),Env); valis typeVar(U) )
	     onerror (
	      _ -> ( sameType(typeVar(U),typeVar(V),Env); valis typeVar(U) )
	     )
	)
      }.
  gb(typeVar(U@isVar()),V,Env) => 
      valof{
	( subType(U.upper(),V,Env); valis typeVar(U))
	onerror(
	 _ -> 
	     sameType(typeVar(U),V,Env);
	     valis typeVar(U)
	)
      }.
  gb(U,typeVar(V@isVar()),Env) =>
      valof{
	( subType(V.upper(),U,Env); valis typeVar(V))
	onerror(
	 _ -> sameType(U,typeVar(V),Env);
	     valis U
	)
      }.
  gb(voidType,_,_)=>voidType.
  gb(_,voidType,_)=>voidType.
  gb(topType,V,_)=>V.
  gb(U,topType,_)=>U.
  gb(conType(uA,uR),conType(vA,vR),Env)=>
      conType(gbList(uA,vA,Env),gb(uR.deRef(),vR.deRef(),Env)).
  gb(sconType(uA,uR),sconType(vA,vR),Env)=>
      sconType(gbArgs(uA,vA,Env),gb(uR.deRef(),vR.deRef(),Env)).
  gb(funType(uA,uR),funType(vA,vR),Env)=>
      funType(gbArgs(uA,vA,Env),gb(uR.deRef(),vR.deRef(),Env)).
  gb(predType(uA),predType(vA),Env)=>predType(gbArgs(uA,vA,Env)).
  gb(actType(uA),actType(vA),Env)=>actType(gbArgs(uA,vA,Env)).
  gb(gramType(uA,uR),gramType(vA,vR),Env)=>
      gramType(gbArgs(uA,vA,Env),gb(uR.deRef(),vR.deRef(),Env)).
  gb(uType(P,N,U),uType(P,N,V),Env) => uType(P,N,gbList(U,V,Env)).
  gb(uType(P,N,U),uType(Q,M,V),Env) => 	
      (Env.isbound(M,typeBind,mT),	 -- we need to find out if one 
       typeDef(vT,vI) = mT.deRef(),	 -- is a subtype of the other
       uType(P,N,nV) in vI,
       action{ sameType(uType(Q,M,V),vT,Env) }?
	 uType(P,N,gbList(U,nV,Env))
     | Env.isbound(N,typeBind,nT),
       typeDef(uT,uI) = nT.deRef(),
       uType(Q,M,mV) in uI,
       action{ sameType(uType(P,N,U),uT,Env) }?
	 uType(Q,M,gbList(V,mV,Env))
     | raise error(uType(P,N,U).show()<>" not consistent with "<>uType(Q,M,V).show()<>
		   "\nbecause neither is a sub-type of the other",'fail')
      ).
  gb(uType(P,N,U),faceType(vM),Env) =>
      valof{
	( subType(typeInterface(uType(P,N,U),Env),faceType(vM),Env);
	  valis faceType(vM)
	) onerror (error(_,'fail') -> valis topType)
      }.
  gb(faceType(vM),uType(P,N,U),Env) =>
      valof{
	( subType(typeInterface(uType(P,N,U),Env),faceType(vM),Env);
	  valis faceType(vM)
	) onerror (error(_,'fail') -> valis topType)
      }.
  gb(faceType(uM),faceType(vM),Env) =>
      faceType(fields(gbMethods(uM.ext(),vM.ext(),Env))) onerror (
       error(Bec,'fail') => raise error(faceType(uM).show()<>
					" not consistent with "<>faceType(vM).show()<>
					"\nbecause "<>Bec,'fail')
      ).
  gb(U,V,_) =>
      raise error(U.show()<> " not consistent with "<>V.show(),'fail').

  private gbMethods:[list[vSpec], list[vSpec],dict] => list[vSpec].
  gbMethods([],V,_) => V.
  gbMethods([vS(uN,uB,uT),..U],V,Env) :: append(Vf,[vS(uN,uB,vT),..Vb],V) =>
      [vS(uN,uB,gb(uT.deRef(),vT.deRef(),Env)),..gbMethods(U,Vf<>Vb,Env)].
  gbMethods([u,..U],V,Env) => [u,..gbMethods(U,V,Env)].

  private gbList:[list[typeTree],list[typeTree],dict]=>list[typeTree].
  gbList([],[],_)=>[].
  gbList([u,..U],[v,..V],Env)=>[gb(u.deRef(),v.deRef(),Env),..gbList(U,V,Env)].
	 
  private gbArgs:[list[(typeTree,flowMode)],list[(typeTree,flowMode)],dict]=>
      list[(typeTree,flowMode)].
  gbArgs([],[],_)=>[].
  gbArgs([(u,inpMode),..U],[(v,inpMode),..V],Env)=>
      [(lub(u.deRef(),v.deRef(),Env),inpMode),..gbArgs(U,V,Env)].
  gbArgs([(u,biMode),..U],[(v,inpMode),..V],Env)=>
      [(gb(u.deRef(),v.deRef(),Env),inpMode),..gbArgs(U,V,Env)].
  gbArgs([(u,inpMode),..U],[(v,biMode),..V],Env)=>
      [(gb(u.deRef(),v.deRef(),Env),inpMode),..gbArgs(U,V,Env)].
  gbArgs([(u,outMode),..U],[(v,outMode),..V],Env)=>
      [(gb(u.deRef(),v.deRef(),Env),outMode),..gbArgs(U,V,Env)].
  gbArgs([(u,biMode),..U],[(v,outMode),..V],Env)=>
      [(gb(u.deRef(),v.deRef(),Env),outMode),..gbArgs(U,V,Env)].
  gbArgs([(u,outMode),..U],[(v,biMode),..V],Env)=>
      [(gb(u.deRef(),v.deRef(),Env),outMode),..gbArgs(U,V,Env)].
  gbArgs([(u,inpMode),.._],[(v,outMode),.._],_) =>
      raise error(u.show()<>" not compatible with "<>v.show(),'fail').
  gbArgs([(u,outMode),.._],[(v,inpMode),.._],_) =>
      raise error(u.show()<>" not compatible with "<>v.show(),'fail').
  gbArgs([],[_,.._],_) => raise error("different arities",'fail').
  gbArgs([_,.._],[],_) => raise error("different arities",'fail').

  
  typeInterface:[typeTree,dict] => typeTree.
  typeInterface(uType(P,N,A),Env)::Env.isbound(N,typeBind,Tp) => 
      valof{
	typeDef(nTp,nI).=freshen(Tp);
	sameType(uType(P,N,A),nTp,Env);
	( faceType(M) in nI ?
	    valis faceType(M)
	| raise error("no interface known for type "<>N.show(),'fail')
	)
      }.
  typeInterface(typeVar(Vr),Env) =>
      typeInterface(Vr.upper(),Env).
  typeInterface(Tp,_) =>
      raise error("no interface known for "<>Tp.show(),'fail').

  typeSupers:[typeTree,dict]=>list[typeTree].
  typeSupers(Tp,Env) =>
      case Tp.deRef() in (
       uType(Pk,Nm,A) =>
	   ((Env.isbound(Nm,typeBind,lDf), 
	     typeDef(uType(Pk,Nm,_),_).=stripForAlls(lDf)) ?
	      valof{
		typeDef(lP,lSuper) = freshen(lDf);
		sameType(lP,uType(Pk,Nm,A),Env);
		valis { T .. (T::uType(_,_,_).=T) in lSuper }
	      }
	  | [])
     | _ => raise error(Tp.show()<>" has no definition",'fail')
      ).

}
  

  
