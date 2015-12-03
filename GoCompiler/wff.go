/*
 * Go! well formed formedness verifier
 */
wff{
  import go.io.
  import errors.
  import opts.
  import abstract.
  import keywords.
  import misc.
  import esc.

  checkWff:[abstract,string]=>abstract.
  checkWff(E,F) => wffPackage(E,F).

  -- Main entry for well formed packages
  wffPackage:[abstract,string]=>abstract.
  wffPackage(BRACE(Mod,[B],L),F)::
    verifyModuleName(extractPackagePath(F),listIfy(Mod,'.')) => BRACE(Mod,wffTheta(B),L).
  wffPackage(APPLY(IDEN('. ',_),[Pk],_),F) =>
      wffPackage(Pk,F).
  wffPackage(X,_) => 
      valof{
	reportError("invalid package: "<>X.show(),X.loc());
	valis VOID(X.loc())
      }.

  -- Convert a file name into a package path
  extractPackagePath:[string]=>list[symbol].
  extractPackagePath(Fl)::append(Pre,".go",Fl) => {implode(Nm)..Nm in expand(Pre,"/")}.
  extractPackagePath(Fl) => {implode(Nm)..Nm in expand(Fl,"/")}.

  -- Verify that a package name is properly formed
  wffPackageName:[abstract]=>abstract.
  wffPackageName(IDEN(P,L))=>IDEN(P,L).
  wffPackageName(APPLY(IDEN('.',L0),[P,K],L))=>
      APPLY(IDEN('.',L0),[wffPackageName(P),wffPackageName(K)],L).
  wffPackageName(P)=> 
      valof{
	reportError("invalid package name: "<>P.show(),P.loc());
	valis VOID(P.loc())
      }.

  verifyModuleName:[list[symbol],list[abstract]]{}.
  verifyModuleName(Path,M) :- vModName(reverse(Path),reverse(M))!.

  vModName:[list[symbol],list[abstract]]{}.
  vModName(_,[]).
  vModName([seg,..L],[IDEN(seg,_),..M]) :- vModName(L,M).
  vModName([seg,.._],[X,.._]) :-
      action{
	reportError("Package name contains invalid segment: "<>X.show()<>"\n"<>
		    explode(seg)<>" expected",X.loc())
      }.

  -- wffTheta deals with package contents and class bodies
  wffTheta:[abstract]=>list[abstract].
  wffTheta(T) => equalize(listIfy(T,'. ')).

  -- collect definitions from the theta 
  equalize:[list[abstract]]=>list[abstract].
  equalize([]) => [].

  equalize([APPLY(IDEN('private',_),[APPLY(IDEN('::=',_),[IDEN(Vr,L2),Tp],L3)],L4),..Lx])::
    checkName(Vr,L2) => privatize(constructorTypeDef(SQUARE(IDEN(Vr,L2),[],L3),Tp),L4)<>equalize(Lx).
  equalize([APPLY(IDEN('private',L0),[APPLY(IDEN('::=',_),
                                            [SQUARE(IDEN(Vr,L2),A,L5),Tp],_)],_),..Lx])::
    checkName(Vr,L2) => privatize(constructorTypeDef(SQUARE(IDEN(Vr,L2),
                                                            {wffTypeArg(Ar)..Ar in A},L5),Tp),L0)<>
                      equalize(Lx).
  equalize([APPLY(IDEN('private',L0),[E],L1),..LL]) => 
      valof{
        RR = equalize([E,..LL]);
        ( [APPLY(Fun,[A,B],L2),..Lx].=RR ?
            valis [APPLY(Fun,[A,APPLY(IDEN('private',L0),[B],L1)],L2),..Lx]
        | valis RR
        )
      }.
  equalize([APPLY(IDEN('=>',L0),[APPLY(IDEN(':',L1),[Nm,A],L3),B],L4),..Lx]) =>
      equalize([APPLY(IDEN(':',L1),[Nm,APPLY(IDEN('=>',L0),[A,B],L4)],L3),..Lx]).
  equalize([APPLY(IDEN('-->',L0),[APPLY(IDEN(':',L1),[Nm,A],L3),B],L4),..Lx]) =>
      equalize([APPLY(IDEN(':',L1),[Nm,APPLY(IDEN('-->',L0),[A,B],L4)],L3),..Lx]).

  equalize([APPLY(IDEN(':',L0),[IDEN(Nm,L1),Tp],L2),..Lx]) ::
    checkName(Nm,L1) =>
      [APPLY(IDEN(':',L0),[IDEN(Nm,L1),wffTypeExp(Tp)],L2),..equalize(Lx)].
  equalize([APPLY(IDEN(':',L0),[Lh,_],_),..Lx]) =>
      valof{
	reportError("left hand side of type declaration: "<>Lh.show()<>" should be an "
		    "identifier",L0);
	valis equalize(Lx)
      }.

  equalize([APPLY(IDEN('=>',L0),[APPLY(IDEN('::',L1),[APPLY(IDEN(Nm,L2),A,L3),G],L4),R],L5),..Lx]) ::
    checkName(Nm,L2) =>
      extractFun([APPLY(IDEN('=>',L0),[APPLY(IDEN('::',L1),
                                             [APPLY(IDEN(Nm,L2),A,L3),G],L4),R],L5),..Lx],Nm,L0).
  equalize([APPLY(IDEN('=>',L0),[APPLY(IDEN(Nm,L2),A,L3),R],L5),..Lx]) :: checkName(Nm,L2) =>
      extractFun([APPLY(IDEN('=>',L0),[APPLY(IDEN(Nm,L2),A,L3),R],L5),..Lx],Nm,L0).

  equalize([APPLY(IDEN(':-',L0),[APPLY(IDEN('::',L1),[APPLY(IDEN(Nm,L2),A,L3),G],L4),R],L5),..Lx]) ::
    checkName(Nm,L2) =>
      extractRel([APPLY(IDEN(':-',L0),[APPLY(IDEN('::',L1),
                                             [APPLY(IDEN(Nm,L2),A,L3),G],L4),R],L5),..Lx],Nm,L0).
  equalize([APPLY(IDEN(':-',L0),[APPLY(IDEN(Nm,L2),A,L3),R],L5),..Lx]) :: checkName(Nm,L2) =>
      extractRel([APPLY(IDEN(':-',L0),[APPLY(IDEN(Nm,L2),A,L3),R],L5),..Lx],Nm,L0).
  equalize([APPLY(IDEN(':--',L0),
                  [APPLY(IDEN('::',L1),[APPLY(IDEN(Nm,L2),A,L3),G],L4),R],L5),..Lx]) ::
    checkName(Nm,L2) =>
      extractSRel([APPLY(IDEN(':--',L0),
                         [APPLY(IDEN('::',L1),
                                [APPLY(IDEN(Nm,L2),A,L3),G],L4),R],L5),..Lx],Nm,L0).
  equalize([APPLY(IDEN(':--',L0),[APPLY(IDEN(Nm,L2),A,L3),R],L5),..Lx])::checkName(Nm,L2) =>
      extractSRel([APPLY(IDEN(':--',L0),[APPLY(IDEN(Nm,L2),A,L3),R],L5),..Lx],Nm,L0).

  equalize([APPLY(IDEN('->',L0),[APPLY(IDEN('::',L1),[APPLY(IDEN(Nm,L2),A,L3),G],L4),R],L5),..Lx]) ::
    checkName(Nm,L2) =>
      extractProc([APPLY(IDEN('->',L0),[APPLY(IDEN('::',L1),
                                             [APPLY(IDEN(Nm,L2),A,L3),G],L4),R],L5),..Lx],Nm,L0).
  equalize([APPLY(IDEN('->',L0),[APPLY(IDEN(Nm,L2),A,L3),R],L5),..Lx]) :: checkName(Nm,L2) =>
      extractProc([APPLY(IDEN('->',L0),[APPLY(IDEN(Nm,L2),A,L3),R],L5),..Lx],Nm,L0).

  equalize([APPLY(IDEN('-->',L0),[APPLY(IDEN(':',L1),[IDEN(Nm,L2),A],L1a),R],L0a),..Lx]) =>
      equalize([APPLY(IDEN(':',L1),[IDEN(Nm,L2),APPLY(IDEN('-->',L0),[A,R],L1a)],L0a),..Lx]).
    
  equalize([APPLY(IDEN('-->',L0),[APPLY(IDEN(',',L1),[APPLY(IDEN(Nm,L2),A,L2a),P],L0a),
                                  G],L0b),..Lx]) :: checkName(Nm,L2) =>
      extractGrammar([APPLY(IDEN('-->',L0),[APPLY(IDEN(',',L1),[APPLY(IDEN(Nm,L2),A,L2a),P],L0a),
                                            G],L0b),..Lx],Nm,L0).
  equalize([APPLY(IDEN('-->',L0),[APPLY(IDEN(Nm,L2),A,L3),R],L5),..Lx]) :: checkName(Nm,L2) =>
      extractGrammar([APPLY(IDEN('-->',L0),[APPLY(IDEN(Nm,L2),A,L3),R],L5),..Lx],Nm,L0).

  equalize([APPLY(IDEN('<=',L0),[APPLY(IDEN(Nm,L1),A,L2),Sp],L3),..Lx]) ::
    checkName(Nm,L1) =>
      extractClass([APPLY(IDEN('<=',L0),[APPLY(IDEN(Nm,L1),A,L2),Sp],L3),..Lx],Nm,L0).
  equalize([APPLY(IDEN('<=',L0),[IDEN(Nm,L2),Sp],L3),..Lx]) :: checkName(Nm,L2) =>
      extractClass([APPLY(IDEN('<=',L0),[IDEN(Nm,L2),Sp],L3),..Lx],Nm,L0).

  equalize([APPLY(IDEN('..',L0),[IDEN(Nm,L2),Bdy],L4),..Lx]) ::
    checkName(Nm,L2) =>
      extractClass([APPLY(IDEN('..',L0),[IDEN(Nm,L2),Bdy],L4),..Lx],Nm,L0).
  equalize([APPLY(IDEN('..',L0),[APPLY(IDEN(Nm,L2),A,L5),Bdy],L4),..Lx]) ::
    checkName(Nm,L2) =>
      extractClass([APPLY(IDEN('..',L0), [APPLY(IDEN(Nm,L2),A,L5),Bdy],L4),..Lx],Nm,L0).
  equalize([APPLY(IDEN('..',_),[Hd,_],L4),..Lx]) =>
      valof{
	reportError("ill-formed class label: "<>Hd.show(),L4);
	valis equalize(Lx)
      }.

  equalize([APPLY(IDEN('<~',L0),[IDEN(Nm,L1),Tp],L2),..Lx]) ::
    checkName(Nm,L1) =>
      extractTypeDef([APPLY(IDEN('<~',L0),[SQUARE(IDEN(Nm,L1),[],L1),Tp],L2),..Lx],Nm,L0).
  equalize([APPLY(IDEN('<~',L0),[SQUARE(IDEN(Nm,L1),A,L1a),Tp],L2),..Lx]) ::
    checkName(Nm,L1) =>
      extractTypeDef([APPLY(IDEN('<~',L0),[SQUARE(IDEN(Nm,L1),A,L1a),Tp],L2),..Lx],Nm,L0).
  equalize([APPLY(IDEN('<~',L0),[Hd,_],_),..Lx]) =>
      valof{
	reportError("ill-formed type definition: "<>Hd.show(),L0);
	valis equalize(Lx)
      }.

  equalize([APPLY(IDEN('::=',_),[SQUARE(IDEN(Vr,L2),A,L1),Tp],_),..Lx])::
    checkName(Vr,L2) => constructorTypeDef(SQUARE(IDEN(Vr,L2),
                                                  {wffTypeArg(Ar)..Ar in A},L1),Tp)<>
                      equalize(Lx).
  equalize([APPLY(IDEN('::=',_),[IDEN(Vr,L2),Tp],_),..Lx])::
    checkName(Vr,L2) => constructorTypeDef(SQUARE(IDEN(Vr,L2),[],L2),Tp)<> equalize(Lx).

  equalize([APPLY(IDEN('=',L0),[APPLY(IDEN(':',L1),[IDEN(Nm,L2),Tp],L1a),Vl],L0a),..Lx])=>
      equalize([APPLY(IDEN(':',L1),[IDEN(Nm,L2),Tp],L1a),
                APPLY(IDEN('=',L0),[IDEN(Nm,L2),Vl],L0a),..Lx]).
  equalize([APPLY(IDEN('=',L0),[IDEN(Nm,L1),E],L2),..Lx]) :: checkName(Nm,L1) =>
      [APPLY(IDEN('=',L0),[IDEN(Nm,L1),wffExp(E)],L2),..equalize(Lx)].
  equalize([APPLY(IDEN('=',L0),[Lh,_],_),..Lx]) =>
      valof{
	reportError("ill-formed left hand side of constant definition: "<>Lh.show(),L0);
	valis equalize(Lx)
      }.

  equalize([APPLY(IDEN(':=',L0),[APPLY(IDEN(':',L1),[IDEN(Nm,L2),Tp],L1a),Vl],L0a),..Lx])=>
      equalize([APPLY(IDEN(':',L1),[IDEN(Nm,L2),Tp],L1a),
                APPLY(IDEN(':=',L0),[IDEN(Nm,L2),Vl],L0a),..Lx]).
  equalize([APPLY(IDEN(':=',L0),[IDEN(Nm,L1),E],L2),..Lx]) ::
    checkName(Nm,L1) =>
      [APPLY(IDEN(':=',L0),[IDEN(Nm,L1),wffExp(E)],L2),..equalize(Lx)].
  equalize([APPLY(IDEN(':=',L0),[Lh,_],_),..Lx]) =>
      valof{
	reportError("ill-formed left hand side of variable definition: "<>Lh.show(),L0);
	valis equalize(Lx)
      }.

  equalize([APPLY(IDEN('import',L0),[Pkg],L1),..Lx]) =>
      [APPLY(IDEN('import',L0),[wffPackageName(Pkg)],L1),..equalize(Lx)].

  equalize([APPLY(IDEN('$',L0),[APPLY(IDEN('{}',_),[A],_)],_),..Lx]) =>
      extractInit(Lx,L0,wffAction(A)).


  equalize([APPLY(IDEN(Nm,L2),A,L3),..Lx]) :: checkName(Nm,L2) =>
      extractRel([APPLY(IDEN(Nm,L2),A,L3),..Lx],Nm,L3).
  equalize([E,..Lx]) =>
      valof{
	reportError("ill-formed program element: "<>E.show(),E.loc());
	valis equalize(Lx)
      }.


  -- look for the the set of equations that define a given function 
  extractFun:[list[abstract],symbol,fileLoc]=>list[abstract].
  extractFun(L,Fn,Lc) => extFun(L,Fn,Lc,[]).

  extFun:[list[abstract],symbol,fileLoc,list[abstract]]=>list[abstract].

  extFun([APPLY(IDEN('=>',L0),
                [APPLY(IDEN('::',L1),[APPLY(IDEN(Fn,L2),A,L3),G],
                       L4),R],L5),..Lx],Fn,Lc,soFar) =>
      extFun(Lx,Fn,Lc,[APPLY(IDEN('=>',L0),[
                                  APPLY(IDEN('::',L1),
                                        [APPLY(IDEN(Fn,L2),{wffPtn(Ar)..Ar in A},L3),
                                         wffGoal(G)],L4),wffExp(R)],L5),..soFar]).
  extFun([APPLY(IDEN('=>',L0),[APPLY(IDEN(Fn,L2),A,L3),R],L5),..Lx],Fn,Lc,soFar) =>
      extFun(Lx,Fn,Lc,[APPLY(IDEN('=>',L0),[APPLY(IDEN(Fn,L2),{wffPtn(Ar)..Ar in A},L3),
                                            wffExp(R)],L5),..soFar]).
  extFun(Lx,Fn,Lc,Eqns)=>
      [APPLY(IDEN('=>',Lc),[IDEN(Fn,Lc),TPL(reverse(Eqns),Lc)],Lc),..equalize(Lx)].

  -- look for the set of clauses that define a relation
  extractRel:[list[abstract],symbol,fileLoc] => list[abstract].
  extractRel(L,Fn,Lc) => extRel(L,Fn,Lc,[]).

  extRel:[list[abstract],symbol,fileLoc,list[abstract]]=>list[abstract].
  extRel([APPLY(IDEN(':-',L0),[APPLY(IDEN('::',_),
                                     [APPLY(IDEN(Fn,L2),A,L3),G],L4),
                               R],L5),..Lx],Fn,Lc,soFar) =>
      extRel(Lx,Fn,Lc,[APPLY(IDEN(':-',L0),[APPLY(IDEN(Fn,L2),{wffPtn(Ar)..Ar in A},L3),
                                            TPL([wffGoal(G),wffGoal(R)],L4)],L5),..soFar]).

  extRel([APPLY(IDEN(':-',L0),[APPLY(IDEN(Fn,L2),A,L3),R],L5),..Lx],Fn,Lc,soFar) =>
      extRel(Lx,Fn,Lc,[APPLY(IDEN(':-',L0),[APPLY(IDEN(Fn,L2),{wffPtn(Ar)..Ar in A},L3),
                                            wffGoal(R)],L5),..soFar]).
  extRel([APPLY(IDEN(':--',L0),[APPLY(IDEN(Fn,_),_,_),_],_),..Lx],Fn,Lc,soFar) =>
      valof{
	reportError("may not mix strong and regular clauses",L0);
	valis extRel(Lx,Fn,Lc,soFar)
      }.
  extRel([APPLY(IDEN(':--',L0),[APPLY(IDEN('::',_),
                                      [APPLY(IDEN(Fn,_),_,_),_],_),_],_),..Lx],
	 Fn,Lc,soFar)=>
      valof{
	reportError("may not mix strong and regular clauses",L0);
	valis extRel(Lx,Fn,Lc,soFar)
      }.
  extRel([APPLY(IDEN(Fn,L2),A,L3),..Lx],Fn,Lc,soFar) =>
      extRel(Lx,Fn,Lc,
             [APPLY(IDEN(':-',L2),
                    [APPLY(IDEN(Fn,L2),{wffPtn(Ar)..Ar in A},L3),
                     IDEN('true',L3)],L3),..soFar]).
  extRel(Lx,Fn,Lc,Clses)=>
      [APPLY(IDEN(':-',Lc),[IDEN(Fn,Lc),TPL(reverse(Clses),Lc)],Lc),..equalize(Lx)].
 
  -- look for a strong relation
  extractSRel:[list[abstract],symbol,fileLoc]=>list[abstract].
  extractSRel(L,Fn,Lc) => extSRel(L,Fn,Lc,[]).

  extSRel:[list[abstract],symbol,fileLoc,list[abstract]]=>list[abstract].
  extSRel([APPLY(IDEN(':--',L0),[APPLY(IDEN('::',L1),
                                       [APPLY(IDEN(Fn,L2),A,L3),G],L4),
                                 R],L5),..Lx],Fn,Lc,soFar)=>
      extSRel(Lx,Fn,Lc,[APPLY(IDEN(':--',L0),[
                                   APPLY(IDEN('::',L1),
                                         [APPLY(IDEN(Fn,L2),{wffPtn(Ar)..Ar in A},L3),
                                          wffGoal(G)],L4),wffGoal(R)],L5),..soFar]).
  extSRel([APPLY(IDEN(':--',L0),[APPLY(IDEN(Fn,L2),A,L3),R],L5),..Lx],Fn,Lc,soFar) =>
      extSRel(Lx,Fn,Lc,[APPLY(IDEN(':--',L0),[
                                   APPLY(IDEN(Fn,L2),{wffPtn(Ar)..Ar in A},L3),
                                   wffGoal(R)],L5),..soFar]).
  extSRel([APPLY(IDEN(':-',L0),[APPLY(IDEN(Fn,_),_,_),_],_),..Lx],Fn,Lc,soFar) =>
      valof{
	reportError("may not mix strong and regular clauses",L0);
	valis extSRel(Lx,Fn,Lc,soFar)
      }.
  extSRel([APPLY(IDEN(Fn,L0),_,_),..Lx],Fn,Lc,soFar)=>
      valof{
	reportError("may not mix strong and regular clauses",L0);
	valis extSRel(Lx,Fn,Lc,soFar)
      }.
  extSRel(Lx,Fn,Lc,Clses)=>
      [APPLY(IDEN(':--',Lc),[IDEN(Fn,Lc),TPL(reverse(Clses),Lc)],Lc),..equalize(Lx)].

  -- look for an action procedure
  extractProc:[list[abstract],symbol,fileLoc]=>list[abstract].
  extractProc(L,Fn,Lc) => extProc(L,Fn,Lc,[]).

  extProc:[list[abstract],symbol,fileLoc,list[abstract]]=>list[abstract].
  extProc([APPLY(IDEN('->',L0),[APPLY(IDEN('::',L1),
                                      [APPLY(IDEN(Fn,L2),A,L3),G],L4),
                                R],L5),..Lx],Fn,Lc,soFar) =>
      extProc(Lx,Fn,Lc,[APPLY(IDEN('->',L0),
                              [APPLY(IDEN('::',L1),
                                     [APPLY(IDEN(Fn,L2),{wffPtn(Ar)..Ar in A},L3),
                                      wffGoal(G)],L4),wffAction(R)],L5),..soFar]).
  extProc([APPLY(IDEN('->',L0),[APPLY(IDEN(Fn,L2),A,L3),R],L5),..Lx],Fn,Lc,soFar) =>
      extProc(Lx,Fn,Lc,[APPLY(IDEN('->',L0),[APPLY(IDEN(Fn,L2),{wffPtn(Ar)..Ar in A},L3),
                                             wffAction(R)],L5),..soFar]).
  extProc(Lx,Fn,Lc,soFar) =>
      [APPLY(IDEN('->',Lc),[IDEN(Fn,Lc),TPL(reverse(soFar),Lc)],Lc),..equalize(Lx)].

  extractGrammar:[list[abstract],symbol,fileLoc]=>list[abstract].

  extractGrammar(L,Fn,Lc) => extGram(L,Fn,Lc,[]).

  extGram:[list[abstract],symbol,fileLoc,list[abstract]]=>list[abstract].
  extGram([APPLY(IDEN('-->',L0),[APPLY(IDEN(Fn,L2),A,L3),R],L5),..Lx],Fn,Lc,soFar) =>
      extGram(Lx,Fn,Lc,[APPLY(IDEN('-->',L0),
                              [APPLY(IDEN(Fn,L2),{wffPtn(Ar)..Ar in A},L3),
                               wffGram(R)],L5),..soFar]).
  
  extGram([APPLY(IDEN('-->',L0),[APPLY(IDEN(',',L0a),[
                                            APPLY(IDEN(Fn,L2),A,L3),
                                            PB],L4),
                                 R],L5),..Lx],Fn,Lc,soFar) =>
      extGram(Lx,Fn,Lc,[APPLY(IDEN('-->',L0),[APPLY(IDEN(',',L0a),[
                                                         APPLY(IDEN(Fn,L2),
                                                               {wffPtn(Ar)..Ar in A},L3),
                                                         wffTerminal(PB)],L4),
                                              wffGram(R)],L5),..soFar]).
  extGram(Lx,Fn,Lc,soFar) => [APPLY(IDEN('-->',Lc),
                                    [IDEN(Fn,Lc),TPL(reverse(soFar),Lc)],Lc),..equalize(Lx)].

  -- pick up all the initialization actions into a single initialization
  extractInit:[list[abstract],fileLoc,abstract] => list[abstract].
  extractInit(Lx,Lc,First) => extInit(Lx,Lc,First,[]).

  extInit:[list[abstract],fileLoc,abstract,list[abstract]]=>list[abstract].
  extInit([APPLY(IDEN('$',L0),[APPLY(IDEN('{}',_),[Act],_)],_),..Lx],Lc,Acts,Rest) =>
      extInit(Lx,Lc,APPLY(IDEN(',',L0),[Acts,wffAction(Act)],L0),Rest).
  extInit([Other,..Lx],Lc,Acts,Rest) => extInit(Lx,Lc,Acts,[Other,..Rest]).
  extInit([],Lc,Acts,Rest) =>
      [APPLY(IDEN('$',Lc),[Acts],Lc),..equalize(reverse(Rest))].

  -- pick up a class definition
  extractClass:[list[abstract],symbol,fileLoc] => list[abstract].
  extractClass(Lx,Fn,Lc) => 
      reformatClass(extClass(Lx,Fn,Lc,VOID(noLoc),[]),Fn,Lc).

  extClass:[list[abstract],symbol,fileLoc,abstract,list[abstract]]=>
      (list[abstract],abstract,list[abstract]).
  extClass([APPLY(IDEN('<=',L0),[APPLY(IDEN(Fn,L2),A,L3),Sp],L4),..Lx],Fn,Lc,Body,soFar) =>
      extClass(Lx,Fn,Lc,Body,
               [APPLY(IDEN('<=',L0),[APPLY(IDEN(Fn,L2),{wffPtn(Ar)..Ar in A},L3),
                                     wffExp(Sp)],L4),..soFar]).
  extClass([APPLY(IDEN('<=',L0),[IDEN(Fn,L2),Sp],L4),..Lx],Fn,Lc,Body,soFar) =>
      extClass(Lx,Fn,Lc,Body,[APPLY(IDEN('<=',L0),[IDEN(Fn,L2),wffExp(Sp)],L4),..soFar]).

  extClass([APPLY(IDEN('..',L0), [IDEN(Nm,L2),B],L3),..Lx],Fn,Lc,VOID(_),soFar) =>
      extClass(Lx,Fn,Lc,
               case B in (
                APPLY(IDEN('{}',Lb0),[cB],Lb1) =>
                    APPLY(IDEN('..',L0),
                          [IDEN(Nm,L2),APPLY(IDEN('{}',Lb0),wffTheta(cB),Lb1)],L3)
              | IDEN('{}',Lb0) =>
                    APPLY(IDEN('..',L0),
                          [IDEN(Nm,L2),APPLY(IDEN('{}',Lb0),[],Lb0)],L3)
              | xB =>
		    valof{
		      reportError("invalid body of class: "<>xB.show(),L0);
		      valis VOID(L0)
		    }
               ),soFar).
  extClass([APPLY(IDEN('..',L0),[IDEN(Fn,_),_],_),..Lx],Fn,Lc,APPLY(Lb,Bd,Lo),soFar)=>
      valof{
	reportError("multiple class body found, prior location at: "<>
		    Lo.show(),L0);
	valis extClass(Lx,Fn,Lc,APPLY(Lb,Bd,Lo),soFar)
      }.

  extClass([APPLY(IDEN('..',L0),[APPLY(IDEN(Nm,L2),A,L2A),B],L3),..Lx],Fn,Lc,
           VOID(_),soFar) =>
      extClass(Lx,Fn,Lc,
               case B in (
                APPLY(IDEN('{}',Lb0),[cB],Lb1) =>
                    APPLY(IDEN('..',L0),
                          [APPLY(IDEN(Nm,L2),wffConPtns(A),L2A),
                           APPLY(IDEN('{}',Lb0),wffTheta(cB),Lb1)],L3)
              | IDEN('{}',Lb0) =>
                    APPLY(IDEN('..',L0),
                          [APPLY(IDEN(Nm,L2),wffConPtns(A),L2A),
                           APPLY(IDEN('{}',Lb0),[],Lb0)],L3)
              | xB => 
		    valof{
		      reportError("invalid body of class: "<>xB.show(),L0);
		      valis VOID(L0)
		    }
               ),soFar).

  extClass([APPLY(IDEN('..',L0),[APPLY(IDEN(Fn,_),_,_),_],_),..Lx],Fn,
           Lc,APPLY(Lbl,Bd,Lo),soFar) => 
      valof{
	reportError("multiple class body found, prior location at: "<>
		    Lo.show(),L0);
	valis extClass(Lx,Fn,Lc,APPLY(Lbl,Bd,Lo),soFar)
      }.

  extClass(Lx,Fn,Lc,VOID(_),soFar) =>
      valof{
	reportError("class body of "<>explode(Fn)<>" appears to be missing",Lc);
	valis (reverse(soFar),VOID(Lc),Lx)
      }.
  extClass(Lx,_,_,Body,soFar) => (reverse(soFar),Body,Lx).

  private reformatClass:[(list[abstract],abstract,list[abstract]),
			 symbol,fileLoc]=> list[abstract].

  reformatClass((Rules,VOID(_),Rest),Fn,Lc) =>
      [APPLY(IDEN('<$',Lc),[IDEN(Fn,Lc),TPL(Rules,Lc)],Lc),..equalize(Rest)].
      
  reformatClass(([],(Body::APPLY(IDEN('..',_),[Hd,_],_).=Body),Rest),Fn,Lc) =>
      [APPLY(IDEN('<$',Lc),[IDEN(Fn,Lc),
			    TPL([APPLY(IDEN('<=',Lc),[Hd,IDEN('thing',Lc)],Lc),
				 Body],Lc)],Lc),..equalize(Rest)].
  reformatClass((Rules,Body,Rest),Fn,Lc)=>
      [APPLY(IDEN('<$',Lc),[IDEN(Fn,Lc),TPL(Rules<>[Body],Lc)],Lc),..equalize(Rest)].
 
  wffConPtns:[list[abstract]]=>list[abstract].
  wffConPtns([])=>[].
  wffConPtns([Ptn,..A]) =>[wffPtn(Ptn),..wffConPtns(A)].
      
  checkName:[symbol,fileLoc]{}.
  checkName(Vr,L):-
      (isKeyword(Vr)?
         action { reportError("unexpected keyword: "<>explode(Vr),L) }
      |isEscape(Vr)?
         action { reportWarning("redefining built-in: "<>explode(Vr),L) }
     | true).
      
  wffExp:[abstract]=>abstract.
  wffExp(IDEN('_',Lc)) => IDEN(genNew("_"),Lc).
  wffExp(IDEN('[]',Lc)) => IDEN('[]',Lc).
  wffExp(IDEN('false',Lc)) => IDEN('false',Lc).
  wffExp(IDEN('true',Lc)) => IDEN('true',Lc).
  wffExp(IDEN('this',Lc)) => IDEN('this',Lc).
  wffExp(IDEN(Ky,Lc)):: isKeyword(Ky) =>
      valof{
	reportError("unexpected keyword: "<>explode(Ky),Lc);
	valis IDEN(Ky,Lc)
      }.
  wffExp(IDEN(Nm,Lc)) => IDEN(Nm,Lc).
  wffExp(INT(Nm,Lc)) => INT(Nm,Lc).
  wffExp(FLT(Nm,Lc)) => FLT(Nm,Lc).
  wffExp(SYM(Sy,Lc)) => SYM(Sy,Lc).
  wffExp(CHR(C,Lc)) => CHR(C,Lc).
  wffExp(STR(St,Lc)) => STR(St,Lc).
  wffExp(APPLY(IDEN('::',L0),[L,G],L1)) => APPLY(IDEN('::',L0),[wffExp(L),wffGoal(G)],L1).
  wffExp(APPLY(IDEN(':',L0),[L,T],L1)) => APPLY(IDEN(':',L0),[wffExp(L),wffTypeExp(T)],L1).
  wffExp(APPLY(IDEN('^',L0),[L,T],L1)) => 
      APPLY(IDEN('^',L0),[wffExp(L),wffTypeExp(T)],L1).
  wffExp(APPLY(IDEN('.',L0),[R,F],L1)) => APPLY(IDEN('.',L0),[wffExp(R),wffExp(F)],L1).
  -- massage package references around a bit
  wffExp(APPLY(IDEN('#',L0),[P,APPLY(IDEN('.',L1),[O,F],L2)],L3)) =>
      wffExp(APPLY(IDEN('.',L1),[APPLY(IDEN('#',L0),[P,O],L2),F],L3)).
  wffExp(APPLY(IDEN('#',L0),[P,APPLY(F,A,L1)],L2)) =>
      wffExp(APPLY(APPLY(IDEN('#',L0),[P,F],L1),A,L2)).
  wffExp(APPLY(IDEN('#',L0),[P,IDEN(Nm,L1)],L2)) =>
      APPLY(IDEN('#',L0),[wffPackageName(P),IDEN(Nm,L1)],L2).
  wffExp(APPLY(IDEN('#',_),[_,E],L2)) =>
      valof{
	reportError("ill-formed package reference: "<>E.show(),L2);
	valis VOID(L2)
      }.
  wffExp(APPLY(IDEN('|',L0),[APPLY(IDEN('?',L1),[G,A],L2),B],L3)) =>
      APPLY(IDEN('|',L0),[APPLY(IDEN('?',L1),
				[wffGoal(G),wffExp(A)],L2),wffExp(B)],L3).

  wffExp(APPLY(IDEN(',..',L0),[H,T],L1)) => APPLY(IDEN(',..',L0),[wffExp(H),wffExp(T)],L1).

  wffExp(APPLY(IDEN(',',L0),[H,T],L1)) => APPLY(IDEN(',',L0),[wffExp(H),wffExp(T)],L1).

  wffExp(APPLY(IDEN('-',L0),[INT(I,_)],_)) => INT(-I,L0).

  wffExp(APPLY(IDEN('-',L0),[FLT(F,_)],_)) => FLT(-F,L0).

  wffExp(APPLY(IDEN('-',L0),[E],L1)) => APPLY(IDEN('-',L0),[INT(0,L0),wffExp(E)],L1).

  wffExp(APPLY(IDEN('-',L0),[D,E],L1)) => APPLY(IDEN('-',L0),[wffExp(D),wffExp(E)],L1).

  wffExp(APPLY(IDEN('+',L0),[D,E],L1)) => APPLY(IDEN('+',L0),[wffExp(D),wffExp(E)],L1).

  wffExp(APPLY(IDEN('*',L0),[D,E],L1)) => APPLY(IDEN('*',L0),[wffExp(D),wffExp(E)],L1).

  wffExp(APPLY(IDEN('/',L0),[D,E],L1)) => APPLY(IDEN('/',L0),[wffExp(D),wffExp(E)],L1).

  wffExp(APPLY(IDEN('{}',L0),[APPLY(IDEN('..',L1),[E,S],L2)],L3)) =>
      APPLY(IDEN('{}',L0),[APPLY(IDEN('..',L1),[wffExp(E),wffSetMem(S)],L2)],L3).

  -- deal with an issue of operator priorities , over ..
  wffExp(APPLY(IDEN('{}',L0),[APPLY(IDEN(',',L1),[
                                         APPLY(IDEN('..',L2),[E,L],L3),R],L4)],L5)) =>
      wffExp(APPLY(IDEN('{}',L0),[APPLY(IDEN('..',L2),[E,APPLY(IDEN(',',L1),[L,R],L3)],L4)],L5)).

  wffExp(APPLY(IDEN('{}',L0),[APPLY(IDEN('||',L1),[E,G],L2)],L3)) =>
      APPLY(IDEN('{}',L0), [APPLY(IDEN('||',L1),[wffExp(E),wffGoal(G)],L2)],L3).

  wffExp(APPLY(IDEN('~',L0),[APPLY(IDEN('^',L1),[E,P],L2),W],L3)) =>
      APPLY(IDEN('.',L0),[wffExp(E),APPLY(IDEN('display',L1),[wffExp(W),wffExp(P)],L2)],L3).

  wffExp(APPLY(IDEN('~',L0),[E,W],L3)) =>
      APPLY(IDEN('.',L0),[wffExp(E),APPLY(IDEN('display',L0),[wffExp(W),INT(0,L0)],L3)],L3).

  wffExp(APPLY(IDEN('^',L0),[E,P],L3)) =>
      APPLY(IDEN('.',L0),[wffExp(E),APPLY(IDEN('display',L0),[INT(0,L0),wffExp(P)],L3)],L3).
  wffExp(APPLY(IDEN('^',L0),[E],L3)) =>
      APPLY(IDEN('.',L0),[wffExp(E),APPLY(IDEN('show',L0),[],L3)],L3).

  wffExp(APPLY(IDEN('raise',L0),[E],L1)) => APPLY(IDEN('raise',L0),[wffExp(E)],L1).

  wffExp(APPLY(IDEN('onerror',L0),[E,R],L1)) =>
      APPLY(IDEN('onerror',L0),[wffExp(E),wffCaseExp(R)],L1).

  wffExp(APPLY(IDEN('case',L0),[APPLY(IDEN('in',L1),[E,C],L2)],L3)) =>
      APPLY(IDEN('case',L0),[APPLY(IDEN('in',L1),[wffExp(E),wffCaseExp(C)],L2)],L3).
  wffExp(APPLY(IDEN('$',L0),[E],L1)) => APPLY(IDEN('$',L0),[wffExp(E)],L1).
  wffExp(APPLY(IDEN('@@',L0),[E,G],L1)) => APPLY(IDEN('@@',L0),[wffExp(E),wffGoal(G)],L1).
  wffExp(APPLY(IDEN('@',L0),[E,G],L1)) => APPLY(IDEN('@',L0),[wffExp(E),wffGoal(G)],L1).
  wffExp(APPLY(IDEN('@',L0),[G],L1)) => APPLY(IDEN('@',L0),[IDEN(genNew("_"),L0),wffGoal(G)],L1).

  -- anonymous class expressions
  wffExp(APPLY(IDEN('..',L0),[APPLY(IDEN(':',L4),[C,Tp],L5),
                              APPLY(IDEN('{}',L2),[Th],L3)],L1)) =>
      APPLY(IDEN('..',L0),[APPLY(IDEN(':',L4),[wffExp(C),wffTypeExp(Tp)],L5),
                           APPLY(IDEN('{}',L2),wffTheta(Th),L3)],L1).
  wffExp(APPLY(IDEN('..',L0),[APPLY(IDEN(':',L4),[Tp],L5),
                              APPLY(IDEN('{}',L2),[Th],L3)],L1)) =>
      APPLY(IDEN('..',L0),[APPLY(IDEN(':',L4),[wffTypeExp(Tp)],L5),
                           APPLY(IDEN('{}',L2),wffTheta(Th),L3)],L1).
  wffExp(APPLY(IDEN('..',L0),[C,APPLY(IDEN('{}',L2),[Th],L3)],L1)) =>
      APPLY(IDEN('..',L0),[wffExp(C),APPLY(IDEN('{}',L2),wffTheta(Th),L3)],L1).
  wffExp(APPLY(IDEN('..',L0),[_,Th],L1)) =>
      valof{
	reportError("class body expected in anonymous class expression: "<>Th.show(),L0);
	valis VOID(L1)
      }.

  wffExp(APPLY(IDEN('%%',L0),[IDEN(G,L1),APPLY(IDEN('~',L2),[S,R],L3)],L4))=>
      APPLY(IDEN('%%',L0),[IDEN(G,L1),APPLY(IDEN('~',L2),[wffExp(S),wffExp(R)],L3)],L4).
  wffExp(APPLY(IDEN('%%',L0),[IDEN(G,L1),S],L2))=>
      APPLY(IDEN('%%',L0),[IDEN(G,L1),wffExp(S)],L2).
  wffExp(APPLY(IDEN('%%',_),[L,_],L2)) =>
      valof{
	reportError("parse expression requires an identifier: "<>L.show(),L2);
	valis VOID(L2)
      }.

  wffExp(APPLY(IDEN(Nm,L0),_,L1)) :: isKeyword(Nm) =>
      valof{
	reportError("unexpected keyword: "<>explode(Nm),L0);
	valis VOID(L1)
      }.

  wffExp(APPLY(F,A,L0))::\+(IDEN(Nm,_).=F,isKeyword(Nm)) =>
      APPLY(wffExp(F),{wffExp(Ar)..Ar in A},L0).
  wffExp(BRACE(IDEN('valof',L0),[A],L1)) => 
      BRACE(IDEN('valof',L0),[wffAction(A)],L1).
  wffExp(BRACE(IDEN('spawn',L0),[A],L1)) => BRACE(IDEN('spawn',L0),[wffAction(A)],L1).

  wffExp(X) =>
      valof{
	reportError("ill-formed expression: "<>X.show(),X.loc());
	valis VOID(X.loc())
      }.

  -- hande the forms of membership tests in a bounded set abstraction
  wffSetMem:[abstract]=>abstract.
  wffSetMem(APPLY(IDEN('in',L0),[L,R],L1)) => APPLY(IDEN('in',L0),[wffPtn(L),wffExp(R)],L1).
  wffSetMem(APPLY(IDEN(',',L0),[L,R],L1)) => 
      APPLY(IDEN(',',L0),[wffSetMem(L),wffSetMem(R)],L1).
  wffSetMem(X) =>
      valof{
	reportError("ill-formed set membership clause: "<>X.show(),X.loc());
	valis VOID(X.loc())
      }.

  -- handle case expression rules and onerror rules
  wffCaseExp:[abstract]=>abstract.
  wffCaseExp(APPLY(IDEN('=>',L0),[L,R],L1)) => APPLY(IDEN('=>',L0),[wffPtn(L),wffExp(R)],L1).
  wffCaseExp(APPLY(IDEN('|',L0),[L,R],L1)) => 
      APPLY(IDEN('|',L0),[wffCaseExp(L),wffCaseExp(R)],L1).
  wffCaseExp(X) =>
      valof{
	reportError("ill-formed case clause: "<>X.show(),X.loc());
	valis VOID(X.loc())
      }.

  -- Well formed patterns, somewhat simpler than expressions, only data values permitted
  wffPtn:[abstract]=>abstract.
  wffPtn(IDEN('_',Lc)) => IDEN(genNew("_"),Lc).
  wffPtn(IDEN('[]',Lc)) => IDEN('[]',Lc).
  wffPtn(IDEN('false',Lc)) => IDEN('false',Lc).
  wffPtn(IDEN('true',Lc)) => IDEN('true',Lc).
  wffPtn(IDEN('this',Lc)) => IDEN('this',Lc).
  wffPtn(IDEN(Ky,Lc)):: isKeyword(Ky) =>
      valof{
	reportError("unexpected keyword: "<>explode(Ky),Lc);
	valis VOID(Lc)
      }.
  wffPtn(IDEN(Nm,Lc)) => IDEN(Nm,Lc).
  wffPtn(INT(Nm,Lc)) => INT(Nm,Lc).
  wffPtn(FLT(Nm,Lc)) => FLT(Nm,Lc).
  wffPtn(SYM(Sy,Lc)) => SYM(Sy,Lc).
  wffPtn(CHR(C,Lc)) => CHR(C,Lc).
  wffPtn(STR(St,Lc)) => STR(St,Lc).
  wffPtn(APPLY(IDEN('::',L0),[L,G],L1)) => APPLY(IDEN('::',L0),[wffPtn(L),wffGoal(G)],L1).
  wffPtn(APPLY(IDEN(':',L0),[L,T],L1)) => APPLY(IDEN(':',L0),[wffPtn(L),wffTypeExp(T)],L1).
  wffPtn(APPLY(IDEN('^',L0),[L,T],L1)) => APPLY(IDEN('^',L0),[wffPtn(L),wffTypeExp(T)],L1).

  wffPtn(APPLY(IDEN(',..',L0),[H,T],L1)) => APPLY(IDEN(',..',L0),[wffPtn(H),wffPtn(T)],L1).
  wffPtn(APPLY(IDEN(',',L0),[H,T],L1)) => APPLY(IDEN(',',L0),[wffPtn(H),wffPtn(T)],L1).

  -- negative literal numbers

  wffPtn(APPLY(IDEN('-',L0),[INT(I,_)],_)) => INT(-I,L0).

  wffPtn(APPLY(IDEN('-',L0),[FLT(F,_)],_)) => FLT(-F,L0).

  -- package references
  wffPtn(APPLY(IDEN('#',L0),[P,IDEN(Nm,L1)],L2)) =>
      APPLY(IDEN('#',L0),[wffPackageName(P),IDEN(Nm,L1)],L2).
  wffPtn(APPLY(IDEN('#',_),[_,E],L2)) =>
      valof{
	reportError("ill-formed package reference: "<>E.show(),L2);
	valis VOID(L2)
      }.

  wffPtn(APPLY(IDEN('$',L0),[E],L1)) => APPLY(IDEN('$',L0),[wffPtn(E)],L1).
  wffPtn(APPLY(IDEN('@@',L0),[E,G],L1)) => APPLY(IDEN('@@',L0),[wffPtn(E),wffGoal(G)],L1).
  wffPtn(APPLY(IDEN('@',L0),[E,G],L1)) => APPLY(IDEN('@',L0),[wffPtn(E),wffGoal(G)],L1).
  wffPtn(APPLY(IDEN('@',L0),[G],L1)) => APPLY(IDEN('@',L0),
                                              [IDEN(genNew("_"),L0),wffGoal(G)],L1).

  wffPtn(APPLY(IDEN(Nm,L0),_,L1)) :: isKeyword(Nm) =>
      valof{
	reportError("unexpected keyword: "<>explode(Nm)<>" in pattern",L0);
	valis VOID(L1)
      }.

  wffPtn(APPLY(F,A,L0))::\+(IDEN(Nm,_).=F,isKeyword(Nm)) =>
      APPLY(wffPtn(F),{wffPtn(Ar)..Ar in A},L0).

  wffPtn(X) =>
      valof{
	reportError("ill-formed pattern: "<>X.show(),X.loc());
	valis VOID(X.loc())
      }.

  -- well formed relation conditions
  wffGoal:[abstract]=>abstract.
  wffGoal(IDEN('true',L0)) => IDEN('true',L0).
  wffGoal(IDEN('false',L0)) => IDEN('false',L0).
  wffGoal(IDEN('fail',L0)) => IDEN('false',L0).
  wffGoal(IDEN(Nm,L0))=> APPLY(IDEN('=',L0),[IDEN(Nm,L0),IDEN('true',L0)],L0).
  wffGoal(APPLY(IDEN(',',Lc),[L,R],L0)) => APPLY(IDEN(',',Lc),[wffGoal(L),wffGoal(R)],L0).
  wffGoal(APPLY(IDEN('!',L0),[G],L1)) => APPLY(IDEN('!',L0),[wffGoal(G)],L1).
  wffGoal(APPLY(IDEN('\\+',L0),[G],L1)) => APPLY(IDEN('\\+',L0),[wffGoal(G)],L1).
  wffGoal(APPLY(IDEN('in',L0),[E,S],L1)) => APPLY(IDEN('in',L0),[wffPtn(E),wffExp(S)],L1).
  wffGoal(APPLY(IDEN('=',L0),[L,R],L1)) => APPLY(IDEN('=',L0),[wffExp(L),wffExp(R)],L1).
  wffGoal(APPLY(IDEN('==',L0),[L,R],L1)) => APPLY(IDEN('==',L0),[wffExp(L),wffExp(R)],L1).
  wffGoal(APPLY(IDEN('\\=',L0),[L,R],L1)) => APPLY(IDEN('!=',L0),[wffExp(L),wffExp(R)],L1).
  wffGoal(APPLY(IDEN('!=',L0),[L,R],L1)) => APPLY(IDEN('!=',L0),[wffExp(L),wffExp(R)],L1).
  wffGoal(APPLY(IDEN('.=',L0),[L,R],L1)) => APPLY(IDEN('.=',L0),[wffPtn(L),wffExp(R)],L1).
  wffGoal(APPLY(IDEN('<=',L0),[L,R],L1)) => APPLY(IDEN('<=',L0),[wffExp(L),wffExp(R)],L1).

  wffGoal(APPLY(IDEN('-->',L0),[L,APPLY(IDEN('~',L2),[S,R],L3)],L1)) => 
      APPLY(IDEN('-->',L0),[wffGram(L),APPLY(IDEN('~',L2),[wffExp(S),wffExp(R)],L3)],L1).
  wffGoal(APPLY(IDEN('-->',L0),[L,S],L1)) => 
      APPLY(IDEN('-->',L0),[wffGram(L),APPLY(IDEN('~',L1),[wffExp(S),IDEN('[]',L1)],L1)],L1).
  wffGoal(APPLY(IDEN('?',L0),[L,R],L1)) => APPLY(IDEN('?',L0),[wffGoal(L),wffGoal(R)],L1).
  wffGoal(APPLY(IDEN('|',L0),[L,R],L1)) => APPLY(IDEN('|',L0),[wffGoal(L),wffGoal(R)],L1).
  wffGoal(APPLY(IDEN('*>',L0),[L,R],L1)) => APPLY(IDEN('*>',L0),[wffGoal(L),wffGoal(R)],L1).
  wffGoal(APPLY(IDEN('.',L0),[L,IDEN(Fld,L2)],L1)) => 
      wffGoal(APPLY(IDEN('=',L0),[APPLY(IDEN('.',L0),[L,IDEN(Fld,L2)],L1),IDEN('true',L0)],L1)).
  wffGoal(APPLY(IDEN('.',L0),[R,F],L1)) => APPLY(IDEN('.',L0),[wffExp(R),wffGoal(F)],L1).
  wffGoal(APPLY(IDEN('#',L0),[P,APPLY(G,A,L2)],L1)) =>
      wffGoal(APPLY(APPLY(IDEN('#',L0),[P,G],L1),A,L2)).
  wffGoal(APPLY(IDEN('onerror',L0),[G,E],L1)) => 
      APPLY(IDEN('onerror',L0),[wffGoal(G),wffGoalError(E)],L1).
  wffGoal(APPLY(IDEN('raise',L0),[L],L1)) => APPLY(IDEN('raise',L0),[wffExp(L)],L1).
  wffGoal(APPLY(IDEN('{}',_),[G],_)) => wffGoal(G).
  wffGoal(BRACE(IDEN('action',L0),[A],L1)) => BRACE(IDEN('action',L0),[wffAction(A)],L1).
  wffGoal(APPLY(IDEN(Ky,L0),_,L1)):: isKeyword(Ky) => 
      valof{
	reportError("unexpected keyword: "<>explode(Ky)<>" in goal",L0);
	valis VOID(L1)
      }.
  wffGoal(APPLY(P,A,L0)) => APPLY(wffExp(P),{wffExp(Ar)..Ar in A},L0).
  wffGoal(G)=> valof{
		 reportError("ill-formed goal: "<>G.show(),G.loc());
		 valis VOID(G.loc()) 
	       }.

  -- the error handler for a goal
  wffGoalError:[abstract]=>abstract.
  wffGoalError(APPLY(IDEN(':-',L0),[L,R],L1)) => APPLY(IDEN('=>',L0),[wffPtn(L),wffGoal(R)],L1).
  wffGoalError(APPLY(IDEN('|',L0),[L,R],L1)) => 
      APPLY(IDEN('|',L0),[wffGoalError(L),wffGoalError(R)],L1).
  wffGoalError(X) =>
      valof{
	reportError("ill-formed error handler: "<>X.show(),X.loc());
	valis VOID(X.loc())
      }.

  -- A well formed action
  wffAction:[abstract]=>abstract.
  wffAction(APPLY(IDEN(';',L0),[L,R],L1)) => APPLY(IDEN(';',L0),[wffAction(L),wffAction(R)],L1).
  wffAction(APPLY(IDEN(';',L0),[L],L1)) => APPLY(IDEN(';',L0),[wffAction(L)],L1).
  wffAction(IDEN('{}',L0)) => IDEN('{}',L0).
  wffAction(APPLY(IDEN('{}',L0),[G],L1)) => APPLY(IDEN('{}',L0),[wffGoal(G)],L1).
  wffAction(APPLY(IDEN('!',L0),[G],L1)) => APPLY(IDEN('{}',L0),[wffGoal(G)],L1).
  wffAction(APPLY(IDEN('\\+',L0),[G],L1)) => APPLY(IDEN('\\+',L0),[wffGoal(G)],L1).
  wffAction(APPLY(IDEN('::',L0),[A,G],L1)) => APPLY(IDEN('::',L0),[wffAction(A),wffGoal(G)],L1).
  wffAction(APPLY(IDEN('=',L0),[L,R],L1)) => APPLY(IDEN('=',L0),[wffExp(L),wffExp(R)],L1).
  wffAction(APPLY(IDEN(':=',L0),[IDEN(Nm,L2),R],L1)) => APPLY(IDEN(':=',L0),[IDEN(Nm,L2),wffExp(R)],L1).
  wffAction(APPLY(IDEN(':=',L0),[L,R],L1)) =>
      valof{
	reportError("lhs of assignment: "<>L.show()<>" should be an identifier",L0);
	valis APPLY(IDEN(':=',L0),[wffExp(L),wffExp(R)],L1)
      }.
  wffAction(APPLY(IDEN('%=',L0),[IDEN(Nm,L2),R],L1)) => APPLY(IDEN('%=',L0),[IDEN(Nm,L2),wffExp(R)],L1).
  wffAction(APPLY(IDEN('%=',L0),[L,R],L1)) =>
      valof{
	reportError("lhs of assignment: "<>L.show()<>" should be an identifier",L0);
	valis  APPLY(IDEN(':=',L0),[wffExp(L),wffExp(R)],L1)
      }.
  wffAction(APPLY(IDEN('valis',L0),[E],L1)) => APPLY(IDEN('valis',L0),[wffExp(E)],L1).
  wffAction(APPLY(IDEN('*>',L0),[L,R],L1)) => APPLY(IDEN('*>',L0),[wffGoal(L),wffAction(R)],L1).
  wffAction(APPLY(IDEN('case',L0),[APPLY(IDEN('in',L1),[E,C],L2)],L3)) =>
      APPLY(IDEN('case',L0),[APPLY(IDEN('in',L1),[wffExp(E),wffCaseRule(C)],L2)],L3).
  wffAction(APPLY(IDEN('case',_),_,L2)) => 
      valof{
	reportError("case action takes the form case Exp in (Ptn->Action|...)",L2);
	valis VOID(L2)
      }.
  wffAction(BRACE(IDEN('spawn',L0),[A],L1)) => BRACE(IDEN('spawn',L0),[wffAction(A)],L1).
  wffAction(BRACE(APPLY(IDEN('sync',L0),[APPLY(IDEN('::',L1),[R,G],L2)],L3),[A],L4)) =>
      BRACE(APPLY(IDEN('sync',L0),[wffExp(R)],L3),[
                 APPLY(IDEN('->',L1),[APPLY(IDEN('::',L0),[VOID(L0),wffGoal(G)],L0),
				      wffAction(A)],L2)],L4).
  wffAction(BRACE(APPLY(IDEN('sync',L0),[R],L2),[],L1)) =>
      BRACE(APPLY(IDEN('sync',L0),[wffExp(R)],L2),[
                 APPLY(IDEN('->',L1),[
			    APPLY(IDEN('::',L0),[VOID(L0),IDEN('true',L0)],L0),
			    IDEN('{}',L0)],L1)],L1).
  wffAction(BRACE(IDEN('sync',L0),[],L1)) =>
      BRACE(APPLY(IDEN('sync',L0),[IDEN('this',L0)],L0),[
                 APPLY(IDEN('->',L1),[
			    APPLY(IDEN('::',L0),[VOID(L0),IDEN('true',L0)],L0),
			    IDEN('{}',L0)],L1)],L1).
  wffAction(BRACE(APPLY(IDEN('sync',L0),[R],L1),[S],L2)) =>
      BRACE(APPLY(IDEN('sync',L0),[wffExp(R)],L1),[wffSyncRules(S)],L2).
  wffAction(BRACE(IDEN('sync',L0),[S],L1)) =>
      BRACE(APPLY(IDEN('sync',L0),[IDEN('this',L0)],L1),[wffSyncRules(S)],L1).
  wffAction(APPLY(IDEN('onerror',L0),[A,E],L2)) =>
      APPLY(IDEN('onerror',L0),[wffAction(A),wffCaseRule(E)],L2).
  wffAction(APPLY(IDEN('raise',L0),[E],L2)) =>
      APPLY(IDEN('raise',L0),[wffExp(E)],L2).
  wffAction(APPLY(IDEN('|',L0),[APPLY(IDEN('?',L1),[G,A],L2),B],L3)) =>
      APPLY(IDEN('|',L0),[APPLY(IDEN('?',L1),[wffGoal(G),wffAction(A)],L2),wffAction(B)],L3).  
  wffAction(APPLY(IDEN('?',L0),[G,A],L1)) =>
      APPLY(IDEN('|',L0),[APPLY(IDEN('?',L0),[wffGoal(G),wffAction(A)],L1),
			  IDEN('{}',L1)],L1).  
  wffAction(APPLY(IDEN('timeout',L0),[L,APPLY(IDEN('->',L1),[T,A],L2)],L3)) =>
      APPLY(IDEN('timeout',L0),[wffAction(L),
                                APPLY(IDEN('->',L1),[wffExp(T),wffAction(A)],L2)],L3).  
  wffAction(APPLY(IDEN('timeout',L0),[_,R],L3)) =>
      valof{
	reportError("ill-formed timeout clause: "<>R.show(),L3);
	valis VOID(L0)
      }.

  wffAction(APPLY(IDEN('#',L0),[P,A],L2)) => APPLY(IDEN('#',L0),[wffPackageName(P),wffAction(A)],L2).

  wffAction(APPLY(IDEN(Nm,L0),_,_)) :: isKeyword(Nm) =>
      valof{
	reportError("unexpected keyword: "<>explode(Nm),L0);
	valis VOID(L0)
      }.

  wffAction(APPLY(P,A,L)) => APPLY(wffExp(P),{wffExp(Ar)..Ar in A},L).

  wffAction(X) =>
      valof{
	reportError("ill-formed action: "<>X.show(),X.loc());
	valis VOID(X.loc())
      }.


  -- case rules for actions apply to the case action as well as the error handler
  wffCaseRule:[abstract]=>abstract.
  wffCaseRule(APPLY(IDEN('->',L0),[G,A],L1)) => 
      APPLY(IDEN('->',L0),[wffPtn(G),wffAction(A)],L1).
  wffCaseRule(APPLY(IDEN('|',L0),[L,R],L1)) => 
      APPLY(IDEN('|',L0),[wffCaseRule(L),wffCaseRule(R)],L1).
  wffCaseRule(X) =>
      valof{
	reportError("ill-formed action case: "<>X.show(),X.loc());
	valis VOID(X.loc())
      }.

  -- sync rules have a slightly complex range of possibilities...
  wffSyncRules:[abstract]=>abstract.
  wffSyncRules(APPLY(IDEN('->',L0),[G,A],L1)) => 
      APPLY(IDEN('->',L0),[APPLY(IDEN('::',L0),[VOID(L0),wffGoal(G)],L0),
			   wffAction(A)],L1).
  wffSyncRules(APPLY(IDEN('|',L0),[APPLY(IDEN('?',L1),[G,A],L2),B],L3)) =>
      APPLY(IDEN('->',L0),[APPLY(IDEN('::',L0),[VOID(L0),IDEN('true',L0)],L0),
                           APPLY(IDEN('|',L0),[APPLY(IDEN('?',L1),[wffGoal(G),wffAction(A)],L2),
                                               wffAction(B)],L3)],L3).
  wffSyncRules(APPLY(IDEN('|',L0),[L,R],L1)) => APPLY(IDEN('|',L0),[wffSyncRules(L),wffSyncRules(R)],L1).
  wffSyncRules(X) =>
      valof{
	reportError("ill-formed sync action: "<>X.show(),X.loc());
	valis VOID(X.loc())
      }.

  
  -- The body of a grammar rule...
  wffGram:[abstract]=>abstract.
  wffGram(IDEN('[]',L))=>IDEN('[]',L).
  wffGram(IDEN('eof',L))=>IDEN('eof',L).
  wffGram(IDEN(Nm,L0)) =>
      valof{
	reportError("identifier: "<>explode(Nm)<>" not permitted in grammar",L0);
	valis VOID(L0)
      }.
  wffGram(STR(S,L0)) => STR(S,L0).
  wffGram(APPLY(IDEN(',',L0),[L,R],L1)) =>
      APPLY(IDEN(',',L0),[wffGram(L),wffGram(R)],L1).
  wffGram(APPLY(IDEN('|',L0),[APPLY(IDEN('?',L1),[T,L],L2),R],L3)) =>
      APPLY(IDEN('|',L0),[APPLY(IDEN('?',L1),[wffGram(T),wffGram(L)],L2),wffGram(R)],L3).
  wffGram(APPLY(IDEN('|',L0),[L,R],L1)) =>
      APPLY(IDEN('|',L0),[wffGram(L),wffGram(R)],L1).
  wffGram(APPLY(IDEN('::',L0),[L,R],L1)) =>
      APPLY(IDEN('::',L0),[wffGram(L),wffGoal(R)],L1).
  wffGram(APPLY(IDEN('{}',L0),[G],L1)) =>
      APPLY(IDEN('{}',L0),[wffGoal(G)],L1).
  wffGram(APPLY(IDEN(',..',L0),[L,R],L1)) =>
      APPLY(IDEN(',..',L0),[wffExp(L),wffTerminal(R)],L1).
  wffGram(APPLY(IDEN('onerror',L0),[L,R],L1)) =>
      APPLY(IDEN('onerror',L0),[wffGram(L),wffGramErrorRule(R)],L1).
  wffGram(APPLY(IDEN('raise',L0),[E],L1)) =>
      APPLY(IDEN('raise',L0),[wffExp(E)],L1).
  wffGram(APPLY(IDEN('!',L0),[G],L1)) =>
      APPLY(IDEN('!',L0),[wffGram(G)],L1).
  wffGram(APPLY(IDEN('\\+',L0),[G],L1)) =>
      APPLY(IDEN('\\+',L0),[wffGram(G)],L1).
  wffGram(APPLY(IDEN('=',L0),[L,R],L1)) =>
      APPLY(IDEN('{}',L0),[APPLY(IDEN('=',L0),[wffExp(L),wffExp(R)],L1)],L1).
  wffGram(APPLY(IDEN('!=',L0),[L,R],L1)) =>
      APPLY(IDEN('{}',L0),[APPLY(IDEN('!=',L0),[wffExp(L),wffExp(R)],L1)],L1).
  wffGram(APPLY(IDEN('\\=',L0),[L,R],L1)) =>
      APPLY(IDEN('{}',L0),[APPLY(IDEN('!=',L0),[wffExp(L),wffExp(R)],L1)],L1).
  wffGram(APPLY(IDEN('.=',L0),[L,R],L1)) =>
      APPLY(IDEN('{}',L0),[APPLY(IDEN('.=',L0),[wffPtn(L),wffExp(R)],L1)],L1).
  wffGram(APPLY(IDEN('*',L0),[NT,APPLY(IDEN('^',L1),[Ptn,Lst],L2)],L3)) =>
      APPLY(IDEN('*',L0),[wffGram(NT),APPLY(IDEN('^',L1),[wffPtn(Ptn),wffExp(Lst)],L2)],L3).
  wffGram(APPLY(IDEN('.',_),[_,APPLY(IDEN(NT,L2),_,_)],L1))::isKeyword(NT) =>
      valof{
	reportError("unexpected keyword: "<>explode(NT),L2);
	valis VOID(L1)
      }.
  wffGram(APPLY(IDEN('.',L0),[Rc,APPLY(NT,A,L3)],L1)) => 
      APPLY(IDEN('.',L0),[wffExp(Rc),APPLY(wffExp(NT),{wffExp(Ar)..Ar in A},L3)],L1).
  wffGram(APPLY(IDEN('#',L0),[P,APPLY(G,A,L3)],L1)) => 
      wffGram(APPLY(APPLY(IDEN('#',L0),[P,G],L3),A,L1)).
  wffGram(APPLY(N,A,L)) => APPLY(wffExp(N),{wffExp(Ar)..Ar in A},L).
  wffGram(BRACE(IDEN('action',L0),[A],L1))=>APPLY(IDEN('{}',L0),[BRACE(IDEN('action',L0),[wffAction(A)],L1)],L0).
  wffGram(G) => 
      valof{
	reportError("ill-formed grammar condition: "<>G.show(),G.loc());
	valis VOID(G.loc())
      }.

  -- error handler cases
  wffGramErrorRule:[abstract]=>abstract.
  wffGramErrorRule(APPLY(IDEN('-->',L0),[G,A],L1)) => APPLY(IDEN('-->',L0),[wffPtn(G),wffGram(A)],L1).
  wffGramErrorRule(APPLY(IDEN('|',L0),[L,R],L1)) => 
      APPLY(IDEN('|',L0),[wffGramErrorRule(L),wffGramErrorRule(R)],L1).
  wffGramErrorRule(X) =>
      valof{
	reportError("ill-formed grammar error clause: "<>X.show(),X.loc());
	valis VOID(X.loc())
      }.

  -- terminal symbols 
  wffTerminal:[abstract]=>abstract.
  wffTerminal(IDEN('[]',L)) => IDEN('[]',L).
  wffTerminal(APPLY(IDEN(',..',L0),[H,T],L1)) => APPLY(IDEN(',..',L0),[wffExp(H),wffTerminal(T)],L1).
  wffTerminal(APPLY(IDEN(',',L0),[L,R],L1)) => APPLY(IDEN(',',L0),[wffTerminal(L),wffTerminal(R)],L1).
  wffTerminal(STR(S,L)) => STR(S,L).
  wffTerminal(K) => valof{
		      reportError("ill-formed terminal: "<>K.show(),K.loc());
		      valis VOID(K.loc())
		    }.

  -- well formed type expressions
  wffTypeExp:[abstract]=>abstract.
  wffTypeExp(IDEN('_',L))=>IDEN(genNew("_"),L).
  wffTypeExp(IDEN('{}',L))=>APPLY(IDEN('{}',L),[],L).
  wffTypeExp(IDEN(Nm,L))=>IDEN(Nm,L).
  wffTypeExp(APPLY(IDEN(',',Lx),[L,R],L0))=>
      APPLY(IDEN(',',Lx),[wffTypeExp(L),wffTypeExp(R)],L0).
  wffTypeExp(APPLY(IDEN('-',L0),[IDEN(Vr,L2),R],L1))=>
      APPLY(IDEN('-',L0),[APPLY(IDEN(',..',L2),[IDEN(Vr,L2),IDEN('[]',L2)],L2),wffTypeExp(R)],L1).
  wffTypeExp(APPLY(IDEN('-',L0),[L,R],L1))=>APPLY(IDEN('-',L0),[listOfIdens(L),wffTypeExp(R)],L1).
  wffTypeExp(APPLY(IDEN('=>',L0),[APPLY(IDEN('-',L2),[V,A],L3),R],L1))=>
      wffTypeExp(APPLY(IDEN('-',L2),[V,APPLY(IDEN('=>',L0),[A,R],L1)],L3)).
  wffTypeExp(APPLY(IDEN('=>',L0),[A,R],L1))=>
      APPLY(IDEN('=>',L0),[wffArgTpl(A),wffTypeExp(R)],L1).
  wffTypeExp(APPLY(IDEN('-->',L0),[APPLY(IDEN('-',L2),[V,A],L3),R],L1))=>
      wffTypeExp(APPLY(IDEN('-',L2),[V,APPLY(IDEN('-->',L0),[A,R],L1)],L3)).
  wffTypeExp(APPLY(IDEN('-->',L0),[A,R],L1))=>
      APPLY(IDEN('-->',L0),[wffArgTpl(A),wffTypeExp(R)],L1).
  wffTypeExp(APPLY(IDEN('*',L0),[A],L1))=>
      APPLY(IDEN('*',L0),[wffArgTpl(A)],L1).
  wffTypeExp(APPLY(IDEN('$=',L0),[A,R],L1))=>
      APPLY(IDEN('@=',L0),[wffArgTpl(A),wffTypeExp(R)],L1).
  wffTypeExp(APPLY(IDEN('@=',L0),[A,R],L1))=>
      APPLY(IDEN('@=',L0),[wffArgTpl(A),wffTypeExp(R)],L1).
  wffTypeExp(APPLY(IDEN('@>',L0),[A,R],L1))=>
      APPLY(IDEN('@>',L0),[wffArgTpl(A),wffTypeExp(R)],L1).
  wffTypeExp(APPLY(IDEN('{}',L0),[M],L1))=>
      APPLY(IDEN('{}',L0),{wffTypeField(A)..A in listIfy(M,'. ')},L1).
  wffTypeExp(APPLY(IDEN('. ',L0),[M],L1))=>
      APPLY(IDEN('()',L0),{wffTypeField(A)..A in listIfy(M,'. ')},L1).
  wffTypeExp(APPLY(IDEN('. ',L0),[L,M],L1))=>
      APPLY(IDEN('()',L0),
	    {wffTypeField(A)..A in listIfy(APPLY(IDEN('. ',L0),[L,M],L1),'. ')},
	    L1).
  wffTypeExp(APPLY(IDEN(':',L0),[L,M],L1))=>
      APPLY(IDEN('()',L0),[wffTypeField(APPLY(IDEN(':',L0),[L,M],L1))],L1).
  wffTypeExp(APPLY(IDEN('<~',L0),[IDEN(A,L2),Tp],L1))=>
      APPLY(IDEN('<~',L0),[IDEN(A,L2),wffTypeExp(Tp)],L1).
  wffTypeExp(APPLY(IDEN('#',L0),[P,SQUARE(IDEN(F,L3),A,L2)],L1)) ::
	  checkName(F,L3) =>
      SQUARE(APPLY(IDEN('#',L0),[wffPackageName(P),IDEN(F,L3)],L1),
	     {wffTypeExp(a)..a in A},L2).
  wffTypeExp(APPLY(IDEN('#',L0),[P,IDEN(F,L2)],L1)) ::
	  checkName(F,L2) =>
      SQUARE(APPLY(IDEN('#',L0),[wffPackageName(P),IDEN(F,L2)],L1),[],L2).
  wffTypeExp(SQUARE(IDEN(Nm,L0),A,Lc)) :: checkName(Nm,L0) => 
      SQUARE(IDEN(Nm,L0),{wffTypeExp(Ar)..Ar in A},Lc).
  wffTypeExp(BRACE(Tp,[],Lc)) => BRACE(wffArgTpl(Tp),[],Lc).
  wffTypeExp(Tp) =>
      valof{
	reportError("ill-formed type expression: "<>Tp.show(),Tp.loc());
	valis VOID(Tp.loc())
      }.

  -- figure out the argument type vector for a program type

  wffArgTpl:[abstract]=>abstract.
  wffArgTpl(APPLY(IDEN(',',_),[L,R],L1)) =>
      valof{
	reportWarning("deprecated form of argument tuple: "<>
		      APPLY(IDEN(',',L1),[L,R],L1).show(),L1);
	valis TPL({wffArgTypeSpec(A)..A in [L,..listIfy(R,',')]},L1)
      }.
  wffArgTpl(APPLY(IDEN(',..',_),[L,R],L1)) => 
      TPL({wffArgTypeSpec(A)..(A::\+IDEN('[]',_).=A) in [L,..listIfy(R,',..')]},L1).
  wffArgTpl(IDEN('[]',Lc))=>TPL([],Lc).
  wffArgTpl(T) =>
      valof{
	reportWarning("deprecated form of argument tuple: "<>T.show(),T.loc());
	valis TPL([wffTypeExp(T)],T.loc())
      }.

  private wffArgTypeSpec:[abstract]=>abstract.
  wffArgTypeSpec(APPLY(IDEN('+',L0),[T],L1))=>
      APPLY(IDEN('+',L0),[wffTypeExp(T)],L1).
  wffArgTypeSpec(APPLY(IDEN('-',L0),[T],L1))=>
      APPLY(IDEN('-',L0),[wffTypeExp(T)],L1).
  wffArgTypeSpec(APPLY(IDEN('-+',L0),[T],L1))=>
      APPLY(IDEN('-+',L0),[wffTypeExp(T)],L1).
  wffArgTypeSpec(T)=>wffTypeExp(T).

  -- bound type variable in a quantified type expression
  listOfIdens:[abstract]=>abstract.
  listOfIdens(IDEN('[]',L))=>IDEN('[]',L).
  listOfIdens(APPLY(IDEN(',..',L0),[IDEN(Nm,L2),T],L1))=>
      APPLY(IDEN(',..',L0),[IDEN(Nm,L2),listOfIdens(T)],L1).
  listOfIdens(APPLY(IDEN(',..',L0),[APPLY(IDEN('<~',L3),[IDEN(Nm,L2),Tp],L4),T],L1))=>
      APPLY(IDEN(',..',L0),[APPLY(IDEN('<~',L3),[IDEN(Nm,L2),wffTypeExp(Tp)],L4),
                            listOfIdens(T)],L1).
  listOfIdens(L)=>
      valof{
	reportError("list of identifiers expected, not: "<>L.show(),L.loc());
	valis VOID(L.loc())
      }.

  -- field in a type interface spec
  wffTypeField:[abstract]=>abstract.
  wffTypeField(APPLY(IDEN(':',L0),[IDEN(Fl,L1),Tp],L2))=>
      APPLY(IDEN(':',L0),[IDEN(Fl,L1),wffTypeExp(Tp)],L2).
  wffTypeField(APPLY(IDEN('=>',L1),[APPLY(IDEN(':',L0),[IDEN(Fld,L2),fTp],L3),rTp],L1a))=>
      APPLY(IDEN(':',L0),[IDEN(Fld,L2),wffTypeExp(APPLY(IDEN('=>',L1),[fTp,rTp],L3))],L1a).
  wffTypeField(APPLY(IDEN('-->',L1),[APPLY(IDEN(':',L0),[IDEN(Fld,L2),fTp],L3),rTp],L1a))=>
      APPLY(IDEN(':',L0),[IDEN(Fld,L2),wffTypeExp(APPLY(IDEN('-->',L1),[fTp,rTp],L3))],L1a).
  wffTypeField(T) => valof{
		       reportError("ill-formed type field: "<>T.show(),T.loc());
		       valis VOID(T.loc())
		     }.


  -- argument of a constructor in a algebraic type definition
  wffTypeArg:[abstract]=>abstract.
  wffTypeArg(IDEN(Nm,Lc))=>IDEN(Nm,Lc).
  wffTypeArg(APPLY(IDEN('<~',L0),[IDEN(Nm,Lc),Tp],Lc))=>
      APPLY(IDEN('<~',L0),[IDEN(Nm,Lc),wffTypeExp(Tp)],Lc).
  wffTypeArg(Tp) =>
      valof{
	reportError("ill-formed type template argument: "<>Tp.show(),Tp.loc());
	valis VOID(Tp.loc())
      }.

  -- pick out a type definition from a theta stream
  extractTypeDef:[list[abstract],symbol,fileLoc]=>list[abstract].
  extractTypeDef(L,Fn,Lc) => extType(L,Fn,Lc,[]).

  extType:[list[abstract],symbol,fileLoc,list[abstract]]=>list[abstract].
  extType([APPLY(IDEN('<~',L0),[IDEN(Fn,L2),Tp],L1),..Lx],Fn,Lc,soFar) =>
      extType(Lx,Fn,Lc,[APPLY(IDEN('<~',L0),
                              [SQUARE(IDEN(Fn,L2),[],L2),
                               wffTypeExp(Tp)],L1),..soFar]).
  extType([APPLY(IDEN('<~',L0),[SQUARE(IDEN(Fn,L2),A,L2A),Tp],L1),..Lx],Fn,Lc,soFar) =>
      extType(Lx,Fn,Lc,[APPLY(IDEN('<~',L0),
                              [SQUARE(IDEN(Fn,L2),{wffTypeArg(Ar)..Ar in A},L2A),
                               wffTypeExp(Tp)],L1),..soFar]).
  extType(Lx,Fn,Lc,Defs) => [APPLY(IDEN('<~',Lc),
                                   [IDEN(Fn,Lc),TPL(addThingType(reverse(Defs)),Lc)],Lc),..equalize(Lx)].

  private addThingType:[list[abstract]]=>list[abstract].
  addThingType([(Tp::APPLY(IDEN('<~',Lc),[Lhs,APPLY(IDEN('{}',_),_,_)],_).=Tp)])=>
      [APPLY(IDEN('<~',Lc),[Lhs, SQUARE(IDEN('thing',Lc),[],Lc)],Lc),Tp].
  addThingType([(Tp::APPLY(IDEN('<~',Lc),[Lhs,APPLY(IDEN('()',_),_,_)],_).=Tp)])=>
      [APPLY(IDEN('<~',Lc),[Lhs, SQUARE(IDEN('thing',Lc),[],Lc)],Lc),Tp].
  addThingType(L)=>L.

  -- convert an algebraic type definition into a class-based one
  constructorTypeDef:[abstract,abstract]=>list[abstract].

  constructorTypeDef((Hd::SQUARE(IDEN(Nm,L0),_,_).=Hd),Tp) => 
      [APPLY(IDEN('<~',L0),[IDEN(Nm,L0),
                            TPL([APPLY(IDEN('<~',L0),[
                                            Hd,
                                            APPLY(IDEN('{}',L0),[
                                                       APPLY(IDEN(':',L0),
                                                             [IDEN('show',L0),
                                                              APPLY(IDEN('=>',L0),
                                                                    [TPL([],L0),
                                                                     SQUARE(IDEN('list',L0),
                                                                            [IDEN('char',L0)],L0)],L0)],L0)],L0)],L0),
                                 APPLY(IDEN('<~',L0),[Hd,SQUARE(IDEN('thing',L0),[],L0)],L0)],L0)],L0),..
       enumerateTypes(listIfy(Tp,'|'),Hd)].
  
  enumerateTypes:[list[abstract],abstract]=>list[abstract].

  enumerateTypes([],_)=>[].
  enumerateTypes([IDEN(E,Lc),..Enums],Tp) => 
      [ APPLY(IDEN(':',Lc),[IDEN(E,Lc),APPLY(IDEN('@=',Lc),[TPL([],Lc),Tp],Lc)],Lc),
        APPLY(IDEN('<$',Lc),[IDEN(E,Lc),
                             TPL([APPLY(IDEN('..',Lc),
                                        [IDEN(E,Lc),
                                         APPLY(IDEN('{}',Lc),[
                                                    APPLY(IDEN('=>',Lc),
                                                          [IDEN('show',Lc),
                                                           TPL([APPLY(IDEN('=>',Lc),
                                                                      [APPLY(IDEN('show',Lc),[],Lc),STR(explode(E),Lc)],
                                                                      Lc)],Lc)],Lc) 
                                               ],Lc)],Lc)],Lc)],Lc),..enumerateTypes(Enums,Tp)].
  enumerateTypes([APPLY(IDEN(E,Lc),A,_),..Enums],Tp) ::
    cArgs = { IDEN(genNew("_"),Lc).. _ in A} => 
      [ APPLY(IDEN(':',Lc),[IDEN(E,Lc),APPLY(IDEN('@=',Lc),
                                             [TPL({ wffTypeExp(a) .. a in A},Lc),Tp],
                                             Lc)],Lc),
        APPLY(IDEN('<$',Lc),[IDEN(E,Lc),
                             TPL([APPLY(IDEN('..',Lc),
                                        [APPLY(IDEN(E,Lc),cArgs,Lc),
                                         APPLY(IDEN('{}',Lc),[
                                                    APPLY(IDEN('=>',Lc),
                                                          [IDEN('show',Lc),
                                                           TPL([APPLY(IDEN('=>',Lc),
                                                                      [APPLY(IDEN('show',Lc),[],Lc),
                                                                       APPLY(IDEN('<>',Lc),
                                                                             [STR(explode(E)<>"(",Lc),
                                                                              showExp(cArgs,Lc)],Lc)],
                                                               Lc)],Lc)],Lc)],Lc)],Lc)],
                                 Lc)],Lc),..enumerateTypes(Enums,Tp)].
  enumerateTypes([H,..Tps],Hd) =>
      valof{
	reportError("ill-formed constructor definition: "<>H.show(),H.loc());
	valis enumerateTypes(Tps,Hd)
      }.

  showExp:[list[abstract],fileLoc]=>abstract.
  showExp([],Lc)=>STR(")",Lc).
  showExp([A],Lc) => APPLY(IDEN('<>',Lc),[
                                APPLY(APPLY(IDEN('.',Lc),
                                            [A,IDEN('show',Lc)],Lc),[],Lc),
                                STR(")",Lc)],Lc).
  showExp([A,..rgs],Lc) => APPLY(IDEN('<>',Lc),[
                                      APPLY(APPLY(IDEN('.',Lc),
                                                  [A,IDEN('show',Lc)],Lc),[],Lc),
                                      APPLY(IDEN('<>',Lc),
                                            [STR(",",Lc),showExp(rgs,Lc)],Lc)],Lc).

  privatize:[list[abstract],fileLoc]=>list[abstract].
  privatize([],_)=>[].
  privatize([APPLY(Fun,[A,B],Lc),..Rx],Lx)=>[APPLY(Fun,[A,APPLY(IDEN('private',Lc),[B],Lc)],Lx),..privatize(Rx,Lx)].
  privatize([A,..Rx],Lx)=>[A,..privatize(Rx,Lx)].
}.
