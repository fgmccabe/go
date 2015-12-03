/*
 * Define the type language for the Go! compiler
 */
types{
  import ops.
  import misc.
  import go.dynamic.
  import go.showable.
  import go.io.

  typeTree <~ {
	deRef:[] =>typeTree. 
	disp:[integer] =>dispTree.
	freshn:[dict]=>typeTree.
	occIn:[vrBind]{}.
	isvar:[]{}.
	lower:[]=>typeTree.
	upper:[]=>typeTree.
      }.

  -- This captures the state of a type variable
  vrBind <~ {
       bind:[typeTree]*.
       setLBound:[typeTree]*.
       setUBound:[typeTree]*. isVar:[]{}. 
       upper:[]=>typeTree. lower:[]=>typeTree. 
       vrKey:[]=>symbol.
       occIn:[vrBind]{}.
       disp:[integer] =>dispTree.
       deRef:[] =>typeTree.
      }.

  typeBinding::= varBind | tvarBind | typeBind | pkgBind | inheritBind.

  flowMode ::= inpMode | outMode | superMode | biMode.  -- flow mode for binding

  private resetNo:integer:=0.
  private resets:list[(integer,do)] := [].

  current:[]=>integer.
  current()=>resetNo.

  reset:[integer]*.
  reset(L)::[(M,D),..R]=resets, M>L -> D.do(); resets:=R; reset(L).
  reset(_) -> {}.

  clearReset:[integer]*.
  clearReset(L)::[(M,_),..R].=resets, M>L -> resets:=R; clearReset(L).
  clearReset(_) -> {}.

  private mkreset:[do]*.
  mkreset(D) -> resetNo := resetNo+1; resets:=[(resetNo,D),..resets].

  vSpec ::= vS(symbol,typeBinding,typeTree).

  dict <~ { 
	lookup:[symbol,typeBinding]=>typeTree.
	isbound:[symbol,typeBinding,typeTree]{}.
	push:[symbol,typeBinding,typeTree]=>dict.
	pushList:[list[vSpec]]=>dict.
	pushDict:[dict]=>dict.
	show:[]=>string.
	disp:[integer]=>dispTree.
	ext:[]=>list[vSpec].
	empty:[]{}.
      }.

  dict:[list[vSpec],dict]@>dict.
  dict(Init,Parent)..{
    contents:dynamic[vSpec] = dynamic(Init).

    lookup(Name,Mode) :: contents.mem(vS(Name,Mode,Val)) => Val.
    lookup(Name,Mode) => Parent.lookup(Name,Mode).

    isbound(Name,Mode,Type) :- contents.mem(vS(Name,Mode,Type)).
    isbound(Name,Mode,Type) :- Parent.isbound(Name,Mode,Type).

    pushList([]) => this.
    pushList(Entries) => dict(Entries,this).

    push(Name,Mode,Entry) => dict([vS(Name,Mode,Entry)],this).
    pushDict(Sub) => dict(Sub.ext(),this).

    show() => disp(100).flatten(", ").
    disp(Lvl)::Lvl>=0 => n([s("{"),
				n(dispLocal(contents.ext())),s("}\n, "),
				Parent.disp(Lvl-1)]).
    disp(_) => s("...").

    dispLocal:[list[vSpec]] => list[dispTree].
    dispLocal([]) => [].
    dispLocal([vS(N,M,T),Nxt,..L]) => [s(explode(N)),shwBind(M),
				       T.disp(746),s(", "),..dispLocal([Nxt,..L])].
    dispLocal([vS(N,M,T)]) => [s(explode(N)),shwBind(M),T.disp(746)].

    ext()=>contents.ext().

    empty() :- \+contents.mem(_) , Parent.empty().
  }.


  emptyDict:[]@=dict.
  emptyDict..{
    lookup(_,_) => raise error("not defined",'eNOTFND').

    isbound(_,_,_):-false.

    pushList([]) => this.
    pushList(Entries) => dict(Entries,this).

    push(Name,Mode,Entry) => dict([vS(Name,Mode,Entry)],this).
    pushDict(Sub) => dict(Sub.ext(),this).

    show() => "empty".
    disp(_) => s("end of dict").

    ext()=>[].
    empty().
  }.

  fields:[list[vSpec]]@=dict.
  fields(Methods)..{
    lookup(Name,Mode) :: vS(Name,Mode,Val) in Methods => Val.
    lookup(_,_) => raise error("not defined",'eNOTFND').

    isbound(Name,Mode,Type) :- vS(Name,Mode,Type) in Methods.

    pushList([]) => this.
    pushList(Entries) => dict(Entries,this).

    push(Name,Mode,Entry) => dict([vS(Name,Mode,Entry)],this).
    pushDict(Sub) => dict(Sub.ext(),this).

    show() => disp(0).flatten(". ").

    disp(_) => n([s("{"),n(dispLocal(Methods,s(", "))),s("}")]).

    dispLocal:[list[vSpec],dispTree] => list[dispTree].
    dispLocal([],_) => [].
    dispLocal([vS(N,M,T),Nxt,..L],Sep) => [s(explode(N)),shwBind(M),T.disp(746),
					   Sep,..dispLocal([Nxt,..L],Sep)].
    dispLocal([vS(N,M,T)],_) => [s(explode(N)),shwBind(M),T.disp(746)].

    ext()=>Methods.
    empty() :- Methods = [].
  }.

  private shwBind:[typeBinding] =>dispTree.
  shwBind(varBind)=>s(":").
  shwBind(tvarBind)=>s(":%").
  shwBind(typeBind)=>s("::").
  shwBind(pkgBind)=>s(":#").
  shwBind(inheritBind)=>s(":<=").

  voidType:[]@=typeTree.
  voidType..{
    deRef() => this.

    show() => this.disp(0).flatten("").
    disp(_)=>s("void").

    freshn(_)=>this.

    occIn(_) :- false.

    isvar():-false.

    lower()=>this.

    upper()=>this.
  }.

  undef:[symbol]@=typeTree.
  undef(_) <= voidType.
  undef(S)..{
    disp(_)=>s([`U,..explode(S)]).

    freshn(L) :: L.isbound(S,tvarBind,Tp) => Tp.
    freshn(_) => this.

    isvar().
  }.

  isUnbound:[typeTree]{}.
  isUnbound(undef(_)) :--true.

  topType:[]@=typeTree.
  topType<=voidType.
  topType..{
    disp(_)=>s("top").
  }.

  private dispSeq:[list[typeTree],dispTree] =>list[dispTree].
  dispSeq([],_) => [].
  dispSeq([E,..L],Sep) => [Sep,E.disp(999),..dispSeq(L,s(", "))].

  private dispArgSeq:[list[(typeTree,flowMode)],dispTree,flowMode] =>list[dispTree].
  dispArgSeq([],_,_) => [].
  dispArgSeq([(E,Def),..L],Sep,Def) => [Sep,E.disp(999),..dispArgSeq(L,s(", "),Def)].
  dispArgSeq([(E,D),..L],Sep,Def) => [Sep,E.disp(999),dispMode(D),..dispArgSeq(L,s(", "),Def)].

  private dispMode:[flowMode]=>dispTree.
  dispMode(inpMode)=>s("+").
  dispMode(outMode)=>s("-").
  dispMode(biMode)=>s("-+").

  enuType:[typeTree]@=typeTree.
  enuType(_)<=voidType.
  enuType(Tp)..{
    disp(_) => n([s("enum("),Tp.disp(2000),s(")")]).
    freshn(L) => enuType(Tp.freshn(L)).

    occIn(Vr) :- Tp.occIn(Vr).

    lower() => enuType(Tp.lower()).

    upper() => enuType(Tp.upper()).
  }.

  conType:[list[typeTree],typeTree]@=typeTree.
  conType(_,_)<=voidType.
  conType(A,Tp)..{
    disp(Pr)::infOp('@=',_,P,R) => n([showParen(Pr,P,s("(")),
                                      s("["),n(dispSeq(A,s(""))),s("]@="),Tp.disp(R),
                                      showParen(Pr,P,s(")"))]).
    freshn(L) => conType({a.freshn(L)..a in A},Tp.freshn(L)).

    occIn(Vr) :- a in A, a.occIn(Vr).
    occIn(Vr) :- Tp.occIn(Vr).

    lower() => conType({a.upper() .. a in A},Tp.lower()).

    upper() => conType({a.lower() .. a in A},Tp.upper()).
  }.

  sconType:[list[(typeTree,flowMode)],typeTree]@=typeTree.
  sconType(_,_)<=voidType.
  sconType(A,Tp)..{
    disp(Pr)::infOp('->',_,P,R) => n([showParen(Pr,P,s("(")),
                                      s("["),
				      n(dispArgSeq(A,s(""),inpMode)),s("]@>"),Tp.disp(R),
                                      showParen(Pr,P,s(")"))]).
    freshn(L) => sconType({(a.freshn(L),flow) .. (a,flow) in A},Tp.freshn(L)).

    occIn(Vr) :- (a,_) in A, a.occIn(Vr).
    occIn(Vr) :- Tp.occIn(Vr).

    lower() => sconType({(a.upper(),flow) .. (a,flow) in A},Tp.lower()).

    upper() => sconType({(a.lower(),flow) .. (a,flow) in A},Tp.upper()).
  }.

  funType:[list[(typeTree,flowMode)],typeTree]@=typeTree.
  funType(_,_)<=voidType.
  funType(A,Tp)..{
    disp(Pr)::infOp('->',_,P,R) => n([showParen(Pr,P,s("(")),
                                      s("["),
				      n(dispArgSeq(A,s(""),inpMode)),s("]=>"),Tp.disp(R),
                                      showParen(Pr,P,s(")"))]).
    freshn(L) => funType({(a.freshn(L),flow) .. (a,flow) in A},Tp.freshn(L)).

    occIn(Vr) :- (a,_) in A, a.occIn(Vr).
    occIn(Vr) :- Tp.occIn(Vr).


    lower() => funType({(a.upper(),flow) .. (a,flow) in A},Tp.lower()).

    upper() => funType({(a.lower(),flow) .. (a,flow) in A},Tp.upper()).
  }.

  predType:[list[(typeTree,flowMode)]]@=typeTree.
  predType(_)<=voidType.
  predType(A)..{
    disp(_) => n([s("["),n(dispArgSeq(A,s(""),biMode)),s("]{}")]).
    freshn(L) => predType({(a.freshn(L),flow) .. (a,flow) in A}).

    occIn(Vr) :- (a,_) in A, a.occIn(Vr).

    lower() => predType({(a.upper(),flow) .. (a,flow) in A}).

    upper() => predType({(a.lower(),flow) .. (a,flow) in A}).
  }.

  actType:[list[(typeTree,flowMode)]]@=typeTree.
  actType(_)<=voidType.
  actType(A)..{
    disp(Pr)::pstOp('*',_,P) => n([showParen(Pr,P,s("(")),
                                   s("["),
				   n(dispArgSeq(A,s(""),inpMode)),s("]*"),
                                   showParen(Pr,P,s(")"))]).
    freshn(L) => actType({(a.freshn(L),flow) .. (a,flow) in A}).

    occIn(Vr) :- (a,_) in A, a.occIn(Vr).

    lower() => actType({(a.upper(),flow) .. (a,flow) in A}).

    upper() => actType({(a.lower(),flow) .. (a,flow) in A}).
  }.

  gramType:[list[(typeTree,flowMode)],typeTree]@=typeTree.
  gramType(_,_)<=voidType.
  gramType(A,S)..{
    disp(Pr)::infOp('-->',_,P,R) => n([showParen(Pr,P,s("(")),
				       s("["),
                                       n(dispArgSeq(A,s(""),biMode)),
				       s("]-->"),S.disp(R),
                                       showParen(Pr,P,s(")"))]).
    freshn(L) => gramType({(a.freshn(L),flow) .. (a,flow) in A},S.freshn(L)).

    occIn(Vr) :- (a,_) in A, a.occIn(Vr).
    occIn(Vr) :- S.occIn(Vr).

    lower() => gramType({(a.upper(),flow) .. (a,flow) in A},S.lower()).

    upper() => gramType({(a.lower(),flow) .. (a,flow) in A},S.upper()).
  }.

  faceType:[dict]@=typeTree.
  faceType(_)<=voidType.
  faceType(M)..{
    disp(_) => n([s("{"),M.disp(0),s("}")]).

    freshn(L) => faceType(fields({vS(Nm,Md,Tp.freshn(L))..vS(Nm,Md,Tp) in M.ext()})).

    occIn(Vr) :- vS(_,_,m) in M.ext(), m.occIn(Vr).

    lower() => faceType(fields({vS(Nm,Md,Tp.lower()) .. vS(Nm,Md,Tp) in M.ext()})).

    upper() => faceType(fields({vS(Nm,Md,Tp.upper()) .. vS(Nm,Md,Tp) in M.ext()})).
  }.

  uType:[symbol,symbol,list[typeTree]]@=typeTree.
  uType(_,_,_) <= voidType.
  uType(Pk,Nm,A)..{
    disp(_)::Nm='list',[uType(_,'char',[])].=A => s("string").
    disp(_)::Pk=='go.stdlib',A=[] => s(explode(Nm)).
    disp(_)::Pk=='go.stdlib' => n([s(explode(Nm)),s("["),n(dispSeq(A,s(""))),s("]")]).
    disp(_)::A==[] => l([explode(Pk),"#",explode(Nm)]).
    disp(_) => n([s(explode(Pk)),s("#"),s(explode(Nm)),
		  s("["),n(dispSeq(A,s(""))),s("]")]).

    freshn(L) => uType(Pk,Nm,{a.freshn(L) .. a in A}).

    occIn(Vr) :- a in A, a.occIn(Vr).

    lower() => uType(Pk,Nm,{a.upper() .. a in A}).

    upper() => uType(Pk,Nm,{a.lower() .. a in A}).
  }.

  allType:[symbol,typeTree,typeTree]@=typeTree.
  allType(_,_,_)<=voidType.
  allType(Vr,G,T)..{
    disp(Pr)::infOp('-',_,P,R) => n([showParen(Pr,P,s("(")),s("["),
                                     showBound(Vr,G),s("]-"),
                                     T.disp(R),showParen(Pr,P,s(")"))]).

    showBound:[symbol,typeTree] =>dispTree.
    showBound(Vr,topType) => s(explode(Vr)).
    showBound(Vr,Tp)::infOp('<~',_,_,R) => n([s(explode(Vr)),s("<~"),Tp.disp(R)]).

    freshn(L) => allType(Vr,G.freshn(L),T.freshn(L)).

    occIn(V) :- G.occIn(V).
    occIn(V) :- T.occIn(V).

    lower() => allType(Vr,G.lower(),T.lower()).

    upper() => allType(Vr,G.upper(),T.upper()).
  }.

  typeDef:[typeTree,list[typeTree]]@=typeTree.
  typeDef(_,_)<=voidType.
  typeDef(A,T)..{
    disp(Pr)::infOp('::=',_,P,_) => 
        n([showParen(Pr,P,s("(")),
           n(showRules(T)),
           showParen(Pr,P,s(")"))]).

    showRules:[list[typeTree]] =>list[dispTree].
    showRules([])=>[].
    showRules([Rl,..Ls]) => [A.disp(1459),s("<~"),Rl.disp(1459),s(".\n"),..showRules(Ls)].

    freshn(L) => typeDef(A.freshn(L),{t.freshn(L)..t in T}).

    occIn(Vr) :- A.occIn(Vr).
    occIn(Vr) :- t in T, t.occIn(Vr).
  }.

  -- Capture the state of a type variable in a vrBind
  vrBind:[symbol,typeTree]@>vrBind.
  vrBind(Nm,Upper)..{
    Orig:typeTree = undef(genNew(explode(Nm))).
    Curr:typeTree := Orig.

    Above:typeTree := Upper.
    Below:typeTree := voidType.

    deRef()::Curr==Orig => typeVar(this).
    deRef() => Curr.deRef().

    stV:[typeTree]*.
    stV(T) -> Curr:=T.

    stA:[typeTree]*.
    stA(T) -> Above:=T.

    stL:[typeTree]*.
    stL(T) -> Below:=T.

--    bind(new)::action{stdout.outLine("Binding "<>__stringOf(Curr,0,0)<>" to "<>__stringOf(new,0,0))},fail -> {}.
    bind(new)::undef(_).=Curr -> C=Curr;mkreset(:do..{ do()->stV(C) }); Curr:=new.
    bind(new)::typeVar(BB).=Curr -> BB.bind(new).
    bind(_) -> raise error("attempt to bind non-variable: "<>__stringOf(Curr,0,0),'fail').

    setUBound(Tp) -> mkreset(:do..{ do()->stA(Above) }); Above := Tp.
    setLBound(Tp) -> mkreset(:do..{ do()->stL(Below) }); Below := Tp.

    isVar() :- Curr.deRef().isvar().

    upper()=>Above.
    lower()=>Below.

    vrKey() => Nm.

    occIn(this).

    show() => disp(1000).flatten("").

    disp(Pr) => n([(Below=voidType?s("")|n([Below.disp(Pr),s("<~")])),
		   Curr.disp(Pr),
		   (Above=topType?s("")|n([s("<~"),Above.disp(Pr)]))]).
  }.

  typeVar:[vrBind]@=typeTree.
  typeVar(B)..{
    disp(Pr) => B.disp(Pr).
    deRef() => B.deRef().
    occIn(Vr) :- B.occIn(Vr).

    freshn(_)=>this.
    isvar() :- B.isVar().

    lower() => B.lower().

    upper() => B.upper().

    show() => B.show().
  }.

  newVr:[]=>typeTree.
  newVr()=>typeVar(vrBind(gensym("_"),topType)).

  freshen:[typeTree]=>typeTree.
  freshen(Tp) :: (fTp,_) = freshenQ(Tp,[]) => fTp.

  freshenQ:[typeTree,list[vSpec]]=>(typeTree,list[vSpec]).
  freshenQ(allType(Nm,U,T),D) => freshenQ(T,[vS(Nm,tvarBind,typeVar(vrBind(Nm,U))),..D]).
  freshenQ(Tp,L) => (Tp.freshn(fields(L)),L).

  isVarType:[typeTree+]{}.
  isVarType(Tp) :- typeVar(_).=Tp.deRef().

  private showParen:[integer,integer,dispTree] => dispTree.
  showParen(Pr,P,_)::Pr>=P=>n([]).
  showParen(_,_,Pn) => Pn.

  lookupPackage:[symbol,dict]=>typeTree.
  lookupPackage(S,Dct)::Dct.isbound(S,pkgBind,T) => freshen(T).
  lookupPackage(_,_) => raise error("not found",'notfound').

  stripForAlls:[typeTree]=>typeTree.
  stripForAlls(allType(_,_,Tp))=>stripForAlls(Tp).
  stripForAlls(Tp)=>Tp.

  fieldsOf:[typeTree]=>dict.
  fieldsOf(typeVar(B))=>fieldsOf(B.deRef()).
  fieldsOf(faceType(M))=>M.
  fieldsOf(T) => raise error(T.show()<>" has no fields",'eFAIL').

  isProgramType:[typeTree]{}.
  isProgramType(allType(_,_,Tp)) :- isProgramType(Tp).
  isProgramType(funType(_,_)).
  isProgramType(actType(_)).
  isProgramType(predType(_)).
  isProgramType(gramType(_,_)).

  foldupType:[dict,typeTree]=>typeTree.
  foldupType(Q,Tp) =>
      valof{
	nQ = fields({ vS(vN,tvarBind,undef(Nm)) .. 
		      vS(Nm,tvarBind,(V::typeVar(Vr).=V.deRef(),vN=Vr.vrKey())) in Q.ext() });
	valis wrapUp(Q.ext(),nQ,substType(Tp,nQ,[]));
      }.

  private upper:[typeTree]=>typeTree.
  upper(typeVar(Vr))=>Vr.upper().
  upper(T)=>T.

  private wrapUp:[list[vSpec],dict,typeTree]=>typeTree.
  wrapUp([],_,Tp) => Tp.
  wrapUp([vS(Nm,tvarBind,V),..Q],nQ,Tp) =>
      wrapUp(Q,nQ,allType(Nm,substType(upper(V.deRef()),nQ,[]),Tp)).

  private substType:[typeTree,dict,list[symbol]]=>typeTree.
  substType(undef(Nm),Env,Outer)::Env.isbound(Nm,tvarBind,xTp),\+Nm in Outer =>
      xTp.
  substType(undef(Nm),_,_) => undef(Nm).
  substType(typeVar(Vr),Env,Outer)::Key = Vr.vrKey(), Env.isbound(Key,tvarBind,xTp),
	  \+ Key in Outer => xTp.
  substType(typeVar(Vr),_,_) => typeVar(Vr).
  substType(topType,_,_) => topType.
  substType(voidType,_,_) => voidType.
  substType(enuType(Tp),Env,Outer) => enuType(substType(Tp,Env,Outer)).
  substType(conType(A,Tp),Env,Outer) => conType(substTypeList(A,Env,Outer),
						substType(Tp,Env,Outer)).
  substType(sconType(A,Tp),Env,Outer) => sconType(substArgTypeList(A,Env,Outer),
						  substType(Tp,Env,Outer)).
  substType(funType(A,Tp),Env,Outer) => funType(substArgTypeList(A,Env,Outer),
						substType(Tp,Env,Outer)).
  substType(predType(A),Env,Outer) => predType(substArgTypeList(A,Env,Outer)).
  substType(actType(A),Env,Outer) => actType(substArgTypeList(A,Env,Outer)).
  substType(gramType(A,Tp),Env,Outer) => gramType(substArgTypeList(A,Env,Outer),
						  substType(Tp,Env,Outer)).
  substType(faceType(M),Env,Outer) => faceType(substTypeMethods(M,Env,Outer)).
  substType(allType(Nm,G,Tp),Env,Outer) =>
      valof{
	nOuter = [Nm,..Outer];
	valis allType(Nm,substType(G,Env,nOuter),substType(Tp,Env,nOuter))
      }.
  substType(uType(Pk,Nm,Args),Env,Outer) =>
      uType(Pk,Nm,substTypeList(Args,Env,Outer)).
  substType(typeDef(Tp,Dfs),Env,Outer) =>
      typeDef(substType(Tp,Env,Outer),substTypeList(Dfs,Env,Outer)).

  private substTypeList:[list[typeTree],dict,list[symbol]]=>
      list[typeTree].
  substTypeList(Tps,Env,Outer) => { substType(Tp,Env,Outer)..Tp in Tps}.

  private substArgTypeList:[list[(typeTree,flowMode)],dict,list[symbol]]=>
      list[(typeTree,flowMode)].
  substArgTypeList(Tps,Env,Outer) => {(substType(Tp,Env,Outer),fl)..(Tp,fl) in Tps}.

  private substTypeMethods:[dict,dict,list[symbol]]=>
      dict.
  substTypeMethods(Mtds,Env,Outer) => 
      fields({vS(Fld,Mode,substType(Tp,Env,Outer))..vS(Fld,Mode,Tp) in Mtds.ext()}).

  isReferenced:[symbol,symbol,list[symbol],typeTree]{}.
  isReferenced(Pkg,TpN,_,uType(Pkg,TpN,_)).
  isReferenced(Pkg,TpN,Outer,uType(_,_,A)) :-
      t in A, isReferenced(Pkg,TpN,Outer,t).
  isReferenced(Pkg,TpN,Outer,typeVar(v)):-
      dT = typeVar(v).deRef(),
      ( typeVar(V).=dT ?
	  K=V.vrKey(),
	  \+K in Outer,
	  ( isReferenced(Pkg,TpN,Outer,V.upper())
	  | isReferenced(Pkg,TpN,Outer,V.lower()))
      | isReferenced(Pkg,TpN,Outer,dT)).
  isReferenced(Pkg,TpN,Outer,allType(Nm,G,T)) :-
      nOuter = [Nm,..Outer],
      ( isReferenced(Pkg,TpN,nOuter,G)
      | isReferenced(Pkg,TpN,nOuter,T)).
  isReferenced(Pkg,TpN,Outer,funType(A,_)) :-
      (t,_) in A,isReferenced(Pkg,TpN,Outer,t).
  isReferenced(Pkg,TpN,Outer,funType(_,T)) :-
      isReferenced(Pkg,TpN,Outer,T).
  isReferenced(Pkg,TpN,Outer,sconType(A,_)) :-
      (t,_) in A,isReferenced(Pkg,TpN,Outer,t).
  isReferenced(Pkg,TpN,Outer,sconType(_,T)) :-
      isReferenced(Pkg,TpN,Outer,T).
  isReferenced(Pkg,TpN,Outer,gramType(A,_)) :-
      (t,_) in A,isReferenced(Pkg,TpN,Outer,t).
  isReferenced(Pkg,TpN,Outer,gramType(_,T)) :-
      isReferenced(Pkg,TpN,Outer,T).
  isReferenced(Pkg,TpN,Outer,actType(A)) :-
      (t,_) in A,isReferenced(Pkg,TpN,Outer,t).
  isReferenced(Pkg,TpN,Outer,predType(A)) :-
      (t,_) in A,isReferenced(Pkg,TpN,Outer,t).
  isReferenced(Pkg,TpN,Outer,conType(A,_)) :-
      t in A,isReferenced(Pkg,TpN,Outer,t).
  isReferenced(Pkg,TpN,Outer,conType(_,T)) :-
      isReferenced(Pkg,TpN,Outer,T).
  isReferenced(Pkg,TpN,Outer,enuType(A)) :-
      isReferenced(Pkg,TpN,Outer,A).
  isReferenced(Pkg,TpN,Outer,typeDef(T,_)) :-
      isReferenced(Pkg,TpN,Outer,T).
  isReferenced(Pkg,TpN,Outer,typeDef(_,S)) :-
      t in S,isReferenced(Pkg,TpN,Outer,t).
  isReferenced(Pkg,TpN,Outer,faceType(M)) :-
      vS(_,_,t) in M.ext(),isReferenced(Pkg,TpN,Outer,t).
}
