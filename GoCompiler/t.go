/*
 * Define the type language for the Go! compiler
 */
t{
  import misc.

  typeTree <~ {
	deRef:[] =>typeTree. 
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
       deRef:[] =>typeTree.
      }.

  typeBinding::= varBind | tvarBind | typeBind | pkgBind | inheritBind.

  flowMode ::= inpMode | outMode | superMode | biMode.  -- flow mode for binding

  private resetNo:integer:=0.
  private resets:list[(integer,do)] := [].

  private mkreset:[do]*.
  mkreset(D) -> resetNo := resetNo+1; resets:=[(resetNo,D),..resets].

  voidType:[]@=typeTree.
  voidType..{
    deRef() => this.

    isvar():-false.

    lower()=>this.

    upper()=>this.
  }.

  undef:[symbol]@=typeTree.
  undef(_) <= voidType.
  undef(S)..{
    isvar().
  }.

  isUnbound:[typeTree]{}.
  isUnbound(undef(_)) :--true.

  topType:[]@=typeTree.
  topType<=voidType.
  topType..{
    show()=>"top".
  }.

  uType:[symbol,symbol,list[typeTree]]@=typeTree.
  uType(_,_,_) <= voidType.
  uType(Pk,Nm,A)..{
    lower() => uType(Pk,Nm,{a.upper() .. a in A}).

    upper() => uType(Pk,Nm,{a.lower() .. a in A}).
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

    bind(new)::undef(_).=Curr -> C=Curr;mkreset(:do..{ do()->stV(C) }); Curr:=new.
    bind(new)::typeVar(BB).=Curr -> BB.bind(new).
    bind(_) -> raise error("attempt to bind non-variable: "<>__stringOf(Curr,0,0),'fail').

    setUBound(Tp) -> mkreset(:do..{ do()->stA(Above) }); Above := Tp.
    setLBound(Tp) -> mkreset(:do..{ do()->stL(Below) }); Below := Tp.

    isVar() :- Curr.deRef().isvar().

    upper()=>Above.
    lower()=>Below.

    vrKey() => Nm.
  }.

  typeVar:[vrBind]@=typeTree.
  typeVar(B)..{
    deRef() => B.deRef().

    isvar() :- B.isVar().

    lower() => B.lower().

    upper() => B.upper().
  }.

  isReferenced:[symbol,symbol,list[symbol],typeTree]{}.
  isReferenced(Pkg,TpN,Outer,typeVar(v)):-
      dT = typeVar(v).deRef(),
      ( typeVar(V).=dT ?
	  K=V.vrKey(),
	  \+K in Outer,
	  ( isReferenced(Pkg,TpN,Outer,V.upper())
	  | isReferenced(Pkg,TpN,Outer,V.lower()))
      | isReferenced(Pkg,TpN,Outer,dT)).
}
