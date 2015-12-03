/*
 * A package that supports the use of OWL DL Lite for ontologies and 
 * Ontology processing in Go!
 */
go.owldllite{
  import go.io.
  import go.dynamic.
  import go.hash.
  import go.xml.

  individual ::= individual(individualId, list[type], list[value]).
  individualId ::= id(symbol) | literal(string) | some(symbol,symbol).
  type ::= type(symbol).
  value ::= value(symbol,individualId).

  Individual <~ { 
	class:[type]{}.			 -- The class of the individual
	propVal:[symbol,individualId]{}.  -- Properties associated with the individual
	addClass:[type]*.
	addProp:[symbol,individualId]*.

	diffFrom:[individualId]{}.
	markDifferentFrom:[individualId]*.

	sameAs:[individualId]{}.
	markSameAs:[individualId]*.

	alias:[]=>Individual.
	setAlias:[Individual]*.
      }.

  Individual:[list[type],list[value]]@>Individual.
  Individual(Ts,Vs)..{
    Class:dynamic[type] = dynamic(Ts).

    Properties:dynamic[value] = dynamic(Vs).

    class(Cl)::hasAlias() :-- alias().class(Cl).
    class(Cl) :-- Class.mem(Cl).

    addClass(type(Cl)) -> {canonicalNameOf(Cl,ConN)};
	(class(type(ConN)) ? {} | Class.add(type(ConN))).

    propVal(P,V) :- Properties.mem(value(P,V)).

    addProp(P,V) :: propVal(P,V) -> {}.
    addProp(P,V) :: \+hasAlias() -> Properties.add(value(P,V)).
    addProp(P,V) -> Properties.add(value(P,V)); alias().addProp(P,V).

    canonAlias:Individual := this.

    alias() :: canonAlias!=this =>canonAlias.alias().
    alias() => this.

    setAlias(I) -> canonAlias := I.
    
    private hasAlias:[]{}.
    hasAlias():-canonAlias!=this.

    knownDiffs:dynamic[individualId] = dynamic([]).

    diffFrom(Nm)::canonAlias==this :--knownDiffs.mem(Nm).
    diffFrom(Nm) :-- canonAlias.diffFrom(Nm).

    markDifferentFrom(Nm) :: knownDiffs.mem(Nm) -> {}.
    markDifferentFrom(Nm) -> knownDiffs.add(Nm).

    -- We only query the canonicalized members knownSameAs DB
    knownSameAs:dynamic[individualId] = dynamic([]).
    sameAs(Nm)::canonAlias==this :-- knownSameAs.mem(Nm).
    sameAs(Nm) :-- canonAlias.sameAs(Nm).

    markSameAs(Nm) :: sameAs(Nm) -> {}. -- already in the same equivalence class
    markSameAs(Nm) :: hasAlias() ->
	ONm = objectFor(Nm);
	OA = ONm.alias();
	( OA!=ONm ?    -- Everyone has already an alias
	    Alias = alias();
	    setAlias(OA);
	    (Alias.diffFrom(Cl) *> OA.markDifferentFrom(Cl));
	    (Alias.sameAs(Cl) *> OA.markSameAs(Cl))
	| OA.setAlias(canonAlias);
	  ( OA.class(Cl) *> canonAlias.addClass(Cl));
	  ( OA.propVal(P,V) *> canonAlias.addProp(P,V))
	).
    markSameAs(Nm) ::\+hasAlias() -> ONm = objectFor(Nm);
	OA = ONm.alias();
	( OA!=ONm ?    -- The Nm has an alias
	    setAlias(OA);
	    (diffFrom(Cl) *> OA.markDifferentFrom(Cl));
	    (sameAs(Cl) *> OA.markSameAs(Cl))
	| canonAlias := Individual(Class.ext(),Properties.ext());
	  OA.setAlias(canonAlias);
	  ( OA.class(Cl) *> canonAlias.addClass(Cl));
	  ( OA.propVal(P,V) *> canonAlias.addProp(P,V))
	).
  }.

  -- Manage the known individuals
  private Individuals:hash[individualId,Individual] = hash([],64).

  new:[individual]*.
  new(individual(Id,Types,Values)) ->
      Individuals.insert(Id,Individual(Types,Values)).

  private objectFor:[individualId]=>Individual.
  objectFor(I) => Individuals.find(I).

  classOf:[individualId,symbol]{}.
  classOf(Id,Cl) :- objectFor(Id).class(type(Cl)).

  valueFor:[individualId,symbol,individualId]{}.
  valueFor(Id,Pr,Vl) :- objectFor(Id).propVal(Pr,Vl).

  addProp:[individualId,symbol,individualId]*.
  addProp(Id,Pr,Vl) -> objectFor(Id).addProp(Pr,Vl).

  -- Ontology axioms

  Class:[symbol+,modality,list[description]]{}.
  defineClass:[symbol,modality,list[description]]*.

  private clss::=clss(symbol,modality,list[description]).

  modality ::= partial | complete.

  description ::= isa(symbol) | hasAValue(symbol) | nothing |
		  intersectionOf(list[description]).

  private Classes:dynamic[clss] = dynamic([]).

  Class(Cl,Mo,Ds) :- Classes.mem(clss(Cl,Mo,Ds)).
  defineClass(Cl,Mo,Ds) -> Classes.add(clss(Cl,Mo,Ds)).
--      generateSubClass().

  private canonicalNameOf:[symbol,symbol]{}.
  canonicalNameOf(C,ConN) :: 
	  EquivalentClasses([ConN,..Cs]), 
	  C in [ConN,..Cs] :-- {}.
  canonicalNameOf(C,C) :-- {}.

  private EquivalentClasses:[list[symbol]]{}.
  private Equiv:dynamic[list[symbol]] = dynamic([]).

  EquivalentClasses(Ds) :- Equiv.mem(Ds).

  declareEquivalent:[symbol,symbol]*.
  declareEquivalent(C1,C2) -> 
      Equiv.add([C1,C2]);
      toNormalize := true.

  private Disjoint:dynamic[(description,description)] = dynamic([]).

  disjoint:[description,description]{}.
  disjoint(D1,D2) :- (Disjoint.mem((D1,D2)) | Disjoint.mem((D2,D1))).

  private Dj:dynamic[list[description]] = dynamic([]).
  DisjointClasses:[list[description]]{}.
  DisjointClasses(D) :- Dj.mem(D).

  defineDisjointClasses:[list[description]]*.
  defineDisjointClasses(D) :: DisjointClasses(D) -> {}.
  defineDisjointClasses(D) -> Dj.add(D); toNormalize := true.
  
  private SubClass:dynamic[(description,description)] = dynamic([]).

  subclass:[description,description]{}.
  subclass(D1,D2) :- SubClass.mem((D1,D2)).

  includedIn:[description,description]{}.
  includedIn(D,D).
  includedIn(D1,D2) :- subclass(D1,D2).

  private done:logical := false.
  private toNormalize:logical := true.

  private normalise:[]*.
  normalise()::\+toNormalize -> {}.
  normalise()-> done := false;
      generateBase();
      extendUntilClosed();
      toNormalize := false.

  private generateBase:[]*.
  generateBase() ->
      (Class(C,_,Desc), D in Desc *> recordSubClass(isa(C),D));
      (Class(Cc,complete,[hasAValue(P)]) *>
       recordSubClass(hasAValue(P),isa(Cc)));
      (Property(P1,domain(C1),_,_) *>
       recordSubClass(hasAValue(P1),isa(C1)));
      (Property(_,_,range(Ci),Flg), inverseOf(IP) in Flg *>
       recordSubClass(hasAValue(IP),isa(Ci)));
      (DisjointClasses(Djs), append(_,[D1,..Ds],Djs), D2 in Ds *>
       recordDisjoint(D1,D2)).

  private extendUntilClosed:[]*.
  extendUntilClosed()::done -> {}.
  extendUntilClosed() ->
      done:=true;
      ( subclass(D1,D2), subclass(D2,D3), D1!=D3 *>
	recordSubClass(D1,D3));
      ( subclass(xD1,xD2), disjoint(xD2,xD3), xD1!=xD3 *>
	recordDisjoint(xD1,xD3));
      ( Class(C,complete,Ds), Des in Ds,
	includedIn(D,Des),D!=isa(C),
	(OD in Ds, OD!=Des *> includedIn(D,OD))
	*> recordSubClass(D,isa(C)));
      extendUntilClosed().
  
  private recordSubClass:[description,description]*.
  recordSubClass(D1,D2) :: SubClass.mem((D1,D2)) -> {}.
  recordSubClass(D1,D2) -> done := false; SubClass.add((D1,D2)).
  
  private recordDisjoint:[description,description]*.
  recordDisjoint(D1,D2)::disjoint(D1,D2) -> {}.
  recordDisjoint(D1,D2) -> done:=false; Disjoint.add((D1,D2)).

  Property:[symbol+,domain,range,list[Flag]]{}.
  defineProperty:[symbol+,domain,range,list[Flag]]*.

  private Properties:dynamic[prop] = dynamic([]).
  private prop ::= prop(symbol,domain,range,list[Flag]).

  domain ::= domain(symbol).
  range ::= range(symbol).
  Flag ::= Functional | inverseOf(symbol) | inverseFunctional.

  Property(Pr,Dm,Rn,Fl) :- Properties.mem(prop(Pr,Dm,Rn,Fl)).

  defineProperty(Pr,Dm,Rn,Fl) -> Properties.add(prop(Pr,Dm,Rn,Fl));
      toNormalize := true.

  propertyRestrictionOfClass:[symbol,description]{}.
  propertyRestrictionOfClass(C,hasAValue(P)) :-
      subclass(isa(C),hasAValue(P)).
  
  ClassOf:[individualId,symbol]{}.
  ClassOf(Id,C) :-
      action{ normalise() },
      canonicalClassOf(Id,ConN),
      canonicalNameOf(C,ConN).

  private canonicalClassOf:[individualId,symbol]{}.
  canonicalClassOf(id(I),C) :-
      classOf(id(I),C).
  canonicalClassOf(id(I),S) :-
      Class(C,_,Descrs),
      isa(S) in Descrs,
      canonicalClassOf(id(I),C).
  canonicalClassOf(id(I),C) :-
      Class(C,complete,Descrs),
      (isa(S) in Descrs *> canonicalClassOf(id(I),S)),
      (hasAValue(P) in Descrs *> valueFor(id(I),P,_)).

  canonicalClassOf(some(_,P),C) :-
      Property(P,_,range(R),_),
      (R=C | subclass(isa(R),isa(C))).

}