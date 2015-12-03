/* Standard Description Logic library
   (c) 2007 F.G. McCabe
 
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

dl.dl{
  import go.io.
  import go.dynamic.
  import go.hash.
  import go.setlib.
  import go.stdparse.
  import go.showable.
  import dl.terms.
  import dl.parse.

  DlOntology:[string]@>Ontology.
  DlOntology(ontologyNm)..{
    def:hash[symbol,dlConcept] = hash([],128).
    sub:dynamic[(dlConcept,dlConcept)] = dynamic([]).
    ind:dynamic[(dlIndividual,dlConcept)] = dynamic([]).
    con:dynamic[(dlIndividual,dlConcept)] = dynamic([]).
    
    name() => ontologyNm.

    classify(dlDefn(dlConcept(Name),D))::\+def.present(Name,_) ->
	classifyDefn(Name,D).
    classify(dlSub(dlConcept(Name),C))::\+def.present(Name,_) ->
	classifySubclass(Name,C).
    classify(dlSat(Name,C))::\+ind.mem((Name,_)) ->
	classifyIndividual(Name,C).
    classify(St) -> raise error("Could not classify "<>St.show(),'eFAIL').

    private classifyDefn:[symbol,dlConcept]*.
    classifyDefn(Name,D) ->
--	stdout.outLine("Classifying definition "<>Name.show());
	MSS = mostSpecificSubsumers(D);
	MGS = mstGSess(D,MSS);
--	stdout.outLine("MSS = "<>MSS.show());
--	stdout.outLine("MGS = "<>MGS.show());
	( MSS/\MGS==[] ?
	    ( a in MGS,sub.mem((a,ss)),ss in MSS *>
	      sub.del((a,ss))));
	(aa in MSS *> sub.add((dlConcept(Name),aa)));
	(gg in MGS *> sub.add((gg,dlConcept(Name))));
	-- handle constants
	C = { c || a in MSS, satisfies(c,a), \+ ( A in MGS, satisfies(c,A)) };
--	stdout.outLine("C = "<>C.show());
	( c in C *>
	  ( satisfies(c,D) ?
	      ( con.delallc(:dynTest[(dlIndividual,dlConcept)]..{
			      check((c,Xc)):- checkSubsumes(Xc,MSS)
--				  X in MSS *> -- bug in the compiler
--				  subsumes(Xc,X)
			    });
		con.add((c,dlConcept(Name))))));
	add(dlDefn(dlConcept(Name),D)).

    private classifySubclass:[symbol,dlConcept]*.
    classifySubclass(Name,C) ->
	MSS = mostSpecificSubsumers(C);
	(aa in MSS *> sub.add((dlConcept(Name),aa)));
	add(dlSub(dlConcept(Name),C)).

    private classifyIndividual:[dlIndividual,dlConcept]*.
    classifyIndividual(Name,C) ->
	MSS = mostSpecificSubsumers(C);
	( aa in MSS *>
	  con.add((Name,aa)));
	ind.add((Name,C)).

    checkSubsumes:[dlConcept,list[dlConcept]]{}.
    checkSubsumes(Xc,MSS) :-
	X in MSS *>
	subsumes(X,Xc).

    -- Most specific Subsumers of a symbol
    mostSpecificSubsumers:[dlConcept]=>list[dlConcept].
    mostSpecificSubsumers(D) => mss(D,[dlThing]).

    mss:[dlConcept,list[dlConcept]]=>list[dlConcept].
    mss(D,S) :: a in S, sub.mem((a1,a)), subsumes(a1,D), \+ subsumes(D,a1) =>
	mss(D,(S \ [a]) \/ { ax || sub.mem((ax,a)), subsumes(ax,D),\+subsumes(D,ax) }).
    mss(_,S) => S.

    
    mstGSess:[dlConcept,list[dlConcept]]=>list[dlConcept].
    mstGSess(D,G) :: a in G, \+ subsumes(D,a) =>
	mstGSess(D,(G \ [a])\/{ax || sub.mem((ax,a))}).
    mstGSess(D,G) => deleteParents(D,G).

    deleteParents:[dlConcept,list[dlConcept]]=>list[dlConcept].
    deleteParents(D,G) :: a in G, sub.mem((ax,a)), subsumes(D,ax) =>
	deleteParents(D, G \ [a]).
    deleteParents(_,G) => G.

    -- Most General subsumees
    mostGeneralSubsumees(D) => convolve(D,{ax||sub.mem((ax,_)), subsumes(D,ax)},[]).

    convolve:[dlConcept,list[dlConcept],list[dlConcept]]=>list[dlConcept].
    convolve(_,[],S)=>S.
    convolve(D,[a,..L],S) :: b in S, subsumes(b,a) => convolve(D,L,S).
    convolve(D,[a,..L],S) :: b in L, subsumes(b,a) => convolve(D,L,S).
    convolve(D,[a,..L],S) :: subsumes(a,D) => convolve(D,L,S).
    convolve(D,[a,..L],S) => convolve(D,L,[a,..S]).
    
--    add:[dlStatement]*.
    add(dlDefn(dlConcept(Name),C)) ->
	def.insert(Name,C).
    add(dlSub(dlConcept(Name),dlAnd(C))) ->
	def.insert(Name,dlAnd([dlExtra(gensym("$extra")),..C])).
    add(dlSub(dlConcept(Name),C)) ->
	def.insert(Name,dlAnd([C,dlExtra(gensym("$extra"))])).

    subsumes(dlConcept(C),dlConcept(C)) :-- {}. -- An optimization hack
    subsumes(dlAnd(C),dlAnd(D)) :-- 
	Dl = tidyConjuncts(liftConjuncts(D),[],[],[],[],[]),
	( c in tidyConjuncts(liftConjuncts(C),[],[],[],[],[]) *>
	  ( d in Dl, subsume(c,d)!)).
    subsumes(dlAnd(C),D) :-- 
	Dl = tidyConjuncts(liftConjuncts([D]),[],[],[],[],[]),
	( c in tidyConjuncts(liftConjuncts(C),[],[],[],[],[]) *>
	  ( d in Dl, subsume(c,d)!)).
    subsumes(C,dlAnd(D)) :-- 
	Dl = tidyConjuncts(liftConjuncts(D),[],[],[],[],[]),
	( c in tidyConjuncts(liftConjuncts([C]),[],[],[],[],[]) *>
	  ( d in Dl, subsume(c,d)!)).
    subsumes(C,D) :-- 
	Dl = tidyConjuncts(liftConjuncts([D]),[],[],[],[],[]),
	( c in tidyConjuncts(liftConjuncts([C]),[],[],[],[],[]) *>
	  ( d in Dl, subsume(c,d)!)).

    subsume:[dlConcept,dlConcept]{}.
    subsume(dlThing,_).
    subsume(X,X).
    subsume(dlFills(R,I),dlFills(R,I)).
    subsume(dlExists(R),dlExists(R)).
    subsume(dlExists(R),dlFills(R,_)).
    subsume(dlAll(R,D),dlAll(R,D)).
    subsume(dlAll(R,D),dlAll(R,E)):- 
--	action{ stdout.outLine("Checking "<>D.show()<>" subsumes "<>E.show()) }, 
	subsumes(D,E).
    subsume(dlEnum(L1),dlEnum(L2)) :- subset(L2,L1).
    subsume(dlExtra(N),dlExtra(N)).

    normalize(dlAnd(conjuncts)) =>
	dlAnd(tidyConjuncts(liftConjuncts(conjuncts),[],[],[],[],[])).
    normalize(C)=>dlAnd(tidyConjuncts(liftConjuncts([C]),[],[],[],[],[])).

    private liftConjuncts:[list[dlConcept]]=>list[dlConcept].
    liftConjuncts([])=>[].
    liftConjuncts([dlAnd(C),..A]) =>
	liftConjuncts(C)<>liftConjuncts(A).
    liftConjuncts([dlConcept(C),..A]) :: def.present(C,Cp) =>
	liftConjuncts([Cp,..A]).
    liftConjuncts([C,..A]) => [C,..liftConjuncts(A)].
    
    private tidyConjuncts:[list[dlConcept],list[dlConcept],
			   list[dlConcept],list[dlConcept],list[dlConcept],
			   list[dlConcept]]=> list[dlConcept].
    tidyConjuncts([],Concepts,Fills,Exists,All,Enum) => 
	Concepts<>Fills<>Exists<>All<>Enum.
    tidyConjuncts([dlExists(R),..L],Concepts,Fills,Exists,All,Enum)::
	    append(FE,[dlExists(R),..RE],Exists) =>
	tidyConjuncts(L,Concepts,Fills,FE<>[dlExists(R),..RE],All,Enum).
    tidyConjuncts([dlExists(R),..L],Concepts,Fills,Exists,All,Enum) =>
	tidyConjuncts(L,Concepts,Fills,[dlExists(R),..Exists],All,Enum).
    tidyConjuncts([dlConcept(C),..L],Concepts,Fills,Exists,All,Enum) ::
	    dlConcept(C) in Concepts =>
	tidyConjuncts(L,Concepts,Fills,Exists,All,Enum).
    tidyConjuncts([dlConcept(C),..L],Concepts,Fills,Exists,All,Enum) =>
	tidyConjuncts(L,[dlConcept(C),..Concepts],Fills,Exists,All,Enum).
    tidyConjuncts([dlExtra(C),..L],Concepts,Fills,Exists,All,Enum) ::
	    dlExtra(C) in Concepts =>
	tidyConjuncts(L,Concepts,Fills,Exists,All,Enum).
    tidyConjuncts([dlExtra(C),..L],Concepts,Fills,Exists,All,Enum) =>
	tidyConjuncts(L,[dlExtra(C),..Concepts],Fills,Exists,All,Enum).
    tidyConjuncts([dlThing,..L],Concepts,Fills,Exists,All,Enum) =>
	tidyConjuncts(L,Concepts,Fills,Exists,All,Enum).
    tidyConjuncts([dlAll(_,dlThing),..L],Concepts,Fills,Exists,All,Enum) =>
	tidyConjuncts(L,Concepts,Fills,Exists,All,Enum).
    tidyConjuncts([dlAll(R,C),..L],Concepts,Fills,Exists,All,Enum)::
	    append(FE,[dlAll(R,C),..RE],All) =>
	tidyConjuncts(L,Concepts,Fills,Exists,
		      FE<>[dlAll(R,C),..RE],Enum).
    tidyConjuncts([dlAll(R,C),..L],Concepts,Fills,Exists,All,Enum)::
	    append(FE,[dlAll(R,D),..RE],All) =>
	tidyConjuncts(L,Concepts,Fills,Exists,
		      FE<>[dlAll(R,dlAnd([C,D])),..RE],Enum).
    tidyConjuncts([dlAll(R,C),..L],Concepts,Fills,Exists,All,Enum) =>
	tidyConjuncts(L,Concepts,Fills,Exists,[dlAll(R,C),..All],Enum).
    tidyConjuncts([dlFills(R,I),..L],Concepts,Fills,Exists,All,Enum) ::
	    dlFills(R,I) in Fills =>
	tidyConjuncts(L,Concepts,Fills,Exists,All,Enum).
    tidyConjuncts([dlFills(R,I),..L],Concepts,Fills,Exists,All,Enum) =>
	tidyConjuncts(L,Concepts,[dlFills(R,I),..Fills],Exists,All,Enum).
    tidyConjuncts([dlEnum(L1),..L],Concepts,Fills,Exists,All,Enum) =>
	tidyConjuncts(L,Concepts,Fills,Exists,All,[dlEnum(L1),..Enum]).

    satisfies(Ind,C)::nonvar(Ind) :--
	con.mem((Ind,D)),
	subsumes(C,D).
    satisfies(Ind,C) :--
	ind.mem((Ind,D)),
	subsumes(C,D).

    disp()=>n([s(ontologyNm),s("{\n"),
	       n(join({dlDefn(dlConcept(C),D).disp()..(C,D) in def.ext()},s(".\n"))),
	       s("\n}.\n")]).

    dump() ->
	stdout.outLine("Contents of def ...");
	( X in def.ext() *> stdout.outLine(X.show()));
	stdout.outLine("Contents of sub ...");
	( sub.mem(Y) *> stdout.outLine(Y.show()));
	stdout.outLine("Individuals ...");
	( ind.mem(Z) *> stdout.outLine(Z.show()));
	stdout.outLine("Individuals most specific subsumers ...");
	( con.mem(U) *> stdout.outLine(U.show()));
	stdout.outLine("========").
  }.

  loadOntology:[string]=>Ontology.
  loadOntology(File)::append(Nm,".dl",File) =>
      valof{
	O = DlOntology(Nm);
	importDlOntology(O,File);
	valis O
      }.
  loadOntology(File) =>
      raise error("File name ["<>File<>"] should end with .dl",'eINVAL').

  importOntology:[Ontology,string]*.
  importOntology(kb,File)::append(_,".dl",File)->
      importDlOntology(kb,File).

}