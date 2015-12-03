/*
 * Parse an ontology file and load into a DL store
 */

dl.parse{
  import go.stdparse.
  import go.io.
  import go.hash.
  import dl.dllex.
  import dl.terms.

  private nameSpaces <~ {
	defineNamespace:[symbol,string]*.
	mapName:[symbol,string]=>string.
      }.

  

  private ns:[]@>nameSpaces.
  ns()..{
    ns:hash[symbol,string] = hash([],8).

    defineNamespace(Nm,url) ->
	ns.insert(Nm,url).

    mapName(Pr,Lcl)::ns.present(Pr,url) => url<>Lcl.
  }.

  -- Load an ontology from a dl file
  importDlOntology:[Ontology,String]*.
  importDlOntology(O,Fname) ->
      ( Tokens = yyTokenizeAll(getFile(Fname,utf8Encoding));
	NS=ns();
	{ parseOntology(O,NS) --> Tokens }
      ) onerror (
       error(Bec,_) ->
	   raise error("Could not import "<>Fname<>"\nbecause "<>Bec,
		       'eNOPARSE')
      ).

  private parseOntology:[Ontology,nameSpaces] --> list[yyToken].
  parseOntology(O,NS) --> header(_),
      parsePrefixes(NS),
      parseOnt(O,NS).

  private parsePrefixes:[nameSpaces] --> list[yyToken].
  parsePrefixes(N) --> [@isToken(PREFIX)], [@isToken(ID(Nm))],
      [@isToken(ST(url))], [@isToken(TERM)],
      action{
	N.defineNamespace(Nm,url)
      },
      parsePrefixes(N).
  parsePrefixes(_) --> [].

  private header:[symbol]-->list[yyToken].
  header(Nm) --> [Itk@isToken(ID(Nm))], 
      ( [@isToken(LBRCE)] | raise error("left brace expected at "<>
					Itk.line().show(),'noPARSE')).
  header(_) --> raise error("ontology name expected",'noPARSE').

  private parseOnt:[Ontology,nameSpaces]-->list[yyToken].
  parseOnt(O,NS) -->
      statement(O,NS)!, [@isToken(TERM)],
      parseOnt(O,NS).
  parseOnt(_,_) --> [@isToken(RBRCE)].

  private statement:[Ontology,nameSpaces] --> list[yyToken].
  statement(O,_) --> [@isToken(ID('import')), @isToken(ST(Fl))],
      action{ importDlOntology(O,Fl) }.
  statement(O,NS) --> simple(O,NS,Df),
      [@isToken(SUB)],
      concept(O,NS,C),
      action{ O.classify(dlSub(Df,C)) }.
  statement(O,NS) --> simple(O,NS,Df),
      [@isToken(IS)],
      concept(O,NS,C),
      action{ O.classify(dlDefn(Df,C)) }.
  statement(O,NS) --> individual(O,NS,I),
      [@isToken(LBRCE)],
      concept(O,NS,C),
      [@isToken(RBRCE)],
      action{O.classify(dlSat(I,C))
      }.

  private concept:[Ontology,nameSpaces,dlConcept-] --> list[yyToken].
  concept(O,NS,dlAnd(Conj)) --> conjuncts(O,NS,Conj).
  
  private conjuncts:[Ontology,nameSpaces,list[dlConcept]-] --> list[yyToken].
  conjuncts(O,NS,[C,..R]) --> simplex(O,NS,C),
      ( [@isToken(AND)], conjuncts(O,NS,R)
      | R=[]).
  conjuncts(_,_,[]) --> [].

  private simplex:[Ontology,nameSpaces,dlConcept] --> list[yyToken].
  simplex(_,_,dlExists(dlRole(C))) --> [@isToken(EXISTS), @isToken(ID(C))].
  simplex(O,NS,dlFills(dlRole(C),Ind)) --> [@isToken(ID(C)), @isToken(ROLE)],
      individual(O,NS,Ind).
  simplex(O,NS,dlEnum(L)) --> [@isToken(LBRCE)], enumerated(O,NS,L), [@isToken(RBRCE)].
  simplex(O,NS,dlAll(dlRole(R),C)) --> [@isToken(ALL),@isToken(ID(R)),@isToken(ROLE)],
      simplex(O,NS,C).
  simplex(O,NS,C) --> simple(O,NS,C).
  simplex(O,NS,C) --> [@isToken(LPAR)], concept(O,NS,C), [@isToken(RPAR)].

  private enumerated:[Ontology,nameSpaces,list[dlIndividual]-]-->list[yyToken].
  enumerated(O,NS,[Id,..L]) --> individual(O,NS,Id), [@isToken(COMMA)],
      enumerated(O,NS,L).
  enumerated(O,NS,[Id]) --> individual(O,NS,Id).
  enumerated(_,_,[]) --> [].

  private simple:[Ontology,nameSpaces,dlConcept] --> list[yyToken].
  simple(_,NS,dlConcept(C)) --> [@isToken(QNAME(P,L))], 
      C=implode(NS.mapName(implode(P),L)).
  simple(_,_,dlThing) --> [@isToken(ID('thing'))].
  simple(_,_,dlConcept(C)) --> [@isToken(ID(C))].

  private individual:[Ontology,nameSpaces,dlIndividual] --> list[yyToken].
  individual(_,_,dlI(Nm)) --> 
      [@isToken(ID(Nm))].
  individual(_,NS,dlI(Nm)) --> 
      [@isToken(QNAME(P,L))], Nm=implode(NS.mapName(implode(P),L)).
  individual(_,_,dlInt(Nm)) --> 
      [@isToken(IN(Nm))].
  individual(_,_,dlFlt(Nm)) --> 
      [@isToken(FT(Nm))].
  individual(_,_,dlStr(S)) --> 
      [@isToken(ST(S))].
}