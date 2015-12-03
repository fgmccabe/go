rdf.n3parser{

  import go.io.
  import go.stdparse.
  import n3lex.

  -- Some standard names

  private RDFS_A:[rdfStore]=>string.
  RDFS_A(Store) => Store.lookupNameSpace('rdfs')<>"type".

  n3parser:[string]=>list[rdfStatement].
  n3parser(File) :: parse%%tokenizeFile(File).

  private parse:[list[rdfStatement]]-->list[yyToken].
  parse([]) --> eof.

  private simpleStatement:[rdfStore]-->list[yyToken].
  simpleStatement(Store) --> subject(Subject), propertyList(Store,Subject).

  private subject:[rdfResource]-->list[yyToken].
  subject(Sub) --> expression(Sub).

  private verb:[rdfResource]-->list[yyToken].
  verb(Pred) --> expression(Pred).
  verb(RDF_A) --> [Tk], { Tk.token()=A }.

  private object:[rdfResource]-->list[yyToken].
  subject(Obj) --> expression(Obj).



}