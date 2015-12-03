rdftest{
  import go.io.
  import go.unit.
  import go.rdfstore.

  rdftest:[]@=harness.
  rdftest<=harness.
  rdftest..{
    doAction() ->
	R = $rdfstore;
	R.put(rdf('s1','v','o'));
	R.put(rdf('s1','v','o2'));
	R.put(rdf('s2','v','o'));
	R.put(rdf('s2','v1','o1'));
	R.put(rdf('o2','v','o2'));
	( R.query([rdf(S,V,O),rdf(O,V,O1)]) *>
	  stdout.outLine(S.show()<>" leads to "<>O1.show()));
	stdout.outLine(R.ext().show());
	stdout.outLine(R.show()).
  }.

  main(_) ->
      checkUnit(rdftest).
}