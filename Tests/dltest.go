dltest{
  import dl.dl.
  import go.xml.
  import go.unit.
  import go.io.

  dlTester:[list[string]]@=harness.
  dlTester(_) <= harness.
  dlTester(Fl)..{
    doAction() ->
	kb = DlOntology("test");
	( F in Fl *>
	  importOntology(kb,F);
	  kb.dump()
	).
  }.

  main([]) -> checkUnit(dlTester(["surgery.dl"])).
  main(F) -> checkUnit(dlTester(F)).
}.