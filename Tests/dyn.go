dyn{
  import go.io.
  import go.unit.
  import go.dynamic.
   
  male:dynamic[symbol] = dynamic(['f','g','h']).
  person:dynamic[symbol] = dynamic(['a','b','g', 'h']).

  dyntest:[]@=harness.
  dyntest<=harness.
  dyntest..{
    doAction() ->
	stdout.outLine("person = "<>person.show());
	stdout.outLine("male = "<>male.show());
        person.add('f');
	stdout.outLine("person = "<>person.show());
        person.add('i');
	stdout.outLine("person = "<>person.show());
	( male.mem(X), person.mem(X) *>
	  stdout.outLine(X.show()<>" is a male person"));
        (male.mem(X) *> person.mem(X) ? 
	   stdout.outLine("one Ok") | stdout.outLine("one Bad"));
        male.del('f');
	( male.mem(X), person.mem(X) *>
	  stdout.outLine(X.show()<>" is now a male person"));
	male.delallc(:dynTest[symbol]..{
		       check(X) :- person.mem(X)
		     });
	stdout.outLine("male now = "<>male.show());
	( male.mem(X), person.mem(X) *>
	  stdout.outLine(X.show()<>" is a male person"));
        (person.mem(X) *> male.mem(X) ? 
           stdout.outLine("two Bad") | stdout.outLine("two Ok")).
  }.

  main(_) ->
      checkUnit(dyntest).
}
