danceOnt{
  import go.io.
  import go.owldllite.

  ${
    new(individual(id('polka'),[type('dance')],[]));
    new(individual(id('mary'),[type('femaleDancer')],
		   [value('wantsToDance',id('polka')),
		    value('hasDancedWithMe',id('bill'))]));
    defineClass('dancer',complete,[isa('person'),hasAValue('wantsToDance')]);
    defineClass('femaleDancer',complete,[isa('dancer'),isa('female')]);
    defineClass('maleDancer',complete,[isa('dancer'),isa('male')]);
    defineClass('dance',partial,[isa('activity')]);
    defineClass('male',partial,[isa('person')]);
    defineClass('female',partial,[isa('person')]);
    defineDisjointClasses([isa('male'),isa('female')]);
    defineProperty('wantsToDance',domain('dancer'),range('dance'),[]);
    defineProperty('hasDancedWithMale',domain('femaleDancer'),
		   range('maleDancer'),
		   [inverseOf('hasDancedWithFemale')]);
    defineClass('student',partial,[isa('person'),hasAValue('college')]);
    defineClass('dancingStudent',complete,[isa('student'),hasAValue('wantsToDance')]);
    normalise()
  }.

  main(_) ->
      ( classOf(id('mary'),Cl) *>
	stdout.outLine("Mary is a "<>Cl.show()));
      ( valueFor(id('mary'),Pr,Vl) *>
	stdout.outLine("Property: "<>Pr.show()<>" = "<>Vl.show()));
      ( subclass(S1,S2) *>
	stdout.outLine(S1.show()<>" is a subclass of "<>S2.show()));
      ( disjoint(D1,D2) *>
	stdout.outLine(D1.show()<>" is disjoint from "<>D2.show())).
}