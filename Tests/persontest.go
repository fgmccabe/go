persontest{
  import go.io.
  import person.
  import go.unit.

  someone:[string]@=person.
  someone(Name)..{
    name()=>Name.
    spouse()=>noone.
    lives(nowhere).
  }.

  personTest:[]@=harness.
  personTest<=harness.
  personTest..{
    doAction() ->
        P = person#person("Pee",noone);
        go.io#stdout.outLine("I am "<>P.show());
        stdout.outLine("spouse is "<>P.spouse().show());
        S = student("Joe",person#imperial,P);
        stdout.outLine("S is "<>S.show());
        stdout.outLine("S's spouse is "<>S.spouse().show());
        ( S<=Cl *> stdout.outLine(S.show()<>" is an instance of "<>Cl.show())).
    doPred() :-
        coHabit(person("P",noone),noone),
        student("Joe",imperial,noone) <= person(_,_).
  }.
        
  main(_) -> 
      checkUnit(personTest).
}
     
  