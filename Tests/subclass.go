subclass{
  import go.io.
  import go.unit.

  person[] <~ { name:[]=>string. spouse:[]=>person. lives:[location[]]{} }.

  location <~ { address:[]=>string }.

  nowhere:[]@=location.
  nowhere..{
    address()=>"no where".
  }.

  street:[string]@=location.
  street(Name)..{
    address()=>Name
  }.

  person:[string,person]@=person.
  person(Name,Sp)..{
    name()=>Name.
    spouse()=>Sp.
    show() => Name<>" married to "<>Sp.show().
    lives(nowhere).
  }.

  noone:[]@=person.
  noone..{
    name()=>"noone".
    spouse()=>this.
    show()=>"noone".
    lives(nowhere).
  }.

  student <~ { school:[]=>string }.
  student <~ person.

  student:[string,string,person]@=student.
  student(Name,S,P) <= person(Name,P).
  student(_,School,_)..{
    school()=>School.
    show()=>person.show()<>" studying at "<>School.
    lives(street(School)).
  }.

  coHabit:[person+,person+]{}.
  coHabit(P1@lives(X),P2@lives(X)).

  subtest:[]@=harness.
  subtest<=harness.
  subtest..{
    doPred() :-
        \+(noone <= person(_,_)),
	(student("Me","There",noone) <= person(_,_)).
    doAction() ->
	(student("Me","There",noone) <= Lbl) *> stdout.outLine(Lbl.show()).
  }.

  main(_) ->
      checkUnit(subtest).
}

  