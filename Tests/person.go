/*
 * A simple module implementing a person class
 */

person{
  person <~ { name:[]=>string. spouse:[]=>person. lives:[location]{} }.

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
    show() => Name.
    lives(nowhere).
  }.

  noone:[]@=person.
  noone..{
    name()=>"noone".
    spouse()=>this.
    show()=>"noone".
    lives(nowhere).
  }.

  student <~ { school:[]=>school }.
  student <~ person.

  school <~ { where:[]=>location}.

  queensgate:[]@=location.
  queensgate..{
    address()=>"180 Queen's gate"
  }.

  imperial:[]@=school.
  imperial..{
    where()=>queensgate
  }.

  student:[string,school,person]@=student.
  student(Name,_,P) <= person(Name,P).
  student(_,School,_)..{
    school()=>School.
    show()=>person.show()<>" studying at "<>School.where().address().
    lives(Lc):-Lc=School.where().
  }.

  coHabit:[person+,person+]{}.
  coHabit(@lives(X),@lives(X)).

  coHabitStudent:[student,person]{}.
  coHabitStudent(S,P) :- coHabit(S,P).
  coHabitStudent(S,P) :- coHabit(P,S).
}
