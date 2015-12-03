person{
  place <~ { name:[]=>string }.

  person <~ { age:[]=>integer. spouse:[]=>person }.

  nowhere:[]$=place.
  nowhere..{
    name()=>"nowhere"
  }.

  noone:[]$=person.
  noone..{
    age()=>0.
    spouse()=>noone.
  }.

  person:[integer,person]$=person.
  person(A,S)..{
    Sp:person := S.

    spouse()=>Sp.
    age()=>A
  }.

  student <~ person.
  student <~ { studies:[]=>list[string] }.

  lazy:[]$=student.
  lazy<=noone.
  lazy..{
    studies()=>[]
  }.

  employee <~ person.
  employee <~ { work:[]=>string. salary:[]=>integer }.

  shiftless:[]$=employee.
  shiftless<=noone.
  shiftless..{
    work()=>"macdonalds".
    salary()=>23.
  }.

  married:[person+,person]{}.
  married(X,Y) :-
      X.spouse()=Y.

  check:[]{}.
  check() :-
      married(shiftless,_).
    
}