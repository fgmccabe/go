/*
 * A simple module implementing a person class
 */

person3{
  import go.io.
  import go.dynamic.
  person <~ { name:[]=>string. spouseName:[]=>string. spouse:[]=>person. lives:[location]{} }.

  location <~ { address:[]=>string }.

  nowhere:location..{
    address()=>"no where".
  }.

  street(Name:string):location..{
    address()=>Name
  }.

  persons:dynamic[person].
  persons=$dynamic([]).

  person(Name:string,SpNm:string):person..{
    name()=>Name.
    spouseName()=>SpNm. -- if . not give get lots of errors re missing braces
    spouse()::SpNm=="noone"=>noone.
    spouse()::SpNm !="noone",persons.mem(P),P.name()==SpNm => P.
    show() => Name<>" married to "<>SpNm.
    lives(nowhere).
    ${persons.add((this))}
  }.

  noone:person..{
    name()=>"noone".
    spouseName()=>"noone".
    spouse()=>this.
    show()=>"noone".
    lives(nowhere).
  }.

  students:dynamic[student].
  students=$dynamic([]).
  student <~ { school:[]=>string }.
  student <~ person.

  student(Name,S,P) <= person(Name,P).
  student(_:string,School:string,_:string):student..{
    school()=>School.
    show()=>person.show()<>" studying at "<>School.
    lives(street(School)). 
    ${students.add(this)}
 }.

  coHabit:[person,person]{}.
  coHabit(P1@lives(X),P2@lives(X)).


  main([]) -> P=$person("bill","noone");stdout.outLine(P.show());
      S=$student("mary","imperial","bill");-- {__break('here',0)}; 
      stdout.outLine(S.show());
      stdout.outLine(P.show());stdout.outLine(persons.ext().show());
      stdout.outLine({Q.name()..Q in persons.ext()}.show());
      stdout.outLine(students.ext().show()).

}
