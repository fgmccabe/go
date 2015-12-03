/*
 * A simple module implementing a person class
 */

p{
  person <~ { name:[]=>string}.

  person:[string,person]@=person.
  person(Name,Sp)..{
    name()=>Name.
    show() => Name<>" married to "<>Sp.show().
  }.

  school <~ {}.

  student <~ { school:[]=>school }.
  student <~ person.

  student:[string,school,person]@=student.
  student(Name,_,P) <= person(Name,P).
  student(_,School,_)..{
    school()=>School.
    show()=>person.show()<>" studying at "<>School.show()
  }.
}
