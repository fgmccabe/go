/*
 * A simple module implementing a person class
 */

org.go.person{
  import app.

  person[] ::= { name:()=>string, spouse:()=>person[], likes:(person[]){} }.

  purson(Name,Sp:person):person[]..{
    name()=>Name.

    likes(X) :- X.likes(this).
    spouse() => Sp.

    check(X) :- (par()-->X).
  }.

  noone:person[]..{
    name()=>"noone":-likes(this).
    name()=>"boo".
    spouse()=>this.
   likes(_):-false.
  }.

  student[] ::= { school:(()=>string) }.
  student[] <~ person[].

  student(Name:string,_) <= purson(Name,noone).
  student(N,School):student[]..{
    school()=>"nowhere" :- name()="noone".
    school()=>School :- \+name()="noone".
--    likes(X) :- X.likes(this).
--    name()=>N.
--    check(X:person[]) :- X.check().
--    spouse()=>purson.spouse(). -- :-spouse()!=noone.
    par() --> "hello", [X], { X in name() }.

  }.

  likes(X<~person[]) :- X.spouse()=noone.

  par(),"bye" --> "hello", [X], { X in noone.name() }.
}
