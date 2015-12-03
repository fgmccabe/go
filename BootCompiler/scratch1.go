/*
 * proposal for objects, terms etc in Go
 */

program{
  import "sys:go/stdio".
  import "sys:go/stdlib".

  person[] ::= { name:(()=>string) }.

  person(Name):person[].{
    name()=>Name.
    person(Name).                       -- auto-generated definition
    equal(P<~person[]).
  }.

  student[] ::= { school:(()=>string) }.
  student[] <~ person[].

  student <= person.
  student(School):student[].{
    school()=>School.
  }.

  main(Args) ->
      stdout.outLine("Hello world").

  countSchools([student(C),..L],S) => countSchools(L,S\/[C]).
  countSchools([],S) => S.

  createStudent(Name,School) => $student(Name,School).

  labelStudent(Name,School) => student(Name,School).

  $term = X::(X=gensym(),assert(X<=term))
}

module org.go.tree{
  import org.go.sys.show.	-- access the show interface

  -- tree is polymorphic in a show-able type, and is itself show-able
  tree[a<~show[]] ::= { isEmpty:(){}, left:(()=>tree[a]), right:(()=>tree[a])}.
  tree[a] <~ show[].

  -- empty is a constructor for the tree type
  empty:tree[a]..{
    isEmpty().

    left()=>exception error("empty tree",'fail').
    ...
    show()=>"()".
  }.

  -- node is also a constructor for the tree type
  node(L:tree[a],R:tree[a],B:a):tree[a]..{
    isEmpty():-false.
    left()=>L.
    ...
    show()=>"("<>L.show()<>B.show()<>R.show()<>")".
  }.

  lrWalk(@isEmpty()).
  lrWalk(@node(L,R,B)) :-
      lrWalk(L),
      B.show(),
      lrWalk(R).

}

