assert{
  /* Test the assertion mechanism */
  import go.io.

  cType[t] <~ { 
	length:[]=>integer. 
	max:[]=>integer. 
	push:[t]*/*|length()<max()*/. 
	top:[]=>t/*|length()>0*/. 
	$( length()<max()).
      }.

  c:[integer]@>cType[t].
  c(Mx)..{
    L:list[t] := [].

    length()=>listlen(L).

    max()=>Mx.

    push(X)->
	L := [X,..L].

    top()::[E,.._].=L=>E.
  }.

  main(_) ->
      CC = c(100).
}
    