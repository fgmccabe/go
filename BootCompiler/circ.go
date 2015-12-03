circ{
  -- This tests circularity checking in the compiler

  t[T]<~{pull:[]=>T. }.

  v1:integer = v2.

  v2:integer = v1+2.

  c:[integer]@>t[integer].

  c(I)..{
    x1:integer := I+x2.

    x2:integer := x3+v1.

    x3:integer := x1+v2.

    pull()=>x1.
  }.

}