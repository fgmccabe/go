er{

  -- an attempt to create a syntax error

  foo((A,B)) => bar(A).

  bar(A) => jar(A).

  jar(A) => foo(A).

}
