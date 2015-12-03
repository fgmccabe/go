-- A test of module-level variables

vtest{
  foo := 1.5.
  bar = "a"<>"b".

  ex(X) ->
    foo:=X*foo.

  main(A) ->
      {__logmsg("foo = "<>foo^0)};
      ex(23);
      {__logmsg("foo = "<>foo^0)};
      ex(foo);
      {__logmsg("foo = "<>foo^0)}.
}