anon{
  -- Test anonymous class notation
  import go.io.
  import go.unit.

  fooType[T] <~ { get:[]=>T. set:[T]* }.

  foo:[T]@>fooType[T].
  foo(I:T)..{
    X:T := I.
    get()=>X.
    set(Y)-> X:=Y.

  }.

  other:[float]@>fooType[float].
  other(I)..{
    XX:fooType[float] = foo(I+34.0)..{   -- Anonymous class inside another class
      get() => foo.get().                -- set is inherited from foo

      check:[]=>float.
      check()=>I.
    }.

    get() => XX.get().
    set(Y) -> XX.set(Y).

    bar:[float]=>fooType[float].
    bar(U) => foo(I)..{
                get() => U+foo.get()
              }.
  }.

  anon:[]@=harness.
  anon<=harness.
  anon..{
    doAction() ->
        Ot = other(23.0);
        stdout.outLine("Ot = "<>Ot.get()^);
        Ot.set(45.0);
        stdout.outLine("Ot = "<>Ot.get()^);
        XXX = 'f';
        O = :fooType[symbol]..{
          XX:symbol := XXX.
          get() => XX.
          set(Y) -> XX := Y.
        };
        stdout.outLine("O = "<>O.get()^);
        O.set('g');
        stdout.outLine("O = "<>O.get()^).
  }.

  main(_) ->
      checkUnit(anon).
}
