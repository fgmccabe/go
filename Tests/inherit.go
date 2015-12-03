inherit{
  import go.io.
  import go.dynamic.

  
 
  spr[T] <~ { beliefs:dynamic[T] }.

  sub[T] <~ { name:symbol, check:(T){}}.

  spr(I):spr[symbol]..{
    beliefs = $dynamic(I).
  }.

  sub(I) <= spr(I).
  sub(_):sub[symbol]..{
    name = 'myName'.

    check(O) :- beliefs.mem(O).
  }.

  main(_) ->
      OO = $sub(['me','you']);
      ( OO.check('me')?
          stdout.outLine("Ok")
      | stdout.outLine("not Ok")).
}
