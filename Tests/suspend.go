/* 
 * Test the variable suspension stuff
 */
suspend{
--  import go.io.

  gen:[symbol]{}.
  gen('a') :- __logmsg("trying a").	 -- this should trigger the check
  gen('b') :- __logmsg("trying b").
  gen('c') :- __logmsg("trying c").
  gen('d') :- __logmsg("trying d").


  doMe:[]{}.
  doMe() :-
      check(X),gen(X),
      otherCode(X).
      

  check:[symbol++]{}.
  check(U) :-
      __logmsg("Testing "),
      chck1(U),
      __logmsg("Ok").

  chck1:[symbol]{}.
  chck1('b').
  chck1('d').

  otherCode:[symbol]{}.
  otherCode(X) :-
      __logmsg("Run ..."),
      X=='d',
      __logmsg("Second check ok").

  main(_) ->
      { doMe() }.
}
