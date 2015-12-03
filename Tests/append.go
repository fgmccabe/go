append{
  import go.io.
  import go.unit.
  import go.stdparse.

  -- Test regular append and 'slow' append

  private naive:[list[t],list[t]]{}.
  naive([],[]).
  naive([E,..L],R) :- naive(L,I),append(I,[E],R).

  private naiveS:[list[t],list[t]]{}.
  naiveS([],[]).
  naiveS([E,..L],R) :- naiveS(L,I),R=app(I,[E]).

  private app:[list[t],list[t]]=>list[t].
  app(@eof(),X)=>X.
  app(@hdtl(E,X),Y) => app(X,Y).cons(E).

  naivetest:[integer,integer]@=harness.
  naivetest(_,_)<=harness.
  naivetest(i,Ln)..{
    doAction() ->
        { O = iota(1,Ln),
          N = ticks(),
          iter(i,O),
          T = 2.0*(ticks()-N)};
        stdout.outLine(i.show()<>" reverses of "<>Ln.show()<>
                       " elements at "<>(n2float((Ln*Ln+Ln)*i)/T).show()<>" LIPS").

    doPred():-
        O = iota(1,Ln),
        naive(O,R),
        R=reverse(O).

    iter:[integer,list[t]]{}.
    iter(0,_).
    iter(N,I):- naiveS(I,_),iter(N-1,I).
  }.

  main([C,I,.._])->
      i = naturalOf%%I;
      count = naturalOf%%C;
      checkAction(naivetest(i,count));
      checkPred(naivetest(i,count)).
  main([]) ->
      checkAction(naivetest(20,300));
      checkPred(naivetest(1,300)).
}