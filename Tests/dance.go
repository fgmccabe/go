/*
 * The dance card scenario
 */
 
dance{
  import go.io.
  import go.stdparse.
  import go.unit.
  import go.mbox.
  import directory.
  
  danceProto ::= shallWe(symbol,dropbox[danceProto]) | 
                 whyNot(symbol,dropbox[danceProto]) | 
                 rainCheck(symbol,dropbox[danceProto]).
  
  dancer<~ {toTheDance:[directory]*. name:[]=>symbol}.

  -- define attribute value type locally
  drop:[dropbox[danceProto]]@=attVal.
  drop(X)..{
    show()=>X.show().
  }.

  gender:[symbol]@=attVal.
  gender(X)..{
    show()=>explode(X).
  }.

  id:[string]@=attVal.
  id(X)..{
    show()=>X.
  }.

  phemail:[integer]@>dancer.
  phemail(Limit)..{
    partners:list[(symbol,dropbox[danceProto])] := [].
    name()=>implode("Female "<>Limit.show()).

    toTheDance(dir) ->
        D = mailbox();
        dir.register([attr('gender',gender('phemail')),
                      attr('loc',drop(D.dropbox())),
                      attr('id',id("Female "<>Limit.show()))]);
        stdout.outLine(explode(name())<>" ready to dance with "<>Limit.show()<>" partners");
        dance(D).

    dance:[mailbox[danceProto]]*.
    dance(D)::listlen(partners)<Limit ->
        case D.next() in (
         shallWe(Who,Mail) ->
             partners := [(Who,Mail),..partners];
             Mail.post(whyNot(name(),D.dropbox()));
             dance(D)
        ).
    dance(D) -> 
        stdout.outLine(explode(name())<>" has cards marked for "<>{Who..(Who,_)in partners}.show());
        hangOut(D).

    hangOut:[mailbox[danceProto]]*.
    hangOut(D) ->
        case D.next() in (
         shallWe(_,M) ->
             M.post(rainCheck(name(),D.dropbox()))
        );
        hangOut(D).
  }.
  
  mail:[symbol]@>dancer.
  mail(Name)..{
    dancers:list[(symbol,dropbox[danceProto])] := [].
    name() => Name.
    
    toTheDance(dir) ->
        L = dir.find([attr('gender',gender('phemail'))],['loc','id']);
        Bx = mailbox();
        (E in L *>
         markCard(E,Bx));
        stdout.outLine("Dancer "<>explode(Name)<>
                       " has marked the cards of "<> {Who..(Who,_)in dancers}.show()).

    markCard:[list[attribute],mailbox[danceProto]]*.
    markCard(Desc,Dx)::attr('loc',drop(P)) in Desc ->
        P.post(shallWe(Name,Dx.dropbox()));
        case Dx.next() in (
         whyNot(Who,P) -> dancers := [(Who,P),..dancers];
             stdout.outLine(explode(Name)<>" marked the card of "<>explode(Who))
       | rainCheck(Who,P) -> stdout.outLine(explode(Who)<>" too busy to go with "<>explode(Name))
        ).
    markCard(Desc,_) ->
        stdout.outLine(Desc.show()<>" has no location").
  }.

  ballRoom:[integer,integer]*.
  ballRoom(M,F) ->
      Dx = client();			 -- directory client
      spawnFems(F,Dx);
      waitForMen(spawnMen(M,Dx)).
    
  spawnFems:[integer,directory]*.
  spawnFems(0,_) -> {}.
  spawnFems(K,D) -> spawn{ phemail(K).toTheDance(D)}; spawnFems(K-1,D).
  
  spawnMen:[integer,directory]=>list[thread].
  spawnMen(0,_) => [].
  spawnMen(K,D) => [spawn{ mail(implode("Male "<>K.show())).toTheDance(D)},.. spawnMen(K-1,D)].
  
  waitForMen:[list[thread]]*.
  waitForMen([]) -> {}.
  waitForMen([M,..en]) ->
      waitfor(M);
      stdout.outLine(M.show()<>" done");
      waitForMen(en).

  dancetest:[integer,integer]@=harness.
  dancetest(_,_) <= harness.
  dancetest(M,F)..{
    doAction() ->
        ballRoom(M,F)
  }.

  main([]) ->
      checkUnit(dancetest(4,4)).
  main([M,F]) ->
      checkUnit(dancetest(naturalOf%%M,naturalOf%%F)).

}.
