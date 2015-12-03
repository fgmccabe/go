/*
 * The square spiral
 * Take a number, such as 3, and construct a square spiral:
 *
 * 1 2 3
 * 8 9 4
 * 7 6 5
 *
 * This works by recursively constructing an inner spiral, recursive step is N-2, 
 * and wrapping the outer layer around the inner spiral
 */
 
spiral{
  import go.io.
  import go.unit.
  import go.stdparse.
   
  spiral:[integer,list[integer]]=>list[list[integer]].
  spiral(0,[]) => [].
  spiral(1,[El]) => [[El]].
  spiral(K,Els) ::
    pickN(K**2-(K-2)**2,Els,fEls,rEls),  -- remove 1st N**2-(N-2)**2 elements, to form wrapper
  pickN(K,fEls,fRow,f1Els),            -- the first row in the spiral
  extendRows(K,spiral(K-2,rEls),f1Els,nRows,_) => -- wrap the outer ring around the inner spiral
                                                  [fRow,..nRows]. 
     
  extendRows:[integer,list[list[t]],list[t],list[list[t]],list[t]]{}.
  extendRows(N,[],Els,[Row],nEls) :--
      pickN(N,Els,nRow,nEls),
      Row=reverse(nRow).
  extendRows(N,[Row,..Rest],[El,..S],Orows,nEls) :--
      extendRows(N,Rest,S,nRows,[F,..nEls]),
      Orows=[[F,..Row]<>[El],..nRows].
  
  pickN:[integer,list[t],list[t],list[t]]{}.
  pickN(0,Els,[],Els).
  pickN(K,[E,..Ls],[E,..Row],Els) :-
      pickN(K-1,Ls,Row,Els).

  spiraltest:[integer]@=harness.
  spiraltest(_)<=harness.
  spiraltest(K)..{

    showLists:[list[list[integer]]]*.
    showLists([]) -> {}.
    showLists([Row,..L]) -> stdout.outLine(Row.show()); showLists(L).

    doAction() ->                        -- test the spiral generator
        showLists(spiral(K,iota(1,K*K))).
  }.

  main([]) ->
      checkUnit(spiraltest(10)).
  main([I]) ->
      checkUnit(spiraltest(naturalOf%%I)).
 }.
