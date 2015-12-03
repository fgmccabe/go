/*
 * Quicksort
 */

qsort{
  import go.io.
  import go.unit.
  import go.sort.

  i:[]@=comparable[integer].
  i..{
    less(X,Y) :- X<Y.
    equal(X,X).
  }.

  s:[]@=comparable[string].
  s..{
    less(X,Y) :- X<Y.
    equal(X,X).
  }.

  sorttest:[]@=harness.
  sorttest<=harness.
  sorttest..{
    doPred() :- checksorted(sort([1,3,2,50,1,23,-1],i),i),
                checksorted(sort(["","a","aa","b","ba","ab","bc"],s),s).

    checksorted:[list[t],comparable[t]]{}.
    checksorted([E,..L],C) :- verifysorted(E,L,C).

    verifysorted:[t,list[t],comparable[t]]{}.
    verifysorted(_,[],_).
    verifysorted(E,[D,..L],C) :- \+C.less(D,E),verifysorted(D,L,C).
  }.

  main(_) ->
      checkUnit(sorttest).
}.


  
