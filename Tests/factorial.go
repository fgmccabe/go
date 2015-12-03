factorial{

  import go.unit.
  import go.io.

  fact:[integer,integer]{}.
  fact(0,1).
  fact(N,F) :-  
      action{stdout.outLine("Entering fact rule: "<>(N,F).show())},     
      action{stdout.outLine("Calling less1: "<>(N1,N).show())},     
      action{stdout.outLine("Calling mult: "<>(N,F1,F).show())},     
      less1(N1,N),
      mult(N,F1,F),
      N@@N>0,
      action{stdout.outLine("Calling fact: "<>(N1,F1).show())},     
      fact(N1,F1),
      action{stdout.outLine("Exiting fact call: "<>(N1,F1).show())}.

  less1:[integer,integer]{}.
  less1(N1,N)::nonvar(N),nonvar(N1) :-- N=N1+1.
  less1(N1,N):-- (var(N1),nonvar(N),N1=N-1 | 
		  action{stdout.outLine("Suspending less1: "<>(N1,N).show()<>" on "<>N1.show())},
		  N1@@(N=N1+1))!.

  mult:[integer,integer,integer]{}.
  mult(N,F1,F)::nonvar(N),nonvar(F1),nonvar(F):-- F=F1*N.
  mult(N,F1,F) :-- (nonvar(N),F1@@(F=F1*N) | 
		    nonvar(F),
		    action{stdout.outLine("Suspending mult: "<>(N,F1,F).show()<>" on "<>N.show())},
		    N@@(F1=F/N) | 
		    action{stdout.outLine("Suspending mult: "<>(N,F1,F).show()<>" on "<>(N,F1).show())},
		    N@@(F1@@F=F1*N))!.

  main(_) -> -- {fact(3,F)};stdout.outLine("fact of 3 ="<>F.show());
      {F=120,fact(N,F)};stdout.outLine("inverse fact of "<>F.show()<>"="<>N.show()).

}
