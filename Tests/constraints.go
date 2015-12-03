constraints{

  import go.io.

  constraints:[integer,integer]{}.
  constraints(X,Y) :-
      (Y@@Y>3)=Y1,
      (X@@X<Y)=X1,
      Y1 in [1,2,3,4,5,6],
      action{stdout.outLine("Y= "<>Y.show())},
      X1 in [4,3,2,1],
      action{stdout.outLine("X= "<>X.show())}.

--  main(_) -> (constraints(X,Y) *> stdout.outLine("Results, X is  "<>X.show()<>" Y is "<>Y.show())).
  main(_) -> (constraints(X,Y) *> stdout.outLine("Result are  "<>(X,Y).show())).
--  main(_) -> stdout.outLine("Tuple "<>(2,3).show()).

}
