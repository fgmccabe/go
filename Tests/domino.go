domino{
/*
 * Another example from the Prolog competition book
 * stone(x,y) is a domino stone with x and y spots on it
 */

  import go.io.

  stone:[integer,integer]{}.
  stone(2,2).
  stone(4,6).
  stone(1,2).
  stone(2,4).
  stone(6,2).

  -- The structure we use has two lists
  place:[list[(integer,integer)],list[(integer,integer)],
	 list[(integer,integer)]]{}.

  place([H,..and],[],S) :- place(and,[H],S).
  place([],stones,stones).

  place(Hand,[(x,u),..stones],S):-
      append(F,[(x,y),..B],Hand),
      place(F<>B,[(y,x),(x,u),..stones],S).
  place(Hand,[(x,u),..stones],S):-
      append(F,[(y,x),..B],Hand),
      place(F<>B,[(y,x),(x,u),..stones],S).
  place(Hand,stones,S):-
      append(sF,[(u,x)],stones),
      append(F,[(x,y),..B],Hand),
      place(F<>B,sF<>[(u,x),(x,y)],S).
  place(Hand,stones,S):-
      append(sF,[(u,x)],stones),
      append(F,[(y,x),..B],Hand),
      place(F<>B,sF<>[(u,x),(x,y)],S).

  domino:[]=>list[(integer,integer)].
  domino()::place({(x,y)||stone(x,y)},[],S) => S.

  main(_) ->
      stdout.outLine("Domino position is "<>domino().show())
}

  
