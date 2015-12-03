/*
 * implement a search solution of the raging rapids puzzle
 */
rapids{
  import go.io.
  import go.unit.

  inout [] ::= i | o.

  
  perim[] ::= perim(inout[],inout[],inout[],inout[],inout[],inout[]).

  -- Top, upperleft, lowerleft, bottom, lowerright, upperright

  tile('A',i,i,o,o,i,o).
  tile('B',i,o,i,o,o,i).
  tile('C',o,o,i,i,o,i).
  tile('D',i,i,o,i,i,o).
  tile('E',o,i,o,o,i,o).
  tile('F',i,i,o,i,o,i).
  tile('G',o,i,o,i,i,o).
  tile('H',i,i,o,o,o,i).
  tile('I',o,o,i,o,i,o).
  tile('J',o,i,o,o,o,i).
  tile('K',o,o,i,i,i,o).
  tile('L',i,o,i,i,i,o).

  left(i,o,i,o,i,o,o,i).
  bottom(o,o,o).
  right(i,o,o,i,o,i,o,i).
  top(i,i,i).

  fitlr(X,Y,Z,X,Y,Z).                   -- fit left/right pair
  fittb(X,X).

  pick([X,..Y],X,Y).
  pick([X,..Y],A,[X,..Z]) :- pick(Y,A,Z).

  fitTiles() :-
      Tiles = ['A','B','C','D','E','F','G','H','I','J','K','L'],
      pick(Tiles,T1,Tl1),               -- first tile
      fit(T1,i,i,o,T11,T12,T13,T14),    -- top left tile
      pick(Tl1,T2,Tl2),                 -- second tile
      fit(T2,i,T14,T13,T21,T22,T23),    -- top middle tile
      pick(Tl2,T3,Tl3),                 -- third tile

}.