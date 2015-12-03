/* A program to solve sudoku puzzles
   (c) 2006 F.G. McCabe
 
   This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Contact: Francis McCabe <frankmccabe@mac.com>
 */
sudoku{
  import go.io.
  import go.setlib.
  import go.showable.
  import go.unit.
  import square.
  import go.stdparse.

  -- Each cell in the table is represented by a constraint structure
  constr <~ { curr:[]=>list[integer]. remove:[integer]*. assign:[integer]*. }.
  constr:[list[integer]]@>constr.
  constr(L0)..{
    L:list[integer] := L0.

    curr()=>L.
    remove(K) ->
	( K in L ? 
	    L := L \ [K]).
    assign(K) ->
	( K in L ? 
	    L := [K]).

    show()::L=[] => " {} ".
    show() => " "<>L.head().show()<>sh(L.tail(),L.head()).

    sh:[list[integer],integer]=>string.
    sh([],_) => "".
    sh([I,..Ir],K)::I=K+1 => "-"<>cSh(Ir,I).
    sh([I,..Ir],_) => ","<>I.show()<>sh(Ir,I).
    
    cSh:[list[integer],integer]=>string.
    cSh([],I) => I.show().
    cSh([I,..Ir],J)::J+1=I => cSh(Ir,I).
    cSh(Ir,I) => I.show()<>sh(Ir,I).
  }.

  lt[t] <~ { val:[]{}. }.

  cycle:[square[constr]]{}.
  cycle(T):-
      (:lt[logical]..{
	 done :logical := true.

	 filter:[list[constr]]*.
	 filter(Set) ->
	     ( El in Set *>
	       ( El.curr()=[K] ?
		   ((Ot::Ot.curr()!=[K]) in Set, K in Ot.curr() *>
		    Ot.remove(K);
		    done := false
		   )
	       )
	     );
	     ( unique(Set,Ot,U) *>
	       Ot.assign(U);
	       done:=false
	     ).

	 filterRows:[square[constr]]*.
	 filterRows(Table) ->
	     ( Table.row(i) *>
	       filter(Table.rowOf(i))).

	 filterCols:[square[constr]]*.
	 filterCols(Table) ->
	     ( Table.col(i) *>
	       filter(Table.colOf(i))).
	 
	 filterQuads:[square[constr]]*.
	 filterQuads(Table) ->
	     ( Table.col(i) *>
	       filter(Table.quadOf(i))).

	 val() :-
	     action{
	       filterRows(T);
	       filterCols(T);
	       filterQuads(T);
	       istrue done
	     }.
       }).val().

  solve:[square[constr]]*.
  solve(Table)->
      ( cycle(Table) ?
	  {}
      | stdout.outLine("After cycle");
	stdout.outLine(Table.show());
	solve(Table)).

  -- unique of a list of lists of integers L is true of some U 
  -- if U is in only one of the non-trivial sets of L
  -- A set is non-trivial if it has more than one element
  unique:[list[constr],constr,integer]{}.
  unique(L,O,U) :- 
      append(F,[O,..B],L),
      Cn = O.curr(),
      listlen(Cn)>1,
      U in Cn, \+ (E in F, U in E.curr()), \+ (E in B, U in E.curr()).

  parseConstr:[list[integer]]@>parseCell[constr].
  parseConstr(Deflt)..{
    parse(C) --> skip(),naturalOf(I), {I = 0 ? C=constr(Deflt) | C = constr([I])}.
  }.

  parseSudoku:[square[constr]]-->string.
  parseSudoku(Sq) --> naturalOf(I), skip(),",",skip(),
      parseSquare(Sq,parseConstr(iota(1,I))),skip().

  testSudoku:[string]@=harness.
  testSudoku(_)<=harness.
  testSudoku(Text)..{
    doAction() ->
	T = parseSudoku%%Text;
	stdout.outLine("Original");
	stdout.outLine(T.show());
	solve(T);
	stdout.outLine("Solved");
	stdout.outLine(T.show()).
  }.

  main([]) ->
      checkUnit(testSudoku("9,[0,0,0,7,0,0,0,0,0,"
			   "0,5,0,0,0,2,0,0,9,"
			   "8,0,0,4,1,0,0,0,0,"
			   "0,0,0,0,2,8,0,4,0,"
			   "0,0,0,0,4,0,5,0,7,"
			   "0,0,0,0,0,0,9,0,0,"
			   "0,0,4,0,9,5,3,0,6,"
			   "2,0,0,0,0,4,0,0,0,"
			   "0,0,7,0,0,0,0,0,8]")).

  main([F]) ->
      f = openInFile(F,unknownEncoding);
      Text = f.inText("");
      f.close();
      T = parseSudoku%%Text;
      stdout.outLine("Original");
      stdout.outLine(T.show());
      solve(T);
      stdout.outLine("Solved");
      stdout.outLine(T.show()).

  main(_) ->
      checkUnit(testSudoku("9,[0,0,0,0,5,0,0,6,0,"
			   "0,0,0,0,0,8,7,3,0,"
			   "0,3,0,1,0,0,0,0,0,"
			   "1,8,0,0,0,0,5,0,0,"
			   "2,0,0,6,0,0,0,1,9,"
			   "0,0,6,9,0,7,0,0,8,"
			   "0,4,0,5,0,1,0,9,0,"
			   "7,0,0,0,0,9,0,0,0,"
			   "3,0,0,7,0,0,0,5,0]")).

  main(_) ->
      checkUnit(testSudoku("9,[0,0,0,8,9,0,0,3,5,"
			   "1,0,0,0,0,0,0,8,0,"
			   "0,5,0,6,7,3,0,2,0,"
			   "0,0,2,9,0,4,0,0,0,"
			   "0,4,0,0,0,0,0,0,0,"
			   "3,6,0,0,0,0,0,5,4,"
			   "0,0,0,0,0,8,0,0,3,"
			   "8,0,7,0,1,0,0,0,2,"
			   "0,0,5,4,0,6,8,7,0]")).

  main(_) ->
      checkUnit(testSudoku("9,[0,0,1,0,2,0,0,0,0,"
			   "0,0,7,5,0,0,0,0,0,"
			   "0,3,0,0,0,0,1,7,0,"
			   "0,0,0,0,0,0,5,0,0,"
			   "1,0,6,0,0,0,9,0,2,"
			   "0,0,5,9,3,8,6,0,0,"
			   "0,0,0,0,0,0,0,8,4,"
			   "3,0,4,0,1,0,2,9,0,"
			   "0,0,0,4,0,0,3,0,1]")).
  main(_) ->
      checkUnit(testSudoku("9,[0,0,0,0,0,2,1,7,0,
			   0,0,7,0,0,5,0,0,0,
			   0,0,0,0,6,0,0,0,0,
			   0,0,4,9,0,0,0,0,0,
			   0,2,0,1,0,0,0,9,8,
			   0,0,0,0,5,4,0,0,6,
			   0,8,0,5,3,0,0,6,0,
			   0,7,3,2,0,9,0,0,0,
			   0,1,5,0,0,0,3,0,9]")).
}