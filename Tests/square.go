square{
  -- Implement a model that can access a square as a set of rows, a set of columns 
  -- or a set of quadrants
  import go.showable.
  import go.io.
  import go.stdparse.

  square[t] <~ { 
	colOf:[integer]=>list[t]. 
	rowOf:[integer]=>list[t].
	quadOf:[integer]=>list[t].
	cellOf:[integer,integer]=>t.
	row:[integer]{}.
	col:[integer]{}.
	quad:[integer]{}.
      }.

  square:[list[t]]@>square[t].
  square(Init)..{
    Els:list[t] = Init.
    Sz:integer = itrunc(sqrt(listlen(Init))).
    Sq:integer = itrunc(sqrt(Sz)).
    S2:integer = Sq*Sq*Sq.

    intRange:[integer,integer+]{}.
    intRange(1,_).
    intRange(J,Mx) :- Mx>1,intRange(K,Mx-1),J=K+1.

    row(I) :- intRange(I,Sz).
    col(I) :- intRange(I,Sz).
    quad(I) :- intRange(I,Sz).

    rowOf(I) => front(drop(Els,(I-1)*Sz),Sz).

    colOf(Col) => clOf(drop(Els,Col-1)).

    clOf:[list[t]] => list[t].
    clOf([]) => [].
    clOf([El,..L]) => [El,..clOf(drop(L,Sz-1))].

    -- The calculation ((i-1) quot sqrt(Sz))*Sz^1.5+((i-1)mod sqrt(Sz))
    -- gives the index of the first element of the ith quadrant
    -- The Sz^1.5 figure is the number of elements in a single quadrant
    quadOf(i) => valof{
		   Ix = ((i-1) quot Sq)*S2+imod((i-1),Sq)*Sq;
		   valis qdOf(drop(Els,Ix),Sq)
		 }.

    qdOf:[list[t],integer] => list[t].
    qdOf(_,0)=>[].
    qdOf(Table,Cnt) => front(Table,Sq)<>qdOf(drop(Table,Sz),Cnt-1).
    
    cellOf(Row,Col) => nth(Els,(Row-1)*Sz+(Col-1)).

    show() => showSquare(Els).flatten("").

    showSquare:[list[t]]=>dispTree.
    showSquare(L) => n(showRow(0,L)).

    showRow:[integer,list[t]]=>list[dispTree].
    showRow(Sz,[]) => [s("|\n")].
    showRow(Sz,L) => [s("|\n"),..showRow(0,L)].
    showRow(Ix,[El,..L]) => [s(El.show()),..showRow(Ix+1,L)].
  }.

  parseCell[t] <~ { parse:[t]-->string }.

  parseSquare:[square[t],parseCell[t]]-->string.
  parseSquare(Sq,P) --> skip(), parseList(L,P), Sq = square(L).

  parseList:[list[t],parseCell[t]]-->string.
  parseList([],_) --> "[]".
  parseList([El,..Rest],P) --> "[", skip(), P.parse(El), skip(), parseMore(Rest,P).

  parseMore:[list[t],parseCell[t]]-->string.
  parseMore([El,..Rest],P) --> ",", P.parse(El), skip(), parseMore(Rest,P).
  parseMore([],_) --> "]".
  parseMore([],_) --> eof.

  skip:[]-->string.
  skip() --> [X],{whiteSpace(X)}, skip().
  skip() --> "#",skipToEol(),skip().
  skip() --> "".

  skipToEol:[]-->string.
  skipToEol() --> "\n".
  skipToEol() --> [C],{__isZlChar(C)}.
  skipToEol() --> [C], {\+__isZlChar(C)}, skipToEol().
  skipToEol() --> eof.

}