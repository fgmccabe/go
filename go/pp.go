go.pp{
  -- A more substantial package for supporting pretty printing.

  -- The core is the pP type which represents different requests for display

  pP <~ { show:[]=>string. }.

  -- There are different constructors for pP for the different cases

  pNil:[]@=pP.
  pNil..{
    show() => flttn(0,[(0,this)]).
  }.

  -- A literal string
  pStr:[string]@=pP.
  pStr(_)<=pNil.

  -- A string intended to be quoted
  pQstr:[string]@=pP.
  pQstr(_)<=pNil.


  -- A potentially removable white space
  pPad:[]@=pP.
  pPad<=pNil.

  -- A symbol -- displaying its print name
  pSym:[symbol]@=pP.
  pSym(_)<=pNil.

  -- A sequence of entries
  pSeq:[list[pP]]@=pP.
  pSeq(_)<=pNil.

  -- A sequence interleaved with another element
  pTwine:[list[pP],pP]@=pP.
  pTwine(_,_)<=pNil.

  -- A each new line shows up aligned
  pAlign:[pP]@=pP.
  pAlign(_)<=pNil.

  -- Throw a new line
  pNewLine:[]@=pP.
  pNewLine<=pNil.

  -- integer
  pInt:[integer]@=pP.
  pInt(_)<=pNil.

  -- float
  pFlt:[float]@=pP.
  pFlt(_)<=pNil.

  -- A class implements pPrintable if it is pretty printable
  pPrintable <~ {
	-- The nesting depth of the term being displayed
	-- if depth = 0, then display ellipsis form
	disp:[integer]=>pP.
      }.

  -- Convenience function
  showPP:[pPrintable]=>string.
  showPP(P) => P.disp(32767).show().

  -- The core algorithm for formatting the pP structure
  private flttn:[integer,list[(integer,pP)]]=>string.
  flttn(_,[(Col,pNewLine),..L]) => [`\n,..spaces(Col,flttn(Col,L))].
  flttn(Col,[(_,pNil),..L])=>flttn(Col,L).
  flttn(Col,[(_,pAlign(S)),..L])=>flttn(Col,[(Col,S),..L]).
  flttn(Col,[(Ind,pSeq(S)),..L]) => flttn(Col,{(Ind,s)..s in S}<>L).
  flttn(Col,[(Ind,pTwine(S,I)),..L]) => flttn(Col,interleave({(Ind,s)..s in S},(Ind,I))<>L).
  flttn(Col,[(_,pStr(S)),..L]) => flttnStr(S,Col,L).
  flttn(Col,[(_,pSym(S)),..L]) => flttnStr(explode(S),Col,L).
  flttn(Col,[(_,pInt(I)),..L]) => flttnStr(I.show(),Col,L).
  flttn(Col,[(_,pFlt(F)),..L]) => flttnStr(F.show(),Col,L).
  flttn(Col,[(_,pPad),(I,pPad),..L])=>flttn(Col,[(I,pPad),..L]).
  flttn(Col,[(_,pPad),..L])=>[` ,..flttn(Col+1,L)].
  flttn(_,[])=>"".
  flttn(Col,L) => raise error("Flattening "<>L.show(),'eINVAL').

  private flttnStr:[string,integer,list[(integer,pP)]]=>string.
  flttnStr([],Col,L)=>flttn(Col,L).
  flttnStr([C,..S],Col,L) => [C,..flttnStr(S,Col+1,L)].

  interleave:[list[t],t]=>list[t].
  interleave([],_)=>[].
  interleave([E],_)=>[E].
  interleave([E,..L],S)=>[E,S,..interleave(L,S)].

  private spaces:[integer,string]=>string.
  spaces(0,L)=>L.
  spaces(N,L)=>[` ,..spaces(N-1,L)].

}