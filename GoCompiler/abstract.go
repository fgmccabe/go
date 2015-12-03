/*
 * Manage aspects of the abstract syntax of Go!
 */
abstract{
  import opts.
  import go.showable.
  import ops.
  import misc.

  abstract <~ { loc:[]=>fileLoc. disp:[string,integer]=>dispTree }.

  abstract:[fileLoc]@=abstract.
  abstract(lc)..{
    loc()=>lc.
    disp(_,_)=>n([]).

    show() => this.disp("",2000).flatten([]).  
  }.

  VOID:[fileLoc]@=abstract.
  VOID(L) <= abstract(L).
  VOID(_)..{
    disp(_,_) => s("**void**").
  }.

  IDEN:[symbol,fileLoc]@=abstract.
  IDEN(_,L) <= abstract(L).
  IDEN(I,_)..{
    disp(_,_) => showName(I).
  }.

  SYM:[symbol,fileLoc]@=abstract.
  SYM(_,L) <= abstract(L).
  SYM(S,_)..{
    disp(_,_)::S='{}' => s("{}").
    disp(_,_)::S='[]' => s("[]").
    disp(_,_) => l(["'",showStr(explode(S)),"'"]).
  }.

  INT:[integer,fileLoc]@=abstract.
  INT(_,L) <= abstract(L).
  INT(N,_)..{
    disp(_,_) => s(N.show()).
  }.

  FLT:[float,fileLoc]@=abstract.
  FLT(_,L) <= abstract(L).
  FLT(N,_)..{
    disp(_,_) => s(N.show()).
  }.

  CHR:[char,fileLoc]@=abstract.
  CHR(_,L) <= abstract(L).
  CHR(C,_)..{
    disp(_,_) => l(["\`",showStr([C])]).
  }.

  STR:[string,fileLoc]@=abstract.
  STR(_,L) <= abstract(L).
  STR(S,_)..{
    disp(_,_) => l(["\"",showStr(S),"\""]).
  }.

  APPLY:[abstract,list[abstract],fileLoc]@=abstract.
  APPLY(_,_,L) <= abstract(L).
  APPLY(_,_,_)..{
    disp(indent,prior) =>
        dApply(this,indent,prior).

    dApply:[abstract,string,integer]=>dispTree.
    dApply(APPLY(IDEN('[]',_),[A],_),_,_) => n([A.disp("",0),s("[]")]).

    dApply(APPLY(IDEN(Op,_),[A,B],_),indent,Prior) :: infOp(Op,_,Pr,_),Pr>Prior =>
        n([s("("),dBinary(APPLY(IDEN(Op,_),[A,B],_),indent),s(")")]).
    dApply(APPLY(IDEN(Op,_),[A,B],_),indent,_) :: infOp(Op,_,_,_) =>
        dBinary(APPLY(IDEN(Op,_),[A,B],_),indent).

    dApply(APPLY(IDEN(Op,_),[A],_),indent,Prior) :: preOp(Op,Pr,rPr),Pr>Prior =>
        n([s("("),showOp(Op),s(" "),A.disp(indent,rPr),s(")")]).
    dApply(APPLY(IDEN(Op,_),[A],_),indent,_) :: preOp(Op,_,rPr) =>
        n([showOp(Op),s(" "),A.disp(indent,rPr)]).
    dApply(APPLY(IDEN(Op,_),[A],_),indent,Prior) :: pstOp(Op,lPr,Pr),Pr>Prior =>
        n([s("("),A.disp(indent,lPr),s(" "),showOp(Op),s(")")]).
    dApply(APPLY(IDEN(Op,_),[A],_),indent,_) :: pstOp(Op,lPr,_) =>
        n([A.disp(indent,lPr),s(" "),showOp(Op)]).
    dApply(APPLY(IDEN(',..',_),[A,B],_),indent,_) =>
        n([s("["),A.disp(indent,999),..dispLst(B,indent,[s("]")])]).
    dApply(APPLY(IDEN('{}',_),[A],_),indent,_) =>
        n([s("{"),A.disp(indent,2000),s("}")]).
    dApply(APPLY(O,A,_),indent,_) =>
        n([O.disp(indent,0),s("("),..showArgs(A,"",", ",999,indent,[s(")")])]).

    dBinary:[abstract,string]=>dispTree.
    dBinary(APPLY(IDEN('->',_),[A,B],_),indent) :: infOp('->',lPr,_,rPr) =>
        n([A.disp(indent,lPr),s("->\n"),s([` ,` ,..indent]),B.disp(indent,rPr)]).

    dBinary(APPLY(IDEN('|',_),[A,B],_),indent)::infOp('|',lPr,_,rPr) =>
        n([A.disp(indent,lPr),l(["\n",indent,"|"]),B.disp(indent,rPr)]).
    dBinary(APPLY(IDEN('. ',_),[A,B],_),indent)::infOp('. ',lPr,_,rPr) =>
        n([s([` ,` ,..indent]),A.disp([` ,` ,..indent],lPr),s(".\n"),B.disp(indent,rPr)]).
    dBinary(APPLY(IDEN(';',_),[A,B],_),indent)::infOp(';',lPr,_,rPr) =>
        n([A.disp([` ,` ,..indent],lPr),s(";\n"),s([` ,` ,..indent]),B.disp(indent,rPr)]).

    dBinary(APPLY(IDEN('.',_),[A,B],_),indent)::infOp('.',lPr,_,rPr) =>
        n([A.disp(indent,lPr),s("."),B.disp(indent,rPr)]).

    dBinary(APPLY(IDEN(Op,_),[A,B],_),indent) :: infOp(Op,lPr,_,rPr) =>
        n([A.disp(indent,lPr),s(" "),showOp(Op),s(" "),B.disp(indent,rPr)]).

    dispLst:[abstract,string,list[dispTree]]=>list[dispTree].
    dispLst(IDEN('[]',_),_,End) => End.
    dispLst(APPLY(IDEN(',..',_),[A,B],_),indent,End) => 
        [s(", "),A.disp(indent,999),..dispLst(B,indent,End)].
    dispLst(X,indent,End) => [s(",.."),X.disp(indent,999),..End].
  }.

  SQUARE:[abstract,list[abstract],fileLoc]@=abstract.
  SQUARE(_,_,L)<=abstract(L).
  SQUARE(O,A,_)..{
    disp(indent,_)=>n([O.disp(indent,0),s("["),..showArgs(A,"",",",999,indent,[s("]")])]).
  }.

  BRACE:[abstract,list[abstract],fileLoc]@=abstract.
  BRACE(_,_,L)<=abstract(L).
  BRACE(O,A,_)..{
    disp(indent,_)=>n([O.disp(indent,0),s("{"),..showArgs(A,"",".\n",2000,indent,[s("}")])]).
  }.

  TPL:[list[abstract],fileLoc]@=abstract.
  TPL(_,L)<=abstract(L).
  TPL(A,_)..{
    disp(_,_)::A==[] => s("()").
    disp(indent,Pr)::Pr<1500=>n([s("(."),..showArgs(A,"",", ",999,indent,[s(".)")])]).
    disp(indent,_)=>n(showArgs(A,"",", ",999,indent,[])).
  }.

  private showArgs:[list[abstract],string,string,integer,string,list[dispTree]]=>
      list[dispTree].
  showArgs([],_,_,_,_,End) => End.
  showArgs([A,..R],P,Fill,Pr,indent,End) => 
      [s(P),A.disp(indent,Pr),..showArgs(R,Fill,Fill,Pr,indent,End)].

  display:[abstract]=>string.
  display(T) => T.disp("",2000).flatten([]). 

  showName:[symbol]=>dispTree.
  showName(Name)::(preOp(Name,_,_) | infOp(Name,_,_,_) | pstOp(Name,_,_)) => 
      l(["(",explode(Name),")"]).
  showName(Name) => s(explode(Name)).

  showStr:[string]=>string.
  showStr([]) => [].
  showStr([C,..L]) => showChar(C,L).

  showChar:[char,string]=>string.
  showChar(`\a,L) => [`\\,`a,..showStr(L)].
  showChar(`\b,L) => [`\\,`b,..showStr(L)].
  showChar(`\d,L) => [`\\,`d,..showStr(L)].
  showChar(`\e,L) => [`\\,`e,..showStr(L)].
  showChar(`\f,L) => [`\\,`f,..showStr(L)].
  showChar(`\n,L) => [`\\,`n,..showStr(L)].
  showChar(`\r,L) => [`\\,`r,..showStr(L)].
  showChar(`\t,L) => [`\\,`t,..showStr(L)].
  showChar(`\v,L) => [`\\,`v,..showStr(L)].
  showChar(`\',L) => [`\\,`\',..showStr(L)].
  showChar(`\",L) => [`\\,`\",..showStr(L)].
  showChar(`\\,L) => [`\\,`\\,..showStr(L)].
  showChar(X,L) => [X,..showStr(L)].

  showOp:[symbol]=>dispTree.
  showOp(N) => s(explode(N)).

  listIfy:[abstract,symbol]=>list[abstract].
  listIfy(APPLY(IDEN(',..',_),[L,IDEN('[]',_)],_),',..')=>[L].
  listIfy(APPLY(IDEN(Op,_),[L,R],_),Op) =>
      listIfy(L,Op)<>listIfy(R,Op).
  listIfy(APPLY(IDEN(Op,_),[L],_),Op) =>
      listIfy(L,Op).
  listIfy(X,_) => [X].
}
