/*
 * Prolog-style term representation
 */

canon{
  import go.showable.
  import types.
  import ops.
  import opts.

  canonTree <~ { disp:[integer]=>dispTree. }.

  private canon:[]@=canonTree.
  canon..{
    disp(_)=>n([]).
    show()=>this.disp(0).flatten([]).

  }.

  canonTerm <~ canonTree.

  vdel:[]@=canonTerm.
  vdel <= canon.
  vdel..{
    show()=>"void".
    disp(_) => s("void").
  }.

  idnt:[symbol]@=canonTerm.
  idnt(Nm)..{
    show()=>explode(Nm).
    disp(_)=>s(show()).
  }.

  symb:[symbol]@=canonTerm.
  symb(S)..{
    disp(_)=>l(["\'",show(),"\'"]).
    show()=>explode(S).
  }.

  intg:[integer]@=canonTerm.
  intg(Nm)..{
    disp(_)=>s(show()).
    show()=>Nm.show().
  }.

  flte:[float]@=canonTerm.
  flte(Nm)..{
    disp(_)=>s(show()).
    show()=>Nm.show().
  }.

  cchr:[char]@=canonTerm.
  cchr(C)..{
    disp(_)=>s(show()).
    show()=>[``,C].
  }.

  strg:[string]@=canonTerm.
  strg(S)..{
    disp(_)=>l(["\"",show(),"\""]).
    show()=>S.
  }.

  clst:[canonTerm,canonTerm]@=canonTerm.
  clst(_,_)..{
    dispList:[canonTerm]=>list[dispTree].
    dispList(symb('go.stdlib#[]'))=>[].
    dispList(clst(H,(T::clst(_,_).=T))) => [H.disp(999),s(","),..dispList(T)].
    dispList(clst(H,T)) => [H.disp(999),s(",.."),T.disp(999)].

    disp(_)=>n([s("["),n(dispList(this)),s("]")]).
  }.

  cons:[symbol,list[canonTerm]]@=canonTerm.
  cons(Nm,Args)..{
    disp(_) => n([s(explode(Nm)),s("("),n(showList(Args,999,"",",")),s(")")]).
  }.

  private showList:[list[canonTree],integer,string,string]=>list[dispTree].
  showList([],_,_,_)=>[].
  showList([E,..L],Pr,Sep,S) => [s(Sep),E.disp(Pr),..showList(L,Pr,S,S)].

  canonCall <~ canonTree.
  canonCall <~ { loc:[]=>fileLoc. }.

  private showGoals:[list[canonCall]]=>list[dispTree].
  showGoals(L)::infOp(',',_,Pr,_)=>showList(L,Pr,"",",").

  neck:[fileLoc]@=canonCall.
  neck(Lc) .. {
    loc() => Lc.
    disp(_) => s("!").
  }.

  defn:[symbol,canonTerm,fileLoc]@=canonCall.
  defn(Nm,Val,Lc)..{
    loc()=>Lc.
    disp(_) => n([s(explode(Nm)),s(" = "),Val.disp(1000)]).
  }.

  call:[symbol,list[canonTerm],fileLoc]@=canonCall.
  call(Nm,Args,Lc)..{
    loc()=>Lc.
    disp(_) => n([s(explode(Nm)),s("("),n(showList(Args,999,"",",")),s(")")]).
  }.

  ocall:[canonTerm,canonTerm,canonTerm,fileLoc]@=canonCall.
  ocall(Ob,Sp,Ar,Lc)..{
    loc()=>Lc.
    disp(_) => n([Ob.disp(0),s("/"),Sp.disp(0),s("."),Ar.disp(0)]).
  }.

  suspc:[canonTerm,canonTerm,fileLoc]@=canonCall.
  suspc(Vr,Gl,Lc)..{
    loc()=>Lc.
    disp(_) => n([Vr.disp(0),s("~"),Gl.disp(0)]).
  }.

  errr:[canonCall,list[canonCall],fileLoc]@=canonCall.
  errr(Gl,Err,Lc)..{
    loc()=>Lc.
    disp(_) => n([Gl.disp(0),s("onerror("),n(showList(Err,999,"",",")),s(")")]).
  }.

  rais:[canonTerm,fileLoc]@=canonCall.
  rais(Val,Lc)..{
    loc()=>Lc.
    disp(_) => n([s(" raise "),Val.disp(1000)]).
  }.

  fale:[fileLoc]@=canonCall.
  fale(Lc)..{
    loc()=>Lc.
    disp(_)=>s("fail").
  }.

  canonCode <~ canonTree.

  mdle:[symbol,symbol,list[symbol],typeTree,list[(symbol,list[canonCode])]]@=canonCode.
  mdle(Name,Version,Imports,Types,Progs)..{
    disp(_) => n([s(explode(Name)),s("{\n"),
		  n({l(["import ",explode(I),".\n"])..I in Imports}),
		  n({showDefn(Nm,Prog)..(Nm,Prog) in Progs}),
		  s("\n}")]).
  }.

  clse:[list[canonTerm],list[canonTerm],list[canonCall],fileLoc]@=canonCode.
  clse(Q,A,B,Lc)..{
    disp(_) => n([s("("),n(showList(A,999,"",", ")),s(") :- "),
		  n(showList(B,999,"","& "))]).
  }.

  private showDefn:[symbol,list[canonCode]]=>dispTree.
  showDefn(Nm,Clses)::E=s(explode(Nm)) => n({ n([E,Cl.disp(0)])..Cl in Clses}).
}.
