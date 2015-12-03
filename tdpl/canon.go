/*
  A simplified form of rule:

  rl(S,[nt(S),..,ch(C)])
*/
canon{
  import go.showable.
  import ctypes.

  rl <~ showable.

  cf <~ showable.

  rl:[symbol,list[cf],string]@=rl.
  rl(N,E,S)..{
    disp() => n([s(explode(N)), s("->"),n(dispList(E,"")),s(S)]).

    dispList:[list[cf],string]=>list[dispTree].
    dispList([],_)=>[].
    dispList([C,..lst],Sep)=>[s(Sep),C.disp(),..dispList(lst," ")].
  }.

  symbolType ::= orig | intro.

  nt:[symbol,symbolType]@=cf.
  nt(Nm,Mode)..{
    disp()::Mode==orig=>s(explode(Nm)).
    disp()::Mode==intro=>s([`%,..explode(Nm)]).
  }.

  ch:[char]@=cf.
  ch(C)..{
    disp() => s([``,C]).
  }.

  any:[]@=cf.
  any..{
    disp() => s("@").
  }.

  strt:[]@=cf.
  strt..{
    disp() => s("^").
  }.

  en:[]@=cf.
  en..{
    disp() => s("$").
  }.

  canon:[list[rule]]=>list[rl].
  canon(Rules) =>
      flatten({canonRule(R) .. R in Rules}).

  canonRule:[rule] => list[rl].
  canonRule(rule(NT,Body,S)) :: (nR,nB) = canonCat(Body) => [rl(NT,nB,S),..nR].

  canonCfg:[cfg]=>(list[rl],list[cf]).
  canonCfg(empty)=>([],[]).
  canonCfg(Nt(Vr,N))=>([],[nt(N,orig)]).
  canonCfg(str(S)) => ([],{ ch(c) .. c in S}).
  canonCfg(chars(S)) => 
      valof{
	N1 = gensym("chars");
	valis ({rl(N1,[ch(c)],"$$=$1") .. c in S},[nt(N1,intro)])
      }.
  canonCfg(choice(L)) =>
      valof{
	N1 = gensym("choices");
	valis (canon({rule(N1,[chce],"$$=$1")..chce in L}),[nt(N1,intro)])
      }.
  canonCfg(star(E))=>
      valof{
	N1 = gensym("star");
	(nR,nE) = canonCfg(E);
	valis ([rl(N1,nE<>[nt(N1,intro)],"$$=[$1,..$2]"),rl(N1,[],"$$=[]"),..nR],[nt(N1,intro)])
      }.
  canonCfg(plus(E))=>
      valof{
	N1 = gensym("plus");
	(nR,nE) = canonCfg(E);
	valis ([rl(N1,nE<>[nt(N1,intro)],"$$=[$1,..$2]"),rl(N1,[],"$$=[]"),..nR],nE<>[nt(N1,intro)])
      }.
  canonCfg(optional(E))=>
      valof{
	N1 = gensym("optional");
	(nR,nE) = canonCfg(E);
	valis ([rl(N1,nE,"$$=[$1]"),rl(N1,[],"$$=[]"),..nR],[nt(N1,intro)])
      }.
  canonCfg(cat(L)) => canonCat(L).
  canonCfg(period) => ([],[any]).
  canonCfg(end) => ([],[en]).
  canonCfg(start) => ([], [strt]).

  canonCat:[list[cfg]] => (list[rl],list[cf]).
  canonCat([]) => ([],[]).
  canonCat([C,..L]) =>
      valof{
	(nR,nC) = canonCfg(C);
	(nlR,nL) = canonCat(L);
	valis (nR<>nlR,nC<>nL)
      }.
}