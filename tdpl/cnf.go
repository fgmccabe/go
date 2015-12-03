/*
   Convert a CFG grammar into Chomsky Normal Form
   A CNF grammar has no rules of the form 
   A -> empty
   and all other rules are of the form
   A -> B C
   or
   A -> a where a is a terminal
*/
cnf{
  import types.
  import canon.
  import go.io.
  import go.showable.
  import go.setlib.

  private elimE:[list[rl],list[symbol]]=>(list[rl],list[symbol]).
  elimE(Rules,new) :: append(F,[rl(NT,[]),..B],Rules) =>
      valof{
	N1 = gensym(explode(NT));
	valis elimE(replaceN(NT,N1,F<>B),[NT,N1,..new])
      }.
  elimE(Rules,new) => (Rules,new).

  private addNewRules:[list[symbol],list[rl]]=>list[rl].
  addNewRules([],Rules)=>Rules.
  addNewRules([NT,N1,..new],Rules) :: rl(NT,_) in Rules =>
      addNewRules(new,{ rl(N1,R) .. rl(NT,R) in Rules} <> 
		  { rl(X,R) .. (rl(X,R)::X!=NT) in Rules}).
  addNewRules([_,N1,..new],Rules) => 
      addNewRules(new,{ rl(N,R) .. (rl(N,R)::\+(nt(N1,_) in R)) in Rules}).

  private replaceN:[symbol,symbol,list[rl]]=>list[rl].
  replaceN(NT,N1,[rl(N,rhs),..Rules])::append(F,[nt(NT,I),..B],rhs) =>
      replaceN(NT,N1,[rl(N,F<>B),rl(N,F<>[nt(N1,I),..B]),..Rules]).
  replaceN(NT,N1,[Rl,..Rules]) =>
      [Rl,..replaceN(NT,N1,Rules)].
  replaceN(_,_,[])=>[].

  private removeUnits:[list[rl]]=>list[rl].
  removeUnits(Rules)::append(F,[rl(N,[nt(M,_)]),..B],Rules) =>
      ( N==M ?
	removeUnits(F<>B)
      | removeUnits(F<>lift(N,M,F<>B)<>B)
      ).
  removeUnits(Rules)=>Rules.

  private lift:[symbol,symbol,list[rl]]=>list[rl].
  lift(N,M,Rules) => {rl(N,R)..rl(M,R)in Rules}.

  -- Eliminating unproductive rules

  private allNTs:[list[rl],list[symbol]]=>list[symbol].
  allNTs([],NTs)=>NTs.
  allNTs([rl(NT,_),..Rls],NTs)::NT in NTs => allNTs(Rls,NTs).
  allNTs([rl(NT,_),..Rls],NTs) => allNTs(Rls,[NT,..NTs]).

  -- Two phase, a successful term of phaseI exits, otherwise phaseII exits 
  -- back to phaseI

  private markNTs:[list[symbol],list[symbol],list[symbol],list[rl]]=>
      (list[symbol],list[symbol]).
  markNTs([],Term,NonProd,_) => (Term,NonProd).
  markNTs([NT,..others],Term,NonProd,Rls)::NT in Term =>
      markNTs(others,Term,NonProd,Rls).
  markNTs([NT,..others],Term,NonProd,Rls)::NT in NonProd =>
      markNTs(others,Term,NonProd,Rls).
  markNTs([NT,..others],Term,NonProd,Rls) :: 
	  rl(NT,Body) in Rls, (B in Body *> terminates(B,Term,Rls)) =>
      markNTsII(others,[NT,..Term],NonProd,Rls).
  markNTs([NT,..others],Term,NonProd,Rls) =>
      markNTs(others,Term,[NT,..NonProd],Rls).

  private markNTsII:[list[symbol],list[symbol],list[symbol],
		     list[rl]]=> (list[symbol],list[symbol]).
  markNTsII([],Term,NonProd,Rules) => markNTs(NonProd,Term,[],Rules).
  markNTsII([NT,..others],Term,NonProd,Rls)::NT in Term =>
      markNTsII(others,Term,NonProd,Rls).
  markNTsII([NT,..others],Term,NonProd,Rls)::NT in NonProd =>
      markNTsII(others,Term,NonProd,Rls).
  markNTsII([NT,..others],Term,NonProd,Rls) :: 
	  rl(NT,Body) in Rls, (B in Body *> terminates(B,Term,Rls)) =>
      markNTsII(others,[NT,..Term],NonProd,Rls).
  markNTsII([NT,..others],Term,NonProd,Rls) =>
      markNTsII(others,Term,[NT,..NonProd],Rls).

  private removeUnProductive:[list[symbol],list[rl]]=>list[rl].
  removeUnProductive(_,[])=>[].
  removeUnProductive(Non,[rl(NT,_),..rules])::NT in Non =>
      removeUnProductive(Non,rules).
  removeUnProductive(Non,[rl(_,Bdy),..rules])::nt(B,_) in Bdy, B in Non =>
      removeUnProductive(Non,rules).
  removeUnProductive(Non,[r,..rules])=>[r,..removeUnProductive(Non,rules)].

  -- test to see if a grammar term is a terminal term
  private terminates:[cf,list[symbol],list[rl]]{}.
  terminates(nt(NT,_),terms,_) :: NT in terms :-- true.
  terminates(hed(NT),terms,_) :: NT in terms :-- true.
  terminates(neg(NT),terms,_) :: NT in terms :-- true.
  terminates(ch(_),_,_) :-- true.

  private reachable:[list[symbol],list[rl],list[rl]]=>list[symbol].
  reachable(R,[],_)=>R.
  reachable(R,[rl(NT,Bdy),..ules],Rules)::NT in R =>
      reachableII(R\/pickNts(Bdy),ules,diff(Rules,[rl(NT,Bdy)])).
  reachable(R,[_,..ules],Rules) => reachable(R,ules,Rules).

  private pickNts:[list[cf]]=>list[symbol].
  pickNts([])=>[].
  pickNts([nt(N1,_),..body]) => [N1,..pickNts(body)].
  pickNts([neg(N1),..body]) => [N1,..pickNts(body)].
  pickNts([hed(N1),..body]) => [N1,..pickNts(body)].
  pickNts([_,..body])=>pickNts(body).

  -- Second phase, will loop back to phaseI at the end
  private reachableII:[list[symbol],list[rl],list[rl]]=>list[symbol].
  reachableII(R,[],Rules)=>reachable(R,Rules,Rules).
  reachableII(R,[rl(NT,Bdy),..ules],Rules)::NT in R =>
      reachableII(R\/pickNts(Bdy),ules,diff(Rules,[rl(NT,Bdy)])).
  reachableII(R,[_,..ules],Rules) => reachableII(R,ules,Rules).

  -- convert to CNF
  private elimTerminals:[list[rl],list[(char,symbol)]]=>list[rl].
  elimTerminals([],Map)=>{ rl(NT,[ch(C)]) .. (C,NT) in Map}.
  elimTerminals([rl(NT,[ch(C)]),..Rules],Map) =>
      [rl(NT,[ch(C)]),..elimTerminals(Rules,[(C,NT),..Map])].
  elimTerminals([rl(NT,Bdy),..Rules],Map) =>
      valof{
	(nBdy,nMap) = findTerminals(Bdy,Map);
	valis [rl(NT,nBdy),..elimTerminals(Rules,nMap)]
      }.

  private findTerminals:[list[cf],list[(char,symbol)]]=>
      (list[cf],list[(char,symbol)]).
  findTerminals([],Map) => ([],Map).
  findTerminals([nt(X,I),..Bdy],Map) =>
      valof{
	(nB,nM) = findTerminals(Bdy,Map);
	valis ([nt(X,I),..nB],nM)
      }.
  findTerminals([neg(X),..Bdy],Map) =>
      valof{
	(nB,nM) = findTerminals(Bdy,Map);
	valis ([neg(X),..nB],nM)
      }.
  findTerminals([hed(X),..Bdy],Map) =>
      valof{
	(nB,nM) = findTerminals(Bdy,Map);
	valis ([hed(X),..nB],nM)
      }.
  findTerminals([ch(C),..Bdy],Map)::(C,NT) in Map =>
      valof{
	(nB,nM) = findTerminals(Bdy,Map);
	valis ([nt(NT,intro),..nB],nM)
      }.
  findTerminals([ch(C),..Bdy],Map) =>
      valof{
	NT = gensym([C]);
	(nB,nM) = findTerminals(Bdy,[(C,NT),..Map]);
	valis ([nt(NT,intro),..nB],nM)
      }.

  private splitBodies:[list[rl]]=>list[rl].
  splitBodies([])=>[].
  splitBodies([rl(NT,L),..Rules])::listlen(L)<3 =>
      [rl(NT,L),..splitBodies(Rules)].
  splitBodies([rl(NT,[N1,N2,..L]),..Rules]) =>
      valof{
	NT1 = gensym("two");
	valis splitBodies([rl(NT,[nt(NT1,intro),..L]),rl(NT1,[N1,N2]),..Rules])
      }.

  cnf:[list[rule]]=>list[rl].
  cnf(Rules) => valof{
		  C = canon(Rules);
		  stdout.outLine("Canonical rules:");
		  ( R in C *>
		    stdout.outLine(show(R)));
		  stdout.outLine("--<<");
		  ( iRls, new) = elimE(C,[]);
		  nRls = addNewRules(new,iRls);
		  stdout.outLine("New symbols: "<>new.show());
		  NonU = removeUnits(nRls);
		  NTs = allNTs(NonU,[]);
		  ( R in NonU *>
		    stdout.outLine(show(R)));
		  stdout.outLine("--");
		  stdout.outLine("Non-terminals: "<>NTs.show());
		  ( Terms,NonTerm) = markNTs(NTs,[],[],NonU);
		  ( NN in Terms *>
		    stdout.outLine("productive: "<>explode(NN)));
		  ( NN in NonTerm *>
		    stdout.outLine("Non-productive: "<>explode(NN)));
		  Prod = removeUnProductive(NonTerm,NonU);
		  Rch = reachable([startSymbol(Rules)],Prod,Prod);
		  stdout.outLine("Reachable NTs = "<>Rch.show());
		  valis splitBodies(elimTerminals(removeUnProductive(diff(NTs,Rch),Prod),[]));
		}.
}