gpg.lalr{
  import gpg.gpgTypes.
  import gpg.gensets.
  import gpg.misc.
  import gpg.closure.
  import go.hash.
  import go.setlib.
  import go.io.
  import go.cell.

  private tableEntry <~ { 
	it:[]=>item.
	mergeSP:[list[symbol]]*.
        SP:[]=>list[symbol].
	prop:[]=>list[(integer,integer)].
	mergeProp:[list[(integer,integer)]]*.
      }.
  private te:[item,list[symbol]]@>tableEntry.
  te(I,lL)..{
    sP:list[symbol]:=lL.
    Prop:list[(integer,integer)] := [].

    it()=>I.

    SP()=>sP.
    mergeSP(S) -> sP := sP\/S.

    prop()=>Prop.
    mergeProp(S) -> 
	Prop := Prop\/S.

    show()=>I.show()<>", sp = "<>sP.show().
  }.

  private showTable:[list[(integer,list[tableEntry])]]*.
  showTable(T) ->
      (Sno,I) in T *>
      stdout.outLine("State: "<>Sno.show());
      stdout.outLine(I.show()).

  private initial:[list[symbol],list[symbol],list[symbol],integer]=>
      list[(integer,list[symbol])].
  initial([],_,_,_) => [].
  initial([A,..rhs],N,NT,R)::A in N => [(R,[A,..rhs])]\/initial(rhs,N,NT,R).
  initial(A,_,_,R)=>[(R,A)].

  private reachable:[list[symbol],list[rule],list[symbol]]=>
      list[(symbol,list[(integer,list[symbol])],list[(integer,list[symbol])])].
  reachable(nonT,rules,N)=>
      (:ev[list[(symbol,list[(integer,list[symbol])],list[(integer,list[symbol])])]]..{

	 ev()=> {(C,NT,T)..((C,X)::(NT,T)=pickup(X.get(),[],[])) in R}.

	 done:logical := false.
	 
	 R:list[(symbol,cell[list[(integer,list[symbol])]])].
	 R = { (C,cell(collapse(setof({initial(RHS,N,nonT,Rn)..rule(Rn,C,RHS,_,_,_) in rules}),[]))) .. C in nonT }.
	 
	 ${
	   do();				-- evaluate the fixed point 
	 }.

	 do:[]*.
	 do()::done->{}.
	 do()->
	     done:=true;
	     ((_,X) in R *>
	      ( (_,[A,.._]) in X.get() *>
		( (A,Y) in R , extends(X.get(),Y.get()) ?
		    done := false;
		    X.set(X.get()\/Y.get())
		)
	      )
	     );
	     do().
	
	 pickup:[list[(integer,list[symbol])],
		 list[(integer,list[symbol])],
		 list[(integer,list[symbol])]]=>
	     (list[(integer,list[symbol])],list[(integer,list[symbol])]).
	 pickup([],NT,T) => (NT,T).
	 pickup([(Rx,[L,..E]),..XX],NT,T)::L in nonT =>
	     pickup(XX,[(Rx,[L,..E]),..NT],T).
	 pickup([(Rx,[L,..E]),..XX],NT,T) =>
	     pickup(XX,NT,[(Rx,[L,..E]),..T]).
       }).ev().
  
  private lookAheadTable:[list[(integer,list[item])]]=>
			       list[(integer,list[tableEntry])].
  lookAheadTable(K) => 
      { (Sno,
	 {te(item(Cnt,A,B,C,[]),isDollar(Sno,A))..item(Cnt,A,B,C,_) in I}) ..
	(Sno,I) in K}.

  private isDollar:[integer,symbol]=>list[symbol].
  isDollar(0,'?')=>['$'].
  isDollar(_,_)=>[].

  lookAheadK:[list[(integer,list[item])],
	      list[(integer,symbol,integer)],
	      list[rule],list[(symbol,list[symbol])],list[symbol]]=>
      list[(integer,list[item])].
  lookAheadK(K,G,rules,F,N) => 
      valof{
	Ktable = lookAheadTable(K);

--	stderr.outLine("Initial lookahead = "<>Ktable.show());

	-- Compute spontaneous lookaheads and targets for propogation

	( (Sno,I) in Ktable *>
	  ( T in I *>
	    ( item(Cnt,B,Gamma,Delta,_) .= T.it();
	      J = closure([item(Cnt,B,Gamma,Delta,['#'])],rules,F,N);
--	      stderr.outLine("Closure of "<>T.it().show()<>" in state "<>Sno.show()<>
--			     " is "<>J.show());
	      ( item(_,A,alpha,[X,..beta],L) in J *>
		( (Sno,X,Nx) in G, (Nx,Items) in Ktable,
		  (Te2::item(IC,A,(pre::append(alpha,[X],pre)),beta,_).=Te2.it()) in Items ?
		    ( LJ in L *>
		      ( LJ=='#' ?
			  T.mergeProp([(Nx,IC)])
		      | Te2.mergeSP([LJ])
		      )
		    )
		| {})
	      )
	    )
	  )
	);

--	stdout.outLine("Ktable now ");
--	showTable(Ktable);
	
	-- Propagate the lookaheads
	(:do..{
	   done:logical := false.

	   do()::done->{}.
	   do()->
	       done:=true;
	       ((_,I) in Ktable *>
		( Te in I *>
		  ( (Nx,Rule) in Te.prop() *>
		    ((Nx,NI) in Ktable, (T::T.it() = item(Rule,_,_,_,_)) in NI ?
		       ( extends(T.SP(),Te.SP()) ?
			   T.mergeSP(Te.SP());
			   done := false
		       | {})
		   | {})
		  )
		)
	       );
	       do().

	 }).do();

	-- canonical LALR(1) set of items
	valis {(Sno,{item(Cnt,A,B,C,T.SP())..
		  (T::T.it()=item(Cnt,A,B,C,_)) in I}) .. 
	  (Sno,I) in Ktable }
      }.
  showStates:[list[(integer,list[item])],list[(integer,symbol,integer)],list[symbol]]*.
  showStates(T,G,nonT) ->
      (S,I) in T *>
      stdout.outLine("St: "<>S.show()<>":"<>I.show());
      ( ((S,X,N)::X in nonT) in G *>
	stdout.outLine("goto("<>X.show()<>")="<>N.show())
      ).

  actions:[list[(integer,list[item])],
	   list[(integer,symbol,integer)],
	   list[symbol],list[symbol],
	   list[(symbol,tokenPriority)],
	   list[rule],
	   list[(symbol,list[symbol])],list[symbol]]=>list[(integer,list[item],list[actionT])].
  actions(States,G,nonT,T,P,rules,F,N) =>
      valof{
	Rch = reachable(nonT,rules,N);

/*	stdout.outLine("Reachable sets:");
	(R in Rch *>
	 stdout.outLine(R.show())
	);
*/

	rawActions = {(Sno,Items,
		       collapse({ stActs(Sno,I,G,nonT,T,rules,F,N,Rch).. I in Items},[]))
	  ..(Sno,Items) in States};

--	stderr.outLine("Raw actions = ");
--	showActions(rawActions,stderr);

	valis resolveConflicts(rawActions,rules,nonT<>T,P)
      }.

  private stActs:[integer,item,list[(integer,symbol,integer)],
		  list[symbol],list[symbol],list[rule],list[F],list[symbol],
		  list[(symbol,list[(integer,list[symbol])],
			list[(integer,list[symbol])])]] =>list[actionT].
  stActs(Sno,item(R,_,_,['error',..delta],_),G,_,_,_,_,_,_)::(Sno,'error',NX) in G =>
      [recoverError(delta,NX,R)].
  stActs(Sno,item(R,_,_,[t,.._],_),G,_,T,_,_,_,_)::
	  t!='error', t in T, (Sno,t,NX) in G =>
      [shiftOn(t,NX,R)].
  stActs(Sno,item(_,_,_,[t,..delta],LA),G,nonT,_,rules,F,N,Rch)::
	  t in nonT, (t,Ry,Rx) in Rch =>
      { (a=='error'?
	   recoverError(aRest,NX,Rn)
       | shiftOn(a,NX,Rn)) .. ((Rn,[a,..aRest])::(Sno,a,NX) in G) in Rx }\/
		       { reduceBy(a,RR) .. (_,[A,..eta]) in Ry,
	rule(RR,A,[],[],_,_) in rules,
	  b in LA,a in firstSeq(eta<>delta<>[b],F,N)}\/
	(rule(RR,t,[],[],_,_) in rules ?
	   { reduceBy(a,RR) .. b in LA, a in firstSeq(delta<>[b],F,N)}
       | []).
  stActs(_,item(_,'?',_,[],LA),_,_,_,_,_,_,_) =>
      { accept(b) .. b in LA }.
  stActs(_,item(R,_,_,[],LA),_,_,_,_,_,_,_) =>
      { reduceBy(b,R) .. b in LA }.

  private
  resolveConflicts:[list[(integer,list[item],list[actionT])],
		    list[rule],
		    list[symbol],
		    list[(symbol,tokenPriority)]]=>list[(integer,list[item],list[actionT])].
  resolveConflicts(A,rules,T,P)=>
      compress({ (Sno,Items,resolve(Sno,Acts,rules,T,P)) .. (Sno,Items,Acts) in A }).

  private resolve:[integer,list[actionT],
		   list[rule],
		   list[symbol],
		   list[(symbol,tokenPriority)]]=>list[actionT].
  resolve(_,Acts,_,[],_) => Acts.
  resolve(Sno,Acts,rules,[a,..T],P)::
	  shiftOn(a,R,Rx) in Acts,reduceBy(a,Ry) in Acts  =>
      ( (a,Ax) in P, rule(Ry,_,_,_,_,Ay) in rules ?
	  ( (smaller(Ax,Ay) | leftOf(Ax,Ay)) ?
	      valof{
		stderr.outLine("Conflict in state "<>Sno.show()<>" between rule "<>Ry.show()<>
			       " and "<>a.show()<>" resolved as reduce");
		valis resolve(Sno,filterOut(Acts,shiftOn(a,R,Rx)),rules,[a,..T],P)
	      }
	  | (smaller(Ay,Ax) | rightOf(Ax,Ay)) ?
	      valof{
		stderr.outLine("Conflict in state "<>Sno.show()<>" between rule "<>Ry.show()<>
			       " and "<>a.show()<>" resolved as shift");
		valis resolve(Sno,filterOut(Acts,reduceBy(a,Ry)),rules,[a,..T],P)
	      }
	  | valof{
	      stderr.outLine("Warning: shift/reduce conflict on "<>explode(a)<>
			     " in state "<>Sno.show());
	      valis resolve(Sno,[disabled(reduceBy(a,Ry)),..filterOut(Acts,reduceBy(a,Ry))],rules,T,P)
	  }
	  )
      | raise error("Problem in conflict resolution "<>explode(a)<>
		    " in state "<>Sno.show(),'eFAIL')
      ).
  resolve(Sno,Acts,rules,[a,..T],P)::
	  reduceBy(a,Rx) in Acts,reduceBy(a,(Ry::Rx!=Ry)) in Acts  =>
      valof{
	stderr.outLine("Warning: reduce/reduce conflict on "<>explode(a)<>
		       " in state "<>Sno.show());
	( Ry<Rx ?
	    valis resolve(Sno,[disabled(reduceBy(a,Rx)),..filterOut(Acts,reduceBy(a,Rx))],rules,[a,..T],P)
	| valis resolve(Sno,[disabled(reduceBy(a,Ry)),..filterOut(Acts,reduceBy(a,Ry))],rules,[a,..T],P)
	)
      }.
  resolve(Sno,Acts,rules,[a,..T],P)::
	  shiftOn(a,R,R1) in Acts,shiftOn(a,R,(R2::R1!=R2)) in Acts  =>
      resolve(Sno,filterOut(Acts,shiftOn(a,R,R2)),rules,[a,..T],P).
  resolve(Sno,Acts,rules,[_,..T],P) =>
      resolve(Sno,Acts,rules,T,P).

  private smaller:[tokenPriority+,tokenPriority+]{}.
  smaller(A,B) :- priorityOf(A)<priorityOf(B).

  private priorityOf:[tokenPriority]=>integer.
  priorityOf(nA(A))=>A.
  priorityOf(lA(A))=>A.
  priorityOf(rA(A))=>A.
  priorityOf(tA)=>0.

  private leftOf:[tokenPriority+,tokenPriority+]{}.
  leftOf(lA(X),nA(X)).
  leftOf(lA(X),lA(X)).
  leftOf(lA(X),rA(X)).

  private rightOf:[tokenPriority+,tokenPriority+]{}.
  rightOf(rA(X),nA(X)).
  rightOf(rA(X),lA(X)).
  rightOf(rA(X),rA(X)).

  private compress:[list[(integer,list[items],list[actionT])]]=>list[(integer,list[items],list[actionT])].
  compress(Ac)=>{(Sno,Items,
		  ( \+ recoverError(_,_,_) in Acts ?
		      case collectReds({A..(A::reduceBy(_,_).=A) in Acts},[]) in(
		       [] => Acts
		     | [(_,R)] => [reduceBy('__default__',R),..
				   {A..(A::\+reduceBy(_,_).=A) in Acts}]
		     | _ => Acts
		      )
		  | Acts)
		  ) .. (Sno,Items,Acts) in Ac}.

  private collectReds:[list[actionT],list[(list[symbol],integer)]]=>list[(list[symbol],integer)].
  collectReds([],Reds)=>Reds.
  collectReds([reduceBy(a,R),..Acts],Reds)::(S,R) in Reds =>
      collectReds(Acts,[([a]\/S,R),..filterOut(Reds,(_,R))]).
  collectReds([reduceBy(a,R),..Acts],Reds) =>
      collectReds(Acts,[([a],R),..Reds]).

  showActions:[list[(integer,list[item],list[actionT])],outChannel]*.
  showActions(Acts,O) ->
      ( (Sno,Items,Ac) in reverse(Acts) *>
	O.outLine("State: "<>Sno.show());
	( I in Items *>
	  O.outLine(I.show())
	);
	( A in Ac *>
	  showAction(A,O)
	)
      ).

  private showAction:[actionT,outChannel]*.
  showAction(A,O) ->
      case A in (
       accept(a) -> O.outLine("accept "<>a.show())
     | reduceBy(a,R) -> O.outLine("reduce "<>a.show()<>" using rule "<>R.show())
     | shiftOn(a,R,_) -> O.outLine("shift "<>a.show()<>" goto "<>R.show())
     | recoverError(a,Nx,_) -> O.outLine("recover from error "<>a.show()<>
					 " goto "<>Nx.show())
     | disabled(a) -> O.outStr("disabled ");showAction(a,O)
      ).
}

