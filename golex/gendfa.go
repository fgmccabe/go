/*
 * Construct the first, last and followpos functions of an
 * augmented regular expression
 * Then construct the dfa from the resulting data
 */
gendfa{
  import go.io.
  import go.setlib.
  import go.do.
  import golextypes.
  import genregexp.

  private rtree ::= tree(re,logical,list[integer],list[integer]).

  private re::= ordR(char,integer)
	      | emptyR
	      | orR(rtree,rtree)
	      | catR(rtree,rtree)
	      | starR(rtree)
	      | charsR(list[char],integer)
	      | finalR(rtree,finalType,integer).

  private sel:[list[t],list[t]]=>list[t].
  sel([],R) => R.
  sel(L,_) => L.

  private slt:[string,integer,list[(string,integer)]]=>
      (rtree,integer,list[(string,integer)]).
  slt([],P,pos) => (tree(emptyR,true,[],[]),P,pos).
  slt([C],P,pos) => (tree(ordR(C,P),false,[P],[P]),P+1,[([C],P),..pos]).
  slt([C,..T],P,pos) => 
      valof{
	((R::tree(_,_,_,last)=R),P1,pos1) = slt(T,P+1,[([C],P),..pos]);
	valis (tree(catR(tree(ordR(C,P),false,[P],[P]),
			 R),false,[P],last),P1,pos1)
      }.
  
  private pickL:[list[t],list[t]]=>list[t].
  pickL([],R) => R.
  pickL(L,_) => L.

  -- Build a labeled tree from a regular expression tree
  private mtree:[regexp,integer,string,list[(string,integer)]]=>
      (rtree,integer,list[(string,integer)]).
  mtree(ordRE(c),P,_,pos) => (tree(ordR(c,P),false,[P],[P]),
			      P+1,[([c],P),..pos]).
  mtree(emptyRE,P,_,pos) => (tree(emptyR,true,[],[]),P,pos).
  mtree(charsRE(l),P,_,pos) => (tree(charsR(l,P),false,[P],[P]),P+1,
			       [(l,P),..pos]).
  mtree(negCharsRE(l),P,A,pos) => 
      (tree(charsR(A\l,P),false,[P],[P]),P+1,[(A\l,P),..pos]).
  mtree(periodRE,P,A,pos) => (tree(charsR(A\ "\+ffff;",P),false,[P],[P]),P+1,
			     [(A\ "\+ffff;",P),..pos]).
  mtree(seqRE(l),P,_,pos) => slt(l,P,pos).
  mtree(catRE(l,r),P,A,pos) =>
      valof{
	((L::tree(_,Ln,Lpre,Llast).=L),P1,pos1) .= mtree(l,P,A,pos);
	((R::tree(_,Rn,Rpre,Rlast).=R),P2,pos2) .= mtree(r,P1,A,pos1);
	FF = ( Ln ? Lpre\/Rpre | Lpre);
	LL = ( Rn ? Llast\/Rlast | Rlast);
	valis (tree(catR(L,R),( Ln ? Rn | false),FF,LL),P2,pos2)
      }.
  mtree(orRE(l,r),P,A,pos) =>
      valof{
	((L::tree(_,Ln,Lpre,Llast).=L),P1,pos1) .= mtree(l,P,A,pos);
	((R::tree(_,Rn,Rpre,Rlast).=R),P2,pos2) .= mtree(r,P1,A,pos1);
	valis (tree(orR(L,R),( Ln ? true | Rn),Lpre\/Rpre,Llast\/Rlast),P2,pos2)
      }.
  mtree(starRE(l),P,A,pos) =>
      valof{
	((L::tree(_,_,Lpre,Llast).=L),P1,pos1) .= mtree(l,P,A,pos);
	valis (tree(starR(L),true,Lpre,Llast),P1,pos1)
      }.
  mtree(plusRE(l),P,A,pos) => mtree(catRE(l,starRE(l)),P,A,pos).
  mtree(optRE(l),P,A,pos) => mtree(orRE(l,emptyRE),P,A,pos).
  mtree(finalRE(e,v),P,A,pos) => 
      valof{
	((L::tree(_,Ln,Lpre,Llast)=L),P1,pos1) .= mtree(catRE(e,
							     ordRE(`\+fffe;)),
						       P,A,pos);
	EP = (("\+fffe;",XX) in pos1 ? XX | raise error("failed",'eFAIL'));
	valis (tree(finalR(L,v,EP),Ln,Lpre,Llast),P1,pos1)
      }.

  private follow:[rtree,list[(integer,list[integer])]]=>list[(integer,list[integer])].
  follow(tree(catR((l::tree(_,_,_,Llast).=l),
		   (r::tree(_,_,Rpre,_).=r)),_,_,_),V) =>
      mergeF(Llast,Rpre,follow(r,follow(l,V))).
  follow(tree(starR((l::tree(_,_,Lpre,Llast).=l)),_,_,_),V) =>
      mergeF(Llast,Lpre,follow(l,V)).
  follow(tree(orR(l,r),_,_,_),V) => follow(r,follow(l,V)).
  follow(tree(finalR(e,_,_),_,_,_),V) => follow(e,V).
  follow(_,V) => V.


  private mergeF:[list[t],list[t],list[(t,list[t])]] => list[(t,list[t])].
  mergeF([],_,V) => V.
  mergeF([i,..I],Rpre,V)::(i,VV) in V =>
      mergeF(I,Rpre,replaceF(i,VV\/Rpre,V)).
  mergeF([i,..I],Rpre,V) =>
      mergeF(I,Rpre,[(i,Rpre),..V]).

  private replaceF:[t,list[t],list[(t,list[t])]]=> list[(t,list[t])].
  replaceF(i,VV,[]) => [(i,VV)].
  replaceF(i,VV,[(i,_),..V]) => [(i,VV),..V].
  replaceF(i,VV,[I,..V]) => [I,..replaceF(i,VV,V)].

  -- Finds the first final state
  private pickFinal:[rtree,list[(string,integer)],list[integer],integer]=>
      list[(integer,list[finalType])].
  pickFinal(Tree,pos,U,Sno) =>
      ((p::("\+fffe;",p) in pos, Final=pick(Tree,p,Sno), Final!=[]) in U ?
	 Final
     | []).
	 
  private pick:[rtree,integer,integer]=>list[(integer,list[finalType])].
  pick(tree(catR(l,r),_,_,_),p,Sno) =>
      sel(pick(l,p,Sno),pick(r,p,Sno)).
  pick(tree(orR(l,r),_,_,_),p,Sno) =>
      sel(pick(l,p,Sno),pick(r,p,Sno)).
  pick(tree(starR(l),_,_,_),p,Sno) =>
      pick(l,p,Sno).
  pick(tree(finalR(_,f,p),_,_,_),p,Sno) => [(Sno,[f])].
  pick(_,_,_) => [].

  private switchify:[list[(integer,list[finalType])],
		     list[(integer,list[integer],string)]] => 
      list[(integer,list[finalType])].
  switchify([],_) => [].
  switchify([(p,[state(S)]),..F],St) :: (P,_,S) in St =>
      [(p,[switch(S,P)]),..switchify(F,St)].
  switchify([Fin,..F],St) => [Fin,..switchify(F,St)].

  private mergeREStates:[list[(string,regexp)]]=>list[(string,regexp)].
  mergeREStates([])=>[].
  mergeREStates([(State,RE),..REs])::(R,S) = getREs(State,REs,RE,[]) =>
      [(State,R),..mergeREStates(S)].

  private getREs:[string,list[(string,regexp)],regexp,list[(string,regexp)]]=>
      (regexp,list[(string,regexp)]).
  getREs(_,[],RE,S) => (RE,S).
  getREs(State,[(State,R),..REs],RE,S) => getREs(State,REs,orRE(RE,R),S).
  getREs(State,[St,..REs],RE,S) => getREs(State,REs,RE,[St,..S]).

  private ev[T] <~ { ev:[]=>T. }.

  private genD:[string,rtree,integer,string,
		list[(integer,list[integer])],list[(string,integer)]] =>
      (list[(integer,list[dfaE])],
       list[(integer,list[integer],string)],
       list[(integer,list[finalType])],integer).

  genD(StName,(Tr::tree(_,_,F,_).=Tr),StNo,sigma,fllw,pos) =>
      (:ev[(list[(integer,list[dfaE])],
	    list[(integer,list[integer],string)],
	    list[(integer,list[finalType])],integer)]..{

	 Sno:integer := StNo.
	 finals:list[(integer,list[finalType])] := pickFinal(Tr,pos,F,StNo).
	 states:list[(integer,list[integer],string)] := [(Sno,F,StName)].
	 trans:list[(integer,list[dfaE])] := [].
	 Mark:integer := Sno.
	 tt:list[dfaE] := [].

	 ${
	   do();
	 }.

	 ev()=>(trans,states,finals,Sno).

	 do:[]*.
	 do()::Mark=<Sno, (Mark,T,_) in states ->
	     tt := [];
	     (a in sigma *>
	      U = setof(collapse({((p,FF) in fllw?FF|[]) .. (p::(C,p) in pos, a in C) in T},[]));
	      
	      (U !=[] ?
		 ( (Nx,U,_) in states ?
		     tt := [dfa(a,Nx),..tt]
		 | Sno := Sno+1;
		   tt := [dfa(a,Sno),..tt];
		   finals := pickFinal(Tr,pos,U,Sno)\/finals;
		   states := [(Sno,U,""),..states];
		 );
	      )
	     );
	     trans := [(Mark,tt),..trans];
	     Mark := Mark+1;
	     do().
	 do()->{}.
       }).ev().

  genDfa:[list[(string,regexp)],string]=>
      list[(integer,list[dfaE],list[finalType])].
  genDfa(Rs,sigma) =>
      (:ev[list[(integer,list[dfaE],list[finalType])]]..{
	 
	 P:integer := 1.
	 Sno:integer := 0.
	 States:list[(integer,list[integer],string)] := [].
	 Finals:list[(integer,list[finalType])] := [].
	 Trans:list[(integer,list[dfaE])] := [].
	 
	 ${
	   ( (Sname,RE) in mergeREStates(Rs) *>
	     ((T,P1,pos) = mtree(RE,P,sigma,[]);
	     
	      P := P1;
	      (Tr,St,Fn,Sno1) = genD(Sname,T,Sno,sigma,follow(T,[]),pos);
	      Sno := Sno1+1;
	      
	      States := St\/States;
	      Finals := Fn\/Finals;
	      Trans := Tr\/Trans
	     )
	   );
	   Finals := switchify(Finals,States);
	 }.
	 
	 ev()=> {(S,
		  setof({ dfa(a,Nx) .. (a::dfa(a,Nx) in T) in sigma}),
		  ((S,F) in Finals?F|[])) ..((S,_,_)::(S,T) in Trans)
		  in States}.
	 
	 mkDfa:[list[(integer,list[integer],string)]]=>
	     list[(integer,list[dfaE],list[finalType])].
	 mkDfa([]) => [].
	 mkDfa([(S,_,_),..Sts])::(S,T) in Trans =>
	     valof{
	       SS = setof({ dfa(a,Nx) .. (a::dfa(a,Nx) in T) in sigma});
	       FF = ((S,F) in Finals?F|[]);
	       valis [(S,SS,FF),..mkDfa(Sts)]
	     }.
       }).ev().

  showDfaStates:[list[(integer,list[dfaE],list[finalType])]]*.
  showDfaStates(Dfas) ->
      (St,D,F) in Dfas *>
      stdout.outLine("\nState: "<>St.show());
      ( dfa(C,Nx) in D *>
	stdout.outLine("   "<>C.show()<>" -> "<>Nx.show())
      );
      ( Fn in F *>
	stdout.outLine("Final: "<>Fn.show())
      ).
}
