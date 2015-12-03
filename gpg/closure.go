gpg.closure{
  import gpg.gpgTypes.
  import gpg.gensets.
  import gpg.misc.
  import go.setlib.
  import go.dynamic.
  import go.io.

  firstSeq:[list[t],list[(t,list[t])],list[t]]=>list[t].
  firstSeq([],_,_)=>[].
  firstSeq([X,.._],F,N) ::\+X in N =>
      ((X,FF) in F ? FF | []).
  firstSeq([X,..Y],F,N) =>
      ((X,FF) in F ? FF | [])\/firstSeq(Y,F,N).

  private firstOf:[list[symbol],
		   list[symbol],list[symbol],list[(symbol,list[symbol])],
		   list[symbol]]=>
      list[symbol].
  firstOf([],NL,_,_,_)=>NL.
  firstOf([b,..LA],NL,Beta,F,N) => firstOf(LA,firstSeq(Beta<>[b],F,N)\/NL,Beta,F,N).

  closure:[list[item],list[rule],list[(symbol,list[symbol])],list[symbol]]=>list[item].
  closure(items,rules,F,N)=>
      (:ev[list[item]]..{

	 ev()=>I.

	 I:list[item]:=items.
	 done:logical:=false.
	 next:list[item] := I.
	 modified:list[item] := I.

	 ${
	   do()
	 }.

	 do:[]*.
	 do()::done -> {}.
	 do()->
	     done:=true;
	     next := [];
	     ( item(_,_,_,[X,..Beta],LA) in modified *>
	       ( rule(C,X,T,_,_,_) in rules *>
		 ( item(C,X,[],T,L) in I ?
		     NL = firstOf(LA,L,Beta,F,N);
		     ( extends(NL,L) ?
			 done := false;
			 I := splice(C,NL,I);
			 next := splice(C,NL,next)
		     | {})
		 | L = firstOf(LA,[],Beta,F,N);
		   I := [item(C,X,[],T,L),..I];
		   next := [item(C,X,[],T,L),..next];
		   done := false
		 )
	       )
	     );
	     modified := next;
	    do().
       }).ev().
  
  private splice:[integer,list[symbol],list[item]]=>list[item].
  splice(C,LA,[item(C,X,[],T,_),..LL])=>[item(C,X,[],T,LA),..LL].
  splice(C,LA,[I,..LL])=>[I,..splice(C,LA,LL)].
  splice(_,_,[])=>[].
}