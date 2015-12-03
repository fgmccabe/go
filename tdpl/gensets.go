tdpl.gensets{
  import tdpl.types.
  import tdpl.misc.
  import go.hash.
  import go.setlib.

  -- We need to know which non-terminals can generate the empty expansion
  nullable:[list[symbol],list[rule]]=>list[symbol].

  nullable(nonterms,rules) => 
      (:ev[list[symbol]]..{

	 N:list[symbol] := [].
	 done:logical := false.

	 ev()=> valof{
		  do();
		  valis N
		}.

	 do:[]*.
	 do()::done -> {}.
	 do() ->
	     done := true;
	    
	     ((nt::\+nt in N) in nonterms *>
	      (rule(_,nt,rhs,_,_,_) in rules,check(rhs) *>
	       N := [nt,..N];
	       done := false
	      )
	     );
	     do().
	
	 check:[list[symbol]]{}.
	 check(rhs) :- R in rhs *> R in N.
       }).ev().

  -- extends(A,B) is true if there is an element of B that is not in A
  extends:[list[t],list[t]]{}.
  extends(A,B) :- e in B, \+e in A.

  -- first -- construct the table of lead-ins for each non-terminal
  first:[list[symbol],list[symbol],list[rule],list[symbol]] => list[(symbol,list[symbol])].
  first(terms,nonterms,rules,null) => 
      (:ev[list[(symbol,list[symbol])]]..{

	 ev()=>valof{
		 do();
		 valis F.ext()
	       }.

	 F:hash[symbol,list[symbol]] = hash({(T,[T]) .. T in terms}, listlen(nonterms)).
	 done:logical := false.
	 
	 do:[]*.
	 do()::done->{}.
	 do()->
	     done := true;
	     ( nt in nonterms *> (
		FF = (F.present(nt,FL) ? FL | []);
		
		FF1 = addToFF(rules,nt,FF);
		( extends(FF,FF1)?
		    ( F.insert(nt,FF1);
		      done := false)
		| {}
		)
	       )
	     );
	    do().
	
	 addToFF:[list[rule],symbol,list[symbol]]=>list[symbol].
	 addToFF([],_,FF) => FF.
	 addToFF([rule(_,nt,rhs,_,_,_),..Rules],nt,FF) =>
	     addToFF(Rules,nt,doRhs(rhs,FF,true)).
	 addToFF([_,..Rules],nt,FF) =>
	     addToFF(Rules,nt,FF).
	 
	 doRhs:[list[symbol],list[symbol],logical]=>list[symbol].
	 doRhs([],FF,_) => FF.
	 doRhs([L,..R],FF,true) =>
	     doRhs(R,(F.present(L,FL) ? FL\/FF | FF),(L in null?true|false)).
	 doRhs(_,FF,false) => FF.
       }).ev().
}