tdpl.lr0{
  import tdpl.types.
  import tdpl.gensets.
  import tdpl.misc.
  import go.hash.
  import go.setlib.
  import go.io.

  private closure:[list[item],list[rule]]=>list[item].
  closure(It,rules) =>
      (:ev[list[item]]..{
	 ev()=>I.

	 I:list[item] := It.
	 done:logical := false.

	 ${
	   do()
	 }.

	 do:[]*.
	 do()::done->{}.
	 do()->
	     done := true;
	     ( item(_,_,_,[X,.._],_) in I *>
	       ( rule(Cnt,X,Tau,_,_,_) in rules *>
		 ( \+ item(Cnt,X,[],Tau,_) in I ?
		     I := [item(Cnt,X,[],Tau,[]),..I];
		     done := false
		 | {} )
	      )
	    );do().
       }).ev().

  goto:[list[item],symbol]=>list[item].
  goto(I,X) =>
      setof({ item(Cnt,A,B<>[X],Beta,LA) .. item(Cnt,A,B,[X,..Beta],LA) in I}).

  lr0states:[list[rule],symbol]=>(list[(integer,list[item])],
				  list[(integer,symbol,integer)]).
  lr0states(rules,start) => 
      (:ev[(list[(integer,list[item])],list[(integer,symbol,integer)])]..{

	 ev()=>(T,G).

	 Sno:integer := 0.
	 T:list[(integer,list[item])] := [(0,[item(0,'?',[],[start],[])])].
	 G:list[(integer,symbol,integer)] := [].
	 done:logical := false.
	 TT:list[(integer,list[item])] := [].

	 ${
	   do()
	 }.

	 do:[]*.
	 do()::done->{}.
	 do() ->
	     TT := T;
	     done := true;
	    
	     ((C,I) in T *>
	      II = closure(I,rules);
	     
	      ( item(_,_,_,[X,.._],_) in II *>
		J = goto(II,X);
		
		( (D,J) in T ?
		    ( (C,X,D) in G ?
			{}
		    | G := [(C,X,D),..G];
		    )
		| Sno := Sno+1;
		  T := [(Sno,J),..T];
		  G := [(C,X,Sno),..G];
		  done := false
		)
	      )
	     );
	     do().
       }).ev().
       
}