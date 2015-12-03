/*
  A contract driven agent
  (c) 1994-2001 Imperial College and F.G. McCabe

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Contact: Francis McCabe <fgm@fla.fujitsu.com>
*/


main .. {

  query(CT) ::= askone(T1,Goal(CT)) | askall(T1,Goal(CT)) .
  
  updateReq(T) ::= addcl(clause(T)) |
  		delcl(clause(T)) | addfact(T) | delfact(T) .


  callterm::= 	male(symbol) | female(symbol) | 
  		parent(symbol,symbol) | age(symbol,number) |
  		ancestor(symbol,symbol) | likes(symbol,symbol).
  		

  Male = dynamic_facts([male('k'),male('t'),male('f'),male('st')]). 
  
  Female =dynamic_facts([female('s'),female('m'),female('a')]).
  
  Parent = dynamic_facts([
  			parent('am','k'),parent('l','k'),parent('k','t'),parent('s','t'),
  			parent('k','a'),parent('s','a'),
  			parent('f','st'),parent('m','st')]).
  			
  Ancestor= dynamic_clauses([cl(ancestor(A,D),?+(parent(A,D))),
  			cl(ancestor(A,D), &(?+(parent(P,D)),?+(ancestor(A,P))))],
  			eval).
  			

  			
  Age = dynamic_facts([ 
  	age('am',85),age('l',88),age('k',50),age('s',48),age('f',43),
  	age('m',40),age('a',4),age('t',6),age('st',1)]).
  	
  Likes= dynamic_clauses(
  			[cl(likes('k',P),?+(parent('k',P))),
  			cl(likes(P,'a'),TRUE)], 
  			eval).
  			
   evalc(male(P)):-call(Male,male(P)).
  evalc(female(P)):-call(Female,female(P)).
  evalc(parent(P,C)):-call(Parent,parent(P,C)).
  evalc(age(P,Ag)):-call(Age,age(P,Ag)).
  evalc(ancestor(A,D)):-call(Ancestor,ancestor(A,D)).
  evalc(likes(P1,P2)):-  call(Likes,likes(P1,P2)).

  eval=evalgen(evalc).			
  	
  
  
  
  addFact(male(X)) -> add(Male,male(X)); 
  			displayVal("After adding\n",X);
  			displayVal("Male list is:\n",{P||evalc(male(P))}).
  								
  
  addFact(female(X)) -> add(Female,female(X)).
  addFact(parent(X,Y)) -> add(Parent,parent(X,Y)).
  
  addClause(cl(likes(P,T),Bdy)) -> add(Likes,cl(likes(P,T),Bdy)).
  
  delFact(male(X)) -> del(Male,male(X)); 
  		displayVal("After deleting\n",X);
  		displayVal("Male list is:\n",{P||evalc(male(P))}).
  delFact(female(X)) -> del(Female,female(X)).
  delFact(parent(X,Y)) -> del(Parent,parent(X,Y)).
  
  delClause(cl(likes(X,Y),Bdy)) -> del(Likes,cl(likes(X,Y),Bdy)).


  askQuery(S,Q) -> displayVal("Query is:\n",Q);
		Q>>S; Ans << S; displayVal("Answer is:\n",Ans).
     
  displayVal(Mess,Val) -> Mess <> Val^0 <> "\n\n" >> stdout.
	
  queryserver() -> 
    (
      addfact(ft) << _ -> addFact(ft)
    | delfact(ft) << _ -> delFact(ft)
    | addcl(Cl) << _ -> addClause(Cl)
    | delcl(Cl) << _ -> delClause(Cl) 
    | askone(A,Q) << C -> (eval(Q)? A >> C | fail >> C)
    | askall(A,Q) << C ->
	displayVal("Received1:\n",(A,Q));
	{A || eval(Q)} >> C
    | M << S -> ("Rejected message",M) >> S 
    );
    queryserver().
		
  test()-> 
    addFact(male('bill'));delFact(male('k'));
    spawn { "Entered spawned client\n" >> stdout;
 	     delfact(male('st')) >> creator;
 	     addcl(cl(likes(P,'apples'),?+(likes(P,'pears'))))>>creator;
 	     addcl(cl(likes('k','pears'),TRUE))>>creator;

            askQuery(creator,askall(P,or(?+(male(P)),?+(female(P)))));
           
            askQuery(creator,'hello');
            askQuery(creator,askall((M,A),
           		& (?+(male(M)), &( ?+(age(M,A)), ?\+(parent(M,_))))));

   	    askQuery(creator,askall((D,A2),& (?+(ancestor('am',D)),
    	   				              ?+(age(D,A2)))));
    	   
    	   askQuery(creator,askall((P1,P2),?+(likes(P1,P2))));
    	   delcl(cl(likes(P,'a'),TRUE))>> creator;
    	   askQuery(creator,askone((P1,P2),?+(likes(P1,P2))));
    	   askQuery(creator,askall((P1,P2),?+(likes(P1,P2))));
    };
    queryserver();

}