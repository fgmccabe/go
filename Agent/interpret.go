/*
  Interpret & manage logic expressions
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

(dynamic_facts,dynamic_clauses,call,cls,add,addz,del,evalgen)..{ 
  include "sys:go/stdlib.gof".
  include "interpret.gh".
      			  	      			  				
  call((Call,_,_,_,_),C):-Call(C).
  cls((_,Cls,_,_,_),Hd,Cl):-Cls(Hd,Cl).
  add((_,_,Add,_,_),Cl)->Add(Cl).
  addz((_,_,_,Addz,_),Cl)->Addz(Cl).
  del((_,_,_,_,Del),Cl)->Del(Cl).
  
  dynamic_facts(Init) => 
    (call,cls,add,addz,del)..{
  	
      FsList:=Init. 			
  			
      add(F) -> FsList := [F,..FsList].
      addz(F) -> FsList := FsList <> [F].
  			
      del(F):: remove(F,FsList,NewFsList) -> FsList := NewFsList.
      del(_) -> {}.
  			
      call(F) :- F in FsList.
  			
      cls(Hd,TRUE):- Hd in FsList
    }.
  			
  dynamic_clauses(Init,eval) => 
    (call,cls,add,addz,del)..{
  	  			
      ClsList:=Init.  			
  			
      add(Cl) -> ClsList := [Cl,..ClsList].
  			
      addz(Cl) -> ClsList := ClsList <> [Cl].
  			
      del(Cl)::remove(Cl,ClsList,NewClsList) -> ClsList := NewClsList.
      del(_) -> {}.
  			
      call(C) :- cls(C,Bdy),eval(Bdy).
  			
      cls(Hd,Bdy):- cl(FrHd,FrBdy) in ClsList,
  		    thaw((FrHd,FrBdy))=(Hd,Bdy)
  }.
  			
  evalgen(evalc)=>eval..{
    eval(TRUE).
    eval(?=(X,Y)):-X=Y.
    eval(?\=(X,Y)):- \+X=Y.
    eval(?+(Call)):- evalc(Call).
    eval(?\+(Call)):- \+ evalc(Call).
    eval(not(Q)) :- \+ eval(Q).
    eval(&(Q1,Q2)):-eval(Q1),eval(Q2).
    eval(or(Q1,Q2)):-eval(Q1).    	
    eval(or(Q1,Q2)):-eval(Q2).  			  	
    
    evalall([]).
    evalall([Call,..Calls]) :- evalc(Call),evalall(Calls)
  }.
    		
  remove(X,[X,..L],L).
  remove(X,[U,..L],[U,..RL]):-remove(X,L,RL).
}
	
