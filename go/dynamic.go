/*
  Standard program for implementing dynamic programs 
  (c) 2004-2006 F.G. McCabe
 
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

  Contact: Francis McCabe <frankmccabe@mac.com>
 */

go.dynamic{ 

  dynTest[T] <~ { check:[T]{} }.         -- We use this as a more sophisticated match

  dynamic[T] <~ { mem:[T]{}.		 -- Test for membership in the relation
	add:[T]*.			 -- Add element to relation
	del:[T]*.			 -- Delete element from relation
	delc:[dynTest[T]]*.	      -- Delete satisfying element from relation
	delall:[T]*.			 -- Delete all matching elements 
	delallc:[dynTest[T]]*.		 -- Delete all satisfying elements
	ext:[]=>list[T].		 -- pick up extension
	match:[dynTest[T]]=>list[T] }.	 -- pick up matching sub-relation

  dynamic:[list[T]]@>dynamic[T].
  dynamic(Init)..{
    Db:list[symbol] := initDB(Init).

    initDB:[list[t]]=>list[symbol].
    initDB([]) => [].
    initDB([E,..L]) => [__term(E),..initDB(L)].
    initDB(X) => raise error("problem with "<>X.show(),'fail').
       
    mem(P) :- Cl in Db, __is(Cl,P).
       
    add(N) -> sync{ Db := Db<>[__term(N)] }.
       
    del(E) -> sync{ append(F,[Cl,..R],Db), __is(Cl,E) ? Db := F<>R | {} }.

    delc(Tst) -> sync{ append(F,[Cl,..R],Db), __is(Cl,E),Tst.check(E) ? Db := F<>R | {} }.

    delall(E) -> sync{ Db := { Cl .. (Cl::\+__is(Cl,E)) in Db } }.

    delallc(Tst) -> sync{ Db := { Cl .. (Cl::\+(__is(Cl,E),Tst.check(E))) in Db }}.

    ext() => valof{
	       sync{
		 valis { E .. (Cl::__is(Cl,E)) in Db }
	       }
	     }.

    match(K) => valof{
		  sync{
		    valis { E .. (Cl::__is(Cl,E),K.check(E)) in Db }
		  }
		}.
    
    show()=> "{" <> showExt(ext(),"") <> "}".

    showExt:[list[symbol],string]=>string.
    showExt([],_)=>"".
    showExt([E,..Rl],Pre) => Pre<>E.show()<> showExt(Rl,". ").
  }.

}.
