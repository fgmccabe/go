/*
  Module to perform simple macro substitutions in abstract parse trees

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
macro{
  import go.io.
  import misc.
  import types.
  import abstract.

  macroEnv:[list[A],list[B],list[(A,B)]]=>list[(A,B)].
  macroEnv([],[],E)=>E.
  macroEnv([P,..rP],[A,..rA],E) => macroEnv(rP,rA,[(P,A),..E]).

  private replace:[abstract,list[(abstract,abstract)]]=>abstract.
  replace(IDEN(Nm,_),E)::(IDEN(Nm,_),R) in E => R.
  replace(IDEN(Nm,Lc),_) => IDEN(Nm,Lc).
  replace(APPLY(IDEN(Nm,_),A,_),E)::(APPLY(IDEN(Nm,_),P,_),R) in E =>
      replace(R,macroEnv(P,A,E)).
  replace(APPLY(F,A,L),E)=>APPLY(replace(F,E),macroList(A,E),L).
  replace(BRACE(F,A,L),E)=>BRACE(replace(F,E),macroList(A,E),L).
  replace(SQUARE(F,A,L),E)=>SQUARE(replace(F,E),macroList(A,E),L).
  replace(TPL(A,L),E)=>TPL(macroList(A,E),L).
  replace(X,_) => X.

  macro:[abstract,list[(abstract,abstract)]]=>abstract.
  macro(A,E) => replace(A,E).

  macroList:[list[abstract],list[(abstract,abstract)]]=>list[abstract].
  macroList(L,E) => { replace(A,E) .. A in L }.
}