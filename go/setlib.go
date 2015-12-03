/* Set library to support Go! programs
   (c) 2004-2005 F.G. McCabe
 
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

go.setlib{
  (\/):[list[t],list[t]]=>list[t].
  []\/X=>X.
  [E,..X]\/Y::E in Y => X\/Y.
  [E,..X]\/Y => [E,..X\/Y].
  
  (/\):[list[t],list[t]]=>list[t].
  []/\_=>[].
  [E,..X]/\Y::E in Y => [E,..X/\Y].
  [_,..X]/\Y => X/\Y.
  
  (\):[list[t],list[t]]=>list[t].
  X\ [] => X.
  X\ [E,..Y] => remove(X,E)\Y.
  
  diff:[list[t],list[t]]=>list[t].
  diff(X,[]) => X.
  diff(X,[E,..Y]) => remove(X,E)\Y.
  
  private remove:[list[t],t]=>list[t].
  remove([],_) => [].
  remove([E,..X],E) => remove(X,E).
  remove([E,..X],D) => [E,..remove(X,D)].
  
  subset:[list[t],list[t]]{}.
  subset([],_).
  subset([x,..X],S) :- x in S, subset(X,S).

  -- Convert a list into a set
  setof:[list[t]]=>list[t].
  setof(L) => stOf(L,[]).

  private stOf:[list[t],list[t]]=>list[t].
  stOf([],S)=>S.
  stOf([E,..L],S)::E in S => stOf(L,S).
  stOf([E,..L],S) => stOf(L,[E,..S]).

  filterIn:[list[t],t]=>list[t].
  filterIn([],_)=>[].
  filterIn([E,..L],T)::\+(\+E=T) => [E,..filterIn(L,T)]. -- safe equality check
  filterIn([_,..L],T) => filterIn(L,T).

  filterOut:[list[t],t]=>list[t].
  filterOut([],_)=>[].
  filterOut([E,..L],T)::\+(\+E=T) => filterOut(L,T). -- safe equality check
  filterOut([E,..L],T) => [E,..filterOut(L,T)].

  
}
