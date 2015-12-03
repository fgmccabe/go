/*
  Platform module, gives Go! programs access to the ACS communications infrastructure
  (c) 2001 F.G. McCabe

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


connectACS .. {
  include "sys:go/acs.gh".
  include "sys:go/stdlib.gof".
  include "sys:so/io.gof".
  
  
  -- construct an agent name, used if no agent name is given
  generateName() => implode("Goagent"<>now()^0<>":"<>irand(10000000)^0);
  
  buildHndl(hndl(L))::('name',_) in L => hndl(L).
  buildHndl(hndl(L)) => hndl([('name',any(generateName())),..L]).
  
  sameHndl(hndl(L1),hndl(L2)) :-
    ('name',Nm) in L1!,
    ('name',Nm) in L2!.
    
  -- This is not a function 'cos there may not be a valid address
  pickAddress(hndl(L),K,url) :-
    ('locator',any(Lx)) in L,
    locator(K,url,Q) in Lx,
    \+( HoldUntil(T) in Q, now()>T).

  
    
  
  
}
