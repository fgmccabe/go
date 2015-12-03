/* Transformation package
 (c) 2006 F.G. McCabe
 
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
graph.transform{
  import graph.matrix.

  transform <~ { apply:[float,float]=>(float,float). 
      combine:[transform]=>transform. }.

  matTrans:[matrix]@=transform.
  matTrans(M)..{
    apply(X,Y)=>M.apply(X,Y).
    combine(matTrans(N))=>matTrans(M.mult(N)).
  }.

  rot:[(float,float),float]=>transform.
  rot((U,V),Theta) => 
      valof{
	cT = cos(Theta);
	sT = sin(Theta);
	valis matTrans(gmtx(cT,sT,-sT,cT,
			    U*cT-U-V*sT,U*sT+V*cT-V))
      }.

  trans:[(float,float)]=>transform.
  trans((U,V))=> matTrans(gmtx(1.0,0.0,0.0,1.0,U,V)).

  scale:[(float,float),float,float]=>transform.
  scale((U,V),sX,sY) =>
      matTrans(gmtx(sX,0.0,sY,0.0,U*sX-U,V*sY-V)).
}