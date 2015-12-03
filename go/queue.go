/*
  A queue package
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

go.queue{
  queue[T] <~ {
	front:[]=>T. 
	push:[T]*. 
	pull:[-T]*. 
	depth:[]=>number. 
	queue:[]=>list[T]}.
                                                             
  queue:[list[T]]@>queue[T].
  queue(Init:list[T])..{
    Q:list[T] := [].

    push(E) -> sync{ Q:= Q<>[E] }.
    pull(E) -> sync{ [E,..R] = Q;
                     Q := R }.

    front() => valof{
		 Q = [H,.._];
		 valis H
	       }.

    queue() => Q.

    depth() => listlen(Q).

    show() => "{"<>showQueue(Q,"")<>"}".

    showQueue:[list[T],string]=>string.
    showQueue([],_) => [].
    showQueue([El,..M],Sep) => Sep<>El.show()<>showQueue(M,", ").
  }
}