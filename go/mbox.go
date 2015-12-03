/*
   Implements a simple mail box facility for use between local threads.
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
go.mbox{
  import go.hash.
  import go.queue.
  import go.dynamic.
  import go.io.

  -- Types that support inter-process communication
  dropbox[M] <~ { post:[M]* }.

  mailbox[M] <~ {
	next:[]=>M. 
	nextW:[number]=>M.
	pending:[]{}.
	msg:[M]*.
	msgW:[M,number]*.
	dropbox:[]=>dropbox[M] }.
  
  nullhandle:[]@=dropbox[_].
  nullhandle..{
    show()=>"nullbox".
    post(_) -> {}.                   -- We discard messages to the nullhandle
  }.
	
  timedout:[]@=exception.
  timedout..{
    cause() => "timedout".
    code() => 'timedout'.
    show() => "timeout".
  }.
			
  private mhandle:[dynamic[M]]@=dropbox[M].
  mhandle(Q)..{
    post(Msg) -> 
	sync(Q){Q.add(Msg)}.
  }.
        
  mailbox:[]@>mailbox[_].
  mailbox():mailbox[m]..{
    Msgs:dynamic[m] = dynamic([]).

    myH:dropbox[m] = mhandle(Msgs).

    pending() :- Msgs.mem(_).
    
    next() => valof{
                sync(Msgs){
                  Msgs.mem(E) ->
                      Msgs.del(E);
		      valis E
                }
              }.
   
    nextW(T) => valof{
		  sync(Msgs){
                    Msgs.mem(E) ->
			Msgs.del(E);
			valis E
                  }
                  timeout (T+now() -> raise timedout)
                }.
    
    msg(P) ->
        sync(Msgs){
          Msgs.mem(P) ->
              Msgs.del(P);
        }.

    msgW(P,T) -> sync(Msgs){
          Msgs.mem(P) ->
              Msgs.del(P);
        }
        timeout (T+now() -> raise timedout).

    dropbox() => myH.
  }.
}