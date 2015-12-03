/*
 * Interface to the ACS communications system
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
 
acsComms{
  import go.io.
  
-- A locator is an address structure
  locator ::= locator(symbol,string,QoS[]).
  
  acsHandle ::= acsHdl((symbol,any)[]).
  
-- A directory entry has a name, quality of service and other attributes
  dir_entry ::= dirE(symbol,QoS[],acsHandle).
  
  acsComms(acsHost,acsPort,dsHost,dsPort,acsHdl(Desc)) => 
      ((genHandle(acsHdl([('address',lcAddr),..Desc])),spawn{ outGoing()}) ..{
	 lcAddr = valof{
	   enCode(acsOut,??(('connectMe',[])));
	  ??(('addressIs',Reply:string)) = deCode(acsIn);
	   spawn{inComing()};
	   valis ??(Reply)
	 }.
    
	 outGoing() -> 
	     ( (For,Opts,Msg) << From ->
		   enCode(acsOut,??((genAddress(For),genAddress(From),[],sencode(Msg))))
	     | X << From -> outLine(stdout,"Unknown message "<>X^2<>" from "<>From^0)
	     ); outGoing().
  
	 inComing()::\+atEof(acsIn) ->
	    ??((_,_,_,Msg)) = deCode(acsIn);
	     distributeMsg(sdecode(Msg));
	     inComing().
	 
	 distributeMsg(??((For,From,Opts,Msg))) ->
	     __postMsg(genHandle(From),Msg,genHandle(For),genReply(Opts,From),genLease(Opts)).
      
	 genReply([],F)=>F.
	 genReply([('replyTo',??(H:handle)),.._],_) => H.
	 genReply([_,..L],F) => genReply(L,F).
	 
	 genLease([])=>0.
	 genLease([('leaseHold',??(N:number)),.._]) => N.
	 genLeas([_,..L]) => genLease(L).
	 
       })..{
	(acsIn,acsOut) = tcpConnect(acsHost,acsPort).
	(genAddress,genHandle) = dsTable(dsHost,dsPort).
	
	dsTable(host,port) =>
	    (genAddress,genHandle) .. {
	      table = $cell[handle]([]).
	      (dsIn,dsOut) = tcpConnect(host,port).
       
	      matchHdl(hdl(L1),hdl(L2)) :-
		  mtch(L1)..{
		    mtch([]):--true.
		    mtch([(K,V),..Lr]) :--
			(K,V) in L2,
			mtch(Lr)
		  }.
	      
	      genHandle(h) => H :- H in table.get(), matchHdl(h,H).
	      genHandle(h) => valof{
				enCode(dsOut,??(('queryOne',h)));
				Rpl = deCode(dsIn);
				table.set([Rpl,.. table.get()]);
				valis Rpl
			      }.
	      
	      genAddress(h) => A:string :- acsHdl(H)=genHandle(h),('address',??(A)) in H.
	    }.
       }.
}.

