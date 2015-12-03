/*
   Interface to the simple agent communications system
   (c) 2004 F.G. McCabe
 
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

  Contact: Francis McCabe <mccabe@fla.fujitsu.com>
 */
 
go.scomms{
  import go.io.
  import go.stdparse.
  import go.hash.
  import go.encoded.
  import go.mbox.

  scsMailbox(N)<=mailbox(N).
  scsMailbox(N):mailbox[]..{
    ${
      known.insert(N,this)
    }
  }.

  defaultHost() => getenv('GO_COMMS_HOST',"localhost").
  defaultPort() => numeric%%getenv('GO_COMMS_PORT',"4545").

  __default(C,Opts,_) => Val :- (C,Val) in Opts.
  __default(_,_,Def) => Def.

  pickHandle(Options) => __default(`n,Options,'<main>').

  scsOut := nullout.
  scsIn := nullin.

  known = $hash([],32).

  ${
    tcpConnect(defaultHost(),defaultPort(),In,Out,rawEncoding);
    scsOut := Out;
    scsIn := In;
    spawn{inComing(In)}
  }.

  inComing(In)::In.eof() -> {}.
  inComing(In) ->
      grabCodedInput(In,Data);
      processIncoming(decodeTerm%%Data);
      inComing(In).

  processIncoming(anyVal(tuple([_,_,strg(Msg)]),_)) ->
      ( ??((For:handle[],F:handle[],_:list[(symbol,any)],M:any)) = sdecode(Msg) ?
          distributeMessage(For,F,M)
      | {__logmsg("Unrecognizable message "<>sdecode(Msg)^0)}
      ).
  processIncoming(anyVal(symb("'Ok"),_)) -> {}.
  
  distributeMessage(For,From,Msg) ->
--      stdout.outLine("Message "<>Msg^0<>" for "<>For.show()<>" from "<>From.show());
      ( known.present(For,H) ?
          H.enQueue(Msg,From)
      | stderr.outLine("discarding message "<>Msg^0<>" for unknown target: "<>For.show())
      ).

  hdl(Thr,_) <= hd(Thr).  -- We inherit from internal handles for internal delivery
  hdl(Thr,Root):handle[] .. {
    register() -> 
        scsOut.encode(??(('register',Root)));
--        ( ??('Ok') = scsIn.decode() ? {}
--        | raise error("register",'eFAIL'));
        known.insert(hdl(Thr,Root),this).
    
    deregister() ->
        known.delete(hdl(Thr,Root)).
    
    show()=>explode(Thr)<>":"<>explode(Root).

    postMsg(Msg)::self<=hdl(T,R) ->  -- Someone has posted a message to this handle, 
        scsOut.outBytes(encodeTerm(anyVal(tuple([hndl(Thr,Root),hndl(T,R),
                                                 strg(sencode(??((hdl(Thr,Root),
                                                                  hdl(T,R),[]:list[(symbol,any)],Msg))))]),
                                          tupleType([handleType,handleType,polyType("list",[charType])])))).
    postMsg(_) ->
        raise error("can only send to this handle from other registered scs handles",'fail')
  }.
}.
