directory{
  import go.io.
  import go.mbox.
  import go.dynamic.

  attVal ::= none.

  attribute ::= attr(symbol,attVal).

  private
  DSreply ::= inform(list[list[attribute]]) | ok.

  private
  DSmessage ::= reg(list[attribute],dropbox[DSreply])
              | search(list[attribute],list[symbol],dropbox[DSreply]).
              
  directory <~ {
        find:[list[attribute],list[symbol]]=>list[list[attribute]].
        register:[list[attribute]]*
      }.

  private dir_server:[mailbox[DSmessage]]@>thread.
  dir_server(_) <= thread().
  dir_server(Mbx)..{
    start() -> directory_server().

    creator() => this.

    matches:[list[t],list[t]]{}.
    matches(D,E) :-
        A in E *> A in D.
    
    description:dynamic[list[attribute]]=dynamic([]).

    directory_server:[]*.
    directory_server() ->
	case Mbx.next() in (
         reg(Descr,Rep) -> description.add(Descr); Rep.post(ok)
       | search(SDescr, AttrNms,Client) ->
             Client.post(inform({ extract(Desc,AttrNms) .. (Desc::matches(Desc,SDescr)) in 
                                  description.ext()}))
        );
        directory_server().

    extract:[list[attribute],list[symbol]]=>list[attribute].
    extract(Descr,Nms) =>
        {attr(Nm,A) .. (Nm::attr(Nm,A) in Descr) in Nms}.
  }.

  private
  dBox:mailbox[DSmessage] = mailbox().

  ${
    _ = dir_server(dBox);
  }.

  client:[]@>directory.
  client()..{
    mH:mailbox[DSreply] = mailbox().

    register(Desc) ->
        sync{ dBox.dropbox().post(reg(Desc,mH.dropbox()));
	  mH.msg(ok) }.
    find(Desc,Atts) =>
        valof{
          sync { 
	    dBox.dropbox().post(search(Desc,Atts,mH.dropbox()));
	    mH.msg(inform(L));
	  };
          valis L
        }
  }
}