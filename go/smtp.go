go.smtp{
  import go.io.

  host:string:="".
  domain:string:="".
  user:string:="".

  config:[string,string,string]*.
  config(smtpHost,domn,usr) ->
      host := smtpHost;
      user := usr;
      domain := domn.

  email<~ { post:[]* }.
  email:[string,string,list[(symbol,string)]]@=email.
  email(Recip,Body,Headers)..{
    post() -> 
	tcpConnect(host,25,inCh,outCh,rawEncoding);
	login(inCh,outCh);
	sendCmd("MAIL FROM:<"<>user<>">",inCh,outCh);
	sendCmd("RCPT TO:<"<>Recip<>">",inCh,outCh);
	sendBody(inCh,outCh);
	sendCmd(".",inCh,outCh);
	quit(outCh);
	inCh.close();
	inCh.close().

    sendCmd:[string,inChannel,outChannel]*.
    sendCmd(Cmd,inCh,outCh) ->
	outCh.outLine(Cmd);
	line = inCh.inLine("\n");
	( [`2,`5,`0,.._] .= line ? {}
	| stdout.outLine("Problem in sending email: "<>line)
	).

    login:[inChannel,outChannel]*.
    login(inCh,outCh) ->
	sendCmd("HELO "<>domain,inCh,outCh).

    quit:[outChannel]*.
    quit(outCh) ->
	outCh.outLine("QUIT").

    sendBody:[inChannel,outChannel]*.
    sendBody(inCh,outCh)->
	outCh.outLine("DATA");
	line = inCh.inLine("\n");
	( [`3,`5,`4,.._] .= line ?
	    ((H,T) in Headers *>
	     outCh.outLine(explode(H)<>": "<>T));
	    outCh.outLine(Body)
	 | stdout.outLine("Problem in sending email: "<>line)
	);
	sendCmd(".",inCh,outCh).
  }.
}