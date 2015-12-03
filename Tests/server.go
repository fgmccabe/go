/*
 * Test of the TCP server
 */
 
server{
  import go.io.
  import go.stdparse.

  mySrvr:[]@=serverProc.
  mySrvr..{
    exec(H,I,_,i,o) ->
        stdout.outLine("Connection from "<>H<>"/"<>I);
        srvr_loop(i,o).

    srvr_loop:[inChannel,outChannel]*.
    srvr_loop(i,_):: i.eof() -> stdout.outLine("Connection closed").
    srvr_loop(i,o) -> L = i.inText("\n\+4;"); stdout.outLine("recvd :"<>L.show()); 
        o.outLine("You sent "<>listlen(L).show()<>" characters");
        (L=[] ? stdout.outLine("control-D received") | srvr_loop(i,o)).
  }.
                   
  main([port]) ->
     stdout.outLine("Starting server on port "<>port);
      tcpServer(naturalOf%%port,mySrvr,utf8Encoding).
 }.
