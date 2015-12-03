/*
  Interface to file I/O services, expressed using object notation
  (c) 2001-2006 F.G. McCabe
 
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

go.io{

  -- Interface declaration of file types

  ioEncoding ::= rawEncoding | 
		 utf16Encoding | utf16SwapEncoding | 
		 utf8Encoding | unknownEncoding.

  inChannel <~ { inCh:[]=>char.
	inBytes:[integer]=>list[integer].
	inB:[]=>integer.
	inChars:[integer]=>string.
	inLine:[string]=>string.
	inText:[string]=>string.
	pos:[]=>integer.
	seek:[integer]*.
	eof:[]{}.
	close:[]*.
	setEncoding:[ioEncoding]*}.

  outChannel <~ { outCh:[char]*.
	outB:[integer]*.
	outBytes:[list[integer]]*.
	outStr:[string]*.
	outLine:[string]*.
	flush:[]*.
	close:[]*.
	setEncoding:[ioEncoding]*
      }.

  private ioFileState ::= open | closed.
  
  private outFile:[opaque,string]@>outChannel.

  outFile(File,name)..{
    state:ioFileState := open.

    isOpen:[]{}.
    isOpen() :- state=open.
      
    outCh(Ch)::isOpen() ->
        __outch(File,Ch).
    outCh(_) ->
        raise error("outCh",'eNOPERM').
        
    outB(Ch)::isOpen() ->
        __outbyte(File,Ch).
    outB(_) ->
        raise error("outB",'eNOPERM').

    outBytes(L)::isOpen() ->
        outBts(L).
    outBytes(_) ->
        raise error("outBytes",'eNOPERM').

    outBts:[list[integer]]*.
    outBts([]) -> __flush(File).
    outBts([B,..L]) -> __outbyte(File,B);outBts(L).
        
    outStr(Str)::isOpen() ->
	__outtext(File,Str);
        __flush(File).
    outStr(_) ->
        raise error("outStr",'eNOPERM').

    flush()::isOpen() ->
        __flush(File).
    flush() ->
        raise error("flush",'eNOPERM').
        
    outLine(Str)::isOpen() ->
	sync{
	  __outtext(File,Str);
	  __outch(File,`\n);
	  __flush(File)
	}.
    outLine(_) ->
        raise error("outLine",'eNOPERM').

    close():: isOpen() ->
        state:=closed;
        __close(File).
    close() -> {}.

    setEncoding(E) -> __setfileencoding(File,mapEncoding(E)).

    show() => name.
  }.

  -- open an output file on local file system
  openOutFile:[string,ioEncoding]=>outChannel.
  openOutFile(fle,encoding) => outFile(__openOutFile(fle,mapEncoding(encoding)),fle).

  -- open a file on local file system in append mode
  openAppendFile:[string,ioEncoding]=>outChannel.
  openAppendFile(fle,encoding) => outFile(__openAppendFile(fle,mapEncoding(encoding)),fle).
  
  -- standard output channel
  stdout:outChannel = outFile(__stdfile(1),"stdout").

  -- standard input channel
  stdin:inChannel = inFile(__stdfile(0),"stdin").

  -- standard error channel
  stderr:outChannel = outFile(__stdfile(2),"stderr").
    
  inFile:[opaque,string]@>inChannel.
  inFile(File,name)..{
    state:ioFileState := open.
      
    isOpen:[]{}.
    isOpen() :- state=open.

    inCh()::isOpen() => __inchar(File).
    inCh() =>
      raise error("inCh",'eNOPERM').
        
    inBytes(C)::isOpen() => __inbytes(File,C).
    inBytes(_) =>
      raise error("inBytes",'eNOPERM').
        
    inB()::isOpen() => __inbyte(File).
    inB() =>
      raise error("inB",'eNOPERM').
        
    inChars(C)::isOpen() => __inchars(File,C).
    inChars(_) =>
      raise error("inChars",'eNOPERM').
        
    inLine(T)::isOpen() => __inline(File,T).
    inLine(_) =>
      raise error("inLine",'eNOPERM').
        
    inText(T)::isOpen() => __intext(File,T).
    inText(_) =>
      raise error("inText",'eNOPERM').
        
    pos() => __fposition(File).

    seek(L) -> __fseek(File,L).
        
    eof() :-
      __eof(File).
        
    close()::isOpen() ->
      state:=closed;
      __close(File).
    close() -> {}.

    setEncoding(E) -> __setfileencoding(File,mapEncoding(E)).
    show()=>name.
  }.

  -- This input channel is useful occasionally
  nullin:[]@=inChannel.
  nullin..{
    inCh() => raise error("inCh",'eEOF').
    inBytes(_) => raise error("inBytes",'eEOF').
    inB() => raise error("inB",'eEOF').
    inLine(_) => raise error("inLine",'eEOF').
    inChars(_) => raise error("inChars",'eEOF').
    inText(_) => raise error("inText",'eEOF').
    pos() => raise error("pos",'eINVAL').
    seek(_) -> {}.
    close() -> {}.
    eof().
    setEncoding(_) -> raise error("setEncoding",'eEOF').
    show()=>"/dev/nullin".
  }.

  nullout:[]@=outChannel.
  nullout..{
    outCh(_)->{}.                       -- nothing happens
    outB(_)->{}.
    outBytes(_)->{}.
    outStr(_)->{}.
    outLine(_)->{}.
    flush()->{}.
    close()->{}.
    setEncoding(_) -> {}.
    show()=>"/dev/nullout".
  }.
    
  -- open a file based on its URL
  openURL:[string,ioEncoding]=>(inChannel,string).
  openURL(fle,encoding) => (inFile(__openURL("",fle,Act,mapEncoding(encoding)),fle),Act).
  
  -- open a file on the local file system
  openInFile:[string,ioEncoding]=>inChannel.
  openInFile(file,encoding) => inFile(__openInFile(file,mapEncoding(encoding)),file).

  -- Acquire the contents of a file as a string
  getFile:[string,ioEncoding]=>string.
  getFile(file,encoding)=>
      valof{
	F = openInFile(file,encoding);
	T = F.inText("");		-- read in everything
	F.close();
	valis T
      }.
  
  -- set up a pipe to a subsidiary process
  pipeConnect:[string,list[string],list[(symbol,string)],ioEncoding]=>
      (outChannel, inChannel,inChannel).
  pipeConnect(Cmd,Args,Env,encoding) =>
      valof{
        __popen(Cmd,Args,Env,inF,outF,errF,mapEncoding(encoding));
        valis (outFile(inF,Cmd<>":stdin"),inFile(outF,Cmd<>":stdout"),
               inFile(errF,Cmd<>":stderr"))
      }.

  tcpConnect:[string,integer,inChannel-,outChannel-,ioEncoding]*.
  tcpConnect(host,port,inChnl,outChnl,encoding) ->
      __connect(host,port,mapEncoding(encoding),inF,outF);
      inChnl = inFile(inF,host<>":stdout");
      outChnl = outFile(outF,host<>":stdin").
           
  serverProc <~ { exec:[string,string,integer,inChannel,outChannel]*}.

  tcpServer:[integer,serverProc,ioEncoding]*.
  tcpServer(port,handler,encoding) -> 
      __listen(port,L);
      serverLoop(L,handler,encoding).

  private serverLoop:[opaque,serverProc,ioEncoding]*.
  serverLoop(L,handler,encoding) -> 
      ( __accept(L,inC,outC,rHost,rIP,rPort,mapEncoding(encoding));
        outP = outFile(outC,rHost<>":stdout");
        inP = inFile(inC,rHost<>":stdin");
        spawn { handler.exec(rHost,rIP,rPort,inP,outP); outP.close(); inP.close()}
      )
      onerror(
       error(_,Code) -> {__logmsg("problem with connection: "<>__errorcode(Code))}
      );
      serverLoop(L,handler,encoding).

  -- report on current working directory
  fcwd:[]=>string.
  fcwd() => __cwd().

  -- change current directory
  fcd:[string]*.
  fcd(D) -> __cd(D).

  -- list the contents of a directory
  fls:[string]=>list[string].
  fls(D) => {F .. (F,_) in __ls(D)}.

  ffileList:[string]=>list[(string,fileType,list[filePerm])].
  ffileList(D) => { (F,T,fmodes(F)) .. (F,T) in __ls(D)}.

  -- report on the type of a file
  fileType ::= fifoSpecial | directory | charSpecial | blockSpecial 
	     | plainFile | symlink | socket | unknownFileType.

  ftype:[string]=>fileType.
  ftype(F) => __file_type(F).

  -- report on the permission modes on a file
  filePerm ::= setUid | setGid | stIcky | rUsr | wUsr | xUsr |
	       rGrp | wGrp | xGrp | rOth | wOth | xOth.
  fmodes:[string]=>list[filePerm].
  fmodes(F) => fileMode(__file_mode(F)).

  private fileMode:[integer]=>list[filePerm].
  fileMode(0) => [].
  fileMode(X)::band(X,0x800)==0x800 => [setUid,..fileMode(band(X,0x7ff))].
  fileMode(X)::band(X,0x400)==0x400 => [setGid,..fileMode(band(X,0xbff))].
  fileMode(X)::band(X,0x200)==0x200 => [stIcky,..fileMode(band(X,0xdff))].
  fileMode(X)::band(X,0x100)==0x100 => [rUsr,..fileMode(band(X,0xeff))].
  fileMode(X)::band(X,0x80)==0x80 => [wUsr,..fileMode(band(X,0xf7f))].
  fileMode(X)::band(X,0x40)==0x40 => [xUsr,..fileMode(band(X,0xfbf))].
  fileMode(X)::band(X,0x20)==0x20 => [rGrp,..fileMode(band(X,0xfdf))].
  fileMode(X)::band(X,0x10)==0x10 => [wGrp,..fileMode(band(X,0xfef))].
  fileMode(X)::band(X,0x8)==0x8 => [xGrp,..fileMode(band(X,0xff7))].
  fileMode(X)::band(X,0x4)==0x4 => [rOth,..fileMode(band(X,0xffb))].
  fileMode(X)::band(X,0x2)==0x2 => [wOth,..fileMode(band(X,0xffd))].
  fileMode(X)::band(X,0x1)==0x1 => [xOth,..fileMode(band(X,0xffe))].

  -- change the permissions on a file
  chmode:[string,list[filePerm]]*.
  chmode(F,P) -> __chmod(F,encodeModes(P,0)).

  private encodeModes:[list[filePerm],integer]=>integer.
  encodeModes([],P)=>P.
  encodeModes([setUid,..L],P) => encodeModes(L,bor(P,0x800)).
  encodeModes([setGid,..L],P) => encodeModes(L,bor(P,0x400)).
  encodeModes([stIcky,..L],P) => encodeModes(L,bor(P,0x200)).
  encodeModes([rUsr,..L],P) => encodeModes(L,bor(P,0x100)).
  encodeModes([wUsr,..L],P) => encodeModes(L,bor(P,0x80)).
  encodeModes([xUsr,..L],P) => encodeModes(L,bor(P,0x40)).
  encodeModes([rGrp,..L],P) => encodeModes(L,bor(P,0x20)).
  encodeModes([wGrp,..L],P) => encodeModes(L,bor(P,0x10)).
  encodeModes([xGrp,..L],P) => encodeModes(L,bor(P,0x8)).
  encodeModes([rOth,..L],P) => encodeModes(L,bor(P,0x4)).
  encodeModes([wOth,..L],P) => encodeModes(L,bor(P,0x2)).
  encodeModes([xOth,..L],P) => encodeModes(L,bor(P,0x1)).

  -- delete a file
  frm:[string]*.
  frm(F) -> __rm(F).

  -- rename a file
  fmv:[string,string]*.
  fmv(O,N) -> __mv(O,N).

  -- Test for the presence of a file
  ffile:[string]{}.
  ffile(F) :-
      __file_present(F).

  -- report on the size of a file
  fsize:[string]=>integer.
  fsize(F) => __file_size(F).

  private mapEncoding:[ioEncoding]=>integer.
  mapEncoding(rawEncoding)=>0.
  mapEncoding(utf16Encoding)=>1.
  mapEncoding(utf16SwapEncoding)=>2.
  mapEncoding(utf8Encoding)=>3.
  mapEncoding(unknownEncoding)=>4.
}.
