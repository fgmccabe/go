mysql.packet{
  import go.io.
  import go.datelib.
  import mysql.sqltypes.

  -- Definition of the standard packet types

  mySqlProtocol ::= version(integer).

  mySqlPacket <~ { encode:[]=>list[integer]. }.

  
  handshakeInitPkt:[integer,string,integer,list[integer],
		    list[clientCapability],
		    string,
		    integer]@=mySqlPacket.
  handshakeInitPkt(protocolVersion,serverVersion,tId,scramble,
		   capabilities,language,status)..{
    encode()=> raise error("not supported",'eNOPERM').
  }.

  errorResponsePkt:[integer,string] @= mySqlPacket.
  errorResponsePkt(Code,Message)..{
    encode()=> raise error("not supported",'eNOPERM').
  }.

  okResponsePkt:[integer,integer,integer,integer,string]@=mySqlPacket.
  okResponsePkt(rowCount,insertId,serverState,warnCount,Msg) .. {
    encode()=> raise error("not supported",'eNOPERM').
  }.

  resultHeaderPkt:[integer,integer]@=mySqlPacket.
  resultHeaderPkt(fieldCount,extra)..{
    encode()=> raise error("not supported",'eNOPERM').
  }.

  fieldPkt:[string,string,string,string,string,integer,integer,integer,integer]@=mySqlPacket.
  fieldPkt(db,Table,AsName,Name,Len,Type,Flags,Decimals,Default)..{
    encode()=> raise error("not supported",'eNOPERM').
  }.

  clientCapability ::= clientLongPassword 
		     | clientFoundRows
		     | clientLongFlag
		     | clientConnectDb
		     | clientNoSchema
		     | clientCompress
		     | clientODBC
		     | clientLocalFiles
		     | clientIgnoreSpace
		     | clientProtocol41
		     | clientInteractive
		     | clientSSL
		     | clientIgnoreSIGPIPE
		     | clientTransactions
		     | clientReserved
		     | clientSecureConnection
		     | clientMultiStatements
		     | clientMultiResults.

  extractClientOptions:[integer]=>list[clientCapability].
  extractClientOptions(0) => [].
  extractClientOptions(Opts)::band(Opts,1)==1 =>
      [clientLongPassword,..extractClientOptions(band(Opts,0xfffffe))].
  extractClientOptions(Opts)::band(Opts,2)==2 =>
      [clientFoundRows,..extractClientOptions(band(Opts,0xfffffd))].
  extractClientOptions(Opts)::band(Opts,4)==4 =>
      [clientLongFlag,..extractClientOptions(band(Opts,0xfffffb))].
  extractClientOptions(Opts)::band(Opts,8)==8 =>
      [clientConnectDb,..extractClientOptions(band(Opts,0xfffff7))].
  extractClientOptions(Opts)::band(Opts,0x10)==0x10 =>
      [clientNoSchema,..extractClientOptions(band(Opts,0xffffef))].
  extractClientOptions(Opts)::band(Opts,0x20)==0x20 =>
      [clientCompress,..extractClientOptions(band(Opts,0xffffdf))].
  extractClientOptions(Opts)::band(Opts,0x40)==0x40 =>
      [clientODBC,..extractClientOptions(band(Opts,0xffffbf))].
  extractClientOptions(Opts)::band(Opts,0x80)==0x80 =>
      [clientLocalFiles,..extractClientOptions(band(Opts,0xffff7f))].
  extractClientOptions(Opts)::band(Opts,0x100)==0x100 =>
      [clientIgnoreSpace,..extractClientOptions(band(Opts,0xfffeff))].
  extractClientOptions(Opts)::band(Opts,0x200)==0x200 =>
      [clientProtocol41,..extractClientOptions(band(Opts,0xfffdff))].
  extractClientOptions(Opts)::band(Opts,0x400)==0x400 =>
      [clientInteractive,..extractClientOptions(band(Opts,0xfffbff))].
  extractClientOptions(Opts)::band(Opts,0x800)==0x800 =>
      [clientSSL,..extractClientOptions(band(Opts,0xfff7ff))].
  extractClientOptions(Opts)::band(Opts,0x1000)==0x1000 =>
      [clientIgnoreSIGPIPE,..extractClientOptions(band(Opts,0xffefff))].
  extractClientOptions(Opts)::band(Opts,0x2000)==0x2000 =>
      [clientTransactions,..extractClientOptions(band(Opts,0xffdfff))].
  extractClientOptions(Opts)::band(Opts,0x4000)==0x4000 =>
      [clientReserved,..extractClientOptions(band(Opts,0xffbfff))].
  extractClientOptions(Opts)::band(Opts,0x8000)==0x8000 =>
      [clientSecureConnection,..extractClientOptions(band(Opts,0xff7fff))].
  extractClientOptions(Opts)::band(Opts,0x10000)==0x10000 =>
      [clientMultiStatements,..extractClientOptions(band(Opts,0xfeffff))].
  extractClientOptions(Opts)::band(Opts,0x20000)==0x20000 =>
      [clientMultiResults,..extractClientOptions(band(Opts,0xfdffff))].

  encodeClientOptions:[list[clientCapability]]=>integer.
  encodeClientOptions([]) => 0.
  encodeClientOptions([clientLongPassword,..Opts]) =>
      bor(encodeClientOptions(Opts),1).
  encodeClientOptions([clientFoundRows,..Opts]) =>
      bor(encodeClientOptions(Opts),2).
  encodeClientOptions([clientLongFlag,..Opts]) =>
      bor(encodeClientOptions(Opts),4).
  encodeClientOptions([clientConnectDb,..Opts]) =>
      bor(encodeClientOptions(Opts),8).
  encodeClientOptions([clientNoSchema,..Opts]) =>
      bor(encodeClientOptions(Opts),0x10).
  encodeClientOptions([clientCompress,..Opts]) =>
      bor(encodeClientOptions(Opts),0x20).
  encodeClientOptions([clientODBC,..Opts]) =>
      bor(encodeClientOptions(Opts),0x40).
  encodeClientOptions([clientLocalFiles,..Opts]) =>
      bor(encodeClientOptions(Opts),0x80).
  encodeClientOptions([clientIgnoreSpace,..Opts]) =>
      bor(encodeClientOptions(Opts),0x100).
  encodeClientOptions([clientProtocol41,..Opts]) =>
      bor(encodeClientOptions(Opts),0x200).
  encodeClientOptions([clientInteractive,..Opts]) =>
      bor(encodeClientOptions(Opts),0x400).
  encodeClientOptions([clientSSL,..Opts]) =>
      bor(encodeClientOptions(Opts),0x800).
  encodeClientOptions([clientIgnoreSIGPIPE,..Opts]) =>
      bor(encodeClientOptions(Opts),0x1000).
  encodeClientOptions([clientTransactions,..Opts]) =>
      bor(encodeClientOptions(Opts),0x2000).
  encodeClientOptions([clientReserved,..Opts]) =>
      bor(encodeClientOptions(Opts),0x4000).
  encodeClientOptions([clientSecureConnection,..Opts]) =>
      bor(encodeClientOptions(Opts),0x8000).
  encodeClientOptions([clientMultiStatements,..Opts]) =>
      bor(encodeClientOptions(Opts),0x10000).
  encodeClientOptions([clientMultiResults,..Opts]) =>
      bor(encodeClientOptions(Opts),0x20000).

  extractFieldFlags:[integer]=>list[fieldFlag].
  extractFieldFlags(0) => [].
  extractFieldFlags(Opts)::band(Opts,1)==1 =>
      [notNullF,..extractFieldFlags(band(Opts,0xfffffe))].
  extractFieldFlags(Opts)::band(Opts,2)==2 =>
      [primaryKeyF,..extractFieldFlags(band(Opts,0xfffffd))].
  extractFieldFlags(Opts)::band(Opts,4)==4 =>
      [uniqueKeyF,..extractFieldFlags(band(Opts,0xfffffb))].
  extractFieldFlags(Opts)::band(Opts,8)==8 =>
      [multipleKeyF,..extractFieldFlags(band(Opts,0xfffff7))].
  extractFieldFlags(Opts)::band(Opts,0x10)==0x10 =>
      [blobF,..extractFieldFlags(band(Opts,0xffffef))].
  extractFieldFlags(Opts)::band(Opts,0x20)==0x20 =>
      [unsignedF,..extractFieldFlags(band(Opts,0xffffdf))].
  extractFieldFlags(Opts)::band(Opts,0x40)==0x40 =>
      [zeroFillF,..extractFieldFlags(band(Opts,0xffffbf))].
  extractFieldFlags(Opts)::band(Opts,0x80)==0x80 =>
      [binaryF,..extractFieldFlags(band(Opts,0xffff7f))].
  extractFieldFlags(Opts)::band(Opts,0x100)==0x100 =>
      [enumF,..extractFieldFlags(band(Opts,0xfffeff))].
  extractFieldFlags(Opts)::band(Opts,0x200)==0x200 =>
      [autoIncrementF,..extractFieldFlags(band(Opts,0xfffdff))].
  extractFieldFlags(Opts)::band(Opts,0x400)==0x400 =>
      [timestampF,..extractFieldFlags(band(Opts,0xfffbff))].
  extractFieldFlags(Opts)::band(Opts,0x800)==0x800 =>
      [setF,..extractFieldFlags(band(Opts,0xfff7ff))].
  extractFieldFlags(Opts)::band(Opts,0x1000)==0x1000 =>
      [noDefaultF,..extractFieldFlags(band(Opts,0xfffefff))].
  extractFieldFlags(Opts)::band(Opts,0x2000)==0x2000 =>
      extractFieldFlags(band(Opts,0xfffdfff)).
  extractFieldFlags(Opts)::band(Opts,0x4000)==0x4000 =>
      extractFieldFlags(band(Opts,0xffbfff)).
  extractFieldFlags(Opts)::band(Opts,0x8000)==0x8000 =>
      [numericF,..extractFieldFlags(band(Opts,0xff7fff))].
}
		  