session{
  import go.io.
  import mysql.packet.
  import mysql.misc.
  import go.setlib.
  import go.stdparse.
  import go.datelib.
  import mysql.sqltypes.

  mysqlConn <~ {
	query:[string]=>(list[fieldDescription],list[list[columnData]]).
	dbNames:[]=>list[string].
	db:[T]-([string,dbConvert[T]]@>mysqlDb[T]).
      }.

  mysql:[string,integer,string,string,string] @> mysqlConn.
  mysql(Host,Port,User,Pass,Schema) .. {
    inCh:inChannel := nullin.
    outCh:outChannel := nullout.

    connect:[]*.
    connect() ->
	(i,o) = connectToDB(Host,Port,User,Pass);
	inCh := i;
	outCh := o;
	selectSchema(i,o,Schema).

    ${
      connect()
    }.

    dbNames() =>
	valof{
	  sendQuery(inCh,outCh,"show tables",_,res);
	  valis { T .. [stringC(T)] in res}
	}.

    query(Q) =>
	valof{
	  sendQuery(inCh,outCh,Q,F,R);
	  valis (F,R)
	}.

    db(qury,Conv)..{
      mem(Tpl) :-
	  action{
	    sendQuery(inCh,outCh,qury,Flds,Data);
--	    stdout.outLine(Data.show());
	    {verifyFields(Conv,Flds)};
--	    stdout.outLine(Flds.show());
	    Tpls = extractData(Conv,Data)
	  },
	  Tpl in Tpls.
    }.
  }.

  
  private connectToDB:[string,integer,string,string]=>(inChannel,outChannel).
  connectToDB(host,port,user,pass) =>
      valof{
	tcpConnect(host,port,inCh,outCh,rawEncoding);
	readInitialPacket(inCh,_Ptcol,_Version,_tId,Salt,svrOpts);
	clientAuth(outCh,inCh,user,pass,Salt,diff(svrOpts,[clientSSL,clientCompress]));
	valis (inCh,outCh)
      }.

  private readInitialPacket:[inChannel,integer-,string-,integer-,
		     list[integer]-,list[clientCapability]-]*.
  readInitialPacket(inCh,Ptcol,Version,tId,Salt,svrOpts)->
      (_PktNo,[Ptcol,..bytes]) = readPkt(inCh);
      {  nullTermString(Version),
	longInt(tId),
	pullBytes(8,Salt1),
	[_,S0,S1,_ChSet,_St0,_St1],
	pullBytes(13,_),
	pullBytes(12,Salt2)
	--> bytes ~ _ };
      Salt = Salt1<>Salt2;
      svrOpts = extractClientOptions(S1*256+S0).

  clientAuth:[outChannel,inChannel,string,string,list[integer],
	      list[clientOption]]*.
  clientAuth(out,inCh,user,pass,Salt,Options) ->
      Scrambled = scramblePassword(pass,Salt);
      Pkt = encInt(encodeClientOptions(Options))<>encInt(1073741824)<>[8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]<>
      __str2utf8(user)<>[0]<>
      lengthEncoded(Scrambled)<>[0,0];
--      stdout.outLine("Auth packet = "<>Pkt.show());
      out.outBytes(createPacket(Pkt,1));
      waitForResponse(inCh,Resp);
      case Resp in ( 
       errorResponsePkt(_,Msg) ->
	   raise error("Error connecting to DB: "<>Msg,'eMYSQL')
     | okResponsePkt(_,_,_,_,[]) -> {}
     | okResponsePkt(_,_,_,_,Msg) ->
	   stdout.outLine("Warning: "<>Msg)
     | _ -> raise error("Unexpected response: "<>Resp.show(),'eINVAL')
      ).

  waitForResponse:[inChannel,mySqlPacket-]*.
  waitForResponse(inCh,Pkt) ->
      (_PktNo,Resp) = readPkt(inCh);
      { decodeResponsePacket(Pkt) --> Resp ~ _ }.

  sendQuery:[inChannel,outChannel,string,list[fieldDescription],list[list[columnData]]-]*.
  sendQuery(inCh,out,Cmd,fields,rowData) ->
      out.outBytes(createPacket([3,..__str2utf8(Cmd)],0));
      waitForResponse(inCh,Resp);
      case Resp in (
       errorResponsePkt(_,Msg) ->
	   raise error("Error from command: "<>Cmd<>":"<>Msg,'eMYSQL')
     | resultHeaderPkt(fieldCount,_extra) ->
	   readFieldDescriptions(fieldCount,inCh,out,fields);
	   readEndMarker(inCh);
	   (_,Pkt) = readPkt(inCh);
	   readRowData(inCh,out,fields,Pkt,rowData)
     | okResponsePkt(_,_,_,_,[]) -> fields = []
     | okResponsePkt(_,_,_,_,Msg) ->
	   stdout.outLine("Warning: "<>Msg);
	   fields = []
      ).

  selectSchema:[inChannel,outChannel,string]*.
  selectSchema(inCh,out,DB) ->
      out.outBytes(createPacket([2,..__str2utf8(DB)],0));
      waitForResponse(inCh,Resp);
      case Resp in (
       errorResponsePkt(_,Msg) ->
	   raise error("Error in trying to select: "<>DB<>":"<>Msg,'eMYSQL')
     | okResponsePkt(_,_,_,_,[]) -> {}
     | okResponsePkt(_,_,_,_,Msg) ->
	   stdout.outLine("Warning: "<>Msg)
      ).

  readFieldDescriptions:[integer,inChannel,outChannel,list[fieldDescription]-]*.
  readFieldDescriptions(0,_,_,[])-> {}.
  readFieldDescriptions(N,i,o,[field(Schema,Table,OrigTable,Name,OrigName,Tp,Len,Flags,Dec,Deflt),..ields]) ->
      (_,Desc) = readPkt(i);
      { parseFieldDescription(Schema,Table,OrigTable,Name,OrigName,Tp,Len,Flags,Dec,Deflt) --> Desc };
      readFieldDescriptions(N-1,i,o,ields).

  readEndMarker:[inChannel]*.
  readEndMarker(i) ->
      (_,Desc) = readPkt(i);
      Desc = [0xfe,.._More].

  parseEndMarker:[integer,integer]-->list[integer].
  parseEndMarker(W,S) -->
      [0xfe], shortInt(W), shortInt(S).
  parseEndMarker(0,0) -->
      [0xfe].

  readRowData:[inChannel,outChannel,list[fieldDescription],list[integer],list[list[columnData]]-]*.
  readRowData(i,o,fields,Pkt,rows) ->
      ( parseEndMarker(_Warnings,_Status)-->Pkt ? rows = [] 
      | parseRowData(fields,row)-->Pkt ? rows = [row,..more] ;
	      (_,nxtPkt) = readPkt(i);
	      readRowData(i,o,fields,nxtPkt,more)
      | stdout.outLine("Could not parse packet "<>Pkt.show())).

  parseRowData:[list[fieldDescription],list[columnData]-]-->list[integer].
  parseRowData([],[]) --> [].
  parseRowData([F,..ields],[C,..olumns]) -->
--      action{ stdout.outLine("Parsing column with desc "<>F.show()) },
      parseColumn(F,C),
      parseRowData(ields,olumns).

  parseColumn:[fieldDescription,columnData-]-->list[integer].
  parseColumn(F,D) -->
      ( parseLengthEncodedBinary(L),
	pullBytes(L,S),
	D = extractColumnData(F,S)
      | [251], D = nullC).

  extractColumnData:[fieldDescription,list[integer]]=>columnData.
  extractColumnData(field(_,_,_,_,_,stringTp,_,_,_,_),D) =>
      stringC(__utf82str(D)).
  extractColumnData(field(_,_,_,_,_,varStringTp,_,_,_,_),D) => 
      stringC(__utf82str(D)).
  extractColumnData(field(_,_,_,_,_,blobTp,_,_,_,_),D) => 
      stringC(__utf82str(D)).
  extractColumnData(field(_,_,_,_,_,tinyTp,_,_,_,_),D) => 
      intC(integerOf%%__utf82str(D)).
  extractColumnData(field(_,_,_,_,_,shortTp,_,_,_,_),D) => 
      intC(integerOf%%__utf82str(D)).
  extractColumnData(field(_,_,_,_,_,longTp,_,_,_,_),D) => 
      intC(integerOf%%__utf82str(D)).
  extractColumnData(field(_,_,_,_,_,longLongTp,_,_,_,_),D) => 
      intC(integerOf%%__utf82str(D)).
  extractColumnData(field(_,_,_,_,_,floatTp,_,_,_,_),D) => 
      floatC(floatOf%%__utf82str(D)).
  extractColumnData(field(_,_,_,_,_,doubleTp,_,_,_,_),D) => 
      floatC(floatOf%%__utf82str(D)).
  extractColumnData(field(_,_,_,_,_,newDecimalTp,_,_,_,_),D) => 
      floatC(floatOf%%__utf82str(D)).
  extractColumnData(field(_,_,_,_,_,bitTp,_,_,_,_),[0]) => 
      logicalC(false).
  extractColumnData(field(_,_,_,_,_,bitTp,_,_,_,_),[1]) => 
      logicalC(true).
  extractColumnData(field(_,_,_,_,_,dateTimeTp,_,_,_,_),D) =>
      dateC(parseTimeStamp%%__utf82str(D)).
  extractColumnData(field(_,_,_,_,_,_,_,_,_,_),D) => binaryC(D).

  parseFieldDescription:[string,string,string,string,string,fieldType,integer,list[fieldFlag],integer,integer] --> list[integer].
  parseFieldDescription(Schema,Table,OriginalTable,Name,OriginalName,Tp,Len,Flags,Dec,Deflt) -->
      parseLengthEncodedString(_),
      parseLengthEncodedString(Schema),
      parseLengthEncodedString(Table),
      parseLengthEncodedString(OriginalTable),
      parseLengthEncodedString(Name),
      parseLengthEncodedString(OriginalName),
      pullBytes(3,_),
      longInt(Len),
      parseFieldType(Tp),
      shortInt(Flgs),
      Flags = extractFieldFlags(Flgs),
      [Dec],
      [_,_],
      ( parseLengthEncodedBinary(Deflt) | Deflt=-1).

  parseFieldType:[fieldType-]-->list[integer].
  parseFieldType(decimalTp) --> [0].
  parseFieldType(tinyTp) --> [1].
  parseFieldType(shortTp) --> [2].
  parseFieldType(longTp) --> [3].
  parseFieldType(floatTp) --> [4].
  parseFieldType(doubleTp) --> [5].
  parseFieldType(nullTp) --> [6].
  parseFieldType(timestampTp) --> [7].
  parseFieldType(longLongTp) --> [8].
  parseFieldType(int24Tp) --> [9].
  parseFieldType(dateTp) --> [0xa].
  parseFieldType(timeTp) --> [0xb].
  parseFieldType(dateTimeTp) --> [0xc].
  parseFieldType(yearTp) --> [0xd].
  parseFieldType(newdateTp) --> [0xe].
  parseFieldType(varcharTp) --> [0xf].
  parseFieldType(bitTp) --> [0x10].
  parseFieldType(newDecimalTp) --> [0xf6].
  parseFieldType(enumTp) --> [0xf7].
  parseFieldType(setTp) --> [0xf8].
  parseFieldType(tinyBlobTp) --> [0xf9].
  parseFieldType(mediumBlobTp) --> [0xfa].
  parseFieldType(longBlobTp) --> [0xfb].
  parseFieldType(blobTp) --> [0xfc].
  parseFieldType(varStringTp) --> [0xfd].
  parseFieldType(stringTp) --> [0xfe].
  parseFieldType(geometryTp) --> [0xff].

  showAllTables:[inChannel,outChannel]*.
  showAllTables(i,o) ->
      sendQuery(i,o,"show tables",_,Rows);
      ( [stringC(T)] in Rows *>
	showTable(i,o,T)).

  showTable:[inChannel,outChannel,string]*.
  showTable(i,o,T) ->
      sendQuery(i,o,"select * from "<>T,fields,Rows);
      showRowData(fields,Rows).

  showRowData:[list[fieldDescription],list[list[columnData]]]*.
  showRowData(F,Rows) ->
      stdout.outLine(F.show());
      ( R in Rows *>
	showRow(F,R)).

  showTables:[inChannel,outChannel]*.
  showTables(i,o) ->
      sendQuery(i,o,"show tables",fields,Rows);
      showRowData(fields,Rows).

  private showRow:[list[fieldDescription],list[columnData]]*.
  showRow(_,Row) ->
      stdout.outStr(" | ");
      ( C in Row *>
	showCol(C); stdout.outStr(" | "));
      stdout.outLine("").

  private showCol:[columnData]*.
  showCol(stringC(S)) -> stdout.outStr(S).
  showCol(binaryC(S)) -> stdout.outStr(S.show()).
  showCol(intC(I)) -> stdout.outStr(I.show()).
  showCol(floatC(N)) -> stdout.outStr(N.show()).
  showCol(charC(N)) -> stdout.outStr(N.show()).
  showCol(logicalC(N)) -> stdout.outStr(N.show()).
  showCol(binaryC(N)) -> stdout.outStr(N.show()).
  showCol(nullC) -> stdout.outStr("(null)").
  showCol(dateC(D)) -> stdout.outStr(D.show()).

  private constructQuery:[dbConvert[T],list[string],string,mysqlConn] => 
      mysqlDb[dbConvert[T]].
  constructQuery(Conv,FieldNames,Table,D) =>
      D.db("select "<>collapse(FieldNames,", ")<>" from "<>Table,Conv).

  main([]) ->
      dbConn = mysql("localhost",3306,"fgm","fgm2Data","genietown");
      R = constructQuery(dbT(dbInt,dbInt),["reference_id","service_id"],
			 "tbl_reference_service",dbConn);
      ( R.mem(X) *> stdout.outLine(X.show())).

  main([U,P]) ->
      dbConn = mysql("localhost",3306,U,P,"genietown");
      (F,R) = dbConn.query("show tables");
      showRowData(F,R).

}