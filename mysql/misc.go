mysql.misc{
  /*
   * General library functions for the MySQL connection
   */

  import go.io.
  import go.datelib.
  import go.stdparse.

  import mysql.packet.

  sendPkt:[outChannel,list[integer],integer]*.
  sendPkt(O,Msg,Code) :: encodeLong(listlen(Msg))=(H,M,L) ->
      O.outBytes([H,M,L,Code,..Msg]);
      O.flush().

  private encodeLong:[integer]=>(integer,integer,integer).
  encodeLong(N) => padOut(enc(N)).
  
  private enc:[integer]=>list[integer].
  enc(N)::N<256 => [N].
  enc(N) => [imod(N,256),..enc(idiv(N,256,256))].

  private padOut:[list[integer]]=>(integer,integer,integer).
  padOut([H,M,L])=>(H,M,L).
  padOut([H,M]) => (H,M,0).
  padOut([H]) => (H,0,0).
  padOut([]) => (0,0,0).

  readPkt:[inChannel]=>(integer,list[integer]).
  readPkt(In)=>
      valof{
	[H,M,L,No]=In.inBytes(4);
	Len = H+M*256+L*65536;
	valis(No,In.inBytes(Len))
      }.

  readEncodedBinary:[inChannel]=>integer.
  readEncodedBinary(In)=>
      valof{
	B0 = In.inB();
	( B0>=0, B0<=250?
		valis B0
	| B0==252 ?
	    B1 = In.inB();
	    B2 = In.inB();
	    valis bor(bleft(B2,8),B1)
	| B0==253 ?
	    B1 = In.inB();
	    B2 = In.inB();
	    B3 = In.inB();
	    B4 = In.inB();
	    valis bor(bleft(B4,24),bor(bleft(B3,16),bor(bleft(B2,8),B1)))
	 | B0==254 ?
	    valis collectInt(In.inBytes(8),0,0)
	)
      }.

  parseLengthEncodedString:[string]-->list[integer].
  parseLengthEncodedString(S) -->
      parseLengthEncodedBinary(L),
      pullBytes(L,M),
      S = __utf82str(M).
  parseLengthEncodedString("") --> eof.
  
  parseLengthEncodedBinary:[integer-] --> list[integer].
  parseLengthEncodedBinary(B0) -->
      [B0], { B0>=0, B0=<250 }.
  parseLengthEncodedBinary(I) -->
      [252], [B1,B2], I = bor(bleft(B2,8),B1).
  parseLengthEncodedBinary(I) -->
      [253], [B1,B2,B3,B4], 
      I = bor(bleft(B4,24),bor(bleft(B3,16),bor(bleft(B2,8),B1))).
  parseLengthEncodedBinary(I) -->  
      [254], pullBytes(8,Bs),
      I = collectInt(Bs,0,0).

  longInt:[integer-]-->list[integer].
  longInt(Ot) --> [B1,B2,B3,B4], 
      {Ot = bor(bleft(B4,24),bor(bleft(B3,16),bor(bleft(B2,8),B1)))}.

  shortInt:[integer-]-->list[integer].
  shortInt(Ot) --> [B1,B2], 
      {Ot = bor(bleft(B2,8),B1)}.

  pullBytes:[integer,list[integer]]-->list[integer].
  pullBytes(0,[])-->[].
  pullBytes(Ct,[C,..S]) --> [C], pullBytes(Ct-1,S).

  pullRest:[list[integer]-]-->list[integer].
  pullRest([]) --> eof.
  pullRest([C,..L]) --> [C], pullRest(L).

  nullTermString:[string-]-->list[integer].
  nullTermString("") --> [0].
  nullTermString([Cx,..S]) --> [C], { Cx=__charOf(C)}, nullTermString(S).

  encodeInteger:[integer]=>list[integer].
  encodeInteger(N)::N<250 => [N].
  encodeInteger(N)::N<65536 => [252,imod(N,256),bright(N,8)].
  encodeInteger(N)::N<2147483648 =>
      valof{
	N1 = bright(N,8);
	N2 = bright(N1,8);
	N3 = bright(N2,8);
	valis [253,imod(N,256),imod(N1,256),imod(N2,256),imod(N3,256)]
      }.
  encodeInteger(N)=>
      valof{
	N1 = bright(N,8);
	N2 = bright(N1,8);
	N3 = bright(N2,8);
	N4 = bright(N3,8);
	N5 = bright(N4,8);
	N6 = bright(N5,8);
	N7 = bright(N6,8);
	valis [254,imod(N,256),imod(N1,256),imod(N2,256),imod(N3,256),
	       imod(N4,256),imod(N5,256),imod(N6,256),imod(N7,256)]
      }.

  collectInt:[list[integer],integer,integer]=>integer.
  collectInt([],I,_)=>I.
  collectInt([C,..L],I,S) => collectInt(L,bor(I,bleft(C,S)),S+8).

  lengthEncoded:[list[integer]]=>list[integer].
  lengthEncoded(L) => encodeInteger(listlen(L))<>L.

  createPacket:[list[integer],integer]=>list[integer].
  createPacket(data,num)=>
      valof{
	Len = listlen(data);
	N1 = bright(Len,8);
	N2 = bright(N1,8);
	valis [imod(Len,256),imod(N1,256),imod(N2,256),num,..data]
      }.

  encInt:[integer]=>list[integer].
  encInt(N) =>
      valof{
	N1 = bright(N,8);
	N2 = bright(N1,8);
	N3 = bright(N2,8);
	valis [imod(N,256),imod(N1,256),imod(N2,256),imod(N3,256)]
      }.

  decodeResponsePacket:[mySqlPacket-]-->list[integer].
  decodeResponsePacket(Pkt) --> [255],	-- An error response
      pullBytes(2,[c1,c2]),
      [35],
      pullBytes(5,_),			-- ignore the sql state
      pullRest(M),
      Pkt = errorResponsePkt(c1+256*c2,__utf82str(M)).
  decodeResponsePacket(Pkt) --> [255],	-- An error response
      pullBytes(2,[c1,c2]),
      pullRest(M),
      Pkt = errorResponsePkt(c1+256*c2,__utf82str(M)).
  decodeResponsePacket(Pkt) --> [0],
      parseLengthEncodedBinary(rowCount),
      parseLengthEncodedBinary(insertId),
      shortInt(status),
      shortInt(warningCount),
      parseLengthEncodedString(M),
      Pkt = okResponsePkt(rowCount,insertId,status,warningCount,M).
  decodeResponsePacket(Pkt) --> 
      parseLengthEncodedBinary(fieldCount),
      ( parseLengthEncodedBinary(extra) | extra=0),
      Pkt = resultHeaderPkt(fieldCount,extra).

  parseTimeStamp:[date]-->string.
  parseTimeStamp(D) -->
      [y1,y2,y3,y4],"-",[m1,m2],"-",[d1,d2]," ",
      [h1,h2],":",[n1,n2],":",[s1,s2],(".",naturalOf(u) | u=0),
      year = naturalOf%%[y1,y2,y3,y4],
      month = naturalOf%%[m1,m2],
      day = naturalOf%%[d1,d2],
      hour = naturalOf%%[h1,h2],
      mins = naturalOf%%[n1,n2],
      secs = naturalOf%%[s1,s2],
      D = dateOf(day,month,year,hour,mins,secs,-8).

  scramblePassword:[string,list[integer]]=>list[integer].
  scramblePassword(Pass,Salt) =>
      valof{
	U1 = __str2utf8(Pass);
	S1 = __sha1(U1);
	S2 = __sha1(S1);
	S3 = __sha1(Salt<>S2);
	valis xor(S3,S1)
      }.

  xor:[list[integer],list[integer]]=>list[integer].
  xor([],[])=>[].
  xor([I1,..L1],[I2,..L2]) => [bxor(I1,I2),..xor(L1,L2)].
 }