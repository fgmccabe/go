mysql.sqltypes{
  import go.datelib.

  -- A converter shell that must be passed in to the mysql package

  dbConvert[T] <~ { 
	extract:[T] --> list[columnData].
	validFieldType:[list[fieldType],list[fieldType]]{}. 
      }.

  mysqlDb[T] <~ { mem:[T]{}. }.

  dbInt:[] @= dbConvert[integer].
  dbInt..{
    extract(I) --> [intC(I)].

    validFieldType([decimalTp,..L],L).
    validFieldType([tinyTp,..L],L).
    validFieldType([shortTp,..L],L).
    validFieldType([longTp,..L],L).
    validFieldType([int24Tp,..L],L).
    validFieldType([longLongTp,..L],L).
    validFieldType([yearTp,..L],L).
    validFieldType([newDecimalTp,..L],L).
  }.

  dbFlt:[] @= dbConvert[float].
  dbFlt..{
    extract(F) --> [floatC(F)].

    validFieldType([floatTp,..L],L).
    validFieldType([doubleTp,..L],L).
  }.

  dbLg:[] @= dbConvert[logical].
  dbLg..{
    extract(I) --> [logicalC(I)].

    validFieldType([tinyTp,..L],L).
  }.

  dbStr:[]@=dbConvert[string].
  dbStr..{
    extract(S) --> [stringC(S)].

    validFieldType([blobTp,..L],L).
    validFieldType([varcharTp,..L],L).
    validFieldType([tinyBlobTp,..L],L).
    validFieldType([mediumBlobTp,..L],L).
    validFieldType([longBlobTp,..L],L).
    validFieldType([varStringTp,..L],L).
    validFieldType([stringTp,..L],L).
  }.

  dbDte:[]@=dbConvert[date].
  dbDte..{
    extract(D) --> [dateC(D)].

    validFieldType([timeStampTp,..L],L).
    validFieldType([dateTp,..L],L).
    validFieldType([timeTp,..L],L).
    validFieldType([dateTimeTp,..L],L).
    validFieldType([yearTp,..L],L).
    validFieldType([newdateTp,..L],L).
  }.

  dbT:[dbConvert[T1],dbConvert[T2]] @= dbConvert[(T1,T2)].
  dbT(p1,p2)..{
    extract((e1,e2)) --> p1.extract(e1), p2.extract(e2).
    
    validFieldType(L1,L2) :- p1.validFieldType(L1,L1a), p2.validFieldType(L1a,L2).
  }.

  extractData:[dbConvert[T],list[list[columnData]]] => list[T].
  extractData(_,[])=>[].
  extractData(C,[Row,..Rest])::(C.extract(Dta)-->Row) => [Dta,..extractData(C,Rest)].

  verifyFields:[dbConvert[T],list[fieldDescription]]{}.
  verifyFields(Conv,Fields) :- 
      Conv.validFieldType({ Tp .. field(_,_,_,_,_,Tp,_,_,_,_) in Fields},[]).

  fieldDescription <~ thing.

  field:[string,string,string,string,string,fieldType,
	 integer,list[fieldFlag],integer,integer]@=fieldDescription.
  field(Schema,Table,OrigTable,Name,OrigName,Tp,Len,Flags,Dec,Deflt)..{
    show() => showName(Table,OrigTable)<>"."<>showName(Name,OrigName)<>"/"<>
	      Tp.show().

    showName:[string,string]=>string.
    showName(N,N) => N.
    showName("",Nme) => Nme.
    showName(Nme,"") => Nme.
    showName(N,O) => N<>"("<>O<>")".
  }.

  fieldType ::= decimalTp | tinyTp | shortTp | longTp | floatTp | doubleTp | nullTp |
		timestampTp | longLongTp | int24Tp | dateTp | timeTp | dateTimeTp | 
		yearTp | newdateTp | varcharTp | bitTp | newDecimalTp | enumTp |
		setTp | tinyBlobTp | mediumBlobTp | longBlobTp | blobTp |
		varStringTp | stringTp | geometryTp.

  fieldFlag ::= notNullF | primaryKeyF | uniqueKeyF | multipleKeyF | blobF |
		unsignedF | zeroFillF | binaryF | enumF | autoIncrementF |
		timestampF | setF | noDefaultF | numericF.

  columnData ::= stringC(string)
	       | intC(integer) | floatC(float) | charC(char) | logicalC(logical) 
	       | binaryC(list[integer]) | dateC(date) | nullC.

}

