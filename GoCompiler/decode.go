/*
 * Parse a compiled program file to extract exported information
 * used in importing a package
 */
decode{
  import go.io.
  import terms.
  import types.
--  import canon.

  decInt:[inChannel]=>integer.
  decInt(Fl) => dcInt(band(Fl.inB(),0xf),0,Fl).

  private dcInt:[integer,integer,inChannel] => integer.
  dcInt(0,X,_) => X.
  dcInt(L,X,Fl) => dcInt(L-1,bor(bleft(X,8),Fl.inB()),Fl).

  decodeSymbol:[inChannel]=>symbol.
  decodeSymbol(Fl) =>
      valof{
	B0 = Fl.inB();

	(band(B0,0xf0) = 0x60 ?
	   _ = dcInt(band(B0,0xf),0,Fl);
	   valis implode(decodeSym(Fl))
       | raise error("expecting string lead-in in code file",'fail')
	)
      }.

  private decodeSym:[inChannel]=>string.
  decodeSym(Fl) =>
      valof{
	B1 = Fl.inB();
	( band(B1,0xf0)=0x40 ?
	    L = dcInt(band(B1,0xf),0,Fl);
	    valis __utf82str(Fl.inBytes(L))
	| raise error("failed to read a symbol",'fail')
	)
      }.

  private decFrac:[list[integer],float]=>float.
  decFrac([],F) => F.
  decFrac([B,..Frac],F) => decFrac(Frac,(F+n2float(band(B,0xff)))/256.0).
	     
  decodeType:[inChannel]=>typeTree.
  decodeType(Fl) =>
      valof{
	B0 = Fl.inB();

	(band(B0,0xf0) = 0x60 ?
	   _ = dcInt(band(B0,0xf),0,Fl);
	   valis decodeTp(Fl)
       | raise error("expecting string lead-in in code file: ",'fail')
	)
      }.

  private decodeTp:[inChannel]=>typeTree.
  decodeTp(Fl) =>
      valof{
	B0 = Fl.inB();
	case band(B0,0xf0) in (
	 0 -> valis undef(implode("_"<>dcInt(band(B0,0xf),0,Fl).show()))
	| 0x10 ->
	     raise error("invalid type stream, number not permitted",'fail')
	| 0x20 ->
	     raise error("invalid type stream, number not permitted",'fail')
	| 0x30 ->
	     raise error("invalid type stream, number not permitted",'fail')
	| 0x40 ->
	     L = dcInt(band(B0,0xf),0,Fl);
	     Bf = __utf82str(Fl.inBytes(L));

	     ( Bf=="*#top" ?
		 valis topType
	     | Bf=="*#void"?
		 valis voidType
	     | append(P,[`#,..tpName],Bf) ?
		 valis uType(implode(P),implode(tpName),[])
	     | valis undef(implode(Bf))
	     )
       | 0x50 ->
	     raise error("invalid type stream, char not permitted",'fail')
       | 0x60 ->
	     raise error("invalid type stream, string not permitted",'fail')
       | 0x70 ->
	     raise error("invalid type stream, code not permitted",'fail')
       | 0x80 ->
	     raise error("invalid type stream, sequence not permitted",'fail')
       | 0x90 ->
	     Arity = dcInt(band(B0,0xf),0,Fl);
	     Fn = decodeSym(Fl);

	     case Fn in (
	      "#$=" ->
		  Args = decodeTypeTuple(Fl);
		  ( listlen(Args)=0 ?
		      valis enuType(decodeTp(Fl))
		  | valis conType(Args,decodeTp(Fl))
		  )
	    | "#$:" ->
		  valis enuType(decodeTp(Fl))

	    | "#@>" ->
		  sArgs = decodeArgTypes(Fl);
		  valis sconType(sArgs,decodeTp(Fl))
	    | "#=>" ->
		  fArgs = decodeArgTypes(Fl);
		  valis funType(fArgs,decodeTp(Fl))
	    | [`#,`(,`),.._] ->
		  tArgs = decodeTpSeq(Arity,Fl);
		  valis conType(tArgs,uType('*','()',[]))
	    | "#{}" ->
		  valis predType(decodeArgTypes(Fl))
	    | "#*" ->
		  valis actType(decodeArgTypes(Fl))
	    | "#-->" ->
		  gArgs = decodeArgTypes(Fl);
		  valis gramType(gArgs,decodeTp(Fl))
	    | "#-" ->
		  Bnd = implode(decodeSym(Fl));
		  Top = decodeTp(Fl);
		  valis allType(Bnd,Top,decodeTp(Fl))
	    | [`#,`<,`~,.._] ->
		  valis faceType(fields(decodeTypeFields(Arity,Fl)))
	    | "#::=" ->
		  B = decodeTp(Fl);
		  valis typeDef(B,decodeTypeTuple(Fl))
	    | ExFn :: append(Pkg,[`#,..Tnm],ExFn) ->
		  valis uType(implode(Pkg),implode(Tnm),decodeTpSeq(Arity,Fl))
	    | _ -> valis uType('',implode(Fn),decodeTpSeq(Arity,Fl))
	     )
       | _ ->
	     raise error("invalid code stream: "<>B0.show(),'fail')
	)
      }.

  private decodeTypeFields:[integer,inChannel]=>list[vSpec].
  decodeTypeFields(0,_) => [].
  decodeTypeFields(Ar,Fl) =>
      valof{
	Bf = Fl.inB();
	( band(Bf,0xf0)=0x90 ?
	    arity = dcInt(band(Bf,0xf),0,Fl);
	    fn = decodeSym(Fl);
	    ( [`(,`),.._].=fn ?
		( arity=3 ?
		    Fld = decodeSym(Fl);
		    Mode = decodeSym(Fl);
		    fTp = decodeTp(Fl);
		    M = ( Mode = "#var" ? varBind
			| Mode = "#tvar" ? tvarBind
			| typeBind);
		    valis [vS(implode(Fld),M,fTp),..decodeTypeFields(Ar-1,Fl)]
		| raise error("arity should be 3, not "<>arity.show(),'fail')
		)
	    | raise error("expecting tuple, not "<>fn,'fail')
	    )
	 | raise error("invalid code stream in type field",'fail')
	)
      }.

  private decodeTypeTuple:[inChannel]=>list[typeTree].
  decodeTypeTuple(Fl) =>
      valof{
	B0 = Fl.inB();
	(band(B0,0xf0)=0x90?
	   arity = dcInt(band(B0,0xf),0,Fl);
	   Fn = decodeSym(Fl);
	   (  [`#,`(,`),.._].=Fn ?
	       valis decodeTpSeq(arity,Fl)
	    | raise error("tuple of types expected, got "<>Fn,'fail')
	   )
       | raise error("tuple of types expected, read "<>B0.show(),'fail')
	)
      }.

  private decodeTpSeq:[integer,inChannel]=>list[typeTree].
  decodeTpSeq(0,_) => [].
  decodeTpSeq(Ar,Fl) =>
      valof{
	Tp = decodeTp(Fl);
	valis [Tp,..decodeTpSeq(Ar-1,Fl)]
      }.

  private decodeArgTypes:[inChannel]=>list[(typeTree,flowMode)].
  decodeArgTypes(Fl) =>
      valof{
	B0 = Fl.inB();
	(band(B0,0xf0)=0x90?
	   arity = dcInt(band(B0,0xf),0,Fl);
	   Fn =decodeSym(Fl);
	   ( [`#,`(,`),.._].=Fn ?
	       valis decodeArgTps(arity,Fl)
	   | raise error("tuple of argument types expected, got "<>Fn,'fail')
	   )
       | raise error("tuple of argument types expected, read "<>B0.show(),'fail')
	)
      }.

  private decodeArgTps:[integer,inChannel]=>list[(typeTree,flowMode)].
  decodeArgTps(0,_) => [].
  decodeArgTps(Ar,Fl) =>
      valof{
	Bf = Fl.inB();
	( band(Bf,0xf0)=0x90 ?
	    arity = dcInt(band(Bf,0xf),0,Fl);
	    fn = decodeSym(Fl);
	    ( [`#,`(,`),.._].=fn ?
		( arity=2 ?
		    Mode = decodeSym(Fl);
		    fTp = decodeTp(Fl);
		    M = ( Mode = "#inpMode" ? inpMode
			| Mode = "#outMode" ? outMode
			| Mode = "#superMode" ? superMode
			| biMode);
		    valis [(fTp,M),..decodeArgTps(Ar-1,Fl)]
		| raise error("arity should be 2, not "<>arity.show(),'fail')
		)
	    | raise error("expecting tuple, got "<>fn,'fail')
	    )
	 | raise error("invalid code stream in arg types",'fail')
	)
      }.
}