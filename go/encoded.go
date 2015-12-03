/*
  A Package to support encoded term format
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
go.encoded{
  import go.io.
  import go.serialize.

  /* Write out a program in encoded form .. */
  encodeTerm:[meta]=>list[integer].
  encodeTerm(Term):: Tail = serialMeta.serialize(Term,[]), Ln = listlen(Tail) => 
      encodeNum(Ln,0x60,Tail).

  decodeMeta:[meta-]-->list[integer].
  decodeMeta(Term)-->serialMeta.deserialize(Term).

  -- A serializer for metas
  serialMeta:[]@=serializer[meta].
  serialMeta..{
    serialize(M,Tl) => encodeTrm(M,Tl).

    encodeTrm:[meta,list[integer]]=>list[integer].
    encodeTrm(X,_)::var(X) => raise error("must be ground",'eFAIL').
    encodeTrm(metaInteger(N),Tail) => encodeNum(itrunc(N),0x10,Tail).
    encodeTrm(metaFloat(N),Tail) => encodeFloat(N,Tail).
    encodeTrm(metaSymbol('[]'),Tail) => [0x80,..Tail].
    encodeTrm(metaSymbol(N),Tail) => encodeName(explode(N),Tail).
    encodeTrm(metaChar(N),Tail) => encodeNum(__charCode(N),0x50,Tail).
    encodeTrm(metaString(N),Tail) => encodeNum(listlen(N),0x60,encodeCharList(N,Tail)).
    encodeTrm(metaList(H,T),Tail) => [0x81,..encodeTrm(H,encodeTrm(T,Tail))].
    encodeTrm(metaCons(N,A),Tail) => encodeNum(listlen(A),0x90,encodeName(explode(N),encodeArgs(A,Tail))).

    encodeArgs:[list[meta],list[integer]]=>list[integer].
    encodeArgs([],Tail) => Tail.
    encodeArgs([A,..L],Tail) => encodeTrm(A,encodeArgs(L,Tail)).

    deserialize(Trm) --> [B0], 
	{ band(B0,0xf0)==0x60 | raise error("string lead-in expected in encoded stream",'eINVAL') },
	decInt(band(B0,0xf),0,Ln), grabN(Ln,Btes), 
	{ decode(Trm) --> Btes }.

    decode:[meta-]-->list[integer].
    decode(Trm) --> [B0], decTerm(band(B0,0xf0),B0,Trm).

    decTerm:[integer,integer,meta]-->list[integer].
    decTerm(0,_,_) --> raise error("cant handle variables",'eFAIL').
    decTerm(0x10,B0,metaInteger(N)) --> decInt(band(B0,0xf),0,N).
    decTerm(0x20,B0,metaFloat(F)) --> decodeNum(Exp),
	decMantissa(band(B0,0xf),M),
	F=ldexp(M,Exp).
    decTerm(0x30,B0,metaFloat(F)) --> decodeNum(Exp), decMantissa(band(B0,0xf),M), F=-ldexp(M,Exp).
    decTerm(0x40,B0,metaSymbol(S)) --> decInt(band(B0,0xf),0,Ln), grabN(Ln,Btes), 
	S=implode({__charOf(B)..B in Btes}).
    decTerm(0x50,B0,metaChar(C)) --> decInt(band(B0,0xf),0,N), C=__charOf(N).
    decTerm(0x60,B0,metaString(S)) --> decInt(band(B0,0xf),0,Ln), grabN(Ln,Btes), S={__charOf(B)..B in Btes}.
    decTerm(0x80,0x80,metaSymbol('[]')) --> [].
    decTerm(0x80,0x81,metaList(H,T)) --> decode(H), decode(T).
    decTerm(0x80,0x83,metaCons('hdl',[T,R])) -->decode(T), decode(R).
    decTerm(0x90,B0,Struct) --> decInt(band(B0,0xf),0,Ar), decode(C), 
	( C = metaSymbol(Cns)?
	    Struct=metaCons(Cns,Args),
	    decodeArgs(Ar,Args)
	| raise error("invalid encoded structure",'eFAIL')
	).
    decTerm(_,_,_) -->raise error("invalid encoded structure",'eFAIL').
    
    decodeArgs:[integer,list[meta]]-->list[integer].
    decodeArgs(0,[]) --> [].
    decodeArgs(N,[A,..rgs]) --> decode(A), decodeArgs(N-1,rgs).
  }.

  encodeNum:[integer,integer,list[integer]]=>list[integer].
  encodeNum(0,Tg,Tl) => [bor(Tg,1),0,..Tl].
  encodeNum((N::N>0),Tg,Tail)::encNum(N,0,length,Tl,Tail) => [bor(Tg,length),..Tl].
  encodeNum(-1,Tg,Tl) => [bor(Tg,1),0xff,..Tl].
  encodeNum((N::N<-1),Tg,Tl)::encNeg(N,0,length,Tail,Tl) => [bor(Tg,length),..Tail].

  encNum:[integer,integer,integer,list[integer],list[integer]]{}.
  encNum((N::N>=0x100),Ln,Len,Tl,Tail) :--
      encNum(bright(N,8),Ln+1,Len,Tl,[band(N,0xff),..Tail]).
  encNum((N::N>=0x80), Ln, Ln2, [0,N,..Tail],Tail) :-- Ln2=Ln+2.
  encNum(N,Ln,Ln1,[N,..Tail],Tail) :-- Ln1=Ln+1.

  encNeg:[integer,integer,integer,list[integer],list[integer]]{}.
  encNeg((N::N<-1),Ln,Len,[Bn,..Tl],Tail) :--Bn=band(N,0xff),
      encNeg(bright(N,8),Ln+1,Len,Tl,Tail).
  encNeg(_,Len,Len,Tail,Tail) :-- {}.

  encodeFloat:[float,list[integer]]=>list[integer].
  encodeFloat(Nm,Tail)::Nm>=0.0 => encodeFlt(Nm,0x20,Tail).
  encodeFloat(Nm,Tail)::Nm<0.0 => encodeFlt(-Nm,0x30,Tail).

  encodeFlt:[float,integer,list[integer]]=>list[integer].
  encodeFlt(Nm,Tg,Tail)::
    frexp(Nm,F,Exp),
  C = encodeMant(F) => [bor(Tg,listlen(C)),..encodeNum(Exp,0x10,C)]<>Tail.

  encodeMant:[float]=>list[integer].
  encodeMant(0.0) => [].
  encodeMant(F)::modf(F*256.0,IP,FF) => [itrunc(IP),..encodeMant(FF)].

  encodeName:[string,list[integer]]=>list[integer].
  encodeName("",Tail) => encodeNum(1,0x40,[0,..Tail]).
  encodeName(Nm,Tail) => encodeNum(listlen(Nm),0x40,encodeCharList(Nm,Tail)).

  encodeCharList:[string,list[integer]]=>list[integer].
  encodeCharList([],Tail) => Tail.
  encodeCharList([C,..L],Tail) => [__charCode(C),..encodeCharList(L,Tail)].

  decodeNum:[integer]-->list[integer].
  decodeNum(N) --> [B0], decInt(band(B0,0xf),0,N).

  decInt:[integer,integer,integer]-->list[integer].
  decInt(0,X,X) --> [].
  decInt((Len::Len>0),X,Y) --> [By], decInt(Len-1,bor(bleft(X,8),By),Y).


  decMantissa:[integer,float]-->list[integer].
  decMantissa(N,M) --> grabN(N,L), { M=computeMant(reverse(L),0.0)}.
  
  grabN:[integer,list[t]]-->list[t].
  grabN(0,[]) --> [].
  grabN(N,[B,..L]) --> [B], grabN(N-1,L).

  computeMant:[list[integer],float]=>float.
  computeMant([],M) => M.
  computeMant([B,..L],M) => computeMant(L,(M+n2float(band(B,0xff)))/256.0).

  -- Grab a block of encoded terms from a file channel
  readNum:[inChannel,integer]*.
  readNum(I,N) -> B0=I.inB(); readInt(I,band(B0,0xf),0,N).

  readInt:[inChannel,integer,integer,integer]*.
  readInt(_,0,X,X) -> {}.
  readInt(I,(Len::Len>0),X,Y) -> By=I.inB(); readInt(I,Len-1,bor(bleft(X,8),By),Y).

  grabCodedInput:[inChannel]=>list[integer].
  grabCodedInput(In) => valof{
			  readNum(In,Len);
			  valis In.inBytes(Len)
			}.
}                                                                              
