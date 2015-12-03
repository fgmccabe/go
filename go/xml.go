/*
  Simple XML parser for Go!
  (c) 2004-2005 F.G. McCabe
 
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

go.xml{
  import go.io.
  import go.hash.
  import go.stdparse.
  import go.showable.

  -- xmlable interface means that you can get the xml representation 
  -- of the entity

  xmlable <~ {
	xml:[]=>string.
      }.

  xmlDOM <~ showable.
  xmlDOM <~ xmlable.
  xmlDOM <~ { 
	pickElement:[symbol]=>xmlDOM. 
        elementPresent:[symbol]{}.
        hasAtt:[symbol,string]{}.
	pickAtt:[symbol]=>string.
        pickText:[symbol,string]=>string.
      }.

  xmlText:[string]@=xmlDOM.
  xmlText(S)..{
    pickElement(_) => raise error("invalid",'eINVAL').
    elementPresent(_) :- false.
    hasAtt(_,_) :- raise error("invalid",'eINVAL').
    pickAtt(_) => raise error("invalid",'eINVAL').
    pickText(_,_) => raise error("invalid",'eINVAL').

    xml() => disp().flatten([]).

    disp()=>s(qtStr(S,"")).
  }.

  xmlPI:[string]@=xmlDOM.
  xmlPI(S)..{
    pickElement(_) => raise error("invalid",'eINVAL').
    elementPresent(_) :- false.
    hasAtt(_,_) :- raise error("invalid",'eINVAL').
    pickAtt(_) => raise error("invalid",'eINVAL').
    pickText(_,_) => raise error("invalid",'eINVAL').

    xml() => disp().flatten([]).

    disp()=>l(["<?",qtStr(S,""),"?>"]).
  }.

  xmlElement:[symbol,list[xmlAttr],list[xmlDOM]]@=xmlDOM.
  xmlElement(Nm,Atts,Els)..{
    pickElement(Ky)::xmlElement(Ky,At,El) in Els => xmlElement(Ky,At,El).
    pickElement(_) => raise error("not found",'eNOTFND').

    elementPresent(Ky) :- xmlElement(Ky,_,_) in Els.

    pickAtt(Ky) :: hasAtt(Ky,Val) => Val.
    pickAtt(_) => raise error("not found",'eNOTFND').
    
    hasAtt(Ky,Val) :- xmlAtt(Ky,Val) in Atts.

    pickText(Ky,_) :: xmlElement(Ky,_,L) in Els, xmlText(V) in L => V.
    pickText(_,Deflt) => Deflt.

    xml() => disp().flatten([]).

    disp()::Els=[] => l(["<",explode(Nm),..dspAtts(Atts,"/>")]).
    disp()=>valof{
	      xNme = explode(Nm);
	      valis n([l(["<",xNme,..dspAtts(Atts,">")]),
		       n({El.disp()..El in Els}),l(["</",xNme,">"])])
	    }.

    dspAtts:[list[xmlAttr],string]=>list[string].
    dspAtts([],L) => [L].
    dspAtts([xmlAtt(atNm,Val),..Ats],L) => 
	[" ",explode(atNm),"=\"",qtStr(Val,"\""),..dspAtts(Ats,L)].
  }.

  hasNameSpace:[xmlDOM+,string]{}.
  hasNameSpace(xmlElement(_,Atts,_),Ns) :- xmlAtt('xmlns',Ns) in Atts.

  hasAtt:[xmlDOM+,symbol+,string]{}.
  hasAtt(xmlElement(_,Atts,_),att,value) :- xmlAtt(att,value) in Atts.

  xmlAttr ::= xmlAtt(symbol,string).

  xmlDisplay:[xmlDOM]=>string.
  xmlDisplay(D) => D.disp().flatten([]).

  private qtStr:[string,string]=>string.
  qtStr([],L)=>L.
  qtStr([C,..R],L)::chEntity(E,C) => [`&,..E]<>";"<>qtStr(R,L).
  qtStr([C,..R],L)=>[C,..qtStr(R,L)].

  private chEntity:[string,char]{}.
  chEntity("amp",`&).
  chEntity("lt",`<).
  chEntity("gt",`>).
  chEntity("apos",`\').
  chEntity("excl",`!).
  chEntity("quot",`\").
  
  private entity <~ ( entity:[symbol,string]{}. define:[symbol,string]* ).
    
  private entities:[]@>entity.
  entities()..{
    E:hash[symbol,string] = hash([('amp',"&"),
				  ('lt',"<"),
				  ('gt',">"),
				  ('apos',"\'"),
				  ('excl',"!"),
				  ('quot',"\"")],16).
  
    entity(K,V) :- E.present(K,V).
    define(K,V) -> E.insert(K,V).
  }.

  private defltEnt:entity = entities().	 -- We use this when no document declaration

  parseT <~ { parse:[xmlDOM]-->string }.

  -- This is the grammar ...

  private xmlParser:[entity]@=parseT.

  xmlParser(Ents)..{

    xmlTags ::= xmlStartTag(symbol,list[xmlAttr])
              | xmlEmptyTag(symbol,list[xmlAttr])
              | xmlEndTag(symbol)
              | xmlPITag
              | xmlTextTag(string)
              | xmlPCDataTag
              | xmlEnd.

    skip:[]-->string.
    skip() --> [X],{whiteSpace(X)}, skip().
    skip() --> [].

    cmntSkip:[]-->string.
    cmntSkip() --> "<!--",skipComment(),cmntSkip().
    cmntSkip() --> [X],{whiteSpace(X)}, cmntSkip().
    cmntSkip()-->[].
    
    whiteSpace:[char]{}.
    whiteSpace(X)::__isZsChar(X) :-- {}.
    whiteSpace(X)::__isZlChar(X) :-- {}.
    whiteSpace(X)::__isZpChar(X) :-- {}.
    whiteSpace(X)::__isCcChar(X) :-- {}.
  
    skipComment:[]-->string.
    skipComment() --> "-->",skip().
    skipComment() --> [_],skipComment().
  
    identifier:[symbol]-->string.
    identifier(Id) --> [X],{isLetterCh(X)}, restIdent(I)!, Id=implode([X,..I]).
    
    restIdent:[list[char]]-->string.
    restIdent([X,..I]) --> [X],{isNameChar(X)}, restIdent(I).
    restIdent([]) --> [].
  
    isLetterCh:[char]{}.
    isLetterCh(X) :: __isLetterChar(X) :-- {}.
    isLetterCh(`_) :-- {}.
    isLetterCh(`:) :-- {}.
    isLetterCh(`.) :-- {}.
  
    isNameChar:[char]{}.
    isNameChar(X) :- isLetterCh(X).
    isNameChar(`.).
    isNameChar(`-).
    isNameChar(X) :- __isNdChar(X).
    isNameChar(X) :- __isMnChar(X).
    isNameChar(X) :- __isMcChar(X).
    isNameChar(X) :- __isPcChar(X).
    isNameChar(X) :- __isCfChar(X).
    
    trim:[string,string]=>string.
    trim([],T)=>T.
    trim([C,..L],T)::whiteSpace(C) => trim(L,T).
    trim([C,..L],T) => reverse(L)<>[C,..T].
    
    parse:[xmlDOM]-->string.
    parse(Doc) --> skip(),
         preamble(_,_,_),parseElement(D)!,skip(),Doc=nameSpaceMap(D,[]).
    
    parseTag:[xmlTags]-->string.
    parseTag(xmlEnd) --> eof.
    parseTag(xmlEndTag(tag)) --> "</", endTag(tag)!.
    parseTag(xmlPITag) --> "<?".
    parseTag(Tag) --> "<!--", skipComment()!, parseTag(Tag).
    parseTag(xmlPCDataTag) --> "<![CDATA[".
    parseTag(Tag) --> "<",identifier(T), skip(),
        attributes(L), skip(), tagTail(Tag,T,L)!.
  
    attributes:[list[xmlAttr]]-->string.
    attributes([xmlAtt(Att,Val),..L]) --> 
        identifier(Att),skip(),"=",skip(),String(Val), skip(), attributes(L).
    attributes([]) --> [].
  
    tagTail:[xmlTags,symbol,list[xmlAttr]]-->string.
    tagTail(xmlEmptyTag(T,L),T,L) --> "/>".
    tagTail(xmlStartTag(T,L),T,L) --> ">".
  
    endTag:[symbol]-->string.
    endTag(tag) --> identifier(tag), skip(), ">".
    endTag(_) --> raise error("invalid end tag",'fail').
  
    parseElement:[xmlDOM]-->string.
    parseElement(DOM) --> skip(), parseTag(Tg)!, parseCases(Tg,DOM).
    parseElement(xmlText(Tx)) --> parseText([],Text),Tx=trim(Text,[]).
  
    parseCases:[xmlTags,xmlDOM] --> string.
    parseCases(xmlEnd,_) --> raise error("unexpected eof",'fail').
    parseCases(xmlEmptyTag(Tag,Atts), xmlElement(Tag,Atts,[])) --> "".
    parseCases(xmlStartTag(Tag,Atts), xmlElement(Tag,Atts,Els)) --> 
        skip(),parseElements(Els,Tag).
    parseCases(xmlPITag, xmlPI(Txt)) --> skip(),getPI(Txt).
    parseCases(xmlPCDataTag,xmlText(Text)) --> parsePCData(Text).
  
    parseElements:[list[xmlDOM],symbol]-->string.
    parseElements([],End) --> ("</",skip(),identifier(End),skip(),">")!.
    parseElements([E,..L],End) --> parseElement(E),skip(),parseElements(L,End).
  
    parseText:[string,string]-->string.
    parseText(T,T) --> eof.
    parseText(T,T),"<" --> "<".
    parseText(Txt,T) --> "&",identifier(Ent),";",{Ents.entity(Ent,C)!},
        parseText(reverse(C)<>Txt,T).       -- need to reverse, because the text is reversed
    parseText(Txt,T) --> [C], parseText([C,..Txt],T).
  
    parsePCData:[string]-->string.
    parsePCData([]) --> eof.
    parsePCData([]) --> "]]>".
    parsePCData([C,..M]) --> [C], parsePCData(M).
  
    preamble:[string,ioEncoding,symbol] --> string.
    preamble(V,Enc,Dc) --> "<?",skip(),"xml",skip(),
		 "version",skip(),"=",skip(),String(V),skip(),
		 parseEncoding(Enc),skip(),"?>",cmntSkip(),
		 parseDocDecl(Dc,_).
    preamble(_,utf8Encoding,_) --> "".
                      
    parseEncoding:[ioEncoding]-->string.
    parseEncoding(Enc) --> "encoding",skip(),"=",skip(),
        String(E),{encoding(E,Enc)!}.
    parseEncoding(utf8Encoding) --> "".
  
    encoding:[string,ioEncoding]{}.
    encoding("utf-8",utf8Encoding).
    encoding("ISO-10646-UCS-4",utf8Encoding).
    encoding("ISO-8859-1",rawEncoding).
  
    parseDocDecl:[symbol,string]-->string.
    parseDocDecl(Top,Url) --> "<!",skip(),"DOCTYPE",skip(),identifier(Top),skip(),
        ( "SYSTEM",skip(),String(Url)
        | "PUBLIC",skip(),String(_),skip(),String(Url)
        | ""),
        skip(),
        parseInternalDecs(),
        ">".
    parseDocDecl(_,"") --> "".

    parseInternalDecs:[]-->string.
    parseInternalDecs() --> "[", 
        skip(),
        getDecs(),
        skip(), "]".
    parseInternalDecs() --> "".

    getDecs:[]-->string.
    getDecs() --> "<", parseMarkupDecl(), skip(), getDecs().
    getDecs() --> "".

    parseMarkupDecl:[]-->string.
    parseMarkupDecl() --> "!ELEMENT", skip(), parseElementDecl().
    parseMarkupDecl() --> "!ATTLIT", skip(), parseAttlistDecl().
    parseMarkupDecl() --> "!ENTITY", skip(), parseEntityDecl().

    parseEntityDecl:[]-->string.
    parseEntityDecl() --> identifier(Id),skip(),String(Val),skip(),">",
        action{Ents.define(Id,Val)}.
    parseEntityDecl() --> identifier(_),skip(),"SYSTEM",skip(),String(_),skip(),">".

    parseAttlistDecl:[]-->string.
    parseAttlistDecl() --> raise error("DTDs not supported",'fail').

    parseElementDecl:[]-->string.
    parseElementDecl() --> raise error("DTDs not supported",'fail').
    
    getPI:[string]-->string.
    getPI([]) --> "?>".
    getPI([C,..L]) --> [C],getPI(L).
  
    String:[string]-->string.
    String(Str) --> "\'", rdString(Str, `\')!.
    String(Str) --> "\"", rdString(Str, `\")!.

    rdString:[string,char]-->string.
    rdString([],T) --> [T].
    rdString([Ch,..Str],Term) --> "&#",hexNum(X),";",{Ch=__charOf(X)},rdString(Str,Term).
    rdString(St,Term) --> "&",identifier(Ent),";",
        {Ents.entity(Ent,V)!}, rdString(Str,Term),St=V<>Str.
    rdString([Ch,..Str],T) --> [Ch], rdString(Str,T).

    nameSpaceMap:[xmlDOM,list[(string,string)]]=>xmlDOM.
    nameSpaceMap(xmlElement(tag,atts,els),nsMap) =>
        mapNameSpaces(xmlElement(tag,atts,els),defineNameSpaces(atts,nsMap)).
    nameSpaceMap(xmlText(T),_) => xmlText(T).
  
    defineNameSpaces:[list[xmlAttr],list[(string,string)]]=>list[(string,string)].
    defineNameSpaces([],Map)=>Map.
    defineNameSpaces([xmlAtt(attr,NS),..List],Map)::
      append("xmlns:",Local,explode(attr)) => 
        [(Local,NS),..defineNameSpaces(List,Map)].
    defineNameSpaces([xmlAtt('xmlns',NS),..List],Map) => 
        [("xmlns",NS),..defineNameSpaces(List,Map)].
    defineNameSpaces([_,..List],Map) => defineNameSpaces(List,Map).
  
    mapNameSpaces:[xmlDOM,list[(string,string)]]=>xmlDOM.
    mapNameSpaces(xmlText(T),_)=>xmlText(T).
    mapNameSpaces(xmlElement(tag,Atts,Elements),Map) =>
        xmlElement(mapName(tag,Map),
                   { xmlAtt(mapName(att,Map),Val) .. xmlAtt(att,Val) in Atts},
                   { nameSpaceMap(El,Map) .. El in Elements }).

    mapName:[symbol,list[(string,string)]]=>symbol.
    mapName(Iden,Map) =>
        implode(mapNm(explode(Iden),Map)).
    
    mapNm:[string,list[(string,string)]]=>string.
    mapNm(Id,Map):: append(Local,[`:,..Suffix],Id)!, (Local,NS) in Map => NS<>Suffix.
    mapNm(Id,Map)::("xmlns",Deflt) in Map => Deflt<>Id.
    mapNm(Id,_) => Id.
  }.

  xmlParse:[xmlDOM] --> string.
  xmlParse(DOM) --> xmlParser(entities()).parse(DOM).

  grabXML:[string,string]=>(string,xmlDOM).
  grabXML(Base,Request) => 
      valof{
        (f,Actual) = openURL(__mergeURL(Base,Request),unknownEncoding);
        Text = f.inText("");           -- This will read the entire file
        f.close();
        {(xmlParser(defltEnt).parse(DOM)-->Text)};
        valis (Actual,DOM)
      }.

  loadXML:[string,string]=>xmlDOM.
  loadXML(Base,Request) => 
      valof{
        (f,_) = openURL(__mergeURL(Base,Request),unknownEncoding);
        Text = f.inText("");           -- This will read the entire file
        f.close();
        {(xmlParser(defltEnt).parse(DOM)-->Text)};
        valis DOM
      }.
}
