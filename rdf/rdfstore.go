/*
  A package to support an RDF store
  (c) 2006 F.G. McCabe

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

rdf.rdfstore{
  import go.hash.
  import go.xml.
  import go.setlib.
  import go.io.

  triple ::= rdf(symbol,symbol,symbol).

  rdfstore <~ {
	put:[triple]*. 
	mem:[triple]{}. 
	query:[list[triple]]{}.
	del:[triple]*. 
	ext:[]=>list[triple].
	load:[string]*.
	addNameSpace:[symbol,string]*.
	lookupNameSpace:[symbol]=>string.
		--	save:[string]*. 
      }.

  RDF_NS:string = "http://www.w3.org/1999/02/22-rdf-syntax-ns#".
  RDFS_NS:string = "http://www.w3.org/2000/01/rdf-schema#".
  DC_NS:string = "http://purl.org/dc/elements/1.1/".

  rdfstore:[]@>rdfstore.
  rdfstore()..{
    store:list[symbol] := [].
    subject:hash[symbol,list[symbol]] = hash([],1024).
    pred:hash[symbol,list[symbol]] = hash([],1024).
    object:hash[symbol,list[symbol]] = hash([],1024).

    nameSpaces:hash[symbol,string] = hash([('rdf',RDF_NS),
					   ('rdfs',RDFS_NS),
					   ('dc',DC_NS)],32).

    put(rdf(Sub,Pred,Obj)) ->
	Cl = __term(rdf(Sub,Pred,Obj));
	store := [Cl,..store];
	addToIndex(subject,Sub,Cl);
	addToIndex(pred,Pred,Cl);
	addToIndex(object,Obj,Cl).

    addToIndex:[hash[symbol,list[symbol]],symbol,symbol]*.
    addToIndex(H,Ky,Cl)::H.present(Ky,Ls) ->
	H.insert(Ky,[Cl,..Ls]).
    addToIndex(H,Ky,Cl) ->
	H.insert(Ky,[Cl]).

    findIx:[hash[symbol,list[symbol]],symbol]=>list[symbol].
    findIx(H,K)::H.present(K,V) => V.
    findIx(_,_) => [].

    mem(rdf(Sub,Pred,Obj))::\+var(Sub),\+var(Pred) :--
	sL = findIx(subject,Sub),
	sP = findIx(pred,Pred),
	findIn(rdf(Sub,Pred,Obj),sL/\sP).
    mem(rdf(Sub,Pred,Obj))::\+var(Sub),\+var(Obj) :--
	sL = findIx(subject,Sub),
	sO = findIx(object,Obj),
	findIn(rdf(Sub,Pred,Obj),sL/\sO).
    mem(rdf(Sub,Pred,Obj))::\+var(Sub) :--
	sL = findIx(subject,Sub),
	findIn(rdf(Sub,Pred,Obj),sL).
    mem(rdf(Sub,Pred,Obj))::\+var(Pred) :--
	sP = findIx(pred,Pred),
	findIn(rdf(Sub,Pred,Obj),sP).
    mem(rdf(Sub,Pred,Obj))::\+var(Obj) :--
	sO = findIx(object,Obj),
	findIn(rdf(Sub,Pred,Obj),sO).
    mem(rdf(Sub,Pred,Obj)) :--
	findIn(rdf(Sub,Pred,Obj),store).

    query([]).
    query([M,..L]) :- mem(M),query(L).

    ext() => { R .. (Cl::__is(Cl,R)) in store }.

    del(rdf(Sub,Pred,Obj)) ->
	(append(Front,[(Cl::__is(Cl,rdf(Sub,Pred,Obj))),..Back],store) ?
	   removeFromIndex(Sub,Cl,subject);
	   removeFromIndex(Pred,Cl,pred);
	   removeFromIndex(Obj,Cl,object);
	   store := Front<>Back
       | {} ).

    removeFromIndex:[symbol,symbol,hash[symbol,list[symbol]]]*.
    removeFromIndex(Ky,Cl,Hs) -> ( Hs.insert(Ky, Hs.find(Ky)\ [Cl]) onerror( _ -> {}) ).
	
    findIn:[_,list[symbol]]{}.
    findIn(O,[Cl,.._]) :- __is(Cl,O).
    findIn(O,[_,..L]) :- findIn(O,L).


    addNameSpace(ns,NS) ->
	nameSpaces.insert(ns,NS).

    lookupNameSpace(ns)::nameSpaces.present(ns,NS) => NS.
    lookupNameSpace(ns) => raise error("not defined",'eNOTFND').

    show()=>"rdf{"<>store.show()<>"S index = "<>subject.ext().show()<>"; V index = "<>pred.ext().show()<>
	    "; O index = "<>object.ext().show()<>"; triples = "<>ext().show()<>"}".

    load(Fl) ->
	(_,Dom) = grabXML("",Fl);
	(El in unwrap('http://www.w3.org/1999/02/22-rdf-syntax-ns#RDF',Dom) *>
	 findTriple(El)).

    findTriple:[xmlDOM]*.
    findTriple(xmlElement(tag,atts,children))->
	stdout.outLine("We have the element: "<>explode(tag));
	stdout.outLine("with attributes: "<>atts.show());
	stdout.outLine("and children: "<>children.show()).

    unwrap:[symbol,xmlDOM]=>list[xmlDOM].
    unwrap(Tg,xmlElement(Tg,_Att,Ch))=>Ch.

  }.

  main(Args)->
      RDF=rdfstore();
      ( Fl in Args *>
	RDF.load(Fl)).
	
}