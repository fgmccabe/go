/* Standard Description Logic library
   (c) 2007 F.G. McCabe

   The structures making up the logical terms
 
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
dl.terms{
  import go.showable.

  -- A resource is a simple wrapper for a symbol
  Resource <~ {
	qname:[]=>string.
	prefix:[]=>string.
	local:[]=>string.
      }.
  
  res:[string,string]@=Resource.
  res(Prefix,Local)..{
    qname()=>Prefix<>":"<>Local.

    prefix()=>Prefix.

    local()=>Local.
  }.


  -- DlOnt: the class that holds an ontology knowledge base
  Ontology <~ {
	classify:[dlStatement]*.
	add:[dlStatement]*.
	subsumes:[dlConcept,dlConcept]{}.
	normalize:[dlConcept]=>dlConcept.
	satisfies:[dlIndividual,dlConcept]{}.
	mostGeneralSubsumees:[dlConcept]=>list[dlConcept].
	mostSpecificSubsumers:[dlConcept]=>list[dlConcept].
	dump:[]*.
	name:[]=>string.
      }.
  Ontology <~ showable.

  -- DL concept
  dlConcept <~ { dl:[]=>string. }.
  dlConcept <~ showable.

  dlThing:[]@=dlConcept.
  dlThing..{
    dl() => this.disp().flatten("").

    disp() => s("thing").
  }.

  dlConcept:[symbol]@=dlConcept.
  dlConcept(_) <= dlThing.
  dlConcept(Nm) .. {
    disp() => s(explode(Nm)).
  }.

  dlExtra:[symbol]@=dlConcept.
  dlExtra(_) <= dlThing.
  dlExtra(Nm) .. {
    disp() => s(explode(Nm)).
  }.

  dlAnd:[list[dlConcept]]@=dlConcept.
  dlAnd(_) <= dlThing.
  dlAnd(L)..{
    disp() => n(join({e.disp() .. e in L},s(" && "))).
  }.

  dlAll:[dlRole,dlConcept]@=dlConcept.
  dlAll(_,_) <= dlThing.
  dlAll(Role,Concept)..{
    disp()::dlRole(Rl).=Role=>n([l(["$",explode(Rl)," = ("]),
				 Concept.disp(),
				 s(")")]).
  }.

  dlExists:[dlRole]@=dlConcept.
  dlExists(_) <= dlThing.
  dlExists(Role)..{
    disp()::dlRole(Rl).=Role=> l(["#",explode(Rl)]).
  }.

  dlFills:[dlRole,dlIndividual]@=dlConcept.
  dlFills(_,_) <= dlThing.
  dlFills(Role,Ind)..{
    disp()::dlRole(Rl).=Role =>
	n([s(explode(Rl)),s(" = ("), Ind.disp(), s(")")]).
  }.

  dlEnum:[list[dlIndividual]]@=dlConcept.
  dlEnum(_) <= dlThing.
  dlEnum(Inds)..{
    disp() =>
	n([s("{"),
	   n(join({e.disp() .. e in Inds},s(", "))),
	   s("}")]).
  }.

  dlRole ::= dlRole(symbol).

  dlIndividual <~ { dl:[]=>string. }.
  dlIndividual <~ showable.

  dlI:[symbol]@=dlIndividual.
  dlI(Name)..{
    disp()=>s(explode(Name)).

    dl()=>this.disp().flatten("").
  }.

  dlInt:[integer]@=dlIndividual.
  dlInt(Nm)..{
    dl()=>this.disp().flatten("").

    disp()=>s(Nm.show()).
  }.

  dlFlt:[float]@=dlIndividual.
  dlFlt(Nm)..{
    dl()=>this.disp().flatten("").

    disp()=>s(Nm.show()).
  }.

  dlStr:[string]@=dlIndividual.
  dlStr(S)..{
    dl()=>disp().flatten("").
    
    disp()=>l(["\"",S,"\""]).
  }.

  join:[list[t],t]=>list[t].
  join([],_)=>[].
  join([X],_)=>[X].
  join([X,..Y],F)=>[X,F,..join(Y,F)].

  dlStatement <~ { dl:[]=>string. }.
  dlStatement <~ showable.

  dlDefn:[dlConcept,dlConcept]@=dlStatement.
  dlDefn(C,D)..{
    disp() => n([C.disp(),s("=="),D.disp(),s(".\n")]).
    dl()=>disp().flatten("").
  }.

  dlSub:[dlConcept,dlConcept]@=dlStatement.
  dlSub(C,D)..{
    disp() => n([C.disp(),s("<="),D.disp(),s(".\n")]).
    dl()=>disp().flatten("").
  }.

  dlSat:[dlIndividual,dlConcept]@=dlStatement.
  dlSat(I,C)..{
    disp() => n([I.disp(),s("{"),C.disp(),s("}.\n")]).
    dl()=>disp().flatten("").
  }.
}