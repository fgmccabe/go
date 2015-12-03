/*
  A package to support N3 notation
  (c) 2007 F.G. McCabe

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

n3{
  import rdf.rdfstore.
  import go.io.
  import go.stdparse.
  import go.dynamic.

  /*
   * The grammar for N3 comes from
   * http://www.w3.org/2000/10/swap/grammar/n3-report.html
   */

  statement ::= prefix(string,uri)
	      | exists(string)
	      | simple(expression,list[property])
	      | universal(verb).

  expression ::= bool(logical) 
	       | lit(string,langSpec)
	       | numLit(float)
	       | qV(string)
	       | u(uri)
	       | q(string,string)
	       | path(list[expression])
	       | props(list[property])
	       | formula(list[statement])
	       | pling(expression,expression)
	       | hat(expression,expression).

  property ::= prop(verb,list[expression]).

  verb ::= vb(expression)
	 | lesseq | equal | greaterq | isa 
	 | has(expression) | isof(expression).

  langSpec::= defLg | lng(string) | ref(expression).

  uri ::= uri(string).

  keywords:dynamic[string] = dynamic(["a", "is", "has", "of"]).

  document:[list[statement]] --> string.
  document(L) --> skip(), formulae(L), eof.

  private skip:[]-->string.
  skip() --> "#", skipToEol()!, skip().
  skip() --> [C],{whiteSpace(C)}, skip().
  skip() --> "".

  private skipToEol:[]-->string.
  skipToEol() --> "\n".
  skipToEol() --> eof.
  skipToEol() --> [C],{C!=`\n},skipToEol().

  formulae:[list[statement]]-->string.
  formulae(L) --> ( statement(S),skip(),".", skip()) *S ^L.

  statement:[statement] --> string.
  statement(S) --> declaration(S).
  statement(E) --> existential(E).
  statement(sS) --> simpleStatement(sS).
  statement(U) --> universalStatement(U).

  declaration:[statement] --> string.
  declaration(prefix(Pr,Uri)) --> "@prefix", skip(),
      (ident(Pr) | Pr=""), ":", skip(), explicitUri(Uri).

  private explicitUri:[uri]-->string.
  explicitUri(uri(S)) --> "<", uriBody(S).

  private uriBody:[string] --> string.
  uriBody(B) --> [C], { whiteSpace(C)}, uriBody(B).
  uriBody("") --> ">".
  uriBody([C,..B]) --> [C], uriBody(B).

  expression:[expression] --> string.
  expression(E) --> item(L), skip(),
      ( "!", skip(), expression(R), E=pling(L,R)
      | "^", skip(), expression(R), E=hat(L,R)
      | E=L).

  private item:[expression]-->string.
  item(E) --> boolean(E).
  item(L) --> literal(L).
  item(N) --> numericLiteral(N).
  item(V) --> quickVar(V).
  item(S) --> sym(S).
  item(path([E,..L])) --> "(", skip(), 
      expression(E), skip(),
      ( expression(e), skip())*e ^L,
      ")".
  item(props(Ps)) --> "[", skip(), 
      properties(Ps),
      "]".
  item(formula(Ss)) --> "{", formulae(Ss), "}".

  private sym:[expression]-->string.
  sym(u(S)) --> explicitUri(S).
  sym(q("",Q)) --> ":",ident(Q).
  sym(q("","")) --> ":".
  sym(q(B,Q)) --> ident(B),":",ident(Q).
      

  private existential:[statement]-->string.
  existential(exists(S)) --> "<", ([C], C!=`>)*C^S, ">".

  private simpleStatement:[statement]-->string.
  simpleStatement(simple(Subject,Props)) -->
      subject(Subject),
      properties(Props).

  private subject:[expression]-->string.
  subject(S) --> expression(S).

  private properties:[list[property]]-->string.
  properties([Pr,..Ps]) -->
      property(Pr), skip(),
      ( ";", property(P), skip())*P^Ps.

  private property:[property]-->string.
  property(prop(V,[O,..Os])) -->
      vrb(V), skip(), 
      object(O), skip(),
      ( ",", object(oO), skip())*oO^Os.

  private object:[expression]-->string.
  object(O)-->expression(O).

  universalStatement:[statement] --> string.
  universalStatement(universal(V)) --> vrb(V).

  vrb:[verb]-->string.
  vrb(lesseq) --> "<=".
  vrb(greaterq) --> "=>".
  vrb(equal) --> "=".
  vrb(isa) --> atA().
  vrb(has(E)) --> atHas(), skip(),expression(E).
  vrb(isof(O)) --> atIs(), skip(),expression(O), 
      skip(),
      atOf().
  vrb(vb(V)) --> expression(V).

  private atA:[]-->string.
  atA() --> "@a".
  atA() --> "a".

  private atHas:[]-->string.
  atHas() --> "@has".
  atHas() --> "has".

  private atIs:[]-->string.
  atIs() --> "@is".
  atIs() --> "is".

  private atOf:[]-->string.
  atOf() --> "@of".
  atOf() --> "of".

  boolean:[expression]-->string.
  boolean(bool(true)) --> "@true".
  boolean(bool(false)) --> "@false".

  literal:[expression]-->string.
  literal(lit(S,Lng)) -->
      parseString(S),
      skip(),
      dtLang(Lng).

  private dtLang:[langSpec] --> string.
  dtLang(lng(L)) --> "@", skip(), languageCode(L).
  dtLang(ref(S)) --> "^^", skip(), sym(S).
  dtLang(defLg) --> "".

  private languageCode:[string]-->string.
  languageCode(L) --> ident(L).

  private numericLiteral:[expression] --> string.
  numericLiteral(numLit(F)) --> floatOf(F).

  private quickVar:[expression] --> string.
  quickVar(qV(V)) --> ident(V).

  main(Files) ->
      F in Files *>
      stdout.outLine((document%%getFile(F,utf8Encoding)).show()).
}