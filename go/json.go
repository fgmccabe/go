go.json{
  /*
   * A package to support the JSON file format
   */

  import go.io.
  import go.stdparse.
  import go.showable.

  json <~ showable.

  private json:[]@=json.
  json..{
    disp()=>s("**").

    show()=>this.disp().flatten("").
  }.

  jObject:[list[(string,json)]]@=json.
  jObject(_)<=json.
  jObject(Els)..{
    disp()=>n([s("{"),n(dispEls(Els,"")),s("}")]).

    dispEls:[list[(string,json)],string]=>list[dispTree].
    dispEls([],_)=>[].
    dispEls([(K,V),..ls],Sep)=>
	[s(Sep),jStr(K).disp(),s(":"),V.disp(),..dispEls(ls,", ")].
  }.

  jArray:[list[json]]@=json.
  jArray(_)<=json.
  jArray(Els)..{
    disp()=>n([s("["),n(dispEls(Els,"")),s("]")]).

    dispEls:[list[json],string]=>list[dispTree].
    dispEls([],_)=>[].
    dispEls([E,..ls],Sep)=>[s(Sep),E.disp(),..dispEls(ls,", ")].
  }.

  jStr:[string]@=json.
  jStr(_)<=json.
  jStr(Str)..{
    disp()=>l(["\"",..dispChars(Str)]).

    dispChars:[string]=>list[string].
    dispChars([])=>["\""].
    dispChars([S,..tr])=>[dChar(S),..dispChars(tr)].

    dChar:[char]=>string.
    dChar(`\b) => "\\b".
    dChar(`\f) => "\\f".
    dChar(`\n) => "\\n".
    dChar(`\r) => "\\r".
    dChar(`\t) => "\\t".
    dChar(`\") => "\\\"".
    dChar(`\\) => "\\\\".
    dChar(C)::X=__charCode(C),X>=32,X=<127 => [C].
    dChar(H) => [`\\,`u,..hexStr(__charCode(H))].

    hexStr:[integer]=>string.
    hexStr(X)::X<10 => [__charOf(X+48)].
    hexStr(X)::X<16 => [__charOf(X+64)].
    hexStr(X) => hexStr(bright(X,4))<>hexStr(band(X,15)).
  }.

  jNum:[float]@=json.
  jNum(_)<=json.
  jNum(Nm)..{
    disp()=>s(Nm.show()).
  }.

  jTrue:[]@=json.
  jTrue..{
    disp()=>s("true").

    show()=>"true".
  }.

  jFalse:[]@=json.
  jFalse..{
    disp()=>s("false").

    show()=>"false".
  }.

  jNull:[]@=json.
  jNull..{
    disp()=>s("null").

    show()=>"null".
  }.

  private skip:[]-->string.
  skip()-->skipWhiteSpace().

  private objEls:[list[(string,json)]]-->string.
  objEls([(Key,Value),..els]) --> 
      jString(Key), skip(), ":", skip(), jValue(Value), skip(),
      ( ",", skip(), objEls(els)
      | els=[]).
  objEls([]) --> "".

  private arEls:[list[json]]-->string.
  arEls([Value,..els]) --> 
      jValue(Value), skip(),
      ( ",", skip(), arEls(els)
      | els=[]).
  arEls([]) --> "".

  private jString:[string]-->string.
  jString(Str) --> "\"", sChar(C)*C^Str, "\"".

  private jValue:[json]-->string.
  jValue(jStr(Str)) --> jString(Str).
  jValue(jNum(N)) --> floatOf(N).
  jValue(jTrue) --> "true".
  jValue(jFalse) --> "false".
  jValue(jNull) --> "null".
  jValue(jObject(els)) --> "{", skip(), objEls(els), "}".
  jValue(jArray(els)) --> "[", skip(), arEls(els), "]".

  private sChar:[char]-->string.
  sChar:[char]-->string.
  sChar(`\b) --> "\\b".
  sChar(`\f) --> "\\f".
  sChar(`\n) --> "\\n".
  sChar(`\r) --> "\\r".
  sChar(`\t) --> "\\t".
  sChar(`\") --> "\\\"".
  sChar(`\\) --> "\\\\".
  sChar(`/) --> "\\/".
  sChar(H) --> "\\u",hexSeq(X,4)! , H=__charOf(X).
  sChar(X) --> [X], {\+X in [`\\,`\"]}.

  parseJson:[json]-->string.
  parseJson(O) --> skip(), jValue(O)!, skip().
}  
