/*
 * A simple debugger server for Go!
  (c) 2001-2004 F.G. McCabe
 
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

debugger{
  import go.io.
  import go.stdparse.
  import go.xml.

  scopeTp ::= inPred | inFun | inAction | inGrammar | inClass.

  debug:[]@=serverProc.
  debug..{
    exec(H,I,_,i,o) ->
        ( stdout.outLine("Connection from "<>H<>"/"<>I);
          loop([],i,o)
        )
        onerror 
        (X -> stderr.outLine("terminated: "<>X.show())).

    loop:[list[scopeTp],inChannel,outChannel]*.
    loop(Scope,i,_)::i.eof() -> 
        stdout.outLine("Connection closed, scope is "<>Scope.show()).
    loop(Scope,i,o) -> L = i.inText("\n\+4;"); -- stdout.outLine("recvd :"<>L^0); 
        (L=[]? stdout.outLine("control-D received") |
         --	      stdout.outLine("Msg: "<>L);
         {(xmlParse(Dom)-->L)!};
         handleMsg(o,Dom,Scope,nScope);
         loop(nScope,i,o)).
  }.
   
  /*
   * Debugger messages are in XML: 
   * <model language="go" version="1.0"/>
   * <pause tId="thread"/>
   * <line tId="thread" line="line">Url</line>
   * <frame tId="thread" key="frameKey"><var var="name">value</var>...</frame>
   * <deframe tId="thread" key="frameKey"/>
   * <asgn tId="thread" var="var">Value</asgn>
   * <send tId="sender" rId="receiver">Message</send>
   * <accept tId="thread" fId="From">Message</accept>
   * <fork tId="creator" child="child"/>
   * <die tId="thread"/>
   * <solve tId="thread" about="name">name(arg1,...,argn)</solve>
   * <retry tId="thread" about="name">name(arg1,...,argn)</retry>
   * <succ  tId="thread" about="name">name(arg1,...,argn)</succ>
   * <fail tId="thread" about="name">name(arg1,...,argn)</fail>
   * <parse  tId="thread" about="name">name(arg1,...,argn)</parse>
   * <parsed  tId="thread" about="name">name(arg1,...,argn)</parsed>
   * <call  tId="thread" about="name">name(arg1,...,argn)</call>
   * <exit  tId="thread" about="name">name(arg1,...,argn)</exit>
   * <eval  tId="thread" about="name">name(arg1,...,argn)</eval>
   * <return  tId="thread" about="name">Value</return>
   * <create  tId="thread" about="name">name(arg1,...,argn)</create>
   * <object  tId="thread" about="name">Value</object>

   * The go engine understands replies to <pause>:
   * <step/>, continue on to the next line
   * <fail/>, force a failure 
   * <quit/>, terminate the go engine
   * <abort/>, terminate the thread
   * <cont/>, continue, do not wait again
   */
   
  handleMsg:[outChannel,xmlDOM,list[scopeTp],list[scopeTp]]*.
  handleMsg(o,xmlElement('pause',(Atts::xmlAtt('tId',Pr) in Atts),_),Sc,Sc) ->
      stdout.outStr(Pr<>" >>");o.outLine(/*"<step/>"*/stdin.inLine("\n")).
  handleMsg(_,xmlElement('line',(Atts::xmlAtt('tId',Pr) in Atts, xmlAtt('line',Ln) in Atts,
                                   xmlAtt('url',Url) in Atts),_),Sc,Sc) ->
      stdout.outLine(Pr<>" "<>Url<>"@"<>Ln).
  handleMsg(_,xmlElement('asgn',(Atts::xmlAtt('tId',Pr) in Atts, xmlAtt('var',Vr) in Atts),
                         (Els::xmlText(Val) in Els)),Sc,Sc) ->
      stdout.outLine(Pr<>" "<>Vr<>" = "<>Val).
  handleMsg(_,xmlElement('send',(Atts::xmlAtt('tId',Pr) in Atts, xmlAtt('rId',Dest) in Atts),
                         (Els::xmlText(Val) in Els)),Sc,Sc) ->
      stdout.outLine(Pr<>" "<>Val<>" >> "<>Dest).
  handleMsg(_,xmlElement('accept',(Atts::xmlAtt('tId',Pr) in Atts, xmlAtt('rId',Src) in Atts),
                         (Els::xmlText(Val) in Els)),Sc,Sc) ->
      stdout.outLine(Pr<>" "<>Val<>" << "<>Src).
  handleMsg(_,xmlElement('fork',(Atts::xmlAtt('tId',Pr) in Atts, xmlAtt('child',Ch) in Atts),_),Sc,Sc) ->
      stdout.outLine(Pr<>" forks "<>Ch).
  handleMsg(_,xmlElement('die',(Atts::xmlAtt('tId',Pr) in Atts),_),Sc,Sc) ->
      stdout.outLine(Pr<>" dies ").
  handleMsg(_,xmlElement('frame',(Atts::xmlAtt('tId',Pr) in Atts),V),Sc,Sc) ->
      stdout.outLine(Pr<>" frame "<>{ Vn .. xmlElement('var',(vAtts::xmlAtt('var',Vn) in vAtts),_) in V}.show()).
  handleMsg(_,xmlElement('deframe',(Atts::xmlAtt('tId',Pr) in Atts),_),Sc,Sc) ->
      stdout.outLine(Pr<>" deframe ").
  handleMsg(_,xmlElement('model',(Atts::xmlAtt('language',Lang) in Atts),_),Sc,Sc) ->
      ( Lang=="go" ? {} | stdout.outLine("Unknown language :"<>Lang)).
  handleMsg(_,Other,Sc,Sc) ->
      stdout.outLine(showMsg(Other)).

  showMsg:[xmlDOM]=>string.
  showMsg(xmlElement(Type,Atts,Args))::
    xmlAtt('tId',Pr) in Atts, xmlAtt('about',Call) in Atts =>
      Pr<>": "<>explode(Type)<>" "<>Call<>showArgs("",Args).

  showMsg(M) => "Other message: "<>xmlDisplay(M).
  
  showArgs:[string,list[xmlDOM]]=>string.
  showArgs(Pre,[xmlElement('arg',_,[Val]),..Args]) =>
      Pre<>xmlDisplay(Val)<>showArgs(" ",Args).
  showArgs(Pre,[xmlText(Value),..Args]) =>
    Value<>showArgs(Pre,Args).
  showArgs(_,[])=>[].
  
  nextScope:[symbol,list[scopeTp]]=>list[scopeTp].
  nextScope('eval',Sc)=>[inFun,..Sc].
  nextScope('return',[inFun,..Sc])=>Sc.
  nextScope('create',Sc)=>[inClass,..Sc].
  nextScope('object',[inClass,..Sc])=>Sc.
  nextScope('exec',Sc)=>[inAction,..Sc].
  nextScope('finish',[inAction,..Sc])=>Sc.
  nextScope('call',Sc)=>[inPred,..Sc].
  nextScope('return',[inPred,..Sc])=>Sc.
  nextScope('parse',Sc)=>[inGrammar,..Sc].
  nextScope('parsed',[inGrammar,..Sc])=>Sc.
  nextScope(_,Sc)=>Sc.
  
  main([P]) ->
      stdout.outLine("Starting server on port "<>P);
      tcpServer(naturalOf%%P,debug,utf8Encoding).
}.
