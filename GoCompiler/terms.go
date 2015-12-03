/*
 * Intermediate term representation
 */

terms{
  import abstract.
  import types.
  import ops.
  import go.showable.
  import go.setlib.
  import opts.

  objTree <~ { loc:[]=>fileLoc. disp:[integer]=>dispTree. }.

  private obj:[fileLoc]@=objTree.
  obj(Lc)..{
    loc() => Lc.
    disp(_)=>n([]).
    show()=>this.disp(0).flatten([]).

  }.

  termTree <~ objTree.
  termTree <~ { tpe:[]=>typeTree. showTerm:[]=>string. }.

  vde:[]@=termTree.
  vde <= obj(noLoc).
  vde..{
    tpe()=>voidType.
    show()=>"void".
    disp(_) => s("void").
    showTerm()=>"void:void".
  }.

  iden:[symbol,typeTree,fileLoc]@=termTree.
  iden(Nm,Tp,Lc)..{
    loc()=>Lc.
    show()=>disp(0).flatten("").
    disp(_)=>n([l([explode(Nm),":"]),Tp.disp(1000)]).
    tpe()=>Tp.deRef().
    showTerm()=>show()<>":"<>Tp.show().
  }.

  enu:[symbol,typeTree,fileLoc]@=termTree.
  enu(Nm,Tp,Lc) <= iden(Nm,Tp,Lc).

  intgr:[integer,fileLoc]@=termTree.
  intgr(Nm,Lc)..{
    loc()=>Lc.
    tpe()=>uType('go.stdlib','integer',[]).
    disp(_)=>s(Nm.show()).
    show()=>disp(0).flatten([]).
    showTerm()=>show()<>":integer".
  }.

  flot:[float,fileLoc]@=termTree.
  flot(Nm,Lc)..{
    loc()=>Lc.
    tpe()=>uType('go.stdlib','float',[]).
    disp(_)=>s(Nm.show()).
    show()=>disp(0).flatten([]).
    showTerm()=>show()<>":float".
  }.

  sym:[symbol,fileLoc]@=termTree.
  sym(S,Lc)..{
    loc()=>Lc.
    tpe()=>uType('go.stdlib','symbol',[]).

    disp(_)=>l(["\'",show(),"\'"]).
    show()=>explode(S).
    showTerm()=>show()<>":symbol".
  }.

  chr:[char,fileLoc]@=termTree.
  chr(C,Lc)..{
    loc()=>Lc.
    tpe()=>uType('go.stdlib','char',[]).
    disp(_)=>s(show()).
    show()=>[``,C].
    showTerm()=>show()<>":char".
  }.

  str:[string,fileLoc]@=termTree.
  str(S,Lc)..{
    loc()=>Lc.
    tpe()=>uType('go.stdlib','list',[uType('go.stdlib','char',[])]).
    disp(_)=>l(["\"",show(),"\""]).
    show()=>S.
    showTerm()=>show()<>":string".
  }.

  lst:[termTree,termTree,fileLoc]@=termTree.
  lst(_,_,Lc)<=obj(Lc).
  lst(hH,_,_)..{
    tpe()=>uType('go.stdlib','list',[hH.tpe()]).

    dispList:[termTree]=>list[dispTree].
    dispList(enu('[]',_,_))=>[].
    dispList(lst(H,(T::lst(_,_,_).=T),_)) => [H.disp(999),s(","),..dispList(T)].
    dispList(lst(H,T,_)) => [H.disp(999),s(",.."),T.disp(999)].

    disp(_)=>n([s("["),n(dispList(this)),s("]")]).
    showTerm()=>show()<>":"<>tpe().show().
  }.

  dot:[termTree,symbol,typeTree,fileLoc]@=termTree.
  dot(_,_,_,Lc)<=obj(Lc).
  dot(O,M,Tp,_)..{
    tpe() => Tp.deRef().

    disp(Pr)::infOp('.',oL,P,_) => n([showPar(Pr,P,`\(),
                                      O.disp(oL), s("."), s(explode(M)),
                                      showPar(Pr,P,`\))]).
    showTerm()=>show()<>":"<>tpe().show().
  }.

  bag:[dict,termTree,goalTree,fileLoc]@=termTree.
  bag(_,_,_,Lc)<=obj(Lc).
  bag(Q,Ex,Gl,_)..{
    tpe() => uType('go.stdlib','list',[Ex.tpe()]).

    disp(_)::infOp('||',iL,_,iR)=>
        n([s("{"), Ex.disp(iL), s(" ||"), Gl.disp(iR), s("}")]).
    showTerm()=>show()<>":"<>tpe().show().
  }.

  bnd:[dict,termTree,list[(termTree,termTree)],fileLoc]@=termTree.
  bnd(_,_,_,Lc)<=obj(Lc).
  bnd(E,Ex,pSets,Lc)..{
    tpe() => uType('go.stdlib','list',[Ex.tpe()]).

    dispSets:[list[(termTree,termTree)],string] => list[dispTree].
    dispSets([],_) => [].
    dispSets([(Ptn,Set),..Sets],Pre)::infOp('in',iL,_,iR) => 
	[s(Pre),Ptn.disp(iL),s(" in "),Set.disp(iR),..dispSets(Sets,", ")].

    disp(_)::infOp('..',dL,_,_)=>
        n([s("{"), Ex.disp(dL), s(" .. "), n(dispSets(pSets,"")),s("}")]).
    showTerm()=>show()<>":"<>tpe().show().
  }.
    
  con:[symbol,list[termTree],typeTree,fileLoc]@=termTree.
  con(_,_,_,Lc)<=obj(Lc).
  con(Nm,Args,Tp,Lc)..{
    tpe() => Tp.deRef().

    disp(Pr)::Nm=',..' => n(dispList(this,Pr,"[")).
    disp(_) => n([s(explode(Nm)),s("("),n(showArgs(Args)),s(")")]).
    showTerm()=>show()<>":"<>tpe().show().

    dispList:[termTree,integer,string]=>list[dispTree].
    dispList(con(',..',[H,T],_,_),Pr,Sep) => [s(Sep),H.disp(Pr),..dispList(T,Pr,",")].
    dispList(enu('[]',_,_),_,_) => [s("]")].
    dispList(Tr,Pr,_) => [s(",.."),Tr.disp(Pr),s("]")].
  }.

  scon:[symbol,list[termTree],typeTree,fileLoc]@=termTree.
  scon(_,_,_,Lc)<=obj(Lc).
  scon(Nm,Args,Tp,Lc)..{
    tpe() => Tp.deRef().

    disp(_) => n([s(explode(Nm)),s("("),n(showArgs(Args)),s(")")]).
    showTerm()=>show()<>":"<>tpe().show().
  }.

  app:[termTree,list[termTree],typeTree,fileLoc]@=termTree.
  app(_,_,_,Lc)<=obj(Lc).
  app(Fn,Args,Tp,Lc)..{
    tpe() => Tp.deRef().

    disp(_) => n([Fn.disp(0),s("("),n(showArgs(Args)),s(")")]).
    showTerm()=>show()<>":"<>tpe().show().
  }.

  spwn:[dict,actTree,fileLoc]@=termTree.
  spwn(_,_,Lc)<=obj(Lc).
  spwn(Q,Act,_)..{
    tpe() => uType('go.stdlib','thread',[]).

    disp(_)::infOp('. ',_,Pr,_)=> n([s("spawn{"),Act.disp(Pr),s("}")]).
    showTerm()=>show()<>":"<>tpe().show().
  }.

  vlof:[dict,actTree,typeTree,fileLoc]@=termTree.
  vlof(_,_,_,Lc)<=obj(Lc).
  vlof(Q,Act,Tp,Lc)..{
    tpe() => Tp.deRef().

    disp(_)::infOp('. ',_,Pr,_)=> n([s("valof{"),Act.disp(Pr),s("}")]).
    showTerm()=>show()<>":"<>tpe().show().
  }.

  grd:[termTree,goalTree,fileLoc]@=termTree.
  grd(_,_,Lc)<=obj(Lc).
  grd(Ex,Gl,Lc)..{
    tpe() => Ex.tpe().

    disp(Pr)::infOp('::',iL,iP,iR)=>
        n([showPar(Pr,iP,`\(), Ex.disp(iL), s(" :: "), Gl.disp(iR), showPar(Pr,iP,`\))]).
    showTerm()=>Ex.showTerm()<>"::"<>Gl.show().
  }.

  lzy:[termTree,goalTree,fileLoc]@=termTree.
  lzy(_,_,Lc)<=obj(Lc).
  lzy(Ex,Gl,Lc)..{
    tpe() => Ex.tpe().

    disp(Pr)::infOp('@@',iL,iP,iR)=>
        n([showPar(Pr,iP,`\(), Ex.disp(iL), s("@@"), Gl.disp(iR), showPar(Pr,iP,`\))]).
    showTerm()=>show()<>":"<>tpe().show().
  }.

  ifEx:[goalTree,termTree,termTree,typeTree,fileLoc]@=termTree.
  ifEx(_,_,_,_,Lc)<=obj(Lc).
  ifEx(Gl,Th,El,Tp,Lc)..{
    tpe() => Tp.deRef().

    disp(Pr)::infOp('?',qL,_,qR),infOp('|',_,eP,eR)=>
        n([showPar(Pr,eP,`\(), Gl.disp(qL),s("?"),
           Th.disp(qR),s("|"),El.disp(eR),showPar(Pr,eP,`\))]).
    showTerm()=>show()<>":"<>tpe().show().
  }.

  csEx:[termTree,list[ruleTree],typeTree,fileLoc]@=termTree.
  csEx(_,_,_,Lc)<=obj(Lc).
  csEx(Ex,Rls,Tp,_)..{                  -- case analysis
    tpe() => Tp.deRef().

    disp(Pr)::preOp('case',xP,_),infOp('in',iL,_,_)=>
        n([showPar(Pr,xP,`\(),
           s("case"),
           Ex.disp(iL),
           s("in ("),
           dispRules(Rls,"|"),
           s(")"),
           showPar(Pr,xP,`\))]).
    showTerm()=>show()<>":"<>tpe().show().
  }.

  errEx:[termTree,list[ruleTree],fileLoc]@=termTree.
  errEx(_,_,Lc)<=obj(Lc).
  errEx(Ex,Rls,Lc)..{                  -- error handled expression
    tpe() => Ex.tpe().

    disp(Pr)::infOp('onerror',iL,iP,_)=>
        n([showPar(Pr,iP,`\(),
           Ex.disp(iL),
           s("onerror ("),
           dispRules(Rls,"|"),
           s(")"),
           showPar(Pr,iP,`\))]).
    showTerm()=>show()<>":"<>tpe().show().
  }.

  exc:[termTree,fileLoc]@=termTree.
  exc(_,Lc)<=obj(Lc).
  exc(Er,Lc)..{
    tpe() => newVr().

    disp(Pr)::preOp('raise',iP,iR)=>
        n([showPar(Pr,iP,`\(),s("raise "), Er.disp(iR),showPar(Pr,iP,`\)) ]).
    showTerm()=>show()<>":"<>tpe().show().
  }.

  pkrf:[symbol,termTree,fileLoc]@=termTree.
  pkrf(_,_,Lc)<=obj(Lc).
  pkrf(Pkg,Nd,Lc)..{
    tpe() => Nd.tpe().

    disp(Pr)::infOp('#',_,iP,iR)=>
        n([showPar(Pr,iP,`\(),s(explode(Pkg)),s("#"),Nd.disp(iR),showPar(Pr,iP,`\)) ]).
    showTerm()=>show()<>":"<>tpe().show().
  }.

  anon:[progTree,typeTree,fileLoc]@=termTree.
  anon(_,_,Lc)<=obj(Lc).
  anon(class,Tp,Lc)..{                  -- case analysis
    tpe() => Tp.deRef().

    disp(Pr)=>class.disp(Pr).
    showTerm()=>show()<>":"<>tpe().show().
  }.

  ntEx:[termTree,typeTree,termTree,termTree,fileLoc]@=termTree.
  ntEx(_,_,_,_,Lc) <= obj(Lc).
  ntEx(G,T,L,R,_) ..{
    tpe() => T.deRef().
    disp(Pr)::R==vde,infOp('%%',iL,iP,iR) =>
        n([showPar(Pr,iP,`\(),G.disp(iL),s("%%"),L.disp(iR),
	   showPar(Pr,iP,`\)) ]).
    disp(Pr)::infOp('%%',iL,iP,_), infOp('~',rL,_,rR) =>
        n([showPar(Pr,iP,`\(),G.disp(iL),s("%%"),L.disp(rL),
	   s("~"),R.disp(rR),showPar(Pr,iP,`\)) ]).
    showTerm()=>show()<>":"<>tpe().show().
  }.

  private showPar:[integer,integer,char]=>dispTree.
  showPar(Pr,xPr,C)::xPr>=Pr => s([C]).
  showPar(_,_,_) => s("").

  private showArgs:[list[termTree]]=>list[dispTree].
  showArgs(L)=>showList(L,999,"",",").
                
  private showList:[list[objTree],integer,string,string]=>list[dispTree].
  showList([],_,_,_)=>[].
  showList([E,..L],Pr,Sep,S) => [s(Sep),E.disp(Pr),..showList(L,Pr,S,S)].

  private showRules:[list[objTree],string,integer,string,string] => list[dispTree].
  showRules([],_,_,_,_) => [].
  showRules([R,..L],Nm,Pr,Sep,S) => [s(Sep),s(Nm),R.disp(Pr),..showRules(L,Nm,Pr,S,S)].

  goalTree <~ objTree.

  private showGoals:[list[goalTree]]=>list[dispTree].
  showGoals(L)::infOp(',',_,Pr,_)=>showList(L,Pr,"",",").

  prdGl:[termTree,list[termTree],fileLoc]@=goalTree.
  prdGl(_,_,Lc)<=obj(Lc).
  prdGl(Fn,Args,Lc)..{
    disp(Pr)::iden(N,_,_).=Fn,infOp(N,pL,pP,pR),[L,R].=Args =>
        n([showPar(Pr,pP,`\(), L.disp(pL), s(explode(N)), R.disp(pR), showPar(Pr,pP,`\))]).
    disp(_) => n([Fn.disp(0),s("("),n(showArgs(Args)),s(")")]).
  }.

  trueGl:[fileLoc]@=goalTree.
  trueGl(Lc)<=obj(Lc).
  trueGl(_)..{
    disp(_)=>s("true").
  }.

  falseGl:[fileLoc]@=goalTree.
  falseGl(Lc)<=obj(Lc).
  falseGl(_)..{
    disp(_)=>s("false").
  }.

  eqGl:[termTree,termTree,fileLoc]@=goalTree.
  eqGl(_,_,Lc)<=obj(Lc).
  eqGl(L,R,Lc)..{
    disp(Pr)::infOp('.=',iL,iP,iR)=>
        n([showPar(Pr,iP,`\(), L.disp(iL), s(" = "),
	   R.disp(iR), showPar(Pr,iP,`\))]).
  }.

  mtcGl:[termTree,termTree,fileLoc]@=goalTree.
  mtcGl(_,_,Lc)<=obj(Lc).
  mtcGl(L,R,Lc)..{
    disp(Pr)::infOp('.=',iL,iP,iR)=>
        n([showPar(Pr,iP,`\(), L.disp(iL), s(" .= "), R.disp(iR), showPar(Pr,iP,`\))]).
  }.

  lblGl:[termTree,termTree,fileLoc]@=goalTree.
  lblGl(_,_,Lc)<=obj(Lc).
  lblGl(L,R,Lc)..{
    disp(Pr)::infOp('<=',iL,iP,iR)=>
        n([showPar(Pr,iP,`\(), L.disp(iL), s(" <= "),
	   R.disp(iR), showPar(Pr,iP,`\))]).
  }.

  inGl:[termTree,termTree,fileLoc]@=goalTree.
  inGl(_,_,Lc)<=obj(Lc).
  inGl(L,R,Lc)..{
    disp(Pr)::infOp('in',iL,iP,iR)=>
        n([showPar(Pr,iP,`\(), L.disp(iL), s(" in "), R.disp(iR), showPar(Pr,iP,`\))]).
  }.

  cnjGl:[list[goalTree],fileLoc]@=goalTree.
  cnjGl(_,Lc)<=obj(Lc).
  cnjGl(L,Lc)..{
    disp(_)::listlen(L)==0 => s("true").
    disp(Pr)::listlen(L)>1,infOp(',',_,gP,_) => n([showPar(Pr,gP,`\(),
                                                   n(showGoals(L)),
                                                   showPar(Pr,gP,`\))]).
    disp(_) => n(showGoals(L)).
  }.

  lzyGl:[termTree,goalTree,fileLoc]@=goalTree.
  lzyGl(_,_,Lc)<=obj(Lc).
  lzyGl(L,R,Lc)..{
    disp(Pr)::infOp('@@',iL,iP,iR)=>
        n([showPar(Pr,iP,`\(), L.disp(iL), s(" @@ "), R.disp(iR), showPar(Pr,iP,`\))]).
  }.

  actGl:[actTree,fileLoc]@=goalTree.
  actGl(_,Lc)<=obj(Lc).
  actGl(Act,Lc)..{
    disp(_)=> n([s("action{"),Act.disp(2000),s("}")]).
  }.

  frllGl:[goalTree,goalTree,fileLoc]@=goalTree.
  frllGl(_,_,Lc)<=obj(Lc).
  frllGl(L,R,Lc)..{
    disp(Pr)::infOp('*>',iL,iP,iR)=>
        n([showPar(Pr,iP,`\(), L.disp(iL), s(" *> "), R.disp(iR), showPar(Pr,iP,`\))]).
  }.

  ifGl:[goalTree,goalTree,goalTree,fileLoc]@=goalTree.
  ifGl(_,_,_,Lc)<=obj(Lc).
  ifGl(T,L,R,Lc)..{
    disp(Pr)::infOp('|',iL,iP,iR), infOp('?',qL,_,_)=>
        n([showPar(Pr,iP,`\(), T.disp(qL),s(" ? "),L.disp(iL), s(" | "), R.disp(iR), showPar(Pr,iP,`\))]).
  }.

  dsjGl:[goalTree,goalTree,fileLoc]@=goalTree.
  dsjGl(_,_,Lc)<=obj(Lc).
  dsjGl(L,R,Lc)..{
    disp(Pr)::infOp('|',iL,iP,iR)=>
        n([showPar(Pr,iP,`\(), L.disp(iL), s(" | "), R.disp(iR), showPar(Pr,iP,`\))]).
  }.

  negGl:[goalTree,fileLoc]@=goalTree.
  negGl(_,Lc)<=obj(Lc).
  negGl(R,Lc)..{
    disp(Pr)::preOp('\\+',iP,iR)=>
        n([showPar(Pr,iP,`\(), s(" \\+ "), R.disp(iR), showPar(Pr,iP,`\))]).
  }.

  oneGl:[goalTree,fileLoc]@=goalTree.
  oneGl(_,Lc)<=obj(Lc).
  oneGl(R,Lc)..{
    disp(Pr)::pstOp('!',iP,iR)=>
        n([showPar(Pr,iP,`\(), R.disp(iR), s("! "), showPar(Pr,iP,`\))]).
  }.

  errGl:[goalTree,list[ruleTree],fileLoc]@=goalTree.
  errGl(_,_,Lc)<=obj(Lc).
  errGl(Ex,Rls,Lc)..{                  -- error handled relation condition
    disp(Pr)::infOp('onerror',iL,iP,_)=>
        n([showPar(Pr,iP,`\(),
           Ex.disp(iL),
           s("onerror ("),
           dispRules(Rls,"|"),
           s(")"),
           showPar(Pr,iP,`\))]).
  }.

  exGl:[termTree,fileLoc]@=goalTree.
  exGl(_,Lc)<=obj(Lc).
  exGl(Er,Lc)..{
    disp(Pr)::preOp('raise',iP,iR)=>
        n([showPar(Pr,iP,`\(),s("raise "), Er.disp(iR),showPar(Pr,iP,`\)) ]).
  }.

  ntGl:[gramTree,termTree,termTree,fileLoc]@=goalTree.
  ntGl(_,_,_,Lc) <= obj(Lc).
  ntGl(Gr,Stm,Rem,_)..{
    disp(Pr)::infOp('-->',gL,gP,_),infOp('~',rL,_,rR) =>
	n([showPar(Pr,gP,`\(),
	   Gr.disp(gL),
	   s(" --> "),
	   Stm.disp(rL),
	   s(" ~ "),
	   Rem.disp(rR),
	   showPar(Pr,gP,`\))]).
  }.

  gramTree <~ objTree.
  gramTree <~ { tpe:[]=>typeTree }.

  ntGr:[termTree,list[termTree],typeTree,fileLoc]@=gramTree.
  ntGr(_,_,_,Lc)<=obj(Lc).
  ntGr(Fn,Args,Tp,Lc)..{
    tpe() => Tp.

    disp(_) => n([Fn.disp(0),s("("),n(showArgs(Args)),s(")")]).
  }.

  strGr:[string,fileLoc]@=gramTree.
  strGr(_,Lc)<=obj(Lc).
  strGr(St,_) .. {
    tpe()=>uType('go.stdlib','list',[uType('go.stdlib','char',[])]).

    disp(_) => l(["\"",St,"\""]).
  }.

  tmGr:[termTree,typeTree,fileLoc]@=gramTree.
  tmGr(_,_,Lc)<=obj(Lc).
  tmGr(Term,Tp,Lc)..{
    tpe() => Tp.

    disp(Pr) => Term.disp(Pr).
  }.

  glGr:[goalTree,typeTree,fileLoc]@=gramTree.
  glGr(_,_,Lc)<=obj(Lc).
  glGr(G,Tp,Lc)..{
    tpe() => Tp.

    disp(Pr) => n([s("{"), G.disp(Pr), s("}")]).
  }.

  seqGr:[list[gramTree],typeTree,fileLoc]@=gramTree.
  seqGr(_,_,Lc)<=obj(Lc).
  seqGr(L,Tp,Lc)..{
    tpe() => Tp.

    disp(_)::listlen(L)==0 => s("[]").
    disp(Pr)::infOp(',',cPl,cPr,_) => n([showPar(Pr,cPr,`\(),
				       n(showList(L,cPl,"",",")),
				       showPar(Pr,cPr,`\))]).
  }.

  eofGr:[typeTree,fileLoc]@=gramTree.
  eofGr(_,Lc)<=obj(Lc).
  eofGr(Tp,Lc)..{
    tpe() => Tp.

    disp(_) => s("eof").
  }.

  oneGr:[gramTree,fileLoc]@=gramTree.
  oneGr(_,Lc)<=obj(Lc).
  oneGr(L,Lc)..{
    tpe() => L.tpe().

    disp(Pr)::pstOp('!',iL,iP)=>
        n([showPar(Pr,iP,`\(),L.disp(iL),s("!"),showPar(Pr,iP,`\)) ]).
  }.

  negGr:[gramTree,fileLoc]@=gramTree.
  negGr(_,Lc)<=obj(Lc).
  negGr(L,Lc)..{
    tpe() => L.tpe().

    disp(Pr)::preOp('\\+',iL,iP)=>
        n([showPar(Pr,iP,`\(),s("\\+"),L.disp(iL),showPar(Pr,iP,`\)) ]).
  }.

  errGr:[gramTree,list[ruleTree],fileLoc]@=gramTree.
  errGr(_,_,Lc)<=obj(Lc).
  errGr(Ex,Rls,Lc)..{                  -- error handled grammar condition
    tpe() => Ex.tpe().

    disp(Pr)::infOp('onerror',iL,iP,_)=>
        n([showPar(Pr,iP,`\(),
           Ex.disp(iL),
           s("onerror ("),
           dispRules(Rls,"|"),
           s(")"),
           showPar(Pr,iP,`\))]).
  }.

  exGr:[termTree,typeTree,fileLoc]@=gramTree.
  exGr(_,_,Lc)<=obj(Lc).
  exGr(Er,Tp,Lc)..{
    tpe()=>Tp.
    disp(Pr)::preOp('raise',iP,iR)=>
        n([showPar(Pr,iP,`\(),s("raise "), Er.disp(iR),showPar(Pr,iP,`\)) ]).
  }.

  itrGr:[dict,gramTree,termTree,termTree,typeTree,fileLoc]@=gramTree.
  itrGr(_,_,_,_,_,Lc)<=obj(Lc).
  itrGr(V,G,P,L,Tp,Lc)..{
    tpe() => Tp.
    
    disp(Pr)::infOp('*',iL,iP,_),infOp('^',sL,_,sR)=>
        n([showPar(Pr,iP,`\(),G.disp(iL),s("*"),P.disp(sL),s("^"),L.disp(sR),
           showPar(Pr,iP,`\)) ]).
  }.

  ifGr:[gramTree,gramTree,gramTree,typeTree,fileLoc]@=gramTree.
  ifGr(_,_,_,_,Lc)<=obj(Lc).
  ifGr(Gl,Th,El,Tp,Lc)..{
    tpe() => Tp.

    disp(Pr)::infOp('?',qL,_,qR),infOp('|',_,eP,eR)=>
        n([showPar(Pr,eP,`\(), Gl.disp(qL),s("?"),
           Th.disp(qR),s("|"),El.disp(eR),showPar(Pr,eP,`\))]).
  }.

  dsjGr:[gramTree,gramTree,typeTree,fileLoc]@=gramTree.
  dsjGr(_,_,_,Lc)<=obj(Lc).
  dsjGr(L,R,Tp,Lc)..{
    tpe()=>Tp.
    disp(Pr)::infOp('|',iL,iP,iR)=>
        n([showPar(Pr,iP,`\(), L.disp(iL), s(" | "), R.disp(iR), showPar(Pr,iP,`\))]).
  }.

  actTree <~ objTree.

  noopAc:[fileLoc]@=actTree.
  noopAc(Lc)<=obj(Lc).
  noopAc(_)..{
    disp(_)=>s("{}").
  }.

  vlisAc:[termTree,fileLoc]@=actTree.
  vlisAc(_,Lc)<=obj(Lc).
  vlisAc(X,Lc)..{
    disp(Pr)::preOp('valis',iP,iR)=>
        n([showPar(Pr,iP,`\(),s("valis "), X.disp(iR),showPar(Pr,iP,`\)) ]).
  }.

  istrueAc:[goalTree,fileLoc]@=actTree.
  istrueAc(_,Lc)<=obj(Lc).
  istrueAc(G,Lc)..{
    disp(Pr) => n([s("istrue "), G.disp(Pr)]).
  }.

  asgAc:[symbol,termTree,fileLoc]@=actTree.
  asgAc(_,_,Lc)<=obj(Lc).
  asgAc(Name,Val,Lc)..{
    disp(Pr)::infOp(':=',_,iP,iR)=>
        n([showPar(Pr,iP,`\(), s(explode(Name)), s(" := "), Val.disp(iR), showPar(Pr,iP,`\))]).
  }.

  glAc:[goalTree,fileLoc]@=actTree.
  glAc(_,Lc)<=obj(Lc).
  glAc(G,Lc)..{
    disp(Pr) => n([s("{"), G.disp(Pr), s("}")]).
  }.

  csAc:[termTree,list[ruleTree],fileLoc]@=actTree.
  csAc(_,_,Lc)<=obj(Lc).
  csAc(Ex,Rls,Lc)..{                  -- case analysis
    disp(Pr)::preOp('case',xP,_),infOp('in',iL,_,_)=>
        n([showPar(Pr,xP,`\(),
           s("case"),
           Ex.disp(iL),
           s("in ("),
           dispRules(Rls,"|"),
           s(")"),
           showPar(Pr,xP,`\))]).
  }.

  seqAc:[list[actTree],fileLoc]@=actTree.
  seqAc(_,Lc)<=obj(Lc).
  seqAc(L,Lc)..{
    disp(_)::listlen(L)==0 => s("{}").
    disp(Pr)::listlen(L)>1,infOp(',',_,gP,_) => n([showPar(Pr,gP,`\(),
                                                   n(showActs(L)),
                                                   showPar(Pr,gP,`\))]).
    disp(_) => n(showActs(L)).
  }.

  private showActs:[list[actTree]]=>list[dispTree].
  showActs(L)::infOp(';',_,Pr,_)=>showList(L,Pr,"",";").

  frllAc:[list[(symbol,typeTree)],goalTree,actTree,fileLoc]@=actTree.
  frllAc(_,_,_,Lc)<=obj(Lc).
  frllAc(_,L,R,Lc)..{
    disp(Pr)::infOp('*>',iL,iP,iR)=>
        n([showPar(Pr,iP,`\(), L.disp(iL), s(" *> "), R.disp(iR), showPar(Pr,iP,`\))]).
  }.

  actAc:[termTree,list[termTree],fileLoc]@=actTree.
  actAc(_,_,Lc)<=obj(Lc).
  actAc(Fn,Args,Lc)..{
    disp(_) => n([Fn.disp(0),s("("),n(showArgs(Args)),s(")")]).
  }.

  errAc:[actTree,list[ruleTree],fileLoc]@=actTree.
  errAc(_,_,Lc)<=obj(Lc).
  errAc(Ex,Rls,Lc)..{                  -- error handled grammar condition
    disp(Pr)::infOp('onerror',iL,_,_)=>
        n([showPar(Pr,xP,`\(),
           Ex.disp(iL),
           s("onerror ("),
           dispRules(Rls,"|"),
           s(")"),
           showPar(Pr,xP,`\))]).
  }.

  exAc:[termTree,fileLoc]@=actTree.
  exAc(_,Lc)<=obj(Lc).
  exAc(Er,Lc)..{
    disp(Pr)::preOp('raise',iP,iR)=>
        n([showPar(Pr,iP,`\(),s("raise "), Er.disp(iR),showPar(Pr,iP,`\)) ]).
  }.

  spwnAc:[dict,actTree,fileLoc]@=actTree.
  spwnAc(_,_,Lc)<=obj(Lc).
  spwnAc(Q,Act,_)..{
    disp(_)::infOp('. ',_,Pr,_)=> n([s("spawn{"),Act.disp(Pr),s("}")]).
  }.

  syncAc:[termTree,list[ruleTree],fileLoc]@=actTree.
  syncAc(_,_,Lc)<=obj(Lc).
  syncAc(Ex,Rls,Lc)..{
    disp(_) =>
        n([s("sync("),
           Ex.disp(999),
           s("){"),
           dispRules(Rls,"|"),
           s("}")]).
  }.

  tmeAc:[actTree,termTree,actTree,fileLoc]@=actTree.
  tmeAc(_,_,_,Lc)<=obj(Lc).
  tmeAc(S,T,A,_)..{
    disp(Pr)::infOp('timeout',qL,_,_),infOp('->',aL,_,aR)=>
        n([showPar(Pr,eP,`\(), S.disp(qL),s("\ntimeout"),
           T.disp(aL),s("->"),A.disp(aR),showPar(Pr,eP,`\))]).
  }.

  ifAc:[goalTree,actTree,actTree,fileLoc]@=actTree.
  ifAc(_,_,_,Lc)<=obj(Lc).
  ifAc(Gl,Th,El,Lc)..{
    disp(Pr)::infOp('?',qL,_,qR),infOp('|',_,eP,eR)=>
        n([showPar(Pr,eP,`\(), Gl.disp(qL),s("?"),
           Th.disp(qR),s("|"),El.disp(eR),showPar(Pr,eP,`\))]).
  }.



  ruleTree <~ objTree.

  eqn:[dict,list[termTree],goalTree,termTree,fileLoc]@=ruleTree.
  eqn(_,_,_,_,Lc)<=obj(Lc).
  eqn(Vars,Args,Guard,Rep,Lc)..{
    disp(Pr) :: infOp('=>',eL,eP,eR) =>
	n([showPar(Pr,eP,`\(),
	   s("("),n(showArgs(Args)),s(")"),
	   showGuard(Guard,"::",eL),
	   s("=>"),
	   Rep.disp(eR),
	   showPar(Pr,eP,`\))]).
  }.

  cls:[dict,list[termTree],goalTree,fileLoc]@=ruleTree.
  cls(_,_,_,Lc)<=obj(Lc).
  cls(Vars,Args,Body,Lc)..{
    disp(Pr) :: infOp(':-',_,eP,eR) =>
	n([showPar(Pr,eP,`\(),
	   s("("),n(showArgs(Args)),s(")"),
	   showGuard(Body,":-",eR),
	   showPar(Pr,eP,`\))]).
  }.

  scls:[dict,list[termTree],goalTree,goalTree,fileLoc]@=ruleTree.
  scls(_,_,_,_,Lc)<=obj(Lc).
  scls(Vars,Args,Guard,Body,Lc)..{
    disp(Pr) :: infOp(':--',eL,eP,eR) =>
	n([showPar(Pr,eP,`\(),
	   s("("),n(showArgs(Args)),s(")"),
	   showGuard(Guard,"::",eL),
	   s(":--"),
	   Body.disp(eR),
	   showPar(Pr,eP,`\))]).
  }.

  grRl:[dict,list[termTree],gramTree,gramTree,fileLoc]@=ruleTree.
  grRl(_,_,_,_,Lc)<=obj(Lc).
  grRl(Vars,Args,Rep,Body,Lc)..{
    disp(Pr) :: infOp('-->',lP,eP,_) =>
	n([showPar(Pr,eP,`\(),
	   s("("),n(showArgs(Args)),s(")"),
	   Rep.disp(lP),
	   s("-->"),
	   Body.disp(999),
	   showPar(Pr,eP,`\))]).
  }.

  acRl:[dict,list[termTree],goalTree,actTree,fileLoc]@=ruleTree.
  acRl(_,_,_,_,Lc)<=obj(Lc).
  acRl(Vars,Args,Guard,Body,Lc)..{
    disp(Pr) :: infOp('->',eL,eP,_),infOp(';',_,cP,_) =>
	n([showPar(Pr,eP,`\(),
	   s("("),n(showArgs(Args)),s(")"),
	   showGuard(Guard,"::",eL),
	   s("->"),
	   Body.disp(cP),
	   showPar(Pr,eP,`\))]).
  }.


  private showGuard:[goalTree,string,integer]=>dispTree.
  showGuard(trueGl(_),_,_)=>n([]).
  showGuard(G,Cnj,Pr)::infOp(',',_,cP,_)=>
      n([showPar(Pr,cP,`\(), s(Cnj), G.disp(cP),showPar(Pr,cP,`\))]).
	   
  clBdy:[dict,termTree,list[progTree],fileLoc]@=ruleTree.
  clBdy(_,_,_,Lc)<=obj(Lc).
  clBdy(Vars,Lbl,Body,Lc)..{
    loc() => Lc.

    disp(Pr)::infOp('..',cL,cP,_),infOp('. ',_,dP,_) =>
	n([showPar(Pr,cP,`\(),
	   Lbl.disp(cL),
	   s("..{"),
	   n(showList(Body,dP,"\n",".\n")),
	   s("}")]).
  }.
	   
  clRule:[dict,termTree,termTree,fileLoc]@=ruleTree.
  clRule(_,_,_,Lc)<=obj(Lc).
  clRule(Vars,Lbl,Super,Lc)..{
    disp(Pr) :: infOp('<=',eL,eP,eR) =>
	n([showPar(Pr,eP,`\(),
	   Lbl.disp(eL),
	   s("<="),
	   Super.disp(eR),
	   showPar(Pr,eP,`\))]).
  }.

  progTree <~ objTree.
  progTree <~ { 
	defines:[symbol]{}. 
	name:[]=>symbol. vis:[]=>visibility. tpe:[]=>typeTree. }.

  visibility ::= privAte | pUblic | anOnymous.

  prg:[symbol,visibility,list[ruleTree],typeTree,fileLoc]@=progTree.
  prg(_,_,_,_,Lc)<=obj(Lc).
  prg(Name,Vis, Rules, Type, Lc)..{
    name()=>Name.
    defines(Name).
    vis()=>Vis.
    tpe()=>Type.

    disp(_)::infOp('. ',dL,_,_) =>
	n([showVis(Vis),n(showRules(Rules,explode(Name),dL,"",".\n")),s(".\n")]).
  }.

  private dispRules:[list[ruleTree],string]=>dispTree.
  dispRules(L,Sep)::infOp('. ',_,Pr,_) => n(showList(L,Pr,"",Sep)).

  private showVis:[visibility]=>dispTree.
  showVis(privAte)=>s("private ").
  showVis(pUblic)=>s("").
  showVis(anOnymous) => s("anonymous ").

  srDf:[symbol,visibility,list[ruleTree],typeTree,fileLoc]@=progTree.
  srDf(Name,Vis, Rules, Type, Lc) <= prg(Name,Vis,Rules,Type,Lc).

  fnDf:[symbol,visibility,list[ruleTree],typeTree,fileLoc]@=progTree.
  fnDf(Name,Vis, Rules, Type, Lc) <= prg(Name,Vis,Rules,Type,Lc).
  
  rDf:[symbol,visibility,list[ruleTree],typeTree,fileLoc]@=progTree.
  rDf(Name,Vis, Rules, Type, Lc) <= prg(Name,Vis,Rules,Type,Lc).

  grDf:[symbol,visibility,list[ruleTree],typeTree,fileLoc]@=progTree.
  grDf(Name,Vis, Rules, Type, Lc) <= prg(Name,Vis,Rules,Type,Lc).
  
  acDf:[symbol,visibility,list[ruleTree],typeTree,fileLoc]@=progTree.
  acDf(Name,Vis, Rules, Type, Lc) <= prg(Name,Vis,Rules,Type,Lc).

  clDf:[symbol,visibility,list[ruleTree],typeTree,fileLoc] @=progTree.
  clDf(_,_,_,_,Lc)<=obj(Lc).
  clDf(Name,Vis, Rules, Type, Lc)..{
    name()=>Name.
    defines(Name).
    vis()=>Vis.
    tpe()=>Type.
    
    disp(_)::infOp('. ',_,dP,_) => n(showList(Rules,dP,"",".\n")).
  }.

  imp:[symbol,visibility,list[symbol],typeTree,fileLoc]@=progTree.
  imp(_,_,_,_,Lc)<=obj(Lc).
  imp(Name,Vis, Pkg, Type, Lc)..{
    name()=>Name.
    defines('').
    vis()=>Vis.
    tpe()=> Type.
    
    disp(_) =>
	n([l(["import ",explode(Name)," from "]),s(collapse({explode(P)..P in Pkg},"."))]).
  }.

  iDef:[actTree,fileLoc] @= progTree.
  iDef(_,Lc)<=obj(Lc).
  iDef(Act,_) ..{
    name()=>'$'.
    defines('$').
    vis() => privAte.
    tpe() => voidType.
    disp(X) => n([s("${"),Act.disp(X),s("}.\n")]).
  }.
  
  cDef:[symbol,visibility,termTree,typeTree,fileLoc]@=progTree.
  cDef(_,_,_,_,Lc)<=obj(Lc).
  cDef(Name,Vis,iVal,Tp,Lc)..{
    name() => Name.
    defines(Name).
    vis() => Vis.
    tpe() => Tp.
    
    disp(Pr)::infOp('=',_,dP,dR) =>
	n([showPar(Pr,dP,`\(),s(explode(Name)),s("="),iVal.disp(dR),showPar(Pr,dP,`\)),s(".\n")]).
  }.

  vDef:[symbol,visibility,termTree,typeTree,fileLoc]@=progTree.
  vDef(_,_,_,_,Lc)<=obj(Lc).
  vDef(Name,Vis,iVal,Tp,Lc)..{
    name() => Name.
    defines(Name).
    vis() => Vis.
    tpe() => Tp.
    
    disp(Pr)::infOp(':=',_,dP,dR) =>
	n([showPar(Pr,dP,`\(),s(explode(Name)),s(":="),iVal.disp(dR),showPar(Pr,dP,`\)),s(".\n")]).
  }.

  tDef:[symbol,visibility,typeTree,fileLoc]@=progTree.
  tDef(_,_,_,Lc)<=obj(Lc).
  tDef(Name,Vis,Tp,Lc)..{
    name() => Name.
    defines(Name).
    vis() => Vis.
    tpe() => Tp.
    
    disp(Pr) => Tp.disp(Pr).
  }.

  pkgTree <~ objTree.

  pkg:[symbol,list[(symbol,symbol)],typeTree,list[progTree],fileLoc]@=pkgTree.
  pkg(_,_,_,_,Lc)<=obj(Lc).
  pkg(Pkg,Imports,Export,Progs,_)..{
    disp(_) => n([s(explode(Pkg)),
		  s("{\n"),
		  n(showList(Progs,2000,"","")),
		  s("\n}")]).
  }.
}