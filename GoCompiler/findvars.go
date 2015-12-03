/*
 * Find the variables in a term/clause/whatever
 */
findvars{
  import errors.
  import opts.
  import abstract.
  import keywords.
  import go.io.
  import misc.
  import types.
  
  findVarsInTerm:[abstract,dict]=>dict.
  findVarsInTerm(T,Outer) => extractVars(findVars(T,[],Outer)).

  findVarsInRule:[abstract,abstract,abstract,dict]=>dict.
  findVarsInRule(Args,Goal,Act,Outer) => 
      extractVars(findActVars(Act,findGoalVars(Goal,findVars(Args,[],Outer),Outer),Outer)).

  findVarsInEqn:[abstract,dict]=>dict.
  findVarsInEqn(APPLY(IDEN('=>',_),[APPLY(IDEN('::',_),[H,G],_),B],_),Outer) =>
      extractVars(findVars(B,findGoalVars(G,findVars(H,[],Outer),Outer),Outer)).
  findVarsInEqn(APPLY(IDEN('=>',_),[H,B],_),Outer) =>
      extractVars(findVars(B,findVars(H,[],Outer),Outer)).

  findVarsInClause:[abstract,dict]=>dict.
  findVarsInClause(APPLY(IDEN(':-',_),[H,B],_),Env) =>
      findVarsInRule(H,B,VOID(noLoc),Env).
  findVarsInClause(APPLY(IDEN(':--',_),[H,B],_),Env) =>
      findVarsInRule(H,B,VOID(noLoc),Env).

  findVarsInActionRule:[abstract,dict]=>dict.
  findVarsInActionRule(APPLY(IDEN('->',_),[APPLY(IDEN('::',_),[H,G],_),B],_),
		       Outer) =>
      findVarsInRule(H,G,B,Outer).
  findVarsInActionRule(APPLY(IDEN('->',_),[H,B],_),Outer) =>
      findVarsInRule(H,VOID(noLoc),B,Outer).

  findVarsInGrammarRule:[abstract,dict]=>dict.
  findVarsInGrammarRule(APPLY(IDEN('-->',_),
			      [APPLY(IDEN(',',_),[H,P],_),B],_),Outer) =>
      extractVars(findGoalVars(B,findVars(P,findVars(H,[],Outer),Outer),Outer)).
  findVarsInGrammarRule(APPLY(IDEN('-->',_),[H,B],_),Outer) =>
      extractVars(findGoalVars(B,findVars(H,[],Outer),Outer)).

  findVarsInType:[abstract,dict]=>dict.
  findVarsInType(T,Outer) => extractVars(findVarsInTp(T,[],Outer)).

  findVarsInClassRule:[abstract,dict]=>dict.
  findVarsInClassRule(APPLY(IDEN('<=',_),[Lb,Sp],_),Outer) => 
      fields({vS(Nm,B,typeVar(vrBind(Nm,topType))) .. 
	       (O::O.name(Nm,B)) in findVars(Lb,findVars(Sp,[],Outer),Outer)}).

  findVarsInClassBody:[abstract,dict]=>dict.
  findVarsInClassBody(APPLY(IDEN('..',_),[H,APPLY(IDEN('{}',_),D,_)],_),Outer) =>
      valof{
	E = findVars(H,[],Outer);
	checkVars(H,E,Outer);
	(Df in D *>checkVars(Df,E,Outer));
	valis extractVars(E)
      }.
    
  /* We use this to keep track of the number of occurrences of a variable */
  occ <~ ( loc:[]=>fileLoc. name:[symbol,typeBinding]{}. inc:[]*. check:[]{}. ).

  occ:[symbol,typeBinding,fileLoc]@>occ.
  occ(Nm,Bn,Lc)..{
    occCount:integer:=0.
    
    inc() -> occCount:=occCount+1.

    count:[]=> integer.
    count() => occCount.

    check()::occCount>0 :-- {}.
    check()::[`_,.._].=explode(Nm) :-- {}.
    check() :-- action{ reportWarning("single occurrence of "<>explode(Nm),Lc)}.
    loc()=>Lc.
    name(Nm,Bn).
  }.
  
  private extractVars:[list[occ]]=>dict.
  extractVars([]) => emptyDict.
  extractVars(Occs) => 
      fields({vS(Nm,B,typeVar(vrBind(Nm,topType))) .. 
	       (O::O.check(), O.name(Nm,B)) in Occs}).

  private findVars:[abstract,list[occ],dict]=>list[occ].
  findVars(VOID(_),Env,_)=>Env.
  findVars(IDEN('[]',_),Env,_)=>Env.
  findVars(IDEN('{}',_),Env,_)=>Env.
  findVars(IDEN('()',_),Env,_)=>Env.
  findVars(IDEN(Nm,_),Env,Outer)::varPresentOn(Nm,Outer) => Env.
  findVars(IDEN(Nm,_),Env,_)::varPrsnt(Nm,Env) => Env.
  findVars(IDEN(Nm,Lc),Env,_) => [occ(Nm,varBind,Lc),..Env].
  findVars(INT(_,_),Env,_)=>Env.
  findVars(FLT(_,_),Env,_)=>Env.
  findVars(SYM(_,_),Env,_)=>Env.
  findVars(CHR(_,_),Env,_)=>Env.
  findVars(STR(_,_),Env,_)=>Env.
  findVars(APPLY(IDEN(':',_),[L,T],_),Env,Outer)=>findVars(L,findVarsInTp(T,Env,Outer),Outer).
  findVars(APPLY(IDEN('{}',_),[APPLY(IDEN('..',_),[P,Sets],_)],_),Env,Outer)=>
      valof{
	EE = findVarsInSets(Sets,Env,Outer);
	checkVars(P,Env,Outer);
	valis EE
      }.
  findVars(APPLY(IDEN('{}',_),[APPLY(IDEN('||',_),[T,G],_)],_),Env,Outer)=>
      valof{
	checkVars(T,Env,Outer);
	checkVars(G,Env,Outer);
	valis Env
      }.
  findVars(APPLY(IDEN('::',_),[T,G],_),Env,Outer)=> findGoalVars(G,findVars(T,Env,Outer),Outer).
  findVars(APPLY(IDEN('@@',_),[T,G],_),Env,Outer)=> findGoalVars(G,findVars(T,Env,Outer),Outer).
  findVars(APPLY(IDEN('<~',_),[L,R],_),Env,Outer)=> findVarsInTp(R,findVars(L,Env,Outer),Outer).
  findVars(BRACE(IDEN('spawn',_),[A],_),Env,Outer)=>
      valof{
	checkVars(A,Env,Outer);
	valis Env
      }.
  findVars(APPLY(IDEN('%%',_),[_,R],_),Env,Outer) => findVars(R,Env,Outer).
  findVars(APPLY(IDEN(',..',_),[L,R],_),Env,Outer)=> findVars(R,findVars(L,Env,Outer),Outer).
  findVars(APPLY(IDEN(',',_),[L,R],_),Env,Outer)=> findVars(R,findVars(L,Env,Outer),Outer).
  findVars(APPLY(IDEN('|',_),[APPLY(IDEN('?',_),[T,L],_),R],_),Env,Outer)=> 
      findGoalVars(T,findVars(R,findVars(L,Env,Outer),Outer),Outer).
  findVars(APPLY(IDEN('case',_),[APPLY(IDEN('in',_),[Ex,Cases],_)],_),Env,Outer)=> 
      valof{
	checkVars(Cases,Env,Outer);
	valis findVars(Ex,Env,Outer)
      }.
  findVars(APPLY(IDEN('onerror',_),[Ex,Cases],_),Env,Outer)=> 
      valof{
	checkVars(Cases,Env,Outer);
	valis findVars(Ex,Env,Outer)
      }.
  findVars(APPLY(IDEN('raise',_),[Ex],_),Env,Outer)=> findVars(Ex,Env,Outer).
  findVars(BRACE(IDEN('valof',_),[Act],_),Env,Outer)=> 
      valof{
	checkVars(Act,Env,Outer);
	valis Env
      }.
  findVars(APPLY(IDEN('.',_),[Ex,_],_),Env,Outer)=> findVars(Ex,Env,Outer).
  findVars(APPLY(IDEN('#',_),_,_),Env,_)=>Env.
  findVars(APPLY(IDEN('$',_),[Ex],_),Env,Outer)=> findVars(Ex,Env,Outer).
  findVars(APPLY(IDEN('@',_),[Ex,G],_),Env,Outer)=> findGoalVars(G,findVars(Ex,Env,Outer),Outer).
  findVars(APPLY(IDEN('..',_),[APPLY(IDEN(':',_),[S,T],_),Th],_),Env,Outer) => 
      valof{
	checkVars(Th,Env,Outer);
	valis findVarsInTp(T,findVars(S,Env,Outer),Outer)
      }.
  findVars(APPLY(IDEN('..',_),[APPLY(IDEN(':',_),[T],_),Th],_),Env,Outer) => 
      valof{
	checkVars(Th,Env,Outer);
	valis findVarsInTp(T,Env,Outer)
      }.
  findVars(APPLY(IDEN('..',_),[S,Th],_),Env,Outer) => 
      valof{
	checkVars(Th,Env,Outer);
	valis findVars(S,Env,Outer)
      }.
  findVars(APPLY(IDEN(_,_),A,_),Env,Outer)=>findVlist(A,Env,Outer).
  findVars(APPLY(F,A,_),Env,Outer)=>findVars(F,findVlist(A,Env,Outer),Outer).

  private findVlist:[list[abstract],list[occ],dict]=>list[occ].
  findVlist([],Ev,_) => Ev.
  findVlist([T,..L],Ev,Outer) => findVlist(L,findVars(T,Ev,Outer),Outer).

  private findVarsInSets:[abstract,list[occ],dict]=>list[occ].
  findVarsInSets(APPLY(IDEN(',',_),[L,R],_),Env,Outer) => 
      findVarsInSets(L,findVarsInSets(R,Env,Outer),Outer).
  findVarsInSets(APPLY(IDEN('in',_),[_,S],_),Env,Outer) => findVars(S,Env,Outer).

  private findGoalVars:[abstract,list[occ],dict]=>list[occ].
  findGoalVars(IDEN('{}',_),Env,_)=>Env.
  findGoalVars(IDEN('true',_),Env,_)=>Env.
  findGoalVars(IDEN('false',_),Env,_)=>Env.
  findGoalVars(IDEN(Nm,_),Env,Outer)::varPresentOn(Nm,Outer) => Env.
  findGoalVars(IDEN(Nm,_),Env,_)::varPrsnt(Nm,Env) => Env.
  findGoalVars(IDEN(Nm,Lc),Env,_) => [occ(Nm,varBind,Lc),..Env].
  findGoalVars(APPLY(IDEN('=',_),[L,R],_),Env,Outer) => findVars(R,findVars(L,Env,Outer),Outer).
  findGoalVars(APPLY(IDEN('.=',_),[L,R],_),Env,Outer) => findVars(R,findVars(L,Env,Outer),Outer).
  findGoalVars(APPLY(IDEN('!=',_),[L,R],_),Env,Outer) => findVars(R,findVars(L,Env,Outer),Outer).
  findGoalVars(APPLY(IDEN('in',_),[L,R],_),Env,Outer) => findVars(R,findVars(L,Env,Outer),Outer).
  findGoalVars(APPLY(IDEN('-->',_),[L,APPLY(IDEN('~',_),[R,S],_)],_),Env,Outer) => 
      findVars(S,findVars(R,findVars(L,Env,Outer),Outer),Outer).
  findGoalVars(APPLY(IDEN('-->',_),[L,R],_),Env,Outer) => findVars(R,findVars(L,Env,Outer),Outer).
  findGoalVars(APPLY(IDEN('!',_),[G],_),Env,Outer) => findGoalVars(G,Env,Outer).
  findGoalVars(APPLY(IDEN('\\+',_),[G],_),Env,Outer) => findGoalVars(G,Env,Outer).
  findGoalVars(APPLY(IDEN('|',_),[L,R],_),Env,Outer) => findGoalVars(R,findGoalVars(L,Env,Outer),Outer).
  findGoalVars(APPLY(IDEN('?',_),[L,R],_),Env,Outer) => findGoalVars(R,findGoalVars(L,Env,Outer),Outer).
  findGoalVars(APPLY(IDEN('*>',_),[L,R],_),Env,Outer) => 
      valof{
	checkVars(L,Env,Outer);
	checkVars(R,Env,Outer);
	valis Env
      }.
  findGoalVars(APPLY(IDEN(',',_),[L,R],_),Env,Outer) => findGoalVars(R,findGoalVars(L,Env,Outer),Outer).
  findGoalVars(APPLY(IDEN('onerror',_),[L,_],_),Env,Outer) => findGoalVars(L,Env,Outer).
  findGoalVars(APPLY(IDEN('raise',_),[R],_),Env,Outer) => findVars(R,Env,Outer).
  findGoalVars(APPLY(IDEN('{}',_),[L],_),Env,Outer) => findGoalVars(L,Env,Outer).
  findGoalVars(BRACE(IDEN('action',_),[A],_),Env,Outer) => findActVars(A,Env,Outer).
  findGoalVars(APPLY(IDEN(_,_),L,_),Env,Outer) => findVlist(L,Env,Outer).
  findGoalVars(APPLY(P,L,_),Env,Outer) => findVlist(L,findVars(P,Env,Outer),Outer).
  findGoalVars(VOID(_),Env,_) => Env.
  findGoalVars(G,_,_) => raise error("Cannot find vars in funny goal: "<>G.show(),'fail').

  findActVars:[abstract,list[occ],dict]=>list[occ].
  findActVars(IDEN('{}',_),Env,_)=>Env.
  findActVars(APPLY(IDEN(';',_),[L,R],_),Env,Outer) => findActVars(R,findActVars(L,Env,Outer),Outer).
  findActVars(APPLY(IDEN(';',_),[L],_),Env,Outer) => findActVars(L,Env,Outer).
  findActVars(APPLY(IDEN('=',_),[L,R],_),Env,Outer) => findVars(R,findVars(L,Env,Outer),Outer).
  findActVars(APPLY(IDEN(':=',_),[L,R],_),Env,Outer) => findVars(R,findVars(L,Env,Outer),Outer).
  findActVars(APPLY(IDEN('{}',_),[L],_),Env,Outer) => findGoalVars(L,Env,Outer).
  findActVars(APPLY(IDEN('!',_),[L],_),Env,Outer) => findGoalVars(L,Env,Outer).
  findActVars(APPLY(IDEN('\\+',_),[L],_),Env,Outer) => findGoalVars(L,Env,Outer).
  findActVars(APPLY(IDEN('|',_),[APPLY(IDEN('?',_),[T,L],_),R],_),Env,Outer) => 
      findActVars(R,findActVars(L,findGoalVars(T,Env,Outer),Outer),Outer).
  findActVars(APPLY(IDEN('*>',_),[L,R],_),Env,Outer) => 
      findActVars(R,findGoalVars(L,Env,Outer),Outer).
  findActVars(APPLY(IDEN('valis',_),[L],_),Env,Outer) => findVars(L,Env,Outer).
  findActVars(APPLY(IDEN('istrue',_),[L],_),Env,Outer) => findVars(L,Env,Outer).
  findActVars(APPLY(IDEN('onerror',_),[L,R],_),Env,Outer) => 
      valof{
	checkVars(R,Env,Outer);
	valis findActVars(L,Env,Outer)
      }.
  findActVars(APPLY(IDEN('case',_),[APPLY(IDEN('in',_),[L,R],_)],_),Env,Outer) => 
      valof{
	checkVars(R,Env,Outer);
	valis findVars(L,Env,Outer)
      }.
  findActVars(APPLY(IDEN('timeout',_),[L,APPLY(IDEN('->',_),[T,A],_)],_),Env,Outer) => 
      valof{
	checkVars(T,Env,Outer);
	checkVars(L,Env,Outer);
	checkVars(A,Env,Outer);
	valis Env
      }.
  findActVars(APPLY(IDEN('->',_),[T,A],_),Env,Outer) => 
      valof{
	checkVars(T,Env,Outer);
	checkVars(A,Env,Outer);
	valis Env
      }.
  findActVars(BRACE(APPLY(IDEN('sync',_),[R],_),[S],_),Env,Outer) => 
      valof{
	checkVars(S,Env,Outer);
	valis findVars(R,Env,Outer)
      }.
  findActVars(BRACE(IDEN('spawn',_),[A],_),Env,Outer) => 
      valof{
	checkVars(A,Env,Outer);
	valis Env
      }.
  findActVars(APPLY(IDEN(_,_),A,_),Env,Outer)=>findVlist(A,Env,Outer).
  findActVars(APPLY(P,A,_),Env,Outer)=>findVars(P,findVlist(A,Env,Outer),Outer).
  findActVars(VOID(_),Env,_) => Env.
  findActVars(A,_,_) => raise error("invalid action in findvars: "<>A.show(),'eFAIL').

  findGrVars:[abstract,list[occ],dict]=>list[occ].
  findGrVars(IDEN('[]',_),Env,_)=>Env.
  findGrVars(IDEN('eof',_),Env,_)=>Env.
  findGrVars(STR(_,_),Env,_)=>Env.
  findGrVars(APPLY(IDEN(',',_),[L,R],_),Env,Outer)=>findGrVars(L,findGrVars(R,Env,Outer),Outer).
  findGrVars(APPLY(IDEN(',..',_),[L,R],_),Env,Outer)=>findVars(L,findVars(R,Env,Outer),Outer).
  findGrVars(APPLY(IDEN('|',_),[APPLY(IDEN('?',_),[T,L],_),R],_),Env,Outer)=>
      findGoalVars(T,findGrVars(L,findGrVars(R,Env,Outer),Outer),Outer).
  findGrVars(APPLY(IDEN('|',_),[L,R],_),Env,Outer)=> findGrVars(L,findGrVars(R,Env,Outer),Outer).
  findGrVars(APPLY(IDEN('::',_),[L,R],_),Env,Outer)=>findGrVars(L,findGoalVars(R,Env,Outer),Outer).
  findGrVars(APPLY(IDEN('\\+',_),[R],_),Env,Outer)=> findGrVars(R,Env,Outer).
  findGrVars(APPLY(IDEN('{}',_),[R],_),Env,Outer)=> findGoalVars(R,Env,Outer).
  findGrVars(APPLY(IDEN('!',_),[R],_),Env,Outer)=> findGrVars(R,Env,Outer).
  findGrVars(APPLY(IDEN('=',_),[L,R],_),Env,Outer)=> findVars(R,findVars(L,Env,Outer),Outer).
  findGrVars(APPLY(IDEN('!=',_),[L,R],_),Env,Outer)=> findVars(R,findVars(L,Env,Outer),Outer).
  findGrVars(APPLY(IDEN('onerror',_),[G,E],_),Env,Outer) =>
      valof{
	checkVars(E,Env,Outer);
	valis findGrVars(G,Env,Outer)
      }.
  findGrVars(APPLY(IDEN('raise',_),[R],_),Env,Outer)=> findVars(R,Env,Outer).
  findGrVars(APPLY(IDEN('action',_),[R],_),Env,Outer)=> findActVars(R,Env,Outer).
  findGrVars(APPLY(IDEN('*',_),[APPLY(IDEN('^',_),_,_),R],_),Env,Outer)=>findVars(R,Env,Outer).
  findGrVars(APPLY(IDEN(_,_),A,_),Env,Outer) => findVlist(A,Env,Outer).
  findGrVars(APPLY(G,A,_),Env,Outer) => findVlist(A,findVars(G,Env,Outer),Outer).
  findGrVars(G,_,_) => raise error("invalid grammar in findvars: "<>G.show(),'eFAIL').

  private findVarsInTp:[abstract,list[occ],dict]=>list[occ].
  findVarsInTp(IDEN('top',_),Env,_)=>Env.
  findVarsInTp(IDEN('void',_),Env,_)=>Env.
  findVarsInTp(IDEN('opaque',_),Env,_)=>Env.
  findVarsInTp(IDEN('string',_),Env,_)=>Env. -- A special hack
  findVarsInTp(IDEN('{}',_),Env,_)=>Env.
  findVarsInTp(IDEN(Nm,_),Env,Outer)::typePresentOn(Nm,Outer) => Env.
  findVarsInTp(IDEN(Nm,_),Env,_)::typePrsnt(Nm,Env) => Env.
  findVarsInTp(IDEN(Nm,Lc),Env,_) => [occ(Nm,tvarBind,Lc),..Env].
  findVarsInTp(APPLY(IDEN(',',_),[L,R],_),Env,Outer) => 
      findVarsInTp(L,findVarsInTp(R,Env,Outer),Outer).
  findVarsInTp(APPLY(IDEN('$=',_),[L,R],_),Env,Outer) => 
      findVarsInTp(L,findVarsInTp(R,Env,Outer),Outer).
  findVarsInTp(APPLY(IDEN('@=',_),[L,R],_),Env,Outer) => 
      findVarsInTp(L,findVarsInTp(R,Env,Outer),Outer).
  findVarsInTp(APPLY(IDEN('@>',_),[L,R],_),Env,Outer) => 
      findVarsInTp(L,findVarsInTp(R,Env,Outer),Outer).
  findVarsInTp(APPLY(IDEN('=>',_),[L,R],_),Env,Outer) => 
      findVarsInTp(L,findVarsInTp(R,Env,Outer),Outer).
  findVarsInTp(APPLY(IDEN('-->',_),[L,R],_),Env,Outer) => 
      findVarsInTp(L,findVarsInTp(R,Env,Outer),Outer).
  findVarsInTp(APPLY(IDEN('*',_),[L],_),Env,Outer) => findVarsInTp(L,Env,Outer).
  findVarsInTp(TPL(L,_),Env,Outer) => findVarsInTpList(L,Env,Outer).
  findVarsInTp(APPLY(IDEN('<~',_),[L,R],_),Env,Outer) => 
      findVarsInTp(L,findVarsInTp(R,Env,Outer),Outer).
  findVarsInTp(BRACE(L,[],_),Env,Outer) =>
      findVarsInTp(L,Env,Outer).
  findVarsInTp(APPLY(IDEN('+',_),[L],_),Env,Outer) => findVarsInTp(L,Env,Outer).
  findVarsInTp(APPLY(IDEN('-',_),[L],_),Env,Outer) => findVarsInTp(L,Env,Outer).
  findVarsInTp(APPLY(IDEN('-+',_),[L],_),Env,Outer) => findVarsInTp(L,Env,Outer).
  findVarsInTp(APPLY(IDEN('{}',_),L,_),Env,Outer) => findVarsInTpList(L,Env,Outer).
  findVarsInTp(APPLY(IDEN(':',_),[IDEN(_,_),L],_),Env,Outer) => findVarsInTp(L,Env,Outer).
  findVarsInTp(APPLY(IDEN('-',_),[IDEN(Nm,_),T],_),Env,Outer) => 
      findVarsInTp(T,Env,Outer.push(Nm,tvarBind,voidType)).
  findVarsInTp(APPLY(IDEN('-',_),[V,T],_),Env,Outer) => 
      valof{
	lV = listIfy(V,',..');
	O = Outer.pushList(collectBoundVars(lV));
	valis findVarsInTp(T,findVarsInTpList({bT .. APPLY(IDEN('<~',_),[_,bT],_) in lV},Env,O),O)
      }.
  findVarsInTp(SQUARE(_,A,_),Env,Outer) => findVarsInTpList(A,Env,Outer).
  findVarsInTp(Tp,Env,_) => 
      valof{
	reportError("funny type expression: "<>Tp.show(),Tp.loc());
	valis Env
      }.

  private findVarsInTpList:[list[abstract],list[occ],dict]=>list[occ].
  findVarsInTpList([],Env,_)=>Env.
  findVarsInTpList([T,..L],Env,Outer) => findVarsInTpList(L,findVarsInTp(T,Env,Outer),Outer).

  private collectBoundVars:[list[abstract]]=>list[vSpec].
  collectBoundVars([])=>[].
  collectBoundVars([APPLY(IDEN('<~',_),[IDEN(V,_),_],_),..lV]) =>
      [vS(V,tvarBind,voidType),..collectBoundVars(lV)].
  collectBoundVars([V,..lV]) =>
      valof{
	reportError("invalid type quantifier: "<>V.show(),V.loc());
	valis collectBoundVars(lV)
      }.

  private checkVars:[abstract,list[occ],dict]*.
  checkVars(IDEN(Nm,_),_,Outer)::varPresentOn(Nm,Outer) -> {}.
  checkVars(IDEN(Nm,_),Env,_)::varPrsnt(Nm,Env) -> {}.
  checkVars(IDEN(_,_),_,_) -> {}.
  checkVars(SYM(_,_),_,_) -> {}.
  checkVars(STR(_,_),_,_) -> {}.
  checkVars(INT(_,_),_,_) -> {}.
  checkVars(FLT(_,_),_,_) -> {}.
  checkVars(CHR(_,_),_,_) -> {}.
  checkVars(APPLY(L,A,_),Env,Outer) -> 
      checkVars(L,Env,Outer);
      (T in A *> checkVars(T,Env,Outer)).
  checkVars(SQUARE(L,A,_),Env,Outer) -> 
      checkVars(L,Env,Outer);
      (T in A *> checkVars(T,Env,Outer)).
  checkVars(BRACE(L,A,_),Env,Outer) -> 
      checkVars(L,Env,Outer);
      (T in A *> checkVars(T,Env,Outer)).
  checkVars(TPL(L,_),Env,Outer) ->
      (T in L *> checkVars(T,Env,Outer)).

  private varPresentOn:[symbol+,dict+]{}.
  varPresentOn(Nm,D) :- D.isbound(Nm,varBind,_).

  private varPrsnt:[symbol+,list[occ]+]{}.
  varPrsnt(Nm,[(O@name(Nm,B)::B in [varBind]),.._]):--action{O.inc()}.
  varPrsnt(Nm,[_,..E]):--varPrsnt(Nm,E).

  private typePresentOn:[symbol+,dict+]{}.
  typePresentOn(Nm,D) :- D.isbound(Nm,B,_), B in [typeBind,tvarBind].

  private typePrsnt:[symbol+,list[occ]+]{}.
  typePrsnt(Nm,[(O@name(Nm,B)::B in [typeBind,tvarBind]),.._]):--action{O.inc()}.
  typePrsnt(Nm,[_,..E]):--typePrsnt(Nm,E).
}
