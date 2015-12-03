gpg.genrules{
  import go.io.
  import go.setlib.
  import gpg.abstract.
  import gpg.gpgTypes.

  -- Extract the token specifications from the %token declarations
  private extractTokenSpecs:[yyType,integer,integer-]=>list[(symbol,tokenPriority,string)].
  extractTokenSpecs(EMPTY,Pr,Pr)=>[].
  extractTokenSpecs(SEQ(L,R),iPr,oPr)=>
      valof{
	lT = extractTokenSpecs(L,iPr,tPr);
	rT = extractTokenSpecs(R,tPr,oPr);
	valis lT<>rT
      }.
  extractTokenSpecs(TOKENSPEC(toks),iPr,iPr)=> extTokSpec(toks,tA).
  extractTokenSpecs(OPERATORSPEC(toks,Ass),iPr,oPr)=>
      valof{
	TS = extTokSpec(toks,mapAssoc(Ass,iPr));
	oPr = iPr+1;
	valis TS
      }.
  extractTokenSpecs(_,Pr,Pr)=>[].

  private extTokSpec:[yyType,tokenPriority]=>list[(symbol,tokenPriority,string)].
  extTokSpec(APPLY(tok,_),A)=>[(implode(tok),A,tok<>"($$)")].
  extTokSpec(SEQ(L,R),A)=>extTokSpec(L,A)<>extTokSpec(R,A).
  extTokSpec(I(t),A) => [(implode(t),A,t)].

  private mapAssoc:[assoc,integer]=>tokenPriority.
  mapAssoc(nonA,Pr)=>nA(Pr).
  mapAssoc(leftA,Pr)=>lA(Pr).
  mapAssoc(rightA,Pr)=>rA(Pr).
  mapAssoc(tokenA,_)=>tA.
  
  private orderPriorities:[list[(symbol,tokenPriority,string)],
			   list[symbol],list[(symbol,tokenPriority)],list[(symbol,string)]
			   ]=>
      (list[symbol],list[(symbol,tokenPriority)],list[(symbol,string)]).
  orderPriorities([],T,P,TD)=>(T,P,TD).
  orderPriorities([(t,a,td),..LL],T,P,TD)=>
      orderPriorities(LL,[t,..T],[(t,a),..P],[(t,td),..TD]).

  private explore:[yyType,yyType]{}.
  explore(X,X).
  explore(SEQ(L,_),X) :- explore(L,X).
  explore(SEQ(_,R),X) :- explore(R,X).

  private extractPreamble:[yyType]=>string.
  extractPreamble(SEQ(L,R))=>extractPreamble(L)<>extractPreamble(R).
  extractPreamble(I(t)) => t.
  extractPreamble(_)=>"".

  private extractRules:[yyType,list[(symbol,tokenPriority)],integer,integer-]=>list[rule].
  extractRules(SEQ(L,R),Pr,Rin,Rout)=>
      valof{
	lR = extractRules(L,Pr,Rin,Ri);
	rR = extractRules(R,Pr,Ri,Rout);
	valis lR<>rR
      }.
  extractRules(RULESET(Nm,Prod),Pr,Rno,Rout) => extractRule(Prod,implode(Nm),Pr,Rno,Rout).
  extractRules(_,_,R,R)=>[].

  private extractRule:[yyType,symbol,list[(symbol,tokenPriority)],integer,integer-]=>
      list[rule].
  extractRule(CHOICE(L,R),Nm,Pr,Rin,Rout)=>
      valof{
	lR = extractRule(L,Nm,Pr,Rin,Ri);
	rR = extractRule(R,Nm,Pr,Ri,Rout);
	valis lR<>rR
      }.
  extractRule(RULE(Prod,Action,Prec),Nm,Pr,Rin,Rout) =>
      valof{
	Rout = Rin+1;
	Rhs = extractProduction(Prod);
	valis [rule(Rout,Nm,Rhs,[],extractAction(Action),extractPriority(Prec,Rhs,Pr))]
      }.
  extractRule(_,_,_,R,R)=>[].

  private extractProduction:[yyType]=>list[symbol].
  extractProduction(SEQ(L,R))=>extractProduction(L)<>extractProduction(R).
  extractProduction(I(Nm)) => [implode(Nm)].
  extractProduction(_)=>[].
  
  private extractAction:[yyType]=>string.
  extractAction(I(txt))=>txt.
  extractAction(_)=>"$1".

  private extractPriority:[yyType,list[symbol],list[(symbol,tokenPriority)]]=>tokenPriority.
  extractPriority(I(Tk),_,Pr)::nT=implode(Tk),(nT,tPr) in Pr => tPr.
  extractPriority(EMPTY,Rhs,Pr)::nT in Rhs, (nT,tPr) in Pr => tPr.
  extractPriority(_,_,_) => nA(0).

  extractRuleset:[yyType]=>(list[symbol],list[symbol],list[(symbol,tokenPriority)],
			    list[(symbol,string)],list[rule],symbol,string).
  extractRuleset(GPGRULESET(tokTree,ruleTree))=>
      valof{
	TkSpecs = extractTokenSpecs(tokTree,0,Pr);
--	stdout.outLine("Highest priority is "<>Pr.show());
	(T,Prs,TD) = orderPriorities(TkSpecs,['#','$'],[('#',nA(0)),('$',nA(0))],[('$',"EOF")]);
	Rules = extractRules(ruleTree,Prs,0,Rcount);
--	stdout.outLine(Rcount.show()<>" rules found");
	Start = (explore(tokTree,STARTSPEC(S))?implode(S)|
		 [rule(_,Nm,_,_,_,_),.._].=Rules ? Nm |
		 raise error("empty rule set",'eFAIL'));

	valis (T,setof({NNm..rule(_,NNm,_,_,_,_) in Rules}),Prs,TD,Rules,Start,
	       extractPreamble(tokTree))
      }.
}