genparse{
  import types.
  import canon.
  import cnf.
  import go.io.
  import go.setlib.

  genTermRecog:[list[rl]] => list[(list[symbol],string)].
  genTermRecog([]) => [].
  genTermRecog([rl(NT,[ch(C)]),..Rules])=>
      valof{
	(NTs,oRules) = extractC(Rules,[NT],[],C);
	valis [(NTs,[`[,C,`,.`.,`.,`X,`]]),..genTermRecog(oRules)]
      }.
  genTermRecog([Rl,..ules])=>genTermRecog(ules).


  private extractC:[char,list[symbol],list[rl],list[rl]]=>(list[symbol],list[rl]).
  extractC(_,NTs,Rls,[]) => (NTs,reverse(Rls)).
  extractC(C,NTs,Rls,[rl(NT,[ch(C)]),..ules]) => extractC(C,NTs\/[NT],Rls,ules).
  extractC(C,NTs,Rls,[rl(NT,Bdy),..ules]) => 
      extractC(C,NTs,[rl(NT,Bdy),..Rls],ules).

}
