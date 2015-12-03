types{
  import go.showable.
  
  item <~ thing.

  item:[integer,symbol,list[symbol],list[symbol],list[symbol]]@=item.
  item(Rule,Nt,Pre,Pos,LA)..{
    show()=>"Rule : "<>Rule.show()<>" "<>Nt.show()<>" -> "<>Pre.show()<>
	    " . "<>Pos.show().
  }.
  

  actionT ::= shiftOn(symbol,integer,integer)
	   | reduceBy(symbol,integer)
	   | accept(symbol)
	   | recoverError(list[symbol],integer,integer)
	   | disabled(actionT).

  tokenPriority ::= lA(integer) | rA(integer) | nA(integer) | tA.

  rule <~ thing.

  rule:[integer,symbol,list[symbol],list[symbol],string,tokenPriority]@=rule.
  rule(R,NT,Pre,Suff,Act,OpAss)..{
    show() => R.show()<>": "<>NT.show()<>" -> "<>
	      showLits(Pre)<>" . "<>showLits(Suff)<>OpAss.show()<>"{"<>Act<>"}".

    showLits:[list[symbol]]=>string.
    showLits([])=>"".
    showLits([A,..B])=>explode(A)<>" "<>showLits(B).
  }.
}