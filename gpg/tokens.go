/*
 * The token types for the GpG parser generator
 */
gpg.tokens{

  assoc ::= leftA | rightA | nonA | tokenA.

  yyType ::= N(number) 
	   | I(string)
	   | GPGRULESET(yyType,yyType)
	   | SEQ(yyType,yyType)
	   | TOKENSPEC(yyType)
	   | OPERATORSPEC(yyType,assoc)
	   | STARTSPEC(string)
	   | EXPECTSPEC(integer)
	   | APPLY(string,string)
	   | TYPESPEC(string)
	   | RULESET(string,yyType)
	   | CHOICE(yyType,yyType)
	   | RULE(yyType,yyType,yyType)
	   | EMPTY.

}