/*
 * The abstract parse types for the GpG parser generator
 */
gpg.abstract{

  assoc ::= leftA | rightA | nonA | tokenA.

  yyType ::= N(number) 
	   | I(string)
	   | GPGRULESET(yyType,yyType)
	   | SEQ(yyType,yyType)
	   | TOKENSPEC(yyType)
	   | OPERATORSPEC(yyType,assoc)
	   | STARTSPEC(string)
	   | LEXERSPEC(string)
	   | EXPECTSPEC(integer)
	   | APPLY(string,string)
	   | TYPESPEC(string)
	   | RULESET(string,yyType)
	   | CHOICE(yyType,yyType)
	   | RULE(yyType,yyType,yyType)
	   | EMPTY.

}