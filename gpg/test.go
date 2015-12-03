test{
  yyToken <~ { tokTp:[]=>yyTok. 
	isToken:[yyTok]{}. 
      }.

  yyTok ::= foo | bar.

  item ::= n(yyToken).

  check:[list[item]]{}.
  check([n(T@isToken(foo))]).
}

