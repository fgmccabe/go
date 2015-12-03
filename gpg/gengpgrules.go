gpg.gengpgrules{
  /*
   * This is a one-time program to generate the rule-set for gpg itself
   */

  import go.io.
  import gpg.gpgTypes.
  import gpg.gensets.
  import gpg.lr0.
  import gpg.lalr.
  import gpg.genparser.

  rules:list[rule] = [
       rule(1,'gpg',['preamble','MARK','rules','MARK'],[],"GPGRULESET($1,$3)", nA(0)),
       rule(2,'preamble',['definition'],[],"$1",nA(0)),
       rule(3,'preamble',['preamble','definition'],[],"SEQ($1,$2)",nA(0)),
       rule(4,'definition',['tokendef'],[],"$1",nA(0)),
       rule(5,'definition',['includedef'],[],"$1",nA(0)),
       rule(6,'definition',['startdef'],[],"$1",nA(0)),
       rule(7,'definition',['expectdef'],[],"$1",nA(0)),
       rule(8,'tokendef',['TOKEN','tokenspecs'],[], "TOKENSPEC($2)",nA(0)),
       rule(9,'tokendef',['LEFT','tokenspecs'],[],"OPERATORSPEC($2,leftA)",nA(0)),
       rule(10,'tokendef',['RIGHT','tokenspecs'],[],"OPERATORSPEC($2,rightA)",nA(0)),
       rule(11,'tokendef',['NONASSOC','tokenspecs'],[],"OPERATORSPEC($2,nonA)",nA(0)),
       rule(12,'tokenspecs',['tokenspec'],[],"$1",nA(0)),
       rule(13,'tokenspecs',['tokenspecs','tokenspec'],[],"SEQ($1,$2)",nA(0)),
       rule(14,'tokenspec',['ID'],[],"I($1)",nA(0)),
       rule(15,'tokenspec',['ID','BRACE'],[],"APPLY($1,$2)",nA(0)),
       rule(16,'tokenspec',['ID','CENTBRACE'],[],"APPLY($1,$2)",nA(0)),
       rule(17,'includedef',['CENTBRACE'],[],"I($1)",nA(0)),
       rule(18,'startdef',['START','ID'],[],"STARTSPEC($2)",nA(0)),
       rule(19,'expectdef',['EXPECT','INT'],[],"EXPECTSPEC($2)",nA(0)),
       rule(20,'rules',['rule'],[], "$1",nA(0)),
       rule(21,'rules',['rules','rule'],[],"SEQ($1,$2)",nA(0)),
       rule(22,'rule',['ID','COLON','ruleset','SEMI'],[],"RULESET($1,$3)",nA(0)),
       rule(23,'rule',['error','SEMI'],[],"EMPTY",nA(0)),
       rule(24,'ruleset',['ruleset','BAR','production'],[], "CHOICE($1,$3)",nA(0)),
       rule(25,'ruleset',['production'],[],"$1",nA(0)),
       rule(26,'production',['rhs','action'],[],"RULE($1,$2,EMPTY)",nA(0)),
       rule(27,'production',['rhs','PREC','ID','action'],[],"RULE($1,$4,I($3))",nA(0)),
       rule(28,'rhs',['ID'],[], "I($1)",nA(0)),
       rule(29,'rhs',['rhs','ID'],[], "SEQ($1,I($2))",nA(0)),
       rule(30,'rhs',[],[],"EMPTY",nA(0)),
       rule(31,'action',['BRACE'],[], "I($1)",nA(0)),
       rule(32,'action',[],[], "EMPTY",nA(0)),
       rule(33,'ruleset',['error','BAR','ruleset'],[],"$3",nA(0))
  ].

  terms:list[symbol] = ['CENTBRACE','TOKEN','LEFT','RIGHT','NONASSOC','START',
			'EXPECT','MARK','COLON','BAR','SEMI','ID','INT',
			'BRACE','PREC','$','#'].
  precs:list[(symbol,tokenPriority)] = [
       ('START',tA),
       ('ID',tA),
       ('INT',tA),
       ('TOKEN',tA),
       ('LEFT',tA),
       ('RIGHT',tA),
       ('NONASSOC',tA),
       ('PREC',tA),
       ('EXPECT',tA),
       ('BRACE',tA),
       ('CENTBRACE',tA),
       ('COLON',tA),
       ('BAR',tA),
       ('SEMI',tA),
       ('MARK',tA),
       ('$',tA),
       ('#',tA)].
  tokendata:list[(symbol,string)] = [
       ('ID',"ID($$)"),
       ('INT',"INT($$)"),
       ('TOKEN',"TOKEN"),
       ('LEFT',"LEFT"),
       ('RIGHT',"RIGHT"),
       ('PREC',"PREC"),
       ('NONASSOC',"NONASSOC"),
       ('CENTBRACE',"CENTBRACE($$)"),
       ('BRACE',"BRACE($$)"),
       ('START',"START"),
       ('EXPECT',"EXPECT"),
       ('COLON',"COLON"),
       ('MARK',"MARK"),
       ('SEMI',"SEMI"),
       ('BAR',"BAR"),
       ('$',"EOF")].

  nonterms:list[symbol] = [
       'gpg','preamble','definition','tokendef','tokenspecs','tokenspec',
       'includedef','startdef','expectdef','rules','rule','ruleset',
       'production','rhs','action'].

  start:symbol = 'gpg'.

  preamble:string = "import gpg.gpglex.\n"
  "import gpg.abstract.\n".
  postamble:string = 
  "max:[list[integer],integer]=>integer.\n"
  "max([],M)=>M.\n"
  "max([NN,..L],M)::NN>M => max(L,NN).\n"
  "max([_,..L],M) => max(L,M).\n".

  main(_) ->
      N = nullable(nonterms,rules);
      F = first(terms,nonterms,rules,N);
      (T,G) = lr0states(rules,start);
/*      stdout.outLine("T = ");
      ( (Sno,I) in T *>
	stdout.outLine("ste: "<>Sno.show()<>":"<>I.show())
      );
*/
--      stdout.outLine("G = "<>G.show());

      XX = lookAheadK(T,G,rules,F,N);
--      stdout.outLine("XX = ");
--      showStates(XX,G,nonterms);

      Acts = actions(XX,G,nonterms,['error',..terms],precs,rules,F,N);

--      showActions(Acts,stderr);

      rulesFile = openOutFile("gpgrules.go",utf8Encoding);

      rulesFile.outStr(genParser(false,preamble,postamble,
				 "gpg.gpgrules",
				 Acts,G,[rule(0,'?',[start],[],[],nA(0)),..rules],terms,nonterms,
				 tokendata,"yyType"));
      rulesFile.outLine("");
      rulesFile.close().

}

      

