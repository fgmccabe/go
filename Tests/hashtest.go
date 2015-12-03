/*
 * Test the hash table functions
 */
 
hashtest{
  import go.io.
  import go.unit.
  import go.hash.


  hashtest:[]@=harness.
  hashtest<=harness.
  hashtest..{
    doAction() -> 
        XX = hash([('a',`a),('b',`b),('c',`c),('d',`d)],10);
        stdout.outLine("Count = "<>XX.count().show());
        stdout.outLine("Finding a: "<>XX.find('a').show());
        stdout.outLine("Finding b: "<>XX.find('b').show());
        stdout.outLine("Finding c: "<>XX.find('c').show());
        stdout.outLine("Finding d: "<>XX.find('d').show());
      -- stdout.outLine("Finding e: "<>XX.find('e').show());
        XX.insert('aa',`1);
        XX.insert('bb',`2);
        XX.insert('cc',`3);
        XX.insert('dd',`4);
        XX.insert('ee',`5);
        XX.insert('a',`e);
        stdout.outLine("Finding a: "<>XX.find('a')^);
        stdout.outLine("Finding b: "<>XX.find('b')^);
        stdout.outLine("Finding c: "<>XX.find('c')^);
        stdout.outLine("Finding d: "<>XX.find('d')^);
        stdout.outLine("Finding aa: "<>XX.find('aa')^);
        stdout.outLine("Finding bb: "<>XX.find('bb')^);
        stdout.outLine("Finding cc: "<>XX.find('cc')^);
        stdout.outLine("Finding dd: "<>XX.find('dd')^);
        stdout.outLine("Finding ee: "<>XX.find('ee')^);
        stdout.outLine("Table = "<>XX.ext()^);
        
        XX.delete('c');
        stdout.outLine("Table (after deleting 'c') = "<>XX.ext()^);
        
        XX.delete('cc');
        stdout.outLine("Table (after deleting 'cc') = "<>XX.ext()^);
        
        XX.delete('bb');
        stdout.outLine("Table (after deleting 'bb') = "<>XX.ext()^);
        
        (XX.present('a',_) ? stdout.outLine("Finding a: "<>XX.find('a')^) | stdout.outLine("'a' not present"));
        (XX.present('e',_) ? stdout.outLine("Finding e: "<>XX.find('e')^) | stdout.outLine("'e' not present"));
	stdout.outLine("Regular actions done").

    errorAction() ->
        XX = hash([('a',`a),('b',`b),('c',`c),('d',`d)],10);
        stdout.outLine("Count = "<>XX.count()^);
        stdout.outLine("Finding a: "<>XX.find('a')^);
        stdout.outLine("Finding e: "<>XX.find('e')^).

    doPred() :-
	XX = hash([('a',`a),('b',`b),('c',`c),('d',`d)],10),
	action { stdout.outLine("doPred test on "<>XX.ext().show()) },
	XX.present('a',_),
	\+XX.present('e',_),
	XX.present('b',_),
	XX.present('c',`c),
	\+XX.present('c',`d),
	XX.present('d',_).
  }.

  main(_)->
      checkUnit(hashtest).
}
