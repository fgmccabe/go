sieve{
  import go.io.
  import go.unit.
  import go.stdparse.

  sieve:[list[logical],integer]=>list[integer].
  sieve([],_)=>[].
  sieve([true,..L],I) => [I,..sieve(filter(L,1,I),I+1)].
  sieve([false,..L],I) => sieve(L,I+1).

  filter:[list[logical],integer,integer]=>list[logical].
  filter([],_,_)=>[].
  filter([_,..L],P,P)=>[false,..filter(L,1,P)].
  filter([B,..L],Ix,P) => [B,..filter(L,Ix+1,P)].

  trues:[integer]=>list[logical].
  trues(0)=>[].
  trues(Ix)=>[true,..trues(Ix-1)].

  testSieve:[integer]@=harness.
  testSieve(_)<=harness.
  testSieve(N)..{
    doAction() ->
	stdout.outLine("Primes up to "<>N.show()<>"\t "<>listlen(sieve(trues(N),2)).show())
  }.

  main([]) -> checkUnit(testSieve(128)).
  main([N]) -> checkUnit(testSieve(naturalOf%%N)).
}