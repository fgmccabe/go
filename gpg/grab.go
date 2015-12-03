gpg.grab{
  import go.io.

  grabData:[string]=>(string,string,string).
  grabData(fn)=>splitUp(split(suckIn(fn),[])).

  private suckIn:[string]=>string.
  suckIn(Fl) =>
      valof{
        f = openInFile(Fl,unknownEncoding);
        valis f.inText("");           -- This will read the entire file
        f.close();
      }.

  private split:[string,string]=>(string,string).
  split([],L) => (reverse(L),[]).
  split([`%,`%,`\n,..R],L) => (reverse(L),R).
  split([C,..R],L) => split(R,[C,..L]).
  
  private splitUp:[(string,string)]=>(string,string,string).
  splitUp((Pre,[])) => 
      valof{
	(T,Post) = split(Pre,[]);
	valis ([],T,Post)
      }.
  splitUp((Pre,P)) => 
      valof{
	(T,Post) = split(P,[]);
	valis (Pre,T,Post)
      }.
}.

