/* Miscellaneous support for the Go in Go compiler */
misc{
  private
  count:integer := 0.
  
  genNew:[string]=>symbol.
  genNew(Prefix)::action{ count:=count+1 } => implode(Prefix<>count.show()).
    
  head:[list[t]]=>t.
  head([X,.._]) => X.

  tail:[list[t]]=>list[t].
  tail([_,..X]) => X.

  fileRoot:[string]=>string.
  fileRoot(f)::append(_,[`/,..T],f) => fileRoot(T).
  fileRoot(f)::append(T,[`.,.._],f) => fileRoot(T).
  fileRoot(f) => f.

  expand:[list[t],list[t]]=>list[list[t]].
  expand([],_) => [].
  expand(Str,Pre) => expnd(Str,[],Pre).

  private expnd:[list[t],list[t],list[t]]=>list[list[t]].
  expnd([],soFar,_) => [reverse(soFar)].
  expnd(Str,soFar,Pre) :: append(Pre,Tail,Str) => [reverse(soFar),..expand(Tail,Pre)].
  expnd([C,..S],soFar,Pre) => expnd(S,[C,..soFar],Pre).

  slashify:[list[string],string]=>string.
  slashify(L,S) => collapse(L,S).

  do <~ { do:[]* }.
  do:[]@=do.
  do..{
    do()->{}
  }.
  
}.
