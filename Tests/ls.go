-- Test the pipe mechanism

ls{
  import go.io.
  import go.unit.
  import go.datelib.

  ls:[string]*.
  ls(Dir) ->
      ((X,Tp,Perm) in ffileList(Dir) *>
        Fl = Dir<>"/"<>X;
	showFile(Tp,Perm,Fl))
      onerror(
       error(_,Code) ->
           stdout.outLine(Dir<>":"<>__errorcode(Code))
      ).

  setMode:[filePerm,list[char]]=>list[char].
  setMode(setUid,[_,G,S,ru,wu,xu,rg,wg,xg,ro,wo,xo]) => 
      [`U,G,S,ru,wu,xu,rg,wg,xg,ro,wo,xo].
  setMode(setGid,[U,_,S,ru,wu,xu,rg,wg,xg,ro,wo,xo]) => 
      [U,`G,S,ru,wu,xu,rg,wg,xg,ro,wo,xo].
  setMode(stIcky,[U,G,_,ru,wu,xu,rg,wg,xg,ro,wo,xo]) =>
      [U,G,`S,ru,wu,xu,rg,wg,xg,ro,wo,xo].
  setMode(rUsr,[U,G,S,_ru,wu,xu,rg,wg,xg,ro,wo,xo]) => 
      [U,G,S,`r,wu,xu,rg,wg,xg,ro,wo,xo].
  setMode(wUsr,[U,G,S,ru,_wu,xu,rg,wg,xg,ro,wo,xo]) => 
      [U,G,S,ru,`w,xu,rg,wg,xg,ro,wo,xo].
  setMode(xUsr,[U,G,S,ru,wu,_xu,rg,wg,xg,ro,wo,xo]) => 
      [U,G,S,ru,wu,`x,rg,wg,xg,ro,wo,xo].
  setMode(rGrp,[U,G,S,ru,wu,xu,_rg,wg,xg,ro,wo,xo]) => 
      [U,G,S,ru,wu,xu,`r,wg,xg,ro,wo,xo].
  setMode(wGrp,[U,G,S,ru,wu,xu,rg,_wg,xg,ro,wo,xo]) => 
      [U,G,S,ru,wu,xu,rg,`w,xg,ro,wo,xo].
  setMode(xGrp,[U,G,S,ru,wu,xu,rg,wg,_xg,ro,wo,xo]) => 
      [U,G,S,ru,wu,xu,rg,wg,`x,ro,wo,xo].
  setMode(rOth,[U,G,S,ru,wu,xu,rg,wg,xg,_ro,wo,xo]) => 
      [U,G,S,ru,wu,xu,rg,wg,xg,`r,wo,xo].
  setMode(wOth,[U,G,S,ru,wu,xu,rg,wg,xg,ro,_wo,xo]) => 
      [U,G,S,ru,wu,xu,rg,wg,xg,ro,`w,xo].
  setMode(xOth,[U,G,S,ru,wu,xu,rg,wg,xg,ro,wo,_xo]) => 
      [U,G,S,ru,wu,xu,rg,wg,xg,ro,wo,`x].

  showModes:[list[filePerm]]=>list[char].
  showModes(M) =>
      showMds(M,[` ,` ,` ,`-,`-,`-,`-,`-,`-,`-,`-,`-]).
  
  showMds:[list[filePerm],string]=>string.
  showMds([],M) => M.
  showMds([Md,..D],M) => showMds(D,setMode(Md,M)).

  showDate:[float] => string.
  showDate(Tm) => time2date(Tm).show().

  showFile:[fileType,list[filePerm],string]*.
  showFile(plainFile,M,X) -> 
      __file_date(X,Ac,Md,Cr);
      stdout.outLine(showModes(M)<>" "
		     <>showDate(Ac)<>" "
		     <>showDate(Md)<>" "
		     <>showDate(Cr)<>" "<>X).
  showFile(directory,_,".") -> stdout.outLine(".").
  showFile(directory,_,"..") -> stdout.outLine("..").
  showFile(directory,M,X) -> 
      __file_date(X,Ac,Md,Cr);
      stdout.outLine(showModes(M)<>" "
		     <>showDate(Ac)<>" "
		     <>showDate(Md)<>" "
		     <>showDate(Cr)<>" "
		     <>X<>"/"); ls(X) .
  showFile(O,M,X) -> 
      __file_date(X,Ac,Md,Cr);
      stdout.outLine(showModes(M)
		     <>showDate(Ac)<>" "
		     <>showDate(Md)<>" "
		     <>showDate(Cr)<>" "<>X<>O^).

  lstest:[string]@=harness.
  lstest(_)<=harness.
  lstest(D)..{
    doAction() ->
        ls(D).
  }.

  main([]) ->
      checkUnit(lstest(".")).
  main(Args) ->
      I in Args *>
      checkUnit(lstest(I)).
}.
