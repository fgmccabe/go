/*
 * Error reporting module of the Go! in Go! compiler
 */
errors{
  import go.io.
  import opts.

  eCount:integer := 0.
  wCount:integer := 0.

  errCount:[]=>integer.
  errCount() => eCount.

  wrnCount:[]=>integer.
  wrnCount() => wCount.

  insideEmacs:[]{}.
  insideEmacs() :-
      getenv('EMACS',"")!="".

  report:[outChannel,string,fileLoc,string,string]*.
  report(out,Msg,Loc,_,_) :: insideEmacs() ->
      out.outStr(relativizePath(Loc.url())<>":"<>Loc.from().show()<>":"<>Msg).
  report(out,_,noLoc,_,_) :: insideEmacs() ->
      out.outStr("unknown:0").
  report(out,Msg,Loc,colStart,colStop) ->
      out.outLine(colStart<>Loc.show()<>":\n"<>colStop<>Msg).

  reportError:[string,fileLoc]*.
  reportError(Msg,Lc) ->
      eCount:=eCount+1;
      report(stderr,"Syntax error: "<>Msg,Lc,"\e[31m","\e[0m").

  reportWarning:[string,fileLoc]*.
  reportWarning(Msg,Lc) ->
      wCount:=wCount+1;
      report(stderr,"Warning: "<>Msg,Lc,"\e[34m","\e[0m").

  testErrors:[]*.
  testErrors() ->
      (eCount>0 ? stdout.outLine(eCount.show()<>" error(s)") | {});
      (wCount>0 ? stdout.outLine(wCount.show()<>" warnings(s)") | {});
      (eCount>0?exit(1) | {}).
    
  relativizePath:[string]=>string.
  relativizePath(Pth) => relativize(getenv('PWD',""),Pth).

  relativize:[list[t],list[t]]=>list[t].
  relativize(C,Pth)::append(C,R,Pth) => R.
  relativize(_,Pth) => Pth.
}

