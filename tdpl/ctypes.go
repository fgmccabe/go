ctypes{
  import go.showable.
  
  cfg <~ showable.

  empty:[]@=cfg.
  empty..{
    disp()=>s("()").
  }.

  Nt:[string,symbol]@=cfg.
  Nt(Vr,Nm)..{
    disp()::Vr==""=>s(explode(Nm)).
    disp() => l([Vr," = ",explode(Nm)]).
  }.

  str:[string]@=cfg.
  str(S)..{
    disp()=>l(["\"",S,"\""]).
  }.

  chars:[string]@=cfg.
  chars(S)..{
    disp()=>n([s("["),s(S),s("]")]).
  }.

  negchars:[string]@=cfg.
  negchars(S)..{
    disp()=>n([s("[^"),s(S),s("]")]).
  }.

  choice:[list[cfg]]@=cfg.
  choice(L)..{
    disp() => n(dispList(L,"","/")).
  }.

  cat:[list[cfg]]@=cfg.
  cat(L)..{
    disp()=>n(dispList(L,""," ")).
  }.

  star:[cfg]@=cfg.
  star(E)..{
    disp()=>n([E.disp(),s("*")]).
  }.

  plus:[cfg]@=cfg.
  plus(E)..{
    disp()=>n([E.disp(),s("+")]).
  }.

  optional:[cfg]@=cfg.
  optional(E)..{
    disp()=>n([E.disp(),s("?")]).
  }.

  period:[]@=cfg.
  period..{
    disp()=>s("@").
  }.

  end:[]@=cfg.
  end..{
    disp()=>s("$").
  }.

  start:[]@=cfg.
  start..{
    disp()=>s("^").
  }.

  rule <~ showable.

  rule:[symbol,list[cfg],string]@=rule.
  rule(N,E,S)..{
    disp() => n([s(explode(N)), s("->"),n(dispList(E,""," ")),showAction()]).

    showAction:[]=>dispTree.
    showAction()::S=="" => s("").
    showAction()=>l(["{ ",S," }"]).
  }.

  private dispList:[list[cfg],string,string]=> list[dispTree].
  dispList([],_,_)=>[].
  dispList([E,..L],S,ep) => [s(S),E.disp(),..dispList(L,ep,ep)].
}