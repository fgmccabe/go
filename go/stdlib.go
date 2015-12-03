/* Standard library to support Go! programs
   (c) 2004-2005 F.G. McCabe
 
   This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Contact: Francis McCabe <frankmccabe@mac.com>
 */

go.stdlib{
  -- foundation entity for the object system

  thing:[]$=thing.
  thing..{
    show() => __stringOf(this,0,0).

/*    __meta(Vr)::var(this), present(Vr,this,0,Nx) => metaVar(Nx).
    __meta(_) =>raise error("cannot generate meta of "<>__stringOf(this,0,0),
			    'eINVAL').

    present:[list[thing]-+,thing+,integer+,integer-]{}.
    present(Vrs,Vt,Cn,Cn) :: var(Vrs):-- [Vt,.._]=Vrs.
    present([Vi,.._],Vt,Cx,Cx) :: Vi==Vt :-- {}.
    present([_,..L],Vt,Cn,Cx) :-- present(L,Vt,Cn+1,Cx).

    _meta() => quantify(this.__meta(V),0,V).

    quantify:[meta,integer,list[thing]-+]=>meta.
    quantify(M,_,[]) => M.
    quantify(M,Cx,[_,..Vx]) => metaQuant(Cx,quantify(M,Cx+1,Vx)).
*/
  }.

  reverse:[list[T]] =>list[T].
  reverse(L) => rv(L,[]).
  
  private rv:[list[T],list[T]] =>list[T].
  rv([],L) => L.
  rv([E,..L],R) => rv(L,[E,..R]).
  
  iota:[integer,integer] =>list[integer].
  iota(N,M)::N>M=>[].
  iota(N,M)=>[N,..iota(N+1,M)].

  nth:[list[T],integer] =>T.
  nth([E,.._],1) => E.
  nth([_,..L],N) => nth(L,N-1).
  nth([],_) => raise error("nth",'eNOTFND').
  nth(L,_)::var(L)=>raise error("nth",'eINSUFARG').
  
  listlen:[list[_]] =>integer.
  listlen(L) => len(L,0).
  
  private len:[list[_],integer] =>integer.
  len([],C) => C.
  len([_,..L],C) => len(L,C+1).
  len(L,_)::var(L) => raise error("listlen",'eINSUFARG').
  
  (<>):[list[t],list[t]] =>list[t].
  []<>X=>X.
  [E,..X]<>Y=>[E,..X<>Y].

  append:[list[t],list[t],list[t]]{}.
  append([],X,X).
  append([E,..X],Y,[E,..Z]) :- append(X,Y,Z).

  front:[list[t],integer] =>list[t].
  front([],_) => [].
  front([E,..L],C)::C>0 => [E,..front(L,C-1)].
  front(_,0) => [].

  -- This definition is convoluted because it does the last N elements in a single
  -- recursion

  tail:[list[t],integer] =>list[t].
  tail(L,C)::__lst(L,C,_Cx,LL) => LL.

  private __lst:[list[t],integer+,integer-,list[t]]{}.
  __lst([],_,0,[]).
  __lst([E,..L],C,Cx,Lx) :-
      __lst(L,C,Cy,Ly),
      (Cy<C?
         Lx=[E,..Ly],
         Cx=Cy+1
    |  Lx=Ly,
       Cx=Cy).

  drop:[list[t],integer]=>list[t].
  drop([],_)=>[].
  drop(L,0) => L.
  drop([_,..L],N) => drop(L,N-1).

  expand:[list[t],list[t]]=>list[list[t]].
  expand([],_) => [].
  expand(Str,Pre) => expnd(Str,[],Pre).

  private expnd:[list[t],list[t],list[t]]=>list[list[t]].
  expnd([],soFar,_) => [reverse(soFar)].
  expnd(Str,soFar,Pre) :: append(Pre,Tail,Str) => [reverse(soFar),..expand(Tail,Pre)].
  expnd([C,..S],soFar,Pre) => expnd(S,[C,..soFar],Pre).

  collapse:[list[list[t]],list[t]]=>list[t].
  collapse([],_) => [].
  collapse([E],_) => E.
  collapse([E,..L],Join) => coll(E,Join,L).

  private coll:[list[t],list[t],list[list[t]]]=>list[t].

  coll([],Join,L) => cll(Join,Join,L).
  coll([E,..X],Join,L) => [E,..coll(X,Join,L)].

  private cll:[list[t],list[t],list[list[t]]]=>list[t].
  cll([],Join,L) => collapse(L,Join).
  cll([E,..J],Join,L) => [E,..cll(J,Join,L)].

  -- Support for the bagof expression
  -- Even though it is private, the compiler has this name hard coded in
  private bagTest[T] <~ { check:[T]{} }.

  private bagExec[T] <~ { bagOf:[]=>list[T]. }.

  private bagOf:[bagTest[T]]=>list[T].
  bagOf(Chk:bagTest[T]) => 
      (:bagExec[T]..{
	 B:list[symbol] := [].

	 bagOf() => valof{
		      (Chk.check(E) *> B:=[__term(E),..B]); -- check is the key to getting answers
		      valis extract(B,[]) -- extracts and reverses the list of collected solutions
		    }.
	 private extract:[list[symbol],list[T]]=>list[T].
	 extract([],L)=>L.
	 extract([Cl,..L],R)::__is(Cl,E) => extract(L,[valof{
							 __retract(Cl);
							 valis E
						       },..R]).
       }).bagOf().

  error:[string,symbol]@=exception.
  error(Cause,Code)..{
    cause()=>Cause.
    code()=>Code.
    show()=>"error: "<>Cause<>": ("<>__errorcode(Code)<>")".
  }.

  -- Used in sort related functions
  comparable[t] <~ { less:[t,t]{}. equal:[t,t]{}}.

  -- Threads and handles
  processState ::=   quiescent          -- process has yet to execute 
                 |   runnable	       -- process is running or available to run
		  |  wait_io             -- process is waiting for I/O
		  |  wait_term  -- process is waiting for another thread to terminate
		  |  wait_timer          -- waiting for an interval times
		  |  wait_lock           -- Waiting for a lock to be released
		  |  wait_child          -- Wait for a child process
		   | wait_rendezvous	-- Waiting for a GC rendezvous
		    | in_exclusion	-- in an exclusion zone
		  |  dead.               -- process has died

  thread <~ { start:[]*. creator:[]=>thread. start_thread:[]* }.

  thread:[]@>thread.
  thread() .. {
    creator()=>__thread().

    start()->{}.		      -- this is overridden by the actual thread

    start_thread() ->                    -- this is the actual entry point
        this.start() 
        onerror(
         E -> {__logmsg("Thread: "<>show()<>" dying with: "<>E.show())}
        ).
 
   ${
      __fork(this)         -- this is the guts of the thread: fork a new process
    }
  }.

  nullThread:[]@=thread.
  nullThread..{
    creator() => raise error("not supported",'eFAIL').
    start() -> raise error("not supported",'eFAIL').
    start_thread() -> raise error("not supported",'eFAIL').
  }.
  
  private rootThread:[]@>thread.
  rootThread()<=thread().
  rootThread()..{
    show()=>"rootThread".
  }.

  -- implement the list interface for lists
  []..{
    eof().
    cons(X) => [X].
    tack(X) => [X].
    hdtl(_,_) :- false.
    show() => "[]".
    head() => raise error("no element",'eFAIL').
    tail() => raise error("no element",'eFAIL').
    eq([]).
  }.

  [H,..T]..{
    eof() :- false.
    hdtl(H,T).
    cons(X) => [X,..this].

    show() => flatten(["[",H.show(),..showList(T)]).

    head()=>H.
    tail()=>T.

    tack(X) => [H,..T.tack(X)].
    eq([H,..T]).

    showList:[list[t]] =>list[list[char]].

    showList([]) => ["]"].
    showList([El,..R]) => [",",El.show(),..showList(R)].
    showList(El) => [",..",El.show(),"]"].
  }.

  -- implement the tupling constructor
  (X,Y)..{
    show() => "("<>X.show()<>","<>Y.show()<>")".
  }.

  -- All literal symbols are instances of the symbol class
  symbol:[]@=symbol.
  symbol..{
    show() => explode(this).

--    __meta:[list[thing]-+]=>meta.
--    __meta(_) => metaSymbol(this).
  }.

  -- All literal characters are instances of the char class
  char:[]@=char.
  char..{
    show() => __stringOf(this,0,0).

--    __meta:[list[thing]-+]=>meta.
--    __meta(_) => metaChar(this).
  }.

  number:[]@=number.
  number..{
    show() => num2str(this,0,0,false,false).
    equal(this).
    less(X) :- X<this.
  }.

  -- All literal integers are automatically instances of this class
  integer:[]@=integer.
  integer<=number.
  integer..{
    show() => int2str(this,10,0,` ).

--    __meta:[list[thing]-+]=>meta.
--    __meta(_) => metaInteger(this).
  }.

  -- All literal floats are automatically instances of this class
  float:[]@=float.
  float<=number.
  float..{
    show() => num2str(this,0,0,false,true).

--    __meta:[list[thing]-+]=>meta.
--    __meta(_) => metaFloat(this).
  }.

  max:[n<~number,n]=>n.
  max(X,Y)::X>Y =>X.
  max(_,Y) => Y.

  min:[n<~number,n]=>n.
  min(X,Y)::X<Y =>X.
  min(_,Y) => Y.

  -- Each element of the trigger list must implement this type
  -- Its private here, but known to the compiler and to the machine

  private delayTrigger <~ { trigger:[]{} }.

  private delayHandler:[list[delayTrigger]+]{}.
  delayHandler(L) :- delayH(L)!.

  private delayH:[list[delayTrigger]+]{}.
  delayH([]).
  delayH([E,..L]) :- E.trigger(), delayH(L).

  flatten:[list[list[s]]] => list[s].
  flatten([]) => [].
  flatten([S,..L]) => fltn(S,L).
  
  private fltn:[list[t],list[list[t]]] => list[t].
  fltn([],L) => flatten(L).
  fltn([C,..S],L) => [C,..fltn(S,L)].

  -- implement the logical constructor
  false..{
    show() => "false".
  }.

  true..{
    show() => "true".
  }.

/*  metaVar:[integer]@=meta.
  metaVar(Nm)..{
    show()=>[`_,..Nm.show()].

--    __reflect(V)::(metaVar(Nm),VV) in V => VV.
  }.

  metaSymbol:[symbol]@=meta.
  metaSymbol(Sy)..{
    show()=>[`\',..explode(Sy)]<>"'".

    __reflect(_) => Sy.
  }.

  metaInteger:[integer]@=meta.
  metaInteger(In)..{
    show() => In.show().
    __reflect(_) => In.
  }.

  metaFloat:[float]@=meta.
  metaFloat(Ft)..{
    show() => Ft.show().

    __reflect(_) => Ft.
  }.

  metaChar:[char]@=meta.
  metaChar(Ch)..{
    show() => [`\`,Ch].

    __reflect(_) => Ch.
  }.

  metaCons:[symbol,list[meta]]@=meta.
  metaCons(Nm,Args)..{
    show()::Nm='go.stdlib#,',Args=[L,R] => "("<>L.show()<>", "<>R.show()<>")".
    show()::Nm='go.stdlib#,..',Args=[L,R] => flatten(["[",L.show(),..showList(R)]).
    show()::Args==[] => explode(Nm).
    show() => explode(Nm)<>"("<>showArgs(Args,"",",")<>")".

    showList:[meta]=>list[list[char]].
    showList(metaCons('go.stdlib#[]',[])) => ["]"].
    showList(metaCons('go.stdlib#,..',[L,R]))=>[",",L.show(),..showList(R)].
    showList(El) => [",..",El.show(),"]"].

    __reflect(V) => __univ(Nm,{A.__reflect(V) .. A in Args}).
  }.

  metaSquare:[symbol,list[meta]]@=meta.
  metaSquare(Nm,Args)..{
    show() => explode(Nm)<>"["<>showArgs(Args,"",",")<>"]".

    __reflect(V) => __univ(Nm,{A.__reflect(V) .. A in Args}).
  }.

  metaBrace:[meta,list[meta]]@=meta.
  metaBrace(Nm,Args)..{
    show() => Nm.show()<>"{"<>showArgs(Args,"",".\n")<>"}".

    __reflect(_) => raise error("cannot reflect "<>this.show(),'eINVAL')
  }.

  private showArgs:[list[meta],string,string]=>string.
  showArgs([],_,_)=>"".
  showArgs([A,..rgs],Sep,Next) => Sep<>A.show()<>showArgs(rgs,Next,Next).

  private metaQuant:[integer,meta]@=meta.
  metaQuant(Vr,Bound)..{
    show() => [`_,..Vr.show()]<>" - "<>Bound.show().

    __reflect(V) => Bound.__reflect([(metaVar(Vr),_),..V]).
  }
*/
}
