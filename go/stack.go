/*
 * A simple stack module
 */
go.stack{
  stack[T] <~ {
	tos:[]=>T. 
	push:[T]*. 
	pop:[T]*.
	drop:[]*. 
	update:[T]*. 
	depth:[]=>integer. 
	stack:[]=>list[T]}.
                                                             
  stack:[list[S]]@>stack[S].
  stack(Init)..{
    __stack:list[S] := Init.

    tos() => valof{
	       sync{
                 [H,.._] = __stack;
		 valis H
	       }
	     }.
    push(E) -> sync{ __stack := [E,..__stack] }.
    pop(E) -> sync{ [E,..R] = __stack; __stack := R}.

    drop() -> pop(_).
    update(E) -> sync{ [_,..R] = __stack; __stack := [E,..R]}.

    stack() => valof{
		 sync{
		   valis __stack
		 }
	       }.
    depth() => valof{
		 sync{
		   valis listlen(__stack)
		 }
	       }.

    show() => flatten(showStack(__stack,"{")).

    showStack:[list[S],string]=>list[string].
    showStack([],_) => ["}"].
    showStack([El,..R],Sep) => [Sep,El.show(),..showStack(R,",")].

    flatten:[list[list[t]]]=>list[t].
    flatten([]) => [].
    flatten([S,..L]) => flt(S,L).

    flt:[list[t],list[list[t]]]=>list[t].
    flt([],L) => flatten(L).
    flt([C,..S],L) => [C,..flt(S,L)].
  }
}