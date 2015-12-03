/* A test of the simple STRIPS planner */
stripsworld{
  import go.io.
  import go.setlib.
  import go.unit.
  import go.dynamic.
  
  /* The blocks world */
  state ::= clear(symbol)
	  | holding(symbol)
	  | ontable(symbol)
	  | on(symbol,symbol)
	  | handempty.
  
  act ::= stack(symbol,symbol)
	| unstack(symbol,symbol)
	| pickup(symbol)
	| putdown(symbol).
  
  world<~{ 
	strips_rule:[act,list[state],list[state],list[state]]{}.
        showPlan:[list[act]]=>string
      }.

  planner <~ {
	plan:[list[state],list[state]]=>list[act]
      }.

  blocks:[]@=world.
  blocks..{
    -- basic operators
    strips_rule(stack(X,Y),[clear(Y),holding(X)],
                [on(X,Y),clear(X),handempty],
                [clear(Y),holding(X)]).
    strips_rule(unstack(X,Y),
                [on(X,Y),clear(X),handempty],
                [clear(Y),holding(X)],
                [on(X,Y),clear(X),handempty]).
    strips_rule(pickup(X),
                [ontable(X),clear(X),handempty],
                [holding(X)],
                [ontable(X),clear(X),handempty]).
    strips_rule(putdown(X),
                [holding(X)],
                [ontable(X),clear(X),handempty],
                [holding(X)]).

    showPlan([]) => "".
    showPlan([Op,..More]) => showOp(Op)<>showPlan(More).

    showOp:[act]=>string.
    showOp(stack(A,B)) => "put "<>A^<>" on top of "<>B^<>"\n".
    showOp(unstack(A,B)) => "remove "<>A^<>" from "<>B^<>"\n".
    showOp(pickup(A)) => "pick up block "<>A^<>"\n".
    showOp(putdown(A)) => "put down block "<>A^<>"\n".
  }.

  planner:[world]@>planner.
  planner(W)..{
    plan(Init,Goal)::strips(Init,Goal,[],Plan) => Plan.

    -- This is a progressive planner ... from the state to the goal
    strips:[list[state],list[state],list[act],list[act]]{}.
    strips(State,Goal,_,[]):-
	subset(Goal,State).
    strips(State,Goal,Forbidden,[Ac,..Plan]) :-
        W.strips_rule(Ac,Prec,Adds,Dels),
	subset(Prec,State),
	\+ Ac in Forbidden,
	strips((State\/Adds)\Dels,Goal,[Ac,..Forbidden],Plan).
  }.

  doPlan:[list[state],list[state]]*.
  doPlan(Init,Goal) ->
      stdout.outLine("Goal is "<>Goal^<>" in initial state: "<>Init^);
      stdout.outLine("plan is "<>blocks.showPlan(planner(blocks).plan(Init,Goal))).

  strips:[]@=harness.
  strips<=harness.
  strips..{
    doAction() ->
	doPlan([clear('a'),clear('b'),clear('c'),
		ontable('a'),ontable('b'),ontable('c'),
		handempty],
	       [on('b','c'),on('a','b')]);
	doPlan([clear('c'),
                 ontable('a'),
                 on('b','a'),on('c','b'),
                 handempty],
	       [on('a','b'),on('b','c')]);
	doPlan([clear('b'),clear('c'),
		ontable('a'),ontable('b'),on('c','a'),
		handempty],
	       [on('a','b'),on('b','c')]);
	doPlan([clear('b'),clear('c'),
                  ontable('a'),ontable('b'),on('c','a'),
                  handempty],
	       [on('a','c')]).
  }.

  main(_) ->
      checkUnit(strips).
}.