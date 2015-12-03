/*
 * Test the stack module
 */

stacks{
  import go.io.
  import go.stack.
  import go.unit.

  stacks:[]@>harness.
  stacks()<=harness.
  stacks()..{
    S:stack[integer] = stack([1,2,3]).

    doAction() ->
        stdout.outLine("Stack is "<>S.show());
        stdout.outLine("Top of stack = "<>S.tos().show());
        S.drop();
        stdout.outLine("After drop, top of stack is now "<>S.tos().show());
        stdout.outLine("Stack is "<>S.show());
        S.push(10);
        stdout.outLine("After push 10, top of stack is "<>S.tos().show());
        stdout.outLine("Stack is "<>S.show());
        S.update(-20);
        stdout.outLine("After update, with -20 tos = "<>S.tos().show());
        stdout.outLine("Stack is "<>S.show());
        S.pop(E1);
        stdout.outLine("Popped 1:"<>E1.show());
        stdout.outLine("Stack is "<>S.show());
        S.pop(E2);
        stdout.outLine("Popped 2:"<>E2.show());
        stdout.outLine("Stack is "<>S.show());
        S.pop(E3);
        stdout.outLine("Popped 3:"<>E3.show());
        stdout.outLine("Stack is "<>S.show()).
    errorAction()->
        SS = stack([]);
        SS.pop(E5);
        stdout.outLine("Popped 5:"<>E5.show()).  -- should never get here
  }.

  main(_) ->
      checkUnit(stacks()).
}
