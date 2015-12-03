queuetest{
  import go.io.
  import go.queue.
  import go.unit.

  test:[]@>harness.
  test<=harness.
  test..{
    doPred() :-
        Q = queue([]),
        action{
          Q.push(34);
          Q.depth()==1;
        }.

    doAction() ->
        Q = queue([]);
        stdout.outLine(Q.show());
        Q.push(34);
        stdout.outLine(Q.show());
        Q.push(23);
        stdout.outLine(Q.show());
        Q.pull(E);
        stdout.outLine("Element pulled: "<>E^0);
        stdout.outLine(Q.show());
        stdout.outLine("Queue depth = "<>Q.depth()^0).
  }.


  main(_) ->
      checkUnit(test).

}
