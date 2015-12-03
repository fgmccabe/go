-- check out the cell library functionality

celltest{
  import go.cell.
  import go.io.
  import go.unit.

  celltest:[list[string]]@=harness.
  celltest(_)<=harness.
  celltest(I)..{
    doAction() ->
        C = cell(I);
        stdout.outLine("C = "<>C.get().show());
        C.set(["foo"]);
        stdout.outLine("C = "<>C.get().show()).
  }.

  main(Cmd) ->
      checkUnit(celltest(Cmd)).
}

