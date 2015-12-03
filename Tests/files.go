/*
 * Test some of the file and directory handling stuff
 */
files{
  import go.io.
  import go.unit.

  files:[]@=harness.
  files<=harness.
  files..{
    doAction() ->
        CWD = fcwd();
        stdout.outLine("Current directory = "<>CWD);
        (F in fls(fcwd()) *>
         stdout.outLine(F));
        fcd("..");
        stdout.outLine("Current directory is now "<>fcwd());
        fcd(CWD);
        stdout.outLine("Current directory is now "<>fcwd()).
  }.

  main(_) ->
      checkUnit(files).
}


  