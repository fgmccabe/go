/*
 * Test the environment variables stuff
 */
 
env{
  import go.unit.
  import go.io.
   
  showEnv:[list[(symbol,string)]]*.
  showEnv([]) -> {}.
  showEnv([(K,V),..L]) -> stdout.outLine(K.show()<>":"<>V);
      showEnv(L).
  
  envtest:[]@=harness.
  envtest<=harness.
  envtest..{
    doAction() ->
        showEnv(envir());
        stdout.outLine("USER="<>getenv('USER',"not found"));
        {setenv('myKey',"This is an environment var")};
        stdout.outLine("myKey="<>getenv('myKey',"not found"));
        showEnv(envir()).
  }.

  main(_) ->
      checkUnit(envtest);
}
