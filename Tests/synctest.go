/*
 * Test various forms of synchronized actions
 */
 
synctest{
  import go.io.

  OO[T] <~ { get:[]=>T. set:[T]* }.

  OO:[]@>OO[number].
  OO()..{
    bal:number := 0.
    get()=> valof{
              sync{
                valis bal
              }
              timeout (now()+0.34 -> valis -1)
            }.
    set(X) -> sync{
          bal := X
        }
  }.
  
  route1:[OO[number]]*.
  route1(Ob) ->
      sync(Ob){
        Ob.get()>0 -> Ob.set(Ob.get()-rand(4));stdout.outLine("Route 1 = "<>Ob.get().show())
      } timeout (now()+1.0 -> stdout.outLine("Route1 timed out"));
      delay(rand(2));
      route1(Ob).
    
  route2:[OO[number]]*.
  route2(Ob) -> 
    sync(Ob){
	Ob.set(Ob.get()+rand(10));stdout.outLine("Route 2 = "<>Ob.get().show())
    };
    delay(rand(5));
    route2(Ob).
    
  main(_) ->
      O = OO();
      spawn{ route1(O) };
      route2(O).
}