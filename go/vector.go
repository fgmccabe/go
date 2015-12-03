vector{
  -- Vector manipulations
  import go.hash.

  vector[t] <~ { ix:[integer]=>t. set:[integer,t]*. count:[]=>integer. }.


  vector:[integer]@>vector[t].
  vector(len)..{
    cont:hash[integer,t] = hash([],len).

    ix(K) => cont.find(K).
    set(K,Vl) -> cont.insert(K,Vl).
    count()=>cont.count().
  }.
}

