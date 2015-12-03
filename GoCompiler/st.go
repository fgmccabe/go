st{
  import types.

  private sm:[typeTree,typeTree,dict]*.
  sm(typeVar(U@isVar()),typeVar(V@isVar()),_) -> 
      ( U==V ? {} | U.bind(V)).
}
  

  
