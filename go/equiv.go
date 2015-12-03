/*
 * A utility package that supports the concept of equivalence classe
 * Each equivalence class has a canonical member, which may have
 * an arbitrary set of properties associated with it
 */
go.equiv{

  Equiv[T] <~ { 
	makeSameAs:[symbol,symbol]*.
	makeDifferentFrom:[symbol,symbol]*.
      }.

  equiv:[list[(symbol,T)],list[(symbol,T)]]@>Equiv[T].
  equiv(Sm,Df)..{
    sameTbl:hash[symbol,T] = hash(Sm,64).
    diffTbl:hash[symbol,T] = hash(Df,64).

  }.
}