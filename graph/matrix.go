graph.matrix{
  -- Graphical matrix manipulations
  import go.io.
  
  matrix <~ { 
	inverse:[]=>matrix.
	apply:[float,float]=>(float,float).
	mult:[matrix]=>matrix.
	det:[]=>float.
      }.

  gmtx:[float,float,float,float,float,float]@=matrix.
  gmtx(a11,a12,a21,a22,a31,a32)..{
    apply(u,v) => (u*a11+v*a21+a31,u*a21+v*a22+a32).

    mult(gmtx(b11,b12,b21,b22,b31,b32))=>
	gmtx(a11*b11+a12*b21,a11*b12+a12*b22,a21*b11+a22*b21,a21*b12+a22*b22,
	     a31*b11+a32*b21+b31,a31*b12+a32*b22+b32).

    inverse()=>valof{
		 D = det();
		 (D==0.0?
		    raise error("inverse",'eINVAL'));
		 valis gmtx(a22/D,-a12/D,-a21/D,a11/D,
			    (a21*a32-a22*a31)/D,(a12*a31-a11*a32)/D)
	       }.

    det() => a11*a22-a12*a21.
  }.

  main(_) ->
      T = gmtx(1.0,5.0,1.0,1.0,0.0,-3.0);
      stdout.outLine("T = "<>T.show());
      I = T.inverse();
      stdout.outLine("I = "<>I.show());
      stdout.outLine("TI = "<>T.mult(I).show());
      stdout.outLine("IT = "<>I.mult(T).show());
      stdout.outLine("T.apply(3,4)="<>T.apply(3.0,4.0).show()).
}