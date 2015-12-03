graph.graph{
--  import go.io.

  graphContext <~ {
	win:[]=>integer.
	fg:[]=>integer.
	bg:[]=>integer.
      }.

  gc:[integer,integer,integer]@=graphContext.
  gc(wId,F,B)..{
    win()=>wId.
    fg()=>F.
    bg()=>B.
  }.

  shape <~ {
	paint:[graphContext,rectangle]*.
      }.

  rectangle <~ shape.
  rectangle <~ {
	intersects:[rectangle]{}.
      }.

  rectangle:[float,float,float,float]@=rectangle.
  rectangle(x,y,w,h)..{
    paint(GC,R) ->
	rectangle(X,Y,W,H) = intersectRect(this,R);
	xFillRectangle(GC.win(),GC.bg(),itrunc(X),itrunc(Y),itrunc(W),itrunc(H));
	xDrawRectangle(GC.win(),GC.fg(),itrunc(X),itrunc(Y),itrunc(W),itrunc(H)).

    intersects(rectangle(X,Y,W,H)) :-
	( x=<X, X=<x+w,
	  y=<Y, Y=<y+h
	| X=<x, x=<X+W,
	  Y=<y, y=<Y+H
	).

  }.

  point <~ shape.
  point:[float,float] @= point.
  point(X,Y)..{
    paint(GC,_) ->
	xDrawPoint(GC.win(),GC.fg(),itrunc(X),itrunc(Y)).
  }.

  dimension <~ {
	width:[]=>float.
	height:[]=>float.
      }.

  dim:[float,float]@=dimension.
  dim(W,H) .. {
    width()=>W.
    height()=>H.
  }.

  intersectRect:[rectangle,rectangle]=>rectangle.
  intersectRect(rectangle(x,y,w,h),rectangle(X,Y,W,H))=>
      valof{
	xX = max(x,X);
	yY = max(y,Y);
	wW = max(min(x+w,X+W)-xX,0.0);
	hH = max(min(y+h,Y+H)-yY,0.0);
	valis rectangle(xX,yY,wW,hH)
      }.

  orientation ::= horizontal | vertical.

  max:[N<~number,N]=>N.
  max(X,Y)::X>Y => X.
  max(_,Y) => Y.

  min:[N<~number,N]=>N.
  min(X,Y)::X<Y => X.
  min(_,Y) => Y.
}
	
