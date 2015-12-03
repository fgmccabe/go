/*
 * The event definition part of the Widget library
 */
graph.event{
  import graph.xlib.
  import graph.graph.
  import go.io.

  event <~ {
	loc:[]=>point.
	type:[]=>eventType.
	shiftEvent:[point]=>event.
      }.

  keyPressEvent:[integer,float,float,string,integer,integer]@=event.
  keyPressEvent(Time,x,y,stroke,key,mask)..{
    loc()=> point(x,y).
    type()=>keyPress.
    shiftEvent(point(X,Y)) => keyPressEvent(Time,x-X,y-Y,stroke,key,mask).
  }.

  keyReleaseEvent:[integer,float,float,string,integer,integer]@=event.
  keyReleaseEvent(Time,x,y,stroke,key,mask)..{
    loc()=> point(x,y).
    type()=>keyRelease.
    shiftEvent(point(X,Y)) => keyReleaseEvent(Time,x-X,y-Y,stroke,key,mask).
  }.

  btnPressEvent:[integer,float,float,float,float,integer,integer]@=event.
  btnPressEvent(Time,x,y,xRoot,yRoot,btn,mask)..{
    loc()=> point(x,y).
    type()=>btnPress.
    shiftEvent(point(X,Y)) => btnPressEvent(Time,x-X,y-Y,xRoot,yRoot,btn,mask).
  }.

  btnReleaseEvent:[integer,float,float,float,float,integer,integer]@=event.
  btnReleaseEvent(Time,x,y,xRoot,yRoot,btn,mask)..{
    loc()=> point(x,y).
    type()=>btnRelease.
    shiftEvent(point(X,Y)) => btnPressEvent(Time,x-X,y-Y,xRoot,yRoot,btn,mask).
  }.

  enterEvent:[integer,float,float,float,float,integer]@=event.
  enterEvent(Time,x,y,xRoot,yRoot,mask)..{
    loc()=> point(x,y).
    type()=>enterNotify.
    shiftEvent(point(X,Y)) => enterEvent(Time,x-X,y-Y,xRoot,yRoot,mask).
  }.

  leaveEvent:[integer,float,float,float,float,integer]@=event.
  leaveEvent(Time,x,y,xRoot,yRoot,mask)..{
    loc()=> point(x,y).
    type()=>leaveNotify.
    shiftEvent(point(X,Y)) => leaveEvent(Time,x-X,y-Y,xRoot,yRoot,mask).
  }.

  motionEvent:[integer,float,float,float,float,integer]@=event.
  motionEvent(Time,x,y,xRoot,yRoot,mask)..{
    loc()=> point(x,y).
    type()=>motionNotify.
    shiftEvent(point(X,Y)) => motionEvent(Time,x-X,y-Y,xRoot,yRoot,mask).
  }.

  btnMotionEvent:[integer,float,float,float,float,integer]@=event.
  btnMotionEvent(Time,x,y,xRoot,yRoot,mask)..{
    loc()=> point(x,y).
    type()=>motionNotify.
    shiftEvent(point(X,Y)) => btnMotionEvent(Time,x-X,y-Y,xRoot,yRoot,mask).
  }.

  configureEvent:[float,float,integer,integer]@=event.
  configureEvent(x,y,width,height)..{
    loc()=> point(x,y).
    type()=>configureNotify.
    shiftEvent(point(X,Y)) => configureEvent(x-X,y-Y,width,height).
  }.

  exposeEvent:[float,float,float,float,integer]@=event.
  exposeEvent(x,y,width,height,count)..{
    loc()=> point(x,y).
    type()=>exposeNotify.
    shiftEvent(point(X,Y)) => exposeEvent(x-X,y-Y,width,height,count).
  }.

  createEvent:[float,float,integer,integer,integer]@=event.
  createEvent(x,y,width,height,border)..{
    loc()=> point(x,y).
    type()=>createNotify.
    shiftEvent(point(X,Y)) => createEvent(x-X,y-Y,width,height,border).
  }.

  destroyEvent:[]@=event.
  destroyEvent..{
    loc()=> raise error("No location for destroy",'eNOPERM').
    type()=>destroyNotify.
    shiftEvent(_) => this.
  }.

  mapEvent:[]@=event.
  mapEvent..{
    loc()=> raise error("No location for map",'eNOPERM').
    type()=>mapNotify.
    shiftEvent(_) => this.
  }.

  unmapEvent:[]@=event.
  unmapEvent..{
    loc()=> raise error("No location for unmap",'eNOPERM').
    type()=>unmapNotify.
    shiftEvent(_) => this.
  }.

  focusInEvent:[]@=event.
  focusInEvent..{
    loc()=> raise error("No location for focus in",'eNOPERM').
    type()=>focusInNotify.
    shiftEvent(_) => this.
  }.

  focusOutEvent:[]@=event.
  focusOutEvent..{
    loc()=> raise error("No location for focus out",'eNOPERM').
    type()=>focusOutNotify.
    shiftEvent(_) => this.
  }.
}