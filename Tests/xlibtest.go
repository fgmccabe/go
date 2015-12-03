-- A test of the basic Xlib capabilities
basicwin{
  import go.io.
  import graph.xlib.

  main([]) ->
      xOpenDisplay(getenv('DISPLAY',":0.0"));
      stdout.outLine("Screen is "<>xDisplayWidth().show()<>" by "<>
		     xDisplayHeight().show()<>" pixels");
      W = xTopLevelWindow(0,0,xDisplayWidth() quot 2,xDisplayHeight() quot 2,xColor("pink",_,_,_),"A Trial Balloon");
      xEventMask(W,['EnterWindow','LeaveWindow','Exposure','FocusChange','KeyPress','ButtonPress','ButtonRelease','Resize','ButtonMotion','PointerMotion']);
      xMapWindow(W,true);
      GC = xCreateGC([gxLineWidth(20),
		      gxForeground(xColor("green",_,_,_)),
		      gxBackground(xColor("red",_,_,_)),gxJoinRound]);
      GC2 = xCreateGC([gxLineWidth(20),
		       gxForeground(xColor("red",_,_,_)),gxJoinRound]);
      loop(GC,GC2).

  loop:[integer,integer]*.
  loop(GC,GC2)->
      case xNextEvent() in (
       exposeEvent(win,x,y,w,h,0) -> 
	   /*stdout.outLine("Exposed region: ("<>x.show()<>","<>y.show()<>
			  ") - ("<>w.show()<>","<>h.show()<>") remaining count"
			  <>c.show());*/
	   xDrawRectangle(win,GC,x,y,w,h);
	   xFillOval(win,GC,100,200,200,50);
	   xDrawString(win,GC,100,200,"This is a string");
	   xStringExtent(GC,"This is a string",ascent,descent,width);
	   stdout.outLine("Ascent of str is "<>ascent.show());
	   stdout.outLine("Descent of str is "<>descent.show());
	   stdout.outLine("Width of str is "<>width.show());


     | E -> stdout.outLine("ignored Event: "<>E.show())
      );
      loop(GC,GC2).


}