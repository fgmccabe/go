/*
 * A library to implement a simple widget set 
 */
graph.widget{
  import graph.xlib.
  import graph.transform.
  import graph.graph.
  import graph.event.
  import go.io.
  import go.hash.

  widget <~ { 
	realize:[]*.				-- Called when the window is created
	paint:[integer,rectangle]*.		  -- Handle repainting
	event:[eventType,event]*.		 	-- Handle an event
	size:[]=>dimension.
	minSize:[]=>dimension.
	maxSize:[]=>dimension.
	preferredSize:[]=>dimension.
	contains:[point]{}.
	pos:[]=>point.
	bounds:[]=>rectangle.
	resize:[dimension]*.
	setSize:[dimension]*.
	setPos:[point]*.
	setMask:[list[eventType]]*.
	inMask:[eventType]{}.
      }.

  layoutMgr <~ {
	layout:[container]*
      }.

  container <~ widget.
  container <~ {
	layout:[]*.
	setLayoutMgr:[layoutMgr]*.
	add:[widget]*.
	remove:[widget]*.
	children:[]=>list[widget].
	requestNewSize:[widget,dimension]*.
	findWidget:[point]=>widget.
      }.
  
  -- Widget are inherently stateful
  widget:[dimension,dimension,dimension,dimension,container]@>widget. 
  widget(MnSz,Sz,PSz,MxSz,Par)..{
    evMask:list[eventType] := [keyPress,keyRelease,
			       enterNotify,leaveNotify,exposeNotify,
			       focusInNotify,focusOutNotify,
			       btnPress,btnRelease,motionNotify,
			       configureNotify].
    realize()->{}.

    paint(_,_)->
	{}.

    setMask(L) ->
	evMask := L.

    inMask(M) :- M in evMask.

    event(Tp,Ev)::Tp in evMask ->
	stdout.outLine("picking up Event: "<>Ev.show()).
    event(_,Ev) ->
	stdout.outLine("ignored Event: "<>Ev.show()).

    S:dimension := Sz.			-- Current size
    mnS:dimension := MnSz.		-- Minimum size
    mxS:dimension := MxSz.		-- Maximum size
    pS:dimension := PSz.		-- Preferred size

    loc:point := point(0.0,0.0).	-- Current location within parent

    size()=>S.
    minSize()=>mnS.
    maxSize()=>mxS.
    preferredSize()=>pS.

    setSize(nSz) ->
	S := nSz.

    bounds()::point(X,Y).=loc,dim(W,H).=size() => rectangle(X,Y,W,H).

    contains(point(U,V)) :- 
	point(A,B).=loc,
	dim(W,H).=size(),
	U>=A, V>=B, U<A+W, V<B+H.

    resize(nS) ->
	dim(W,H) .= nS;
	dim(mW,mH).=mnS;
	dim(xW,xH).=mxS;
	Par.requestNewSize(this,dim(minmax(W,mW,xW),minmax(H,mH,xH))).

    pos() => loc.
    setPos(Pt) ->
	loc := Pt.
  }.

  -- This is a container, default layout is hrizontal box
  container:[dimension,dimension,dimension,container,layoutMgr]@>container.
  container(MnSz,iSz,MxSz,Par,_) <= widget(MnSz,iSz,iSz,MxSz,Par).
  container(MnSz,iSz,MxSz,Par,Mgr)..{
    els:list[widget] := [].

    mgr:layoutMgr := Mgr.

    setLayoutMgr(M) -> mgr:= M;
	layout().

    layout() ->
	mgr.layout(this).

    preferredSize()=> prefSizeOfChildren(els,0.0,0.0).

    prefSizeOfChildren:[list[widget],float,float]=>dimension.
    prefSizeOfChildren([],W,H) => dim(W,H).
    prefSizeOfChildren([Wdgt,..rWidgets],W,H) =>
	valof{
	  dim(wW,wH).=Wdgt.preferredSize();
	  valis prefSizeOfChildren(rWidgets,W+wW,max(wH,H))
	}.
    
    add(Ch)::\+Ch in els ->
	els:=els<>[Ch];
	layout().
    add(_) -> {}.

    remove(Ch)::append(F,[Ch,..B],els) ->
	els := F<>B;
	layout().

    children()=>els.

    requestNewSize:[widget,dimension]*.
    requestNewSize(W,D)::W in els ->
	pSze .= preferredSize();
	( pSze!=size() ?
	    Par.requestNewSize(this,pSze)
	| W.setSize(D);
	  layout()
	).

    event(Tp,Ev)::inMask(Tp),Wdgt in els, Wdgt.inMask(Tp) ->
	inWidgetEvent(Wdgt,Tp,Ev).
    event(_,_) -> {}.

    inWidgetEvent:[widget,eventType,event]*.
    inWidgetEvent(Wdgt,Tp,Ev) -> Wdgt.event(Tp,Ev.shiftEvent(Wdgt.pos())).

    findWidget(Pt) :: (W::W.contains(Pt)) in els => W.
    findWidget(_) => raise error("not found",'eNOTFND').

    paint(Win,R) ->
	( (Wdgt::Wdgt.bounds().intersects(R)) in els *>
	  Wdgt.paint(Win,intersectRect(R,Wdgt.bounds()))).
  }.

  boxLayout:[orientation] @= layoutMgr.
  boxLayout(Or)..{
    layout(C) ->
	dim(pW,pH) .= C.preferredSize();
	dim(aW,aH) .= C.size();
	layoutKids(C.children(),0.0,0.0,Or,pW/aW,pH/aH).

    layoutKids:[list[widget],float,float,orientation,float,float]*.
    layoutKids([],_,_,_,_,_) -> {}.
    layoutKids([Wdg,..Rest],xPos,yPos,horizontal,xFactor,yFactor) ->
	Wdg.setPos(point(xPos,yPos));
	dim(wW,wH).=Wdg.preferredSize();
	Wdg.setSize(dim(wW/xFactor,wH/yFactor));
	layoutKids(Rest,xPos+wW/xFactor,yPos,horizontal,xFactor,yFactor).
    layoutKids([Wdg,..Rest],xPos,yPos,vertical,xFactor,yFactor) ->
	Wdg.setPos(point(xPos,yPos));
	dim(wW,wH).=Wdg.preferredSize();
	Wdg.setSize(dim(wW/xFactor,wH/yFactor));
	layoutKids(Rest,xPos,yPos+wH/yFactor,vertical,xFactor,yFactor).
  }.

  window:[float,float,float,float,string]@>container.
  window(_,_,W,H,_) <= container(dim(W,H),dim(W,H),displaySize(),this,
				 boxLayout(horizontal)).
  window(X,Y,W,H,title)..{
    
    wId:integer = xTopLevelWindow(itrunc(X),itrunc(Y),itrunc(W),itrunc(H),xColor("pink",_,_,_),title).

    focus:widget := this.

    hasFocus:[widget+]{}.
    hasFocus(focus).

    ${
      xEventMask(wId,[keyPress,keyRelease,
		      enterNotify,leaveNotify,exposeNotify,
		      focusInNotify,focusOutNotify,
		      btnPress,btnRelease,motionNotify,
		      configureNotify]);
      winList.insert(wId,this)
    }.

    realize() ->
	layout();
	xMapWindow(wId,true);
	( Ch in children() *> Ch.realize() ).

    event(keyPress,Ev)::\+hasFocus(this) -> focus.event(keyPress,Ev).
    event(keyRelease,Ev)::\+hasFocus(this) -> focus.event(keyRelease,Ev).
    event(btnPress,Ev)::contains(Ev.loc()) -> 
	findWidget(Ev.loc()).event(btnPress,Ev).
    event(btnRelease,Ev)::contains(Ev.loc()) -> 
	findWidget(Ev.loc()).event(btnPress,Ev).
    event(exposeNotify,
	  exposeEvent(x,y,w,h,_)) ->
	paint(wId,rectangle(x,y,w,h)).
    event(_,Ev) ->
	stdout.outLine("event: "<>Ev.show()<>" ignored").
  }.

  winList:hash[integer,container] = hash([],32).

  private nextEvent:[]*.
  nextEvent()->
      xNextEvent(Tp,Win,x1,x2,x3,x4,x5,x6,x7,st);
      case Tp in (
       keyPress ->
	   distributeEvent(Win,Tp,
			   keyPressEvent(x1,n2float(x2),n2float(x3),st,x4,x5))
     | keyRelease ->
	   distributeEvent(Win,Tp,
			   keyReleaseEvent(x1,n2float(x2),n2float(x3),st,x4,x5))
     | btnPress ->
	   distributeEvent(Win,Tp,
			   btnPressEvent(x1,n2float(x2),n2float(x3),
					 n2float(x4),n2float(x5),x6,x7))
     | btnRelease ->
	   distributeEvent(Win,Tp,
			   btnReleaseEvent(x1,n2float(x2),n2float(x3),
					   n2float(x4),n2float(x5),x6,x7))
     | enterNotify ->
	   distributeEvent(Win,Tp,
			   enterEvent(x1,n2float(x2),n2float(x3),
				      n2float(x4),n2float(x5),x6))
     | leaveNotify ->
	   distributeEvent(Win,Tp,
			   leaveEvent(x1,n2float(x2),n2float(x3),
				      n2float(x4),n2float(x5),x6))
     | motionNotify ->
	   distributeEvent(Win,Tp,
			   motionEvent(x1,n2float(x2),n2float(x3),
				       n2float(x4),n2float(x5),x6))
     | btnMotionNotify ->
	   distributeEvent(Win,Tp,
			   btnMotionEvent(x1,n2float(x2),n2float(x3),
					  n2float(x4),n2float(x5),x6))
     | configureNotify ->
	   distributeEvent(Win,Tp,configureEvent(n2float(x2),n2float(x3),x4,x5))
     | exposeNotify ->
	   distributeEvent(Win,Tp,
			   exposeEvent(n2float(x1),n2float(x2),
				       n2float(x3),n2float(x4),x5))
     | createNotify ->
	   distributeEvent(Win,Tp,createEvent(n2float(x2),n2float(x3),x4,x5,x6))
     | destroyNotify ->
	   winList.delete(Win)
     | reparentNotify ->
	   {}
     | mapNotify ->
	   distributeEvent(Win,Tp,mapEvent)
     | unmapNotify ->
	   distributeEvent(Win,Tp,unmapEvent)
     | focusInNotify ->
	   distributeEvent(Win,Tp,focusInEvent)
     | focusOutNotify ->
	   distributeEvent(Win,Tp,focusOutEvent)
      ).

  private distributeEvent:[integer,eventType,event]*.
  distributeEvent(wId,Tp,Ev)::winList.present(wId,Win) ->
      Win.event(Tp,Ev).
  distributeEvent(wId,_,Ev) ->
      stdout.outLine("Event: "<>Ev.show()<>" for unknown window: "<>wId.show()).

  processEvents:[]*.
  processEvents() ->
      (
       nextEvent()
       onerror(
	error(Bec,_) ->
	    stderr.outLine("Error processing event: "<>Bec)
       )
      );
      processEvents().

/*  ${
    spawn { 
      stdout.outLine("Starting to process events");
      processEvents()
    }
  }.
*/

  private eventMask:[list[eventType],integer]=>integer.
  
  eventMask([],M)=>M.
  eventMask([keyPress,..L],M) => eventMask(L,bor(M,1)).
  eventMask([keyRelease,..L],M) => eventMask(L,bor(M,2)).
  eventMask([btnPress,..L],M) => eventMask(L,bor(M,4)).
  eventMask([btnRelease,..L],M) => eventMask(L,bor(M,8)).
  eventMask([enterNotify,..L],M) => eventMask(L,bor(M,16)).
  eventMask([leaveNotify,..L],M) => eventMask(L,bor(M,32)).
  eventMask([motionNotify,..L],M) => eventMask(L,bor(M,64)).
  eventMask([btnMotionNotify,..L],M) => eventMask(L,bor(M,256)).
  eventMask([exposeNotify,..L],M) => eventMask(L,bor(M,32768)).
  eventMask([configureNotify,..L],M) => eventMask(L,bor(M,131072)).
  eventMask([focusInNotify,..L],M) => eventMask(L,bor(M,2097152)).
  eventMask([focusOutNotify,..L],M) => eventMask(L,bor(M,2097152)).
  eventMask([reparentNotify,..L],M) => eventMask(L,bor(M,1572864)).
  eventMask([mapNotify,..L],M) => eventMask(L,bor(M,65536)).
  eventMask([unmapNotify,..L],M) => eventMask(L,bor(M,65536)).


  private eventType:[integer]=>eventType.
  eventType(2) => keyPress.
  eventType(3) => keyRelease.
  eventType(4) => btnPress.
  eventType(5) => btnRelease.
  eventType(6) => motionNotify.
  eventType(7) => enterNotify.
  eventType(8) => leaveNotify.
  eventType(9) => focusInNotify.
  eventType(10) => focusOutNotify.
  eventType(11) => keyMapNotify.
  eventType(12) => exposeNotify.
  eventType(13) => graphicsExpose.
  eventType(14) => noExpose.
  eventType(15) => visibilityNotify.
  eventType(16) => createNotify.
  eventType(17) => destroyNotify.
  eventType(18) => unmapNotify.
  eventType(19) => mapNotify.
  eventType(20) => mapRequest.
  eventType(21) => reparentNotify.
  eventType(22) => configureNotify.
  eventType(23) => configureRequest.
  eventType(24) => gravityNotify.
  eventType(25) => resizeRequest.
  eventType(26) => circulateNotify.
  eventType(27) => circulateRequest.
  eventType(28) => propertyNotify.
  eventType(29) => selectionClear.
  eventType(30) => selectionRequst.
  eventType(31) => selectionNotify.
  eventType(32) => colorMapRequest.
  eventType(33) => clientMessage.
  eventType(34) => mappingNotify.


  private minmax:[N<~number,N,N]=>N.
  minmax(A,M,_)::A=<M => M.
  minmax(A,_,X)::A>=X => X.
  minmax(A,_,_) => A.
}