/*
 * Implement some of the features needed to support Xlib in Go
 */
graph.xlib{
  import graph.graph.

  -- Color management
  color <~ { r:[]=>integer. b:[]=>integer. g:[]=>integer. px:[]=>integer. }.
  
  color:[integer,integer,integer,integer]@=color.
  color(R,B,G,P)..{
    r()=>R.
    b()=>B.
    g()=>G.
    px()=>P.
  }.

  rgb:[integer,integer,integer]=>color.
  rgb(R,B,G) => color(R,G,B,xRGBColor(R,B,G)).

  namedColor:[string]=>color.
  namedColor(Name) => color(R,G,B,xColor(Name,R,G,B)).

  -- Event management
  eventType ::= keyPress | keyRelease | btnPress | btnRelease
	      | enterNotify | leaveNotify | motionNotify | btnMotionNotify
	      | configureNotify | configureRequest | keyMapNotify
	      | exposeNotify | graphicsExpose | noExpose
	      | visibilityNotify | createNotify | destroyNotify 
	      | reparentNotify | mapNotify | unmapNotify | mapRequest
	      | focusInNotify | focusOutNotify
	      | gravityNotify | resizeRequest 
	      | circulateNotify | circulateRequest
	      | selectionClear | selectionRequest | selectionNotify
	      | colorMapRequest | clientMessage | mappingNotify.

  gcOption ::=
	  gxClear | gxAnd | gxAndReverse | gxCopy | gxNoop | gxXor |
	  gxNor | gxEquiv | gxInvert | gxOrReverse | gxCopyInverted |
	  gxXorInverted | gxNand | gxSet |
	  gxForeground(integer) | gxBackground(integer) |
	  gxLineWidth(integer) | gxLineSolid | 
	  gxOnOffDash(integer,integer) | gxDoubleDash(integer,integer) |
	  gxCapButt | gxCapNoLast | gxCapRound | gxCapProjecting |
	  gxJoinMiter |  gxJoinRound | gxJoinBevel | gxFontName(string).

  displaySize:[]=>dimension.
  displaySize() => dim(n2float(xDisplayWidth()),n2float(xDisplayHeight())).

  font <~ {
	fontName:[]=>string.
	fontSize:[]=>float.
	fontCharacteristics:[]=>(float,float).  -- Ascent, Descent
	charCharacteristics:[char]=>(float,float,float,float,float). -- lBearing, rBearing, advance, ascent, descent
	stringSize:[string]=>dimension.
      }.

  font:[string,float]@>font.
  font(Name,Size)..{
    fontName()=>Name.
    fontSize()=>Size.

    xF:integer = xLoadFont(Name).
    fontCharacteristics()=> 
	valof{
	  xQueryFont(xF,As,Ds);
	  valis (As,Ds)
	}.

    charCharacteristics(Ch)=>
	valof{
	  xCharInfo(xF,Ch,lB,rB,Wd,As,Ds);
	  valis (lB,rB,Wd,As,Ds)
	}.

    stringSize(St) => 
	valof{
	  xStringExtent(xF,St,Ascent,Descent,Width);
	  valis dim(Width,Ascent+Descent)
	}.

  }.

  ${
    xOpenDisplay(getenv('DISPLAY',":0.0"))
  }.
    
}