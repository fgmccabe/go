/*
 * A library to implement a the label widget
 */
graph.xlabel{
  import graph.widget.
  import graph.graph.
  import graph.event.
  import graph.xlib.
  import go.io.


  private gLbl:[string,font,dimension,dimension,dimension,dimension,container]@>widget.
  gLbl(_,_,mnSze,Sze,prSze,mxSze,Par)<=widget(mnSze,Sze,prSze,mxSze,Par).
  gLbl(Lbl,Fnt,mnSze,Sze,prSze,mxSze,Par)..{
    gc:integer = xCreateGC([gxFontName(Fnt.fontName())]).

    paint(wId,rectangle(X,Y,_,_)) ->
	dim(_,H).=size();
	stdout.outLine("Painting label: "<>Lbl<>" at "<>(X,Y+H).show());
	xDrawString(wId,gc,X,Y+H,Lbl).
  }.

  gLabel:[string,font,container]=>widget.
  gLabel(Lbl,Fnt,Par) =>
      valof{
	Sze = Fnt.stringSize(Lbl);
	valis gLbl(Lbl,Fnt,Sze,Sze,Sze,Sze,Par)
      }.

}