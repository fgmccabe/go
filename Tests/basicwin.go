-- A test of the widget library
basicwin{
  import go.io.
  import graph.widget.
  import graph.xlabel.
  import graph.xlib.

  main([]) ->
      W = window(0.0,0.0,200.0,100.0,"A trial balloon");
      W.add(gLabel("Hello world",font("-adobe-courier-*-r-*-*-14-*-*-*-*-*-*-*",14.0),W));
      W.add(gLabel("Again",font("-adobe-courier-*-r-*-*-14-*-*-*-*-*-*-*",14.0),W));
      W.add(gLabel("And again",font("-*-helvetica-*-r-*-*-14-*-*-*-*-*-*-*",14.0),W));
--      W.add(gLabel("Hello world",font("9x15",14.0),W));
      W.realize();
      processEvents().
}