xtest{
  import go.io.
  import go.xml.
  import go.unit.

  xmltest:[list[string]]@=harness.
  xmltest(_)<=harness.
  xmltest(Fls)..{
    doAction() ->
        F in Fls *> (
         (_,DOM) = grabXML("file:///"<>fcwd()<>"/",F);
         stdout.outLine("DOM is "<>DOM.show());
	 stdout.outLine("XML is "<>DOM.xml())).
  }.

  main([]) ->
      checkUnit(xmltest(["fact.xml"])).
  main(Fls) ->
      checkUnit(xmltest(Fls)).
}.
    
    