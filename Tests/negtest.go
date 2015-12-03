negtest{

  import go.io.

  testnum:integer := 0.

  main(_) ->
	  testnum := -128;
	  stdout.outLine( "Test integer (-128)    = "<>testnum.show() );
	   
	  testnum := -129;
	  stdout.outLine( "Test integer (-129)   = "<>testnum.show() );

	  testnum := -200;
	  stdout.outLine( "Test integer (-200)   = "<>testnum.show() );

	  testnum := -256;
	  stdout.outLine( "Test integer (-256)    = "<>testnum.show() );

	  testnum := -257;
	  stdout.outLine( "Test integer (-257)  = "<>testnum.show() );

	  testnum := 200 * -1;
	  stdout.outLine( "Test integer (200*-1) = "<>testnum.show() ).

}
