obTest{
  import go.io.
  import go.unit.

  lbl <~ thing.
  lbl:[symbol,symbol,list[(symbol,symbol)]] @> lbl.
  lbl(A,B,L) .. {
    show() =>
	"A = "<>A.show()<>
	"\nB = "<>B.show()<>
	"\nL = "<>L.show().
  }.

  main(_) ->
      O=lbl(A,B,[('A',A),('B',B)]);
      stdout.outLine(O.show()).
}
