/*
   benchmark, adapted from  BENCH.PL : The classic Prolog benchmark

	Supplied by Quintus Computer Systems, Inc.
	April 30th 1984
*/
bench{
  import go.io.
  import go.unit.

/* ======================================================================
   This benchmark gives the raw speed of a Prolog system.

   The measure of logical inferences per second (Lips) used here is taken to
   be procedure calls per second over an example with not very complex
   procedure calls. The example used is that of "naive reversing" a list,
   which is an expensive, and therefore stupid, way of reversing a list.  It
   does, however, produce a lot of procedure calls. (In theoretical terms,
   this algorithm is O(n^2) on the length of the list).

   The use of a single simple benchmark like this cannot, of course, be
   taken to signify a great deal. However, experience has shown that this
   benchmark does provide a very good measure of basic Prolog speed and
   produces figures which match more complex benchmarks. The reason for
   this is that the basic operations performed here: procedure calls with a
   certain amount of data structure access and construction; are absolutely
   fundamental to Prolog execution. If these are done right, then more
   complex benchmarks tend to scale accordingly. This particular benchmark
   has thus been used as a good rule of thumb by Prolog implementors for
   over a decade and forms a part of the unwritten Prolog folklore. So -
   use this benchmark, with this in mind, as a quick, but extremely useful,
   test of Prolog performance.

   In a complete evaluation of a Prolog system you should also be taking
   account speeds of asserting and compiling, tail recursion, memory
   utilisation, compactness of programs, storage management and garbage
   collection, debugging and editing facilities, program checking and help
   facilities, system provided predicates, interfaces to external
   capabilities, documentation and support, amongst other factors.

   ====================================================================== */


/* ----------------------------------------------------------------------
	get_cpu_time(T) -- T is the current cpu time.

   ---------------------------------------------------------------------- */

  get_cpu_time:[float-]{}.
  get_cpu_time(X):-X=ticks().


/* ----------------------------------------------------------------------
	nrev(L1,L2)	 -- L2 is the list L1 reversed.
	append(L1,L2,L3) -- L1 appended to L2 is L3.
	data(L)		 -- L is a thirty element list.

	This is the program executed by the benchmark.
	It is called "naive reverse" because it is a very expensive way
	of reversing a list. Its advantage, for our purposes, is that
	it generates a lot of procedure calls. To reverse a thirty element
	list requires 496 Prolog procedure calls.
   ---------------------------------------------------------------------- */

  nrev:[list[t],list[t]]{}.
  nrev([],[]).
  nrev([X,..Rest],Ans) :- nrev(Rest,L), append(L,[X],Ans).

  append:[list[t]+,list[t]+,list[t]]{}.
  append([],L,L).
  append([X,..L1],L2,[X,..L3]) :- append(L1,L2,L3).

  data:[list[integer]]{}.
  data([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
        21,22,23,24,25,26,27,28,29,30]).


/* ----------------------------------------------------------------------
	lots -- Run benchmark with a variety of iteration counts.

	Call this to run the benchmark with increasing numbers
	of iterations. The figures produced should be about the same -
	except that there may be inaccuracies at low iteration numbers
	if the time these examples take to execute on your machine are
	too small to be very precise (because of the accuracy the
	operating system itself is capable of providing).
	If the time taken for these examples is too long or short then
	you should adjust the eg_count(_) facts.
   ---------------------------------------------------------------------- */

  lots:[]{}.
  lots() :-
      eg_count(Count),
      bench(Count),
      fail.
  lots().
  
  eg_count:[integer]{}.
  eg_count(10).
  eg_count(20).
  eg_count(50).
  eg_count(100).
  eg_count(200).
  eg_count(500).
  eg_count(1000).
  eg_count(2000).
  eg_count(5000).
  eg_count(10000).
  
  
/* ----------------------------------------------------------------------
 bench(Count) -- Run the benchmark for Count iterations.
 
 bench provides a test harness for running the naive reverse
 benchmark. It is important to factor out the overhead of setting
 the test up and using repeat(_) to iterate the right number of
 times. This is done by running some dummy code as well to see how
 much time the extra operations take.
 ---------------------------------------------------------------------- */
  
  bench:[integer]{}.
  bench(Count) :-
      get_cpu_time(T0),
      dodummy(Count),
      get_cpu_time(T1),
      dobench(Count),
      get_cpu_time(T2),
      report(Count,T0,T1,T2).

/* ----------------------------------------------------------------------
 dobench(Count) -- nrev a 30 element list Count times.
 dodummy(Count) -- Perform the overhead operations Count times.
 repeat(Count)  -- Predicate which succeeds Count times
 
 This is the supporting code, which is reasonably clear.
 ---------------------------------------------------------------------- */
  
  dobench:[integer]{}.
  dobench(Count) :-
      data(List),
      repeat(Count),
      nrev(List,_),
      fail.
  dobench(_).
  
  
  dodummy:[integer]{}.
  dodummy(Count) :-
      data(List),
      repeat(Count),
      dummy(List,_),
      fail.
  dodummy(_).
  
  dummy:[t,t]{}.
  dummy(_,_).
  
  repeat:[integer]{}.
  repeat(_N).
  repeat(N) :- N > 1, repeat(N-1).	
  
  
/* ----------------------------------------------------------------------
 report(Count,T0,T1,T2) -- Report the results of the benchmark.
 calculate_lips(Count,Time,Lips,Units) --
 Doing Count interations in Time implies Lips lips assuming
 that time is given in Units.
 
 This calculates the logical inferences per second (lips) figure.
 Remember that it takes 496 procedure calls to naive reverse a
	thirty element list once. Lips, under this benchmark, thus means
	"Prolog procedure calls per second, where the procedure calls
	are not too complex (i.e. those for nrev and append)".

	** This version of the code assumes that the times (T0.. etc)
	   are integers giving the time in milliseconds. This is true for
	   Quintus Prolog.  Your Prolog system may use some other
	   representation.  If so, you will need to adjust the Lips
	   calculation.  There is a C-Prolog version below for the case
	   where times are floating point numbers giving the time in
	   seconds.
   ---------------------------------------------------------------------- */

  report:[integer,float,float,float]{}.
  report(Count,T0,T1,T2) :-
      Time1 = T1-T0,
      Time2 = T2-T1,
      Time  = Time2-Time1,		/* Time spent on nreving lists */
      calculate_lips(Count,Time,Lips,Units),
      action{
        stdout.outLine(Lips<>" lips for "<>Count.show()<>" iterations taking "<>
                       Time.show()<>" "<>Units)
      }.


  calculate_lips:[integer,float,string-,string]{}.
  calculate_lips(_Count,0.0,"0","msecs") :-- {}. /* Time can be 0 for small */
  calculate_lips(Count,Time,Sp,"secs") :-- Sp=megaLip(n2float(496*Count)/Time).

  megaLip:[float] => string.
  megaLip(0.0) => "0".
  megaLip(L)::L<1000.0 => L.show().
  megaLip(L)::L>=1000.0,L<1000000.0 => (L/1000.0).show()<>"K".
  megaLip(L)::L>=1000000.0 => (L/1000000.0).show()<>"M".

  main(_) ->
      { lots()}.
}

