package week2

/*
Summary
State and assignments
make our mental model of computation
more complicated.
In particular,
we lose
'referential transparency'.
On the other hand,
'assignments' allow us to
formulate certain programs in
an elegant way.
Example: discrete event simulation.
>> Here,
  a system is
  represented by
  a mutable list of actions.
>> The effect of actions,
  when they are called,
  change
  the state of objects and
  can also
  install other actions to
  be executed in the future.
*/
object test {
  /*
  'Circuits' extends 'Gates'
  'Gates' extends 'Simulation'
  'Simulation' defines 'run()'
  */
  object sim extends Circuits with Parameters
  /*Setting Up a Simulation
  Here is
  a sample simulation that
  you can
  do in the worksheet.
  Define
  four wires and
  place some probes.*/
  //'import' for convenience
  //else do not work
  import sim._

  val in1, in2, sum, carry = new Wire             //> in1  : week2.test.sim.Wire = week2.Gates$Wire@1be847c
                                                  //| in2  : week2.test.sim.Wire = week2.Gates$Wire@d64342
                                                  //| sum  : week2.test.sim.Wire = week2.Gates$Wire@1ba5a14
                                                  //| carry  : week2.test.sim.Wire = week2.Gates$Wire@11baa65

  halfAdder( in1, in2, sum, carry )
  /* ??? 'probe' to
  monitor / check the value change ??? */
  /* only output values are interesting &
  make sense to check */
  /*value printed when
  it is changes from old or
  it is all new*/
  probe( "sum", sum )                             //> sum 0 new-value = false
  probe( "carry", carry )                         //> carry 0 new-value = false

  /*Launching the Simulation
  Now
  give the value 'true' to 'input1' and
  launch the simulation:*/
  in1 setSignal true
  run()                                           //> *** simulation started, time = 0 ***
                                                  //| sum 5 new-value = true
                                                  //| sum 10 new-value = false
                                                  //| sum 10 new-value = true
  /*To continue:*/
  in2 setSignal true
  run()                                           //> *** simulation started, time = 10 ***
                                                  //| carry 13 new-value = true
                                                  //| sum 18 new-value = false
  in1 setSignal false
  run()                                           //> *** simulation started, time = 18 ***
                                                  //| carry 21 new-value = false
                                                  //| sum 26 new-value = true
  /*setUp*/
  orGate(
    in1 = in1,
    in2 = in2,
    output = carry )
  /* only output values are interesting &
  make sense to check */
  probe( "carry", carry )                         //> carry 26 new-value = false
  /*start run*/
  in1 setSignal true
  run()                                           //> *** simulation started, time = 26 ***
                                                  //| carry 28 new-value = true
                                                  //| carry 28 new-value = true
                                                  //| sum 33 new-value = false
  in2 setSignal true
  run()                                           //> *** simulation started, time = 33 ***
  in1 setSignal false
  run()                                           //> *** simulation started, time = 33 ***
                                                  //| carry 36 new-value = false
                                                  //| carry 36 new-value = false
                                                  //| sum 41 new-value = true
  /*end run*/
  /*setUp*/
  orGateAlt(
    in1 = in1,
    in2 = in2,
    output = carry )
  probe( "carry", carry )                         //> carry 41 new-value = false
  /*start run*/
  in1 setSignal true
  run()                                           //> *** simulation started, time = 41 ***
                                                  //| carry 44 new-value = true
                                                  //| carry 44 new-value = true
                                                  //| carry 44 new-value = true
                                                  //| sum 49 new-value = false
  in2 setSignal true
  run()                                           //> *** simulation started, time = 49 ***
  in1 setSignal false
  run()                                           //> *** simulation started, time = 49 ***
                                                  //| carry 52 new-value = false
                                                  //| carry 52 new-value = false
                                                  //| carry 52 new-value = false
                                                  //| carry 54 new-value = true
                                                  //| carry 54 new-value = true
                                                  //| carry 54 new-value = true
                                                  //| sum 57 new-value = true
                                                  //| sum 59 new-value = false
  /*end run*/

  /*
  'Wires' transport 'signals'
  that are
  transformed by 'components'.
  We represent 'signals' using
  booleans
  'true' and 'false'.
  */
  /*
  Exercise
  What logical function does this program describe ?*/
  //from
  //http://en.wikipedia.org/wiki/Logical_connective
  //http://en.wikipedia.org/wiki/Logical_equivalence
  /*
  ~(~p) == p Double negation law
  ( p || q ) || r == p || ( q || r ) Associative laws
  ( p && q ) && r == p && ( q && r ) Associative laws
  p || ( q && r ) == ( p || q ) && ( p || r ) Distributive laws
  p && ( q || r ) == ( p && q ) || ( p && r ) Distributive laws
  ~( p && q ) == ~p || ~q  De Morgan's laws
  ~( p || q )== ~p && ~q  De Morgan's laws
  p || ( p && q ) == p Absorption laws
  p && ( p || q ) == p Absorption laws
  */
  def f( a: Wire, b: Wire, c: Wire ): Unit = {
    val d, e, f, g = new Wire

    inverter( a, d )
    //d = ~a
    inverter( b, e )
    //e = ~b
    andGate( a, e, f )
    // f = a && e = a && (~b)
    andGate( b, d, g )
    // g = b && d = b && (~a)
    orGate( f, g, c )
    // c = f || g = (a && ~b) || (b && ~a)
    //a && ~b == ~(~a || b)
    //b && ~a == ~(~b || a)
  }                                               //> f: (a: week2.test.sim.Wire, b: week2.test.sim.Wire, c: week2.test.sim.Wire)
                                                  //| Unit
  /*
	1) a & ~b
	2) a & ~(b & a)
	3) b & ~a
	4) a == b
	5*) a != b //true only a & b different
	6) a * b
	*/

  /*Setting Up a Simulation*/
  var aWire = new Wire                            //> aWire  : week2.test.sim.Wire = week2.Gates$Wire@f438e
  var bWire = new Wire                            //> bWire  : week2.test.sim.Wire = week2.Gates$Wire@c7da1e
  var cWire = new Wire                            //> cWire  : week2.test.sim.Wire = week2.Gates$Wire@1464ce8

  /*aWire.setSignal( true )
	bWire.setSignal( true )
	cWire.setSignal( true )*/
  val gateTest1 = f( aWire, bWire, cWire )        //> gateTest1  : Unit = ()
  /* ??? return initial values ???
  then monitor signal changes in
  the 'attached' wires*/
  probe(
    name = "aWire",
    wire = aWire )                                //> aWire 59 new-value = false
  probe(
    name = "bWire",
    wire = bWire )                                //> bWire 59 new-value = false
  /* only output values are interesting &
  make sense to check */
  probe(
    name = "cWire",
    wire = cWire )                                //> cWire 59 new-value = false
  /*ending Setting Up a Simulation*/

  /*Launching the Simulation*/
  /*after signal changes*/
  aWire.setSignal( false )
  run()                                           //> *** simulation started, time = 59 ***
                                                  //| cWire 61 new-value = true
                                                  //| cWire 66 new-value = false
  bWire.setSignal( false )
  run()                                           //> *** simulation started, time = 66 ***
  cWire.setSignal( false )
  run()                                           //> *** simulation started, time = 66 ***
  /*f( aWire, bWire, cWire )*/
  aWire.getSignal                                 //> res0: Boolean = false
  bWire.getSignal                                 //> res1: Boolean = false
  cWire.getSignal                                 //> res2: Boolean = false
  /*end run*/
  
  /*re-run with new values*/
  aWire.setSignal( true )                         //> aWire 66 new-value = true
  run()                                           //> *** simulation started, time = 66 ***
                                                  //| cWire 76 new-value = true
  bWire.setSignal( false )
  run()                                           //> *** simulation started, time = 76 ***
  cWire.setSignal( false )                        //> cWire 76 new-value = false
  run()                                           //> *** simulation started, time = 76 ***
  //f( aWire, bWire, cWire )
  aWire.getSignal                                 //> res3: Boolean = true
  bWire.getSignal                                 //> res4: Boolean = false
  cWire.getSignal                                 //> res5: Boolean = false
  /*end run*/
  
  /*re-run with new values*/
  aWire.setSignal( false )                        //> aWire 76 new-value = false
  run()                                           //> *** simulation started, time = 76 ***
  bWire.setSignal( true )                         //> bWire 86 new-value = true
  run()                                           //> *** simulation started, time = 86 ***
                                                  //| cWire 96 new-value = true
  cWire.setSignal( false )                        //> cWire 96 new-value = false
  run()                                           //> *** simulation started, time = 96 ***
  /*f( aWire, bWire, cWire )*/
  aWire.getSignal                                 //> res6: Boolean = false
  bWire.getSignal                                 //> res7: Boolean = true
  cWire.getSignal                                 //> res8: Boolean = false
  /*end run*/
  
  /*
	Exercise
    Question:
      What would change
      in the circuit simulation if
      the implementation of
      'orGateAlt' was used for OR ?
      
    1) Nothing. The two simulations behave the same.
    2) The simulations produce the same events, but
      the indicated times are different.
    3*) The times are different, and
      'orGateAlt' may also
      produce additional events.
    4) The two simulations produce different events altogether.*/
  /*Note:
  as in real (actual) device
  components need
  the time to
  'stabilize' themself to right output
  that means
  before final output value
  component value may changes several times
  */
}