package week2

object orGateTest {
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
  import sim._

  val in1, in2, sum, carry = new Wire             //> in1  : week2.orGateTest.sim.Wire = week2.Gates$Wire@1be847c
                                                  //| in2  : week2.orGateTest.sim.Wire = week2.Gates$Wire@d64342
                                                  //| sum  : week2.orGateTest.sim.Wire = week2.Gates$Wire@1ba5a14
                                                  //| carry  : week2.orGateTest.sim.Wire = week2.Gates$Wire@11baa65
  /*setUp*/
  orGate(
    in1 = in1,
    in2 = in2,
    output = carry )
  /*monitoring changes*/
  /*just for reference*/
  probe( "in1", in1 )                             //> in1 0 new-value = false
  probe( "in2", in2 )                             //> in2 0 new-value = false
  /* only output values are interesting &
  make sense to check */
  probe( "carry", carry )                         //> carry 0 new-value = false
  /*start run*/
  in1 setSignal true                              //> in1 0 new-value = true
  run()                                           //> *** simulation started, time = 0 ***
                                                  //| carry 2 new-value = true
                                                  //| carry 7 new-value = false
                                                  //| carry 7 new-value = true
  carry.getSignal                                 //> res0: Boolean = true
  in2 setSignal true                              //> in2 7 new-value = true
  run()                                           //> *** simulation started, time = 7 ***
  carry.getSignal                                 //> res1: Boolean = true

  in1 setSignal false                             //> in1 12 new-value = false
  run()                                           //> *** simulation started, time = 12 ***
  carry.getSignal                                 //> res2: Boolean = true
  in2 setSignal true
  run()                                           //> *** simulation started, time = 17 ***
  carry.getSignal                                 //> res3: Boolean = true

  in1 setSignal true                              //> in1 17 new-value = true
  run()                                           //> *** simulation started, time = 17 ***
  carry.getSignal                                 //> res4: Boolean = true
  in1 setSignal false                             //> in1 22 new-value = false
  run()                                           //> *** simulation started, time = 22 ***
  carry.getSignal                                 //> res5: Boolean = true
  currentTime                                     //> res6: Int = 27
  /*end run*/
}