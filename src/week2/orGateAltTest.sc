package week2

object orGateAltTest {
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

  val in1, in2, sum, carry = new Wire             //> in1  : week2.orGateAltTest.sim.Wire = week2.Gates$Wire@1be847c
                                                  //| in2  : week2.orGateAltTest.sim.Wire = week2.Gates$Wire@d64342
                                                  //| sum  : week2.orGateAltTest.sim.Wire = week2.Gates$Wire@1ba5a14
                                                  //| carry  : week2.orGateAltTest.sim.Wire = week2.Gates$Wire@11baa65
  /*setUp*/
  orGateAlt(
    in1 = in1,
    in2 = in2,
    output = carry )
  /*to monitoring changes*/
  probe( "in1", in1 )                             //> in1 0 new-value = false
  probe( "in2", in2 )                             //> in2 0 new-value = false
  /* only output values are interesting &
  make sense to check */
  probe( "carry", carry )                         //> carry 0 new-value = false
  carry.getSignal                                 //> res0: Boolean = false
  /*start run*/
  in1 setSignal true                              //> in1 0 new-value = true
  run()                                           //> *** simulation started, time = 0 ***
                                                  //| carry 5 new-value = true
  carry.getSignal                                 //> res1: Boolean = true
  in2 setSignal true                              //> in2 5 new-value = true
  run()                                           //> *** simulation started, time = 5 ***
  carry.getSignal                                 //> res2: Boolean = true

  in1 setSignal false                             //> in1 10 new-value = false
  run()                                           //> *** simulation started, time = 10 ***
  carry.getSignal                                 //> res3: Boolean = true
  in2 setSignal true
  run()                                           //> *** simulation started, time = 15 ***
  carry.getSignal                                 //> res4: Boolean = true

  in1 setSignal true                              //> in1 15 new-value = true
  run()                                           //> *** simulation started, time = 15 ***
  carry.getSignal                                 //> res5: Boolean = true
  in1 setSignal false                             //> in1 20 new-value = false
  run()                                           //> *** simulation started, time = 20 ***
  carry.getSignal                                 //> res6: Boolean = true

  probe( "carry", carry )                         //> carry 25 new-value = true
  currentTime                                     //> res7: Int = 25
  /*end run*/
  /*probe( "in1", in1 )
  probe( "in2", in2 )*/

}