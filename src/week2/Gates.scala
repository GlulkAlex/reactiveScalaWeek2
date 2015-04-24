package week2

abstract class Gates extends Simulation {
  /*actual values implemented / instantiated in 'trait Parameters'*/
  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  class Wire {

    private var sigVal = false
    private var actions: List[ Action ] = List()

    /*at the current simulated time*/
    def getSignal = sigVal

    def setSignal( s: Boolean ) =
      if ( s != sigVal ) {
        sigVal = s
        /*runs all stored actions after
         * signal changes*/
        actions foreach ( _() )
        /*or
        actions for ( a <- actions) { a() }*/
      }

    /*
     * Attaches the specified procedure to 
     * the actions of the wire. 
     * All 
     * of the attached actions are 
     * executed 
     * 'at each change' of 
     * the transported signal.*/
    def addAction( a: Action ) = {
      actions = a :: actions
      /*??? perform passed action
       * current or
       * first in list ???*/
      //type Action = () => Unit
      /*immediately perform just added action
       * else wire state never changes with time &
       * be the same forever*/
      a()
    }
  }

  def inverter(
    input: Wire,
    output: Wire ): Unit = {
      def invertAction(): Unit = {
        val inputSig = input.getSignal

        afterDelay( InverterDelay ) {
          output setSignal !inputSig
        }
      }
    /*to perform wire action every time
     * when wire signal changes*/
    input addAction invertAction
  }

  def andGate(
    in1: Wire,
    in2: Wire,
    output: Wire ) = {
      def andAction() = {
        val in1Sig = in1.getSignal
        val in2Sig = in2.getSignal

        afterDelay( AndGateDelay ) {
          output setSignal ( in1Sig & in2Sig )
        }
      }
    in1 addAction andAction

    in2 addAction andAction
  }

  /** Design `orGate` analogously to `andGate` */
  def orGateAlt(
    in1: Wire,
    in2: Wire,
    output: Wire ): Unit = {
      def orAction() = {
        val in1Sig = in1.getSignal
        val in2Sig = in2.getSignal

        afterDelay( OrGateDelay ) {
          output setSignal ( in1Sig | in2Sig )
        }
      }
    in1 addAction orAction

    in2 addAction orAction
  }

  /*Exercise
  What happens if 
  we compute 'in1Sig' and 'in2Sig' inline 
  inside 'afterDelay' instead of 
  computing them as values ?*/
  def orGate2( in1: Wire, in2: Wire, output: Wire ): Unit = {
      def orAction(): Unit = {
        afterDelay( OrGateDelay ) {
          /* ??? wires states changes
           * during or after 'OrGateDelay' time delay ??? */
          output setSignal ( in1.getSignal | in2.getSignal )
        }
      }
    in1 addAction orAction
    in2 addAction orAction
  }
  /*
   * 1) 'orGate' and 'orGate2' have the same behavior.
   * 2*) 'orGate2' does not model OR gates faithfully.*/

  /*Probes
  Before launching the simulation, 
  we still need 
  a way to 
  examine 
  the changes of 
  the signals on the wires.
  To this end, 
  we define 
  the function probe.
   */
  /*works as oscillator attached to the wire*/
  def probe(
    name: String,
    wire: Wire ): Unit = {
      def probeAction(): Unit = {
        println( name + " " + currentTime +
          " new-value = " + wire.getSignal )
        /*or with string formatting
         * //println(s"$name $currentTime value = ${wire.getSignal}")
         * where
         * ${compound.variableName} used with '$' & '{}'
         * */  
      }

    wire addAction probeAction
  }

  /*'Delay' derived from underling functions
   * as they 'sum'
   * == AndGateDelay + ( 3 * InverterDelay ) */
  /** Design `orGate` in terms of `andGate`, `inverter` */
  def orGate(
    in1: Wire,
    in2: Wire,
    output: Wire ): Unit = {
    val notIn1, notIn2, notOut = new Wire

    inverter( in1, notIn1 )
    inverter( in2, notIn2 )
    andGate( notIn1, notIn2, notOut )

    inverter( notOut, output )
  }
}
