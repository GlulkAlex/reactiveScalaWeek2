package week2

import scala.util.DynamicVariable

/*each signal Maintenance / maintenes
 * >>its current value
 * >>the current expression that defines the signal value
 * >>a set of observers: 
 *  the other signals that depend on its value
 *  if signal changes then
 *  observers need to be re-evaluated  
 * */
class Signal[ T ]( expr: => T ) {
  import Signal._

  /* not / (un)initialized function from [Unit] to [T] type */
  /*current expression*/
  private var myExpr: () => T = _
  /*current signal value*/
  private var myValue: T = _
  /*as it is defined / declared*/
  private var observers: Set[ Signal[ _ ] ] = Set()
  private var observed: List[ Signal[ _ ] ] = Nil
  /*initialization*/
  update( expr )

  protected def computeValue(): Unit = {
    /*simple version is:
     * myValue = caller.withValue( this )( myExpr() )
     * */
    for ( sig <- observed ) {
      sig.observers -= this
    }

    observed = Nil
    /*curring*/
    val newValue = caller
      /*method from 'DynamicVariable'*/
      .withValue( this )( myExpr() )

    /* Disable the following "optimization" for the assignment, 
     * because 
     * we want to 
     * be able to 
     * track 
     * the actual dependency graph in the tests.
     */
    //if (myValue != newValue) {
    myValue = newValue

    val obs = observers

    observers = Set()
    /*signal's current value changes in cases when:
     * >>somebody calls an 'update' operator on a 'Var', or
     * >>the value of dependent signal changes*/
    /*need to:
     * detect such changes &
     * propagate them to the observers / observing signals*/
    /*re-evaluating the 'callers'*/
    obs
      .foreach( _.computeValue() )
    //}
  }

  /*only reachable / available from subclasses*/
  protected def update( expr: => T ): Unit = {
    myExpr = () => expr
    computeValue()
  }

  /*returns current signal value*/
  def apply() = {
    /*adds current caller to the 'observers' Set*/
    observers += caller.value
    /*to prevent cases like this:
     * signal() = signal() + 1
     * which is pointless for function definition
     * so
     * observers may / must not contain signal itself
     * if check was omitted then
     * will be 
     * infinite loop calculation with
     * thrown stackOverflow */
    assert(
      !caller
        .value
        .observers
        .contains( this ),
      "cyclic signal definition" )
    /*list element addition ?*/
    caller
      .value
      .observed ::= this
    /*returns current value of the signal*/
    myValue
  }
}
/*'Var' is a signal that can be
 * updated by the client program*/
class Var[ T ]( expr: => T ) extends Signal[ T ]( expr ) {
  /*'update' only reachable / available from subclasses of 'Signal'*/
  /*'override' makes 'update' public & ?var?*/
  override def update( expr: => T ): Unit =
    /*super class 'Signal' ?*/
    super
      .update( expr )
}

object Var {
  def apply[ T ]( expr: => T ) = new Var( expr )
}

/*? black magic '???' instead of 'expr'ession ?*/
//Predef.`???` can be used for 
//marking methods that remain to be implemented
/*special kind of signal that
 * does not have value at all &
 * has no actual implementation
 * that what '???' stands for here as
 * unimplemented for no signal*/
object NoSignal extends Signal[ Nothing ]( ??? ) {
  /*as it is impossible to 
   * evaluate expression '???' with 'computeValue'*/
  /*empty expression that returns 'Unit' directly*/
  override def computeValue() = ()
}
/*signal expression also evaluates at the top-level when
 * there is no other signal
 * that is defined or updated
 * to do this used 
 * `sentinel` object 'NoSignal' as
 * 'caller' for such / these expressions 
 * */
object Signal {
  /*what is in the box ?
   * see 'StackableVariable'*/
  /*for 'Thread-Local State'
   * has drawbacks from:
   * >>its imperative nature
   * >>JDK implementation as global hash lookup
   * >>when threads are multiplexed
   * (no message exchange between)*/
  //*val caller = new DynamicVariable[Signal[_]](NoSignal)
  /*with initial value 'NoSignal'*/
  /*where 'StackableVariable' uses 'global state' / scope 
   * so
   * that rises a problem where
   * the caller signal will become "garbled"
   * [reproduce 
   * (a message, sound, or transmission) 
   * in a confused & distorted way]
   *  by concurrent updates
   *  (in parallel computations)
   * */
  private val caller = new StackableVariable[ Signal[ _ ] ]( NoSignal )
  /*better solution is use of 'implicit parameters'
   * but
   * for current Scala version
   * more boilerplate required*/

  def apply[ T ]( expr: => T ) = new Signal( expr )
}