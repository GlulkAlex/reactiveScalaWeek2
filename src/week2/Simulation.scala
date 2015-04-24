package week2

/* top level hierarchy object / instance 
 * where each class represent
 * application / model
 * layer */
abstract class Simulation {
  /*
   * An action is 
   * a function that 
   * doesn’t take any parameters and 
   * which
  returns Unit:*/
  /* ??? '()' as empty Unit ??? or 
   * block placeholder ??? */
  type Action = () => Unit

  case class Event(
    time: Int,
    action: Action )

  private var curtime = 0
  /** The current simulated time */
  def currentTime: Int = curtime

  private var agenda: List[ Event ] = List()

  /*make / maintain priority queue invariant*/
  private def insert(
    ag: List[ Event ],
    item: Event ): List[ Event ] = ag match {
    /*black magic ? 
     * no precondition filter*/
    case first :: rest if first.time <= item.time =>
      first :: insert( rest, item )
    case _ => item :: ag
  }

  /*curring with by name parameter*/
  /*form / add / insert a queue Action item to a 'Wire' */
  /** Registers
    * an action ‘block‘ to
    * perform after a given delay
    * relative to
    * the current time
    */
  def afterDelay( delay: Int )( block: => Unit ): Unit = {
    val item = Event(
      currentTime + delay,
      () => block )

    agenda = insert( agenda, item )
  }

  /*The run method 
   * executes 
   * the event loop after 
   * installing 
   * an initial message that 
   * signals 
   * the start of simulation.
   */  
  /** Performs
    * the simulation until
    * there are no actions waiting
    */
  def run() {
    afterDelay( 0 ) {
      println( "*** simulation started, time = " + currentTime + " ***" )
    }

    loop()
  }

  /*The Event Handling Loop
  removes successive elements from the agenda,
  and 
  performs the associated actions.
   */   
  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curtime = first.time
      first.action()

      loop()
    /*black magic?
     * what does this case return ?*/
    /*end & exit*/  
    case Nil => /* ??? same as '()' ??? 
      as no expression is / has Unit type ??? */
  }
}
