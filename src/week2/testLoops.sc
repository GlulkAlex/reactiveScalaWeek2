package week2

object testLoops {
  /*
  Definition of 'while'
  The function WHILE
  can be defined as follows:*/
  def whileLoop( condition: => Boolean )( command: => Unit ): Unit =
    /*no return value
    just 'Unit' type or simply '()'*/
    if ( condition ) {
      command
      whileLoop( condition )( command )
    } else ()
  /*Note:
  The condition and
  the command
  must be passed 'by name' so that
  they’re 'reevaluated' in each iteration.*/
  //else if
  //'condition' is passed by value &
  //has initial value set to 'true' then
  //calculations not /are never ending &
  //form an infinite loop
  /*Note:
  WHILE is
  'tail recursive', so
  it can
  operate with
  a 'constant stack size'.*/

  /*
  Exercise
  Write
  a function implementing
  a repeat loop that is
  used as follows:
		REPEAT {
		command
		} ( condition )
  It should
  execute command
  one or more times, until
  condition is 'true'.*/
  //curring order changes
  def untilLoop( command: => Unit )( condition: => Boolean ): Unit = {
    /*no return value
    just 'Unit' type or simply '()'*/
    command
    /*order changes from 'while' ?*/
    if ( condition ) {
      ()
    } else {
      untilLoop( command )( condition )
    }
  }

  var i = 3
  untilLoop( /*i -= 1*/ i = i - 1 )( i > 0 )
  i

  i = 3
  whileLoop( i > 0 )( i -= 1 )
  i

  /*
  Exercise (open-ended)
  Is it also possible to
  obtain the following syntax ?
		REPEAT {
		command
		} UNTIL ( condition )
  ?
  */

  /*
  Translation of 'For-Loops'
  'For-loops' translate similarly to 'for-expressions, but
  using the 'foreach'
  'combinator'
  instead of
  'map' and 'flatMap'.
  
  'foreach' is
  defined on collections with
  elements of type T as follows:*/
  /** {{{
    * def foreach(f: T => Unit): Unit =
    * // apply ‘f‘ to each element of the collection
    * }}}
    */
  /*Example*/
  for ( i <- 1 until 3; j <- "abc" ) println( i + " " + j )
  /*translates to:*/
  //using: 'closures'
  ( 1 until 3 ) foreach ( i => "abc" foreach ( j => println( i + " " + j ) ) )

}