package week2

object testStatefulness {
  /*
  Statefulness and Variables
	Remember
	the definition of streams
	(lazy sequences) in week 7, #progfun.
	Instead of
	using
	a 'lazy val',
	we could also
	implement
	non-empty streams using
	a 'mutable variable':*/
  /** {{{
    * //inheritance
    * //from class Stream
    * //in package immutable is deprecated:
    * //This class will be sealed.
	//class varStream extends Stream
    //'cons' is Stream constructor
    * def cons[ T ](
                    hd: T,
                    tl: => Stream[ T ] ) = new Stream[ T ] {
    * def head = hd
    *
    * private var tlOpt: Option[ Stream[ T ] ] = None
    *
    * def tail: T = tlOpt match {
    * //type mismatch;
    * //found   : x.type (with underlying type Stream[T])
    * //required: T
    * case Some( x ) => x
    * case None      => {
                         //changes mutable state
                          tlOpt = Some( tl )
                         //recursively return new 'tail' for Stream
                         //as it became 'case Some( x )'
                         tail }
    * }
    * }
    * }}}
    */
  /*Question:
	Is the result of 'cons' a 'stateful object' ?*/
	/*
	Complicated explanation
	may be both yes & no
	it depends on / of veiw point
	*/

  /*
  Consider the following class:*/
  class BankAccountProxy( ba: BankAccount ) {
    def deposit( amount: Int ): Unit = ba.deposit( amount )
    def withdraw( amount: Int ): Int = ba.withdraw( amount )
  }
  /*Question:
  Are instances of 'BankAccountProxy' 'stateful objects' ?*/
  /*stateful as it's behaviour changes with operation history*/

  println( "Welcome to the Scala worksheet" )     //> Welcome to the Scala worksheet
}