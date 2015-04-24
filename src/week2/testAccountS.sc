package week2

object testAccountS {
  def consolidated( accounts: List[ BankAccountSignal ] ): Signal[ Int ] =
    /*take 'balance' at current time*/
    Signal( accounts.map( _.balance() ).sum )     //> consolidated: (accounts: List[week2.BankAccountSignal])week2.Signal[Int]

  val a = new BankAccountSignal                   //> a  : week2.BankAccountSignal = week2.BankAccountSignal@f82f98
  val b = new BankAccountSignal                   //> b  : week2.BankAccountSignal = week2.BankAccountSignal@1f983a6
  val c = consolidated( List( a, b ) )            //> c  : week2.Signal[Int] = week2.Signal@1143ee9

  c()                                             //> res0: Int = 0
  a.deposit( 10 )
  c()                                             //> res1: Int = 10
  b.deposit( 20 )
  c()                                             //> res2: Int = 30
  b.withdraw( 5 )
  c()                                             //> res3: Int = 25
  /*rate for BitCoins*/
  val xChange:Signal[ /*Float*/Double ] = Signal( 246.00 )
                                                  //> xChange  : week2.Signal[Double] = week2.Signal@b7bfc0
  val inDollar:Signal[ Double ] = Signal( c() * xChange() )
                                                  //> inDollar  : week2.Signal[Double] = week2.Signal@6e2eef

  inDollar()                                      //> res4: Double = 6150.0
  b.withdraw( 5 )
  /*connection: changes in 'b' reflected in 'inDollar'*/
  inDollar()                                      //> res5: Double = 4920.0
  
  /*exercise*/
  val num = Var(1)                                //> num  : week2.Var[Int] = week2.Var@1f97b55
  /*??? binding ???*/
  val twice = Signal( num() * 2 )                 //> twice  : week2.Signal[Int] = week2.Signal@bbc1e0
  
  twice()                                         //> res6: Int = 2
  /*changes ('.update') existing signal &
  all dependencies*/
  num() = 2
  /*??? cascaded update of binded values ???
  through 'map' ?*/
  twice()                                         //> res7: Int = 4
  
  var num2 = Var(1)                               //> num2  : week2.Var[Int] = week2.Var@a097cc
  val twice2 = Signal( num2() * 2 )               //> twice2  : week2.Signal[Int] = week2.Signal@f6e879
  twice2()                                        //> res8: Int = 2
  
  /*create new signal*/
  num2 = Var(2)
  twice2()                                        //> res9: Int = 2
  
  twice == twice2                                 //> res10: Boolean = false
  /*is value the same ? Yes ? No . */
  twice() == twice2()                             //> res11: Boolean = false
  twice()                                         //> res12: Int = 4
  twice2()                                        //> res13: Int = 2
  /*no relation / connection */
  num2() = 3
  twice2()                                        //> res14: Int = 2
  num2() = 7
  twice2()                                        //> res15: Int = 2
}