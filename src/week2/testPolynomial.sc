package week2

object testPolynomial {
  def computeSolutions(
    a: Signal[Double],
    b: Signal[Double],
    c: Signal[Double],
    delta: Signal[Double]): Signal[Set[Double]] = {
      /*if ( delta() < 0 ) {
        new Signal( Set( 0.0 ) )
      } else if (delta() == 0) {
        val root: Double =  -b() / 2 * a()
        
        new Signal( Set( root ) )
      } else {
        //& 2 signals ?
        val root1: Double = ( -b() + sqrt( delta() )) / 2 * a()
        val root2: Double = ( -b() - sqrt( delta() )) / 2 * a()
        
        new Signal( Set( root1, root2 ) )
      }*/
      val root = Var( Set( 0.0 ) )
    
      root() = delta() match {
        case d if d < 0 => Set( 0.0 )
        case d if d == 0 =>
          val root: Double =  -b() / 2 * a()
          
          Set( root )
        
        case _ =>
          val root1: Double = ( -b() + ( delta() )) / 2 * a()
          val root2: Double = ( -b() - ( delta() )) / 2 * a()
          
          Set( root1, root2 )
        
      }
      
      root
  }                                               //> computeSolutions: (a: week2.Signal[Double], b: week2.Signal[Double], c: wee
                                                  //| k2.Signal[Double], delta: week2.Signal[Double])week2.Signal[Set[Double]]
  
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
}