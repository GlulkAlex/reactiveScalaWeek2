package week2

object testSubstitutionModel {
  /*
  Reminder:
  'Substitution Model'
  Programs can be evaluated by 'rewriting'.*/
  /*
  Rewriting Example:
  Say
  you have
  the following two functions
  'iterate' and 'square':*/
  /** function 'f' applies 'n' times to 'x'
    * clever & beautiful
    */
  def iterate(
    n: Int,
    f: Int => Int,
    x: Int ): Int =
    if ( n == 0 ) {
      /*base case*/
      x
    } else {
      /*iterative step
      trick is
      to apply function as parameter for function &
      in the same time as
      result accumulator*/
      iterate( n - 1, f, f( x ) )
    }                                             //> iterate: (n: Int, f: Int => Int, x: Int)Int

  def square( x: Int ): Int = x * x               //> square: (x: Int)Int
  /*Then
  the call*/
  iterate( 1, square, 3 )                         //> res0: Int = 9
  iterate( 2, square, 3 )                         //> res1: Int = 81
  iterate( 3, square, 3 )                         //> res2: Int = 6561

  /*Note:
  'Confluence' as
  substitution order does not matter &
  gain same result
  */
  /*gets rewritten as follows:
	->> if (1 == 0) 3 else iterate(1-1, square, square(3))
	->> iterate(0, square, square(3))
	->> iterate(0, square, 3 * 3)
	->> iterate(0, square, 9)
	->> if (0 == 0) 9 else iterate(0-1, square, square(9))
	->> 9*/


  def power(
    x: Double,
    exp: Int,//exponent
    f: Int => Int ): Double = {
    var r = 1.0 //result accumulated
    var i = exp //stop condition

    while ( i > 0 ) {
      //*r = r * x//increment
      r = if (r == 1.0) {
        f(x.toInt)//base case
      } else {
      f(r.toInt) //increment
      }
      i = i - 1//next step
    }
    /*return value*/
    r
  }                                               //> power: (x: Double, exp: Int, f: Int => Int)Double

  power( x = 3, exp = 3, f = square )             //> res3: Double = 6561.0
  square(square(square(3)))                       //> res4: Int = 6561
  
  /*not accumulate result as
  parameter for next iteration
  not so cool without recursion & 'var'
  how about 'fold' / 'reduce' ?
  somehow fails*/
  def iterateRange(
    n: Int,
    f: Int => Int,
    x: Int ): Int = ( for ( i <- n to 0 by -1 ) yield f( i ) ).sum
                                                  //> iterateRange: (n: Int, f: Int => Int, x: Int)Int
    
  def product3( xs: List[ Int ] ) = ( 1 :: xs ) reduceLeft ( _ * _ )
                                                  //> product3: (xs: List[Int])Int
  def product4( xs: List[ Int ] ) = ( xs foldLeft 1 )( _ * _ )
                                                  //> product4: (xs: List[Int])Int
  //(3 to 0 by -1 ).fold (1)( binOp: (Int, Int) /*(n, x)*/ =>
  //square( binOp._2/*x*/ ))
  ( 3 to 0 by -1 ).foldLeft[ Int ]( 1 )( ( x, y ) =>
    square( y ) )                                 //> res5: Int = 0
  ( 3 to 0 by -1 ).foldLeft[ Int ]( 1 )( ( x, y ) =>
    square( x ) )                                 //> res6: Int = 1
  ( 0 to 3 ).foldLeft[ Int ]( 1 )( ( x, y ) =>
    square( y ) )                                 //> res7: Int = 9
  ( 0 to 3 ).foldLeft[ Int ]( 1 )( ( x, y ) =>
    x * square( y ) )                             //> res8: Int = 0
  ( 0 to 3 ).foldLeft[ Int ]( 1 )( ( x, y ) =>
    square( x ) )                                 //> res9: Int = 1
  ( 0 to 3 ).reduceLeft[ Int ]( ( x, y ) =>
    square( x ) )                                 //> res10: Int = 0
  ( 0 to 3 ).reduceLeft[ Int ]( ( x, y ) =>
    square( y ) )                                 //> res11: Int = 9
  ( 0 to 3 ).reduceLeft[ Int ]( ( x, y ) =>
    x * square( y ) )                             //> res12: Int = 0
  ( 0 to 3 ).reduceLeft[ Int ]( ( x, y ) =>
    square( square( y ) ) )                       //> res13: Int = 81
  ( 0 to 3 ).reduceLeft[ Int ]( ( x, y ) =>
    square( square( x ) ) )                       //> res14: Int = 0

  iterateRange( 1, square, 3 )                    //> res15: Int = 1
  iterateRange( 2, square, 3 )                    //> res16: Int = 5
  iterateRange( 3, square, 3 )                    //> res17: Int = 14
}