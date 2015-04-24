package week2

object testTweetLength {
  /*UTF-16 (16-bit Unicode Transformation Format) is
  a character encoding capable of
  encoding all 1,112,064 possible characters in Unicode.
  
  Code points from
  the other planes (called Supplementary Planes) are
  encoded in as
  two 16-bit code units called 'surrogate pairs'*/
  def tweetLength( text: String ): Int = {
    /* This should be simply
    * text.codePointCount(0, text.length), but
    * it is not implemented in Scala.js 0.6.2.
     */
    if ( text.isEmpty ) { 0 }
    else {
      val someTextManipulation = text
        .init
        .zip( text.tail )
        .count(
          ( Character.isSurrogatePair _ ).tupled )

      text.length - someTextManipulation
    }
  }                                               //> tweetLength: (text: String)Int

  val testetText1 = "text for test"               //> testetText1  : String = text for test

  testetText1.init                                //> res0: String = text for tes
  testetText1.tail                                //> res1: String = ext for test
  testetText1.init.zip( testetText1.tail )        //> res2: scala.collection.immutable.IndexedSeq[(Char, Char)] = Vector((t,e), (e
                                                  //| ,x), (x,t), (t, ), ( ,f), (f,o), (o,r), (r, ), ( ,t), (t,e), (e,s), (s,t))
  /*Parameters
    high - the high-surrogate code value to be tested
    low - the low-surrogate code value to be tested*/
  Character.isSurrogatePair( 'x', 't' )           //> res3: Boolean = false
  // assign values to ch1, ch2
  val ch1 = '\ud800'                              //> ch1  : Char = ?
  val ch2 = '\udc00'                              //> ch2  : Char = ?
  val surrogatePair = ( ch1, ch2 )                //> surrogatePair  : (Char, Char) = (?,?)
  val testetText2 = "text for test" + ch1         //> testetText2  : String = text for test?
  val testetText3 = "text for test" + ch2         //> testetText3  : String = text for test?
  val testetText4 = "text for test" + ch1 + ch2   //> testetText4  : String = text for test?

  Character.isSurrogatePair( ch1, ch2 )           //> res4: Boolean = true

  val pairCheck = ( Character.isSurrogatePair _ ).tupled
                                                  //> pairCheck  : ((Char, Char)) => Boolean = <function1>

  pairCheck( surrogatePair )                      //> res5: Boolean = true

  tweetLength( testetText1 )                      //> res6: Int = 13
  testetText1.length()                            //> res7: Int = 13
  tweetLength( testetText2 )                      //> res8: Int = 14
  testetText2.length()                            //> res9: Int = 14
  tweetLength( testetText3 )                      //> res10: Int = 14
  testetText3.length()                            //> res11: Int = 14
  tweetLength( testetText4 )                      //> res12: Int = 14
  testetText4.length()                            //> res13: Int = 15

  5 >= 0                                          //> res14: Boolean(true) = true
  5 <= 14                                         //> res15: Boolean(true) = true
  5 >= 0 && 5 <= 14                               //> res16: Boolean = true
  "foo" * 200                                     //> res17: String = foofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofo
                                                  //| ofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofo
                                                  //| ofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofo
                                                  //| ofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofo
                                                  //| ofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofo
                                                  //| ofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofo
                                                  //| ofoofoofo
                                                  //| Output exceeds cutoff limit.
  val testetText5 = Var( "foo blabla \uD83D\uDCA9 bar" )
                                                  //> testetText5  : week2.Var[String] = week2.Var@8b7f67

  testetText5()                                   //> res18: String = foo blabla ? bar

  def sqrt( x: Double ) = {
      def abs( x: Double ) = if ( x >= 0 ) {
        x
      } else {
        -x
      }

      def sqrtIter( guess: Double ): Double =
        if ( isGoodEnough( guess ) ) {
          guess
        } else {
          sqrtIter( improve( guess ) )
        }

      def isGoodEnough( guess: Double ) =
        abs( guess * guess - x ) < x * 0.0001 //epsilon value
      //or:
      //abs(guess * guess - x) / x < 0.001

      def improve( guess: Double ) =
        ( guess + x / guess ) / 2

    sqrtIter( 1.0 )
  }                                               //> sqrt: (x: Double)Double

  def sqrtStream( x: Double ): Stream[ Double ] = {
      def improve( guess: Double ) = ( guess + x / guess ) / 2

    lazy val guesses: Stream[ Double ] = 1 #:: ( guesses map improve )

    guesses
  }                                               //> sqrtStream: (x: Double)Stream[Double]

  def isGoodEnough( guess: Double,
                    x: Double,
                    tolerance: Double ) = {
      def abs( x: Double ) = if ( x >= 0 ) {
        x
      } else {
        -x
      }
    abs( ( guess * guess - x ) / x ) < x * tolerance
  }                                               //> isGoodEnough: (guess: Double, x: Double, tolerance: Double)Boolean

  Math.sqrt( 3 )                                  //> res19: Double = 1.7320508075688772
  Math.sqrt( 4 )                                  //> res20: Double = 2.0
  Math.sqrt( 7 )                                  //> res21: Double = 2.6457513110645907
  sqrt( 3 )                                       //> res22: Double = 1.7320508100147274
  sqrt( 4 )                                       //> res23: Double = 2.0000000929222947
  sqrt( 7 )                                       //> res24: Double = 2.64576704419029
  (sqrtStream( 3 ) filter ( isGoodEnough( _, 3, 0.00001 ) )).head
                                                  //> res25: Double = 1.7320508100147274
  (sqrtStream( 4 ) filter ( isGoodEnough( _, 4, 0.00001 ) )).head
                                                  //> res26: Double = 2.0000000929222947
  (sqrtStream( 7 ) filter ( isGoodEnough( _, 7, 0.00001 ) )).head
                                                  //> res27: Double = 2.64576704419029
  sqrtStream( 3 ).take(9).head                    //> res28: Double = 1.0
  sqrtStream( 3 ).drop(9).head                    //> res29: Double = 1.7320508075688772
  
  sqrtStream( 0.003 ).drop(9).head                //> res30: Double = 0.05477225575051661
  Math.sqrt( 0.003 )                              //> res31: Double = 0.05477225575051661
  174.154 * 174.154                               //> res32: Double(30329.615716) = 30329.615716
  sqrt( 0.003 )                                   //> res33: Double = 0.05477234364155629
  sqrtStream( 30003 ).drop(9).head                //> res34: Double = 174.15413843868905
  Math.sqrt( 30003 )                              //> res35: Double = 173.21374079443004
  173.213 * 173.213                               //> res36: Double(30002.743368999996) = 30002.743368999996
  sqrt( 30003 )                                   //> res37: Double = 173.21627977437407
  
  0.0                                             //> res38: Double(0.0) = 0.0
}