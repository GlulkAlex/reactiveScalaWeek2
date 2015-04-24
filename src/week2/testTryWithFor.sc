package week2

import scala.util.{ Try, Success, Failure }

object testTryWithFor {
  trait Coin {
    val nominal: Int
    def coinKind(): Try[ Coin ]
  }
  case class Gold( amount: Int = 0 ) extends Coin {
    val nominal: Int = if ( amount > 0 ) { amount } else { 0 }
    def coinKind(): Try[ Coin ] = nominal match {
      case n if n > 0 => Success( Silver( n * 10 ) )
      case _          => Failure( new Exception( "No gold" ) )
      //case failure @ Failure(e) => failure  /* returns
      //for example 'Failure(java.Lang.Exception: WRONG)'
      //that must be defined earlier as
      //return case for 'match'*/
    }
    def coinKind1: PartialFunction[ Coin, Try[ Coin ] ] = {
      case Gold( n ) =>
        //"a golden coin"
        Success( Silver() )
      // no case for Silver(), because
      // we're only interested in Gold()
    }
  }
  case class Silver( amount: Int = 0 ) extends Coin {
    val nominal: Int = if ( amount > 0 ) { amount } else { 0 }

    def coinKind(): Try[ Coin ] = nominal match {
      case n if n >= 10 => Success( Gold( 1 ) )
      //*case _ => Failure(new Exception("Too cheap coin"))
    }
  }

  val o1 = /*SomeTrait*/
    //Silver()
    /*And where
    The 'Try' class helps handle exceptions elegantly ?
    for 'partial function' ?*/
    Gold()                                        //> o1  : week2.testTryWithFor.Gold = Gold(0)
  //Gold(5)
  val ans = for {
    //*o2 <- o1.coinKind()
    //*o3 <- o2.coinKind()
    /*Failure(scala.MatchError
    o2 <- o1.coinKind1(Gold(1))
    o2 <- o1.coinKind1(Gold())*/
    /*fails not gracefully
    o2 <- o1.coinKind1(Silver())*/
    o2 <- o1.coinKind1(Silver(10))
    o3 <- o2.coinKind()
  } yield o3                                      //> scala.MatchError: Silver(10) (of class week2.testTryWithFor$Silver)
                                                  //| 	at scala.PartialFunction$$anon$1.apply(PartialFunction.scala:253)
                                                  //| 	at scala.PartialFunction$$anon$1.apply(PartialFunction.scala:251)
                                                  //| 	at week2.testTryWithFor$Gold$$anonfun$coinKind1$1.applyOrElse(week2.test
                                                  //| TryWithFor.scala:20)
                                                  //| 	at week2.testTryWithFor$Gold$$anonfun$coinKind1$1.applyOrElse(week2.test
                                                  //| TryWithFor.scala:20)
                                                  //| 	at scala.runtime.AbstractPartialFunction.apply(AbstractPartial
                                                  //| Output exceeds cutoff limit.
  ans
}