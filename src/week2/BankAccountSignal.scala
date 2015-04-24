package week2

class BankAccountSignal {
  val balance = Var( 0 );

  def deposit( amount: Int ): Unit =
    if ( amount > 0 ) {
      //cyclic function definition (in terms of itself)
      //balance() = balance() + amount;
      /*store current value*/
      val b = balance()

      balance() = b + amount;
    }

  def withdraw( amount: Int ): /*Int*/ Unit =
    if ( 0 < amount && amount <= balance() ) {
      //balance() = balance() - amount;
      val b = balance()

      balance() = b - amount;

      //balance
    } else {
      throw new Error( "insufficient funds" );
    }

}