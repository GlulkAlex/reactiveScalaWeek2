package week2

/*when state of 'BankAccountExt'
 * namely 'balance'
 * change then
 * invoke 'Publisher'*/
class BankAccountExt extends Publisher{
  private var balance = 0;
  /*view for 'balance'
   * as getter*/
  def currentBalance = balance
  
  def deposit(amount: Int): Unit =
    if (amount > 0) {
      balance = balance + amount;
      publish()}

  def withdraw(amount: Int): /*Int*/Unit =
    if (0 < amount && amount <= balance) {
      balance = balance - amount;
      publish()
      //*balance//*for /if  return: Int
    } else {
      throw new Error( "insufficient funds" );
    }
}