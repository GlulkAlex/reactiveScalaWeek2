package week2

/*to maintain the total balance of a list of accounts */
class Consolidator( observed: List[ BankAccountExt ] ) extends Subscriber {
  observed.foreach { _.subscribe( this ) }

  private var total: Int = _/*'_' here stands for uninitialized*/
  /*'total' initialization*/
  compute()

  private def compute(): /*Int*/Unit = {
    total = observed.map( _.currentBalance ).sum
    
    //*total//*if it return value
  }
  
  /*recompute 'totalBalance' in 'total'
   * when 'BankAccountExt' state changes*/
  def handler( publ: Publisher ) = compute()
  /*Getter*/
  def totalBalance = total
}