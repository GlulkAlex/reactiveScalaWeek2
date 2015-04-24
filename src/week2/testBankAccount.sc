package week2

object testBankAccount {
  /*
  Working with
  'Mutable Objects'
  Here is
  a worksheet that
  manipulates 'bank accounts'.*/
  val account = new BankAccount /* account: BankAccount = BankAccount*/
                                                  //> account  : week2.BankAccount = week2.BankAccount@12a0b8e
  
  account deposit 50
  account withdraw 20                             //> res0: Int = 30
  account withdraw 20                             //> res1: Int = 10
  account withdraw 15                             //> java.lang.Error: insufficient funds
                                                  //| 	at week2.BankAccount.withdraw(BankAccount.scala:14)
                                                  //| 	at week2.testBankAccount$$anonfun$main$1.apply$mcV$sp(week2.testBankAcco
                                                  //| unt.scala:15)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.
                                                  //| Output exceeds cutoff limit.
  /*Applying
  the same operation to
  an account twice in a row produces
  different results.
  Clearly,
  accounts are
  'stateful objects'.*/

}