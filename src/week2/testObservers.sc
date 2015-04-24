package week2

object testObservers {
  var a = new BankAccountExt                      //> a  : week2.BankAccountExt = week2.BankAccountExt@1464ce8
  var b = new BankAccountExt                      //> b  : week2.BankAccountExt = week2.BankAccountExt@16532d6
  var c = new Consolidator( observed = List( a, b ) )
                                                  //> c  : week2.Consolidator = week2.Consolidator@167135c
  c.totalBalance                                  //> res0: Int = 0
  a deposit 20
  c.totalBalance                                  //> res1: Int = 20
  b deposit 50
  c.totalBalance                                  //> res2: Int = 70
}