package week2

abstract class Circuits extends Gates {
  /*from:
   * http://en.wikipedia.org/wiki/Adder_(electronics)
   * The half adder 
   * adds two single binary digits A and B. 
   * It has two outputs, 
   * sum (S) and carry (C). 
   * The carry signal represents 
   * an overflow into the next digit of 
   * a multi-digit addition. 
   * The value of the sum is 2C + S.*/
  def halfAdder(
    a: Wire,
    b: Wire,
    s: Wire,
    c: Wire ) {
    val d, e = new Wire

    orGate( a, b, d )
    andGate( a, b, c )
    inverter( c, e )
    andGate( d, e, s )
  }

  /*return
   * sum &
   * carry*/
  //Full-adder logic diagram
  def fullAdder(
    a: Wire,
    b: Wire,
    cin: Wire,
    sum: Wire,
    cout: Wire ) {
    val s, c1, c2 = new Wire

    halfAdder( a, cin/*carry input*/, s, c1 )
    halfAdder( b, s, sum, c2 )
    orGate( c1, c2, cout/*carry outout*/ )
  }
}
