package week2

/*Defining Technology-Dependent Parameters
It's convenient to 
pack all delay constants into 
their own trait 
which can be 
mixed into 
a simulation. 
For instance:
 */
trait Parameters {
  /* ??? why 'def' not 'val' ???
   * not computes once ??? */
  /*because implement
   * abstract class Gates
   * methods*/
  def InverterDelay = 2
  def AndGateDelay = 3
  def OrGateDelay = 5
}
