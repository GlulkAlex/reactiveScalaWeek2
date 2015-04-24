package week2

/*'DynamicVariable'
 * internals
 * as
 * 'stack'
 * */
/*.Thread-Local State*/
/*API matches 'DynamicVariable'*/
class StackableVariable[T](init: T) {
  private var values: List[T] = List(init)
  
  /*& what if empty ?*/
  def value: T = values.head

  /*curring*/
  def withValue[R](newValue: T)(op: => R): R = {
      /*put 'newValue' as 'head'*/
		  values = newValue :: values
      
      /*perform operation then
       * remove 'newValue' from stack list*/
      try op
      finally values = values.tail
  }
}