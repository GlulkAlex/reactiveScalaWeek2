package week2

/*
compile to JavaScript with
'webUI/fastOptJS' and
refresh your browser
*/
/** If
  * all 'ten' values are
  * highlighted every time you modify something, then
  * something is wrong with
  * the way you construct your signals.
  */
object testCalculator {

  sealed abstract class Expr
  /*operands*/
  final case class Literal( v: Double ) extends Expr
  /*reference to 'namedExpressions: Map[String, Signal[Expr]]'*/
  final case class Ref( name: String ) extends Expr
  /*binAry operations*/
  final case class Plus( a: Expr, b: Expr ) extends Expr
  final case class Minus( a: Expr, b: Expr ) extends Expr
  final case class Times( a: Expr, b: Expr ) extends Expr
  final case class Divide( a: Expr, b: Expr ) extends Expr

  /** Notice that,
    * as you modify dependencies
    * between variables in the expressions,
    * the dependencies between signals
    * adapt dynamically.
    */
  /*
  >>'Refs' to other variables could cause
	  cyclic dependencies
	  (e.g., a = b + 1 and b = 2 * a).
	  Such cyclic dependencies are
	  considered as errors
	  (failing to detect this will cause infinite loops).
  >>'Referencing' a variable that is
	  not in the map is
	  an error.
	  Such errors should be handled by
	  returning 'Double.NaN', the Not-a-Number value.*/
	/*for all in 'Map' ?*/
	/*it seems like
	'getReferenceExpr
	was enough for
	Any error, such as
	a malformed expression, or
	cyclic references between variables,
	will result in NaN (Not-a-Number).
	without additional
	'assert' statment*/
  def computeValues(
    namedExpressions: Map[ String, Signal[ Expr ] ] ): Map[ String, Signal[ Double ] ] = {
    /*observers may / must not contain signal itself
     * if check was omitted then
     * will be
     * infinite loop calculation with
     * thrown stackOverflow */
    /*assert(
      !caller
        .value
        .observers
        .contains( this ),
      "cyclic signal definition" )*/

    val refEspr = getReferenceExpr(
      name = "String",
      references = Map.empty[ String, Signal[ Expr ] ] )
    val evalRef = eval(
      expr = Literal( 1.0 ),
      references = Map() )
    val refName = namedExpressions.head._1

    Map(
      refName ->
        Var( evalRef ) )
        
    for {
    (name, expr) <- namedExpressions
    } yield
      name -> Var(eval(expr(), namedExpressions))
  }                                               //> computeValues: (namedExpressions: Map[String,week2.Signal[week2.testCalcula
                                                  //| tor.Expr]])Map[String,week2.Signal[Double]]

  /*may be recursive definition when
  until
  'expr' is Literal( v: Double )*/
  def eval(
    expr: Expr,
    /*may be 'cyclic dependencies' for 'Ref'
    (e.g., a = b + 1 and b = 2 * a)*/
    references: Map[ String, Signal[ Expr ] ] ): Double = {
    /*assert(
      !references
        .value
        .observers
        .contains( expr ),
      "cyclic signal definition" )*/
      
    expr match {
      /*base case*/
      case Literal( v ) => v
      case Ref( r ) =>
        eval(
          getReferenceExpr(
            name = r,
            /*or .tail ?*/
            references = references ),
          references )
      case Plus( a: Expr, b: Expr ) =>
        eval( a, references ) +
          eval( b, references )
      case Minus( a: Expr, b: Expr ) =>
        eval( a, references ) -
          eval( b, references )
      case Times( a: Expr, b: Expr ) =>
        eval( a, references ) *
          eval( b, references )
      case Divide( a, b ) =>
        eval( a, references ) /
          eval( b, references )
      /*if all fails*/
      case _ => Double.NaN
    }
  }                                               //> eval: (expr: week2.testCalculator.Expr, references: Map[String,week2.Signal
                                                  //| [week2.testCalculator.Expr]])Double

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  /*private*/ def getReferenceExpr(
    name: String,
    references: Map[ String, Signal[ Expr ] ] ) = {
    references
      .get( name )
      .fold[ Expr ] {
        Literal( Double.NaN )
      } {
        /*case ?
        when got current element with '.fold'
        return element signal current value*/
        exprSignal => exprSignal()
      }
  }                                               //> getReferenceExpr: (name: String, references: Map[String,week2.Signal[week2.
                                                  //| testCalculator.Expr]])week2.testCalculator.Expr

  val liter0 = Literal( 0.0 )                     //> liter0  : week2.testCalculator.Literal = Literal(0.0)
  val liter1 = Literal( 1.0 )                     //> liter1  : week2.testCalculator.Literal = Literal(1.0)
  val liter2 = Literal( 2.0 )                     //> liter2  : week2.testCalculator.Literal = Literal(2.0)
  val liter3 = Literal( 3.0 )                     //> liter3  : week2.testCalculator.Literal = Literal(3.0)
  val ref0 = Ref( name = "liter0" )               //> ref0  : week2.testCalculator.Ref = Ref(liter0)
  val ref1 = Ref( name = "liter1" )               //> ref1  : week2.testCalculator.Ref = Ref(liter1)
  val ref2 = Ref( name = "liter2" )               //> ref2  : week2.testCalculator.Ref = Ref(liter2)
  val ref3 = Ref( name = "liter3" )               //> ref3  : week2.testCalculator.Ref = Ref(liter3)
  Plus( liter1, liter3 )                          //> res0: week2.testCalculator.Plus = Plus(Literal(1.0),Literal(3.0))
  Minus( liter3, liter2 )                         //> res1: week2.testCalculator.Minus = Minus(Literal(3.0),Literal(2.0))
  Minus( ref3, liter2 )                           //> res2: week2.testCalculator.Minus = Minus(Ref(liter3),Literal(2.0))
  Minus( ref3, ref2 )                             //> res3: week2.testCalculator.Minus = Minus(Ref(liter3),Ref(liter2))
  eval( liter3, Map( "liter3" -> Var( liter3 ) ) )//> res4: Double = 3.0
  eval( ref3, Map( "liter3" -> Var( liter3 ) ) )  //> res5: Double = 3.0
  eval( ref3, Map( "liter2" -> Var( liter2 ) ) )  //> res6: Double = NaN
  eval( Plus( liter1, liter3 ),
    Map(
      "liter1" -> Var( liter1 ),
      "liter2" -> Var( liter2 ),
      "liter3" -> Var( liter3 )
    ) )                                           //> res7: Double = 4.0
  eval( Plus( ref1, liter3 ),
    Map(
      "liter1" -> Var( liter1 ),
      "liter2" -> Var( liter2 ),
      "liter3" -> Var( liter3 )
    ) )                                           //> res8: Double = 4.0
  eval( Plus( ref1, ref3 ),
    Map(
      "liter1" -> Var( liter1 ),
      "liter2" -> Var( liter2 ),
      "liter3" -> Var( liter3 )
    ) )                                           //> res9: Double = 4.0
    
  /*'cyclic dependencies' for 'Ref'
    (e.g., a = b + 1 and b = 2 * a) or
    more clearly as self reference
    a = ( 2 * a ) + 1 that is pointless for function definiton */
  //val aCyclic: Expr =
   //warning: Reference to uninitialized value 'bCyclic'
   //forward reference extends over definition of value 'bCyclic'
   /*translates as
   'Plus(null,Literal(1.0))'*/
   //Plus( bCyclic, liter1 )
   /*compiler replace it by
   'Plus(Minus(Literal(2.0),null),Literal(1.0))'*/
   //Plus( Minus(liter2 , aCyclic), liter1 )
  
  /*translates as
  'Minus(Literal(2.0),Plus(null,Literal(1.0)))'*/
  /*val bCyclic: Expr =
   Minus( liter2, aCyclic )*/
   
  //eval( aCyclic,
  val cyclicRefMap: Map[ String, Signal[ Expr ] ] = Map(
      /*'cyclic dependencies' for 'Ref'*/
      //"aCyclic" -> Var( Plus( Minus(liter2 , aCyclic), liter1 ) ),
      /*'cyclic dependencies' for 'Ref'*/
      //"bCyclic" -> Var( Minus( liter2, aCyclic ) ),
      "a" -> Var( Plus( liter2, liter1 ) ),
      "b" -> Var( Minus( liter2, liter1 ) ),
      "liter1" -> Var( liter1 ),
      "liter2" -> Var( liter2 ),
      "liter3" -> Var( liter3 )
      /*"liter1" -> Var( liter1 ).update(liter0),
      "liter2" -> Var( liter2 ).update(liter1),
      "liter3" -> Var( liter3 ).update(liter2)*/
      /*"liter1" -> Var( liter1 )(),
      "liter2" -> Var( liter2 )(),
      "liter3" -> Var( liter3 )()*/
      /*"liter1" -> Signal( liter1 ),
      "liter2" -> Signal( liter2 ),
      "liter3" -> Signal( liter3 )*/
    )                                             //> cyclicRefMap  : Map[String,week2.Signal[week2.testCalculator.Expr]] = Map(l
                                                  //| iter1 -> week2.Var@653222, a -> week2.Var@132ec19, b -> week2.Var@c4039c, l
                                                  //| iter3 -> week2.Var@e45eb6, liter2 -> week2.Var@19f99ea)
    //)
  cyclicRefMap.get("aCyclic")                     //> res10: Option[week2.Signal[week2.testCalculator.Expr]] = None
  val  currentSignal = cyclicRefMap.get("aCyclic")//> currentSignal  : Option[week2.Signal[week2.testCalculator.Expr]] = None
  /*None.get
  currentSignal.get()*/
  getReferenceExpr(
            "aCyclic",
            /*why 'Expr' ?
            when same syntax work inside eval ?
            because
            'cyclicRefMap' type must be the same as parameter
            not iduced / dedused by compiler*/
            cyclicRefMap )                        //> res11: week2.testCalculator.Expr = Literal(NaN)
  //case class NamedExpression(exprName: String, exprBody: Expr)
  val keyExistAsVal = for {
    (name, expr) <- cyclicRefMap
    //if {expr match {case Signal(x) => true}}
    } yield
      //name
      eval(expr(), cyclicRefMap)                  //> keyExistAsVal  : scala.collection.immutable.Iterable[Double] = List(1.0, 3.
                                                  //| 0, 1.0, 3.0, 2.0)
        //.contains( aCyclic )
    
  /*abstract class Term
  case class VarName( name: String ) extends Term
  case class Fun( arg: String, body: Term ) extends Term
  case class App( f: Term, v: Term ) extends Term

  def printTerm( term: Term ) {
    term match {
      case VarName( n ) =>
        print( n )
      case Fun( x, b ) =>
        print( "^" + x + "." )
        printTerm( b )
      case App( f, v ) =>
        Console.print( "(" )
        printTerm( f )
        print( " " )
        printTerm( v )
        print( ")" )
    }
  }

  def isIdentityFun( term: Term ): Boolean = term match {
    case Fun( x, VarName( y ) ) if x == y => true
    case _                                => false
  }

  val id = Fun( "x", VarName( "x" ) )
  val t = Fun( "x", Fun( "y", App( VarName( "x" ), VarName( "y" ) ) ) )

  printTerm( t )
  println
  println( isIdentityFun( id ) )
  println( isIdentityFun( t ) )*/

}