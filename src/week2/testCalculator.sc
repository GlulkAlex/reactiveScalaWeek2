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
  final case class Literal( v: Double ) extends Expr {
    override def toString = s"$v"
  }
  /*reference to 'namedExpressions: Map[String, Signal[Expr]]'*/
  final case class Ref( name: String ) extends Expr {
    override def toString = s"Ref:$name"
  }
  /*binAry operations*/
  final case class Plus( a: Expr, b: Expr ) extends Expr {
    override def toString = s"{$a + $b}"
  }
  final case class Minus( a: Expr, b: Expr ) extends Expr {
    override def toString = s"{$a - $b}"
  }
  final case class Times( a: Expr, b: Expr ) extends Expr {
    override def toString = s"{$a * $b}"
  }
  final case class Divide( a: Expr, b: Expr ) extends Expr {
    override def toString = s"{$a / $b}"
  }

  /*when first occurence of
  same as expression key name happends then
  stop*/
  /*too bulky & not work*/
  def flatExp(
    expr: Expr,
    expressionsMap: Map[ String, Signal[ Expr ] ],
    output: Seq[ String ] = Seq.empty[ String ] ): Seq[ String ] = expr match {
    /*base case*/
    case Literal( v ) => v.toString() +: output
    case Ref( r ) =>
      val getExpr = getReferenceExpr(
        name = r,
        references = expressionsMap )

      if ( output.contains( r ) || expr == getExpr ) {
        /*cyclyc reference*/
        s"cyclyc '$r'" +: output
      } else {
        /*going deeper*/
        flatExp(
          expr = getReferenceExpr(
            name = r,
            references = expressionsMap ),
          expressionsMap = expressionsMap,
          output = output )
      }
    /*must compare 'a' & 'b' against exspression 'name'*/
    case Plus( a: Expr, b: Expr ) =>
      flatExp( a, expressionsMap, output ) ++:
        flatExp( b, expressionsMap, output )
    case Minus( a: Expr, b: Expr ) =>
      flatExp( a, expressionsMap, output ) ++:
        flatExp( b, expressionsMap, output )
    case Times( a: Expr, b: Expr ) =>
      flatExp( a, expressionsMap, output ) ++:
        flatExp( b, expressionsMap, output )
    case Divide( a, b ) =>
      flatExp( a, expressionsMap, output ) ++:
        flatExp( b, expressionsMap, output )
    /*if all fails*/
    case _ => s"$Double.NaN" +: output
  }                                               //> flatExp: (expr: week2.testCalculator.Expr, expressionsMap: Map[String,week2
                                                  //| .Signal[week2.testCalculator.Expr]], output: Seq[String])Seq[String]

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
	'assert' statement*/
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

    /*val refEspr = getReferenceExpr(
      name = "String",
      references = Map.empty[ String, Signal[ Expr ] ] )
    val evalRef = eval(
      expr = Literal( 1.0 ),
      references = Map() )
    val refName = namedExpressions.head._1

    Map(
      refName ->
        Var( evalRef ) )*/

    /*cyclic cases:
    >> "name1" -> Signal(Ref("name1"))
    >> "name1" -> Signal(Plus(Ref("name1") + Literal(7.0)))
    >> "name1" -> Signal(Plus(Ref("name1") + Ref("name2")))
    >> "name1" -> Signal(Plus(Minus(Ref("name1") - Literal(7.0)) + Ref("name2")))
    so
    to track it down
    must be
    'flat' sequence with all 'Ref' 'names'
    */
    for {
      ( name, expr ) <- namedExpressions
    } yield name -> Var( eval( expr(), namedExpressions ) )
  }                                               //> computeValues: (namedExpressions: Map[String,week2.Signal[week2.testCalcula
                                                  //| tor.Expr]])Map[String,week2.Signal[Double]]

  /*may be recursive definition when
  until
  'expr' is Literal( v: Double )*/
  def eval(
    expr: Expr,
    /*may be 'cyclic dependencies' for 'Ref'
    (e.g., a = b + 1 and b = 2 * a)*/
    /*to prevent 'cyclic dependencies'
    current expression must be removed from further use
    by excluding from 'references'*/
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
            /*references - expr ? garanty NaN in any case*/
            references = references ),
          /* one tiny binary operation on passed parameter
          added to the right place make a huge difference */
          references - r )
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
    references: Map[ String, Signal[ Expr ] ] ): Expr = {

    references
      .get( name )
      /*? '.fold' used to get rid of 'Option' type ?*/
      .fold[ Expr ] {
        /*'ifEmpty' then*/
        Literal( Double.NaN )
      } {
        /*case ?
	        when got current element with '.fold'
	        return element signal current value*/
        exprSignal => exprSignal()
      }
  }                                               //> getReferenceExpr: (name: String, references: Map[String,week2.Signal[week2.
                                                  //| testCalculator.Expr]])week2.testCalculator.Expr

  val liter0 = Literal( 0.0 )                     //> liter0  : week2.testCalculator.Literal = 0.0
  val liter1 = Literal( 1.0 )                     //> liter1  : week2.testCalculator.Literal = 1.0
  val liter2 = Literal( 2.0 )                     //> liter2  : week2.testCalculator.Literal = 2.0
  val liter3 = Literal( 3.0 )                     //> liter3  : week2.testCalculator.Literal = 3.0
  val ref0 = Ref( name = "liter0" )               //> ref0  : week2.testCalculator.Ref = Ref:liter0
  val ref1 = Ref( name = "liter1" )               //> ref1  : week2.testCalculator.Ref = Ref:liter1
  val ref2 = Ref( name = "liter2" )               //> ref2  : week2.testCalculator.Ref = Ref:liter2
  val ref3 = Ref( name = "liter3" )               //> ref3  : week2.testCalculator.Ref = Ref:liter3
  Plus( liter1, liter3 )                          //> res0: week2.testCalculator.Plus = {1.0 + 3.0}
  Minus( liter3, liter2 )                         //> res1: week2.testCalculator.Minus = {3.0 - 2.0}
  Minus( ref3, liter2 )                           //> res2: week2.testCalculator.Minus = {Ref:liter3 - 2.0}
  Minus( ref3, ref2 )                             //> res3: week2.testCalculator.Minus = {Ref:liter3 - Ref:liter2}
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
  )                                               //> cyclicRefMap  : Map[String,week2.Signal[week2.testCalculator.Expr]] = Map(l
                                                  //| iter1 -> week2.Var@1ce4f0a, a -> week2.Var@3a8624, b -> week2.Var@3bd550, l
                                                  //| iter3 -> week2.Var@11161c7, liter2 -> week2.Var@4ed9f0)
  //)
  cyclicRefMap.get( "aCyclic" )                   //> res10: Option[week2.Signal[week2.testCalculator.Expr]] = None
  val currentSignal = cyclicRefMap.get( "aCyclic" )
                                                  //> currentSignal  : Option[week2.Signal[week2.testCalculator.Expr]] = None
  /*None.get
  currentSignal.get()*/
  getReferenceExpr(
    "aCyclic",
    /*why 'Expr' ?
            when same syntax work inside eval ?
            because
            'cyclicRefMap' type must be the same as parameter
            not iduced / dedused by compiler*/
    cyclicRefMap )                                //> res11: week2.testCalculator.Expr = NaN
  //case class NamedExpression(exprName: String, exprBody: Expr)
  val keyExistAsVal = for {
    ( name, expr ) <- cyclicRefMap
    //if {expr match {case Signal(x) => true}}
  } yield //name
  eval( expr(), cyclicRefMap )                    //> keyExistAsVal  : scala.collection.immutable.Iterable[Double] = List(1.0, 3.
                                                  //| 0, 1.0, 3.0, 2.0)
  //.contains( aCyclic )

  val names = ( 0 until 10 ).map( i => ( 'a' + i ).toChar.toString )
                                                  //> names  : scala.collection.immutable.IndexedSeq[String] = Vector(a, b, c, d,
                                                  //|  e, f, g, h, i, j)

  //import reactive-week1.
  //import week1.testRandomGenerators
  //import java.util.Random

  //val rand = new Random
  /*arbitrary : Int*/
  //rand.nextInt()

  trait Generator[ +T ] {
    self => // an alias for "this".
    /*abstract*/
    def generate: T

    def map[ S ]( f: T => S ): Generator[ S ] =
      new Generator[ S ] {
        def generate = f( self.generate )
      }

    def flatMap[ S ]( f: T => Generator[ S ] ): Generator[ S ] =
      new Generator[ S ] {
        def generate = f( self.generate ).generate
        /*or*/
        //*def generate = f( Generator1.generate ).generate
      }
  }
  /*Some instances:*/
  val integers = new Generator[ Int ] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }                                               //> integers  : week2.testCalculator.Generator[Int]{val rand: java.util.Random
                                                  //| } = week2.testCalculator$$anonfun$main$1$$anon$3@8e7350

  val doubles = new Generator[ Double ] {
    val rand = new java.util.Random
    def generate = rand.nextDouble()
  }                                               //> doubles  : week2.testCalculator.Generator[Double]{val rand: java.util.Rand
                                                  //| om} = week2.testCalculator$$anonfun$main$1$$anon$4@968a59

  doubles.generate                                //> res12: Double = 0.7235135333897901
  doubles.generate                                //> res13: Double = 0.6691058848042188
  doubles.generate                                //> res14: Double = 0.9119201853137251
  val booleans = new Generator[ Boolean ] {
    def generate = integers.generate > 0
  }                                               //> booleans  : week2.testCalculator.Generator[Boolean] = week2.testCalculator
                                                  //| $$anonfun$main$1$$anon$5@67ae56
  val booleans3 = for ( x <- integers ) yield x > 0
                                                  //> booleans3  : week2.testCalculator.Generator[Boolean] = week2.testCalculato
                                                  //| r$$anonfun$main$1$Generator$1$$anon$1@ebc39e
  booleans.generate                               //> res15: Boolean = true
  booleans3.generate                              //> res16: Boolean = true
  /*return just input itself */
  def single[ T ]( x: T ): Generator[ T ] = new Generator[ T ] {
    def generate = x
  }                                               //> single: [T](x: T)week2.testCalculator.Generator[T]

  def endLessIteration(
    lowBound: Int = 0,
    hiBound: Int = 10,
    elem: Int = 0 ): Int = {
    ( elem + 1 ) % hiBound
  }                                               //> endLessIteration: (lowBound: Int, hiBound: Int, elem: Int)Int

  //*for (i <- 1 to 20) println(endLessIteration(elem = i))

  /*how about negative with positive &
  what bounds included in interval
  what excluded ?*/
  /*cases:
  (-3, -2, -1), 0, 1, 2, 3
  -3, (-2, -1, 0), 1, 2, 3
  -3, -2,( -1, 0, 1, 2), 3
  -3, -2, -1, (0, 1, 2), 3
  -3, -2, -1, 0, (1, 2, 3)
  */
  def intervalInt(
    lo: Int,
    hi: Int ): Generator[ Int ] =
    /*lower bound + 0 or
    + difference between lo & hi */
    /*'+1' to include upper bound*/
    for { x <- integers } yield lo + {
      val choise = x % ( hi + 1 - lo )
      /*'abs'*/
      choise match {
        case v if v > 0 => v
        case v if v < 0 => -v
        case _          => 0
      }
    }                                             //> intervalInt: (lo: Int, hi: Int)week2.testCalculator.Generator[Int]

  intervalInt( -1000, 1000 ).generate             //> res17: Int = -715
  /*must return: 1,2,3 */
  intervalInt( 1, 3 ).generate                    //> res18: Int = 1
  intervalInt( 1, 3 ).generate                    //> res19: Int = 1
  intervalInt( 1, 3 ).generate                    //> res20: Int = 1
  intervalInt( 1, 3 ).generate                    //> res21: Int = 2
  intervalInt( 1, 3 ).generate                    //> res22: Int = 1

  1 % 1                                           //> res23: Int(0) = 0
  1 % 2                                           //> res24: Int(1) = 1
  1 % 3                                           //> res25: Int(1) = 1
  1 % 4                                           //> res26: Int(1) = 1
  /*so '5' excludet but
  if low is '1' then '1 + 4 = 5'*/
  1 % 5                                           //> res27: Int(1) = 1
  2 % 5                                           //> res28: Int(2) = 2
  3 % 5                                           //> res29: Int(3) = 3
  4 % 5                                           //> res30: Int(4) = 4
  5 % 5                                           //> res31: Int(0) = 0

  val mod1 = 0 + ( -331 ) % ( 3 - 1 )             //> mod1  : Int = -1
  0 + ( -11 ) % ( 3 - 1 )                         //> res32: Int = -1
  0 + ( -7 ) % ( 3 - 1 )                          //> res33: Int = -1
  0 + ( -6 ) % ( 3 - 1 )                          //> res34: Int = 0
  0 + ( -4 ) % ( 3 - 1 )                          //> res35: Int = 0
  0 + ( -2 ) % ( 3 - 1 )                          //> res36: Int = 0
  0 + ( 0 ) % ( 3 - 1 )                           //> res37: Int = 0
  0 + ( 1 ) % ( 3 - 1 )                           //> res38: Int = 1
  0 + ( 2 ) % ( 3 - 1 )                           //> res39: Int = 0
  0 + 3 % ( 3 - 1 )                               //> res40: Int = 1
  0 + 4 % ( 3 - 1 )                               //> res41: Int = 0
  0 + 5 % ( 3 - 1 )                               //> res42: Int = 1
  0 + 6 % ( 3 - 1 )                               //> res43: Int = 0
  0 + 7 % ( 3 - 1 )                               //> res44: Int = 1
  0 + 11 % ( 3 - 1 )                              //> res45: Int = 1
  0 + 311 % ( 3 - 1 )                             //> res46: Int = 1

  /*
  simple expressions :
  >>A number
  >>The name of a variable (from 'a' to 'j')
  >>An expression of the form
  '<expr> <op> <expr>' where
  <expr> is
  a number or
  a variable name, and
  <op> is one of '+' '-' '*' '/'
  Note that
  'spaces' are important, and
  that there cannot be more than 'one operation' per cell.*/
  //A B C D E F G H I J
  val varNames = Set( "a", "b", "c", "d", "e", "f", "g", "h", "i", "j" )
                                                  //> varNames  : scala.collection.immutable.Set[String] = Set(e, j, f, a, i, b,
                                                  //|  g, c, h, d)
  varNames.size                                   //> res47: Int = 10
  //varNames(0)
  //varNames(9)
  varNames.drop( 0 ).head                         //> res48: String = e
  varNames.drop( 9 ).head                         //> res49: String = d
  for { elem <- varNames } yield elem             //> res50: scala.collection.immutable.Set[String] = Set(e, j, f, a, i, b, g, c
                                                  //| , h, d)
  for { index <- 1 to 10 } yield varNames.drop( index - 1 ).head
                                                  //> res51: scala.collection.immutable.IndexedSeq[String] = Vector(e, j, f, a, 
                                                  //| i, b, g, c, h, d)
  def randomLiteral(): Expr = intervalInt( 1, 3 ).generate match {
    case 1 => Literal( integers.generate + doubles.generate )
    case 2 => Literal( doubles.generate )
    case 3 => Literal( integers.generate.toDouble )
    case _ => Literal( 0.0 )
  }                                               //> randomLiteral: ()week2.testCalculator.Expr

  randomLiteral()                                 //> res52: week2.testCalculator.Expr = -1.4416466294422278E9
  randomLiteral()                                 //> res53: week2.testCalculator.Expr = 8.08304457436032E8
  randomLiteral()                                 //> res54: week2.testCalculator.Expr = -1.50581151E8
  randomLiteral()                                 //> res55: week2.testCalculator.Expr = 0.7158047628251004
  randomLiteral()                                 //> res56: week2.testCalculator.Expr = -3.8728215832261586E8

  def valOrRef(): Expr =
    if ( booleans.generate ) {
      randomLiteral()
    } else {
      Ref( varNames.drop( intervalInt( 0, 9 ).generate ).head )
    }                                             //> valOrRef: ()week2.testCalculator.Expr

  valOrRef()                                      //> res57: week2.testCalculator.Expr = -1.60353041E9
  valOrRef()                                      //> res58: week2.testCalculator.Expr = 1.3271870521142333E9
  valOrRef()                                      //> res59: week2.testCalculator.Expr = -4.44163509E8

  ( for {
    input <- 1 to 10
    //valOrRef <- booleans
  } yield {
    val expr: Expr = if ( booleans.generate ) {
      valOrRef()
    } else {
      intervalInt( 1, 4 ).generate match {
        case 1 => Plus( valOrRef(), valOrRef() )
        case 2 => Minus( valOrRef(), valOrRef() )
        case 3 => Times( valOrRef(), valOrRef() )
        case 4 => Divide( valOrRef(), valOrRef() )
      }
    }

    ( varNames.drop( input - 1 ).head -> expr /*Signal( expr )*/ )
  } ).toMap                                       //> res60: scala.collection.immutable.Map[String,week2.testCalculator.Expr] = 
                                                  //| Map(e -> {0.7405903990827071 - -3.88090072E8}, j -> {1.744436687E9 / 2.965
                                                  //| 68135E8}, f -> {-1.027843615E9 / Ref:i}, a -> {2.142658409E9 - 1.594626066
                                                  //| E9}, i -> Ref:j, b -> -1.4953783102130432E9, g -> {5.199563E7 + Ref:d}, c 
                                                  //| -> {Ref:g / Ref:h}, h -> {Ref:a / 0.503093218851059}, d -> {Ref:d + -2.133
                                                  //| 74766E9})
  lazy val signalMapGen: Generator[ Map[ String, Signal[ Expr ] ] ] =
    new Generator[ Map[ String, Signal[ Expr ] ] ] {
      def generate = ( for {
        input <- 1 to 10
      } yield {
        val expr: Expr = if ( booleans.generate ) {
          valOrRef()
        } else {
          intervalInt( 1, 4 ).generate match {
            case 1 => Plus( valOrRef(), valOrRef() )
            case 2 => Minus( valOrRef(), valOrRef() )
            case 3 => Times( valOrRef(), valOrRef() )
            case 4 => Divide( valOrRef(), valOrRef() )
          }
        }

        ( varNames.drop( input - 1 ).head -> Signal( expr ) )
      } ).toMap
    }                                             //> signalMapGen: => week2.testCalculator.Generator[Map[String,week2.Signal[we
                                                  //| ek2.testCalculator.Expr]]]

  val signalMap1 = signalMapGen.generate          //> signalMap1  : Map[String,week2.Signal[week2.testCalculator.Expr]] = Map(e 
                                                  //| -> week2.Signal@ac3a89, j -> week2.Signal@1c20538, f -> week2.Signal@d818d
                                                  //| 1, a -> week2.Signal@6451e, i -> week2.Signal@1494225, b -> week2.Signal@1
                                                  //| 567f1, g -> week2.Signal@b9f472, c -> week2.Signal@cb644e, h -> week2.Sign
                                                  //| al@1805618, d -> week2.Signal@ef9176)
  signalMap1.keys.headOption.getOrElse( "empty?" )//> res61: String = e
  signalMap1.values.headOption.getOrElse( Var( Literal( Double.NaN ) ) )()
                                                  //> res62: _2808 = {0.6142704574325037 + Ref:j}
  val signalMap1Head = signalMap1.headOption.getOrElse( "NaN" -> Var( Literal( Double.NaN ) ) )
                                                  //> signalMap1Head  : (String, week2.Signal[_ >: week2.testCalculator.Literal 
                                                  //| <: week2.testCalculator.Expr]) = (e,week2.Signal@ac3a89)
  signalMap1.size                                 //> res63: Int = 10
  val signalMap1Tail = signalMap1 - signalMap1Head._1
                                                  //> signalMap1Tail  : scala.collection.immutable.Map[String,week2.Signal[week2
                                                  //| .testCalculator.Expr]] = Map(j -> week2.Signal@1c20538, f -> week2.Signal@
                                                  //| d818d1, a -> week2.Signal@6451e, i -> week2.Signal@1494225, b -> week2.Sig
                                                  //| nal@1567f1, g -> week2.Signal@b9f472, c -> week2.Signal@cb644e, h -> week2
                                                  //| .Signal@1805618, d -> week2.Signal@ef9176)
  signalMap1Tail.size                             //> res64: Int = 9
  /*cyclic cases:
    >> "name1" -> Signal(Ref("name1"))
    >> "name1" -> Signal(Plus(Ref("name1") , Literal(7.0)))
    >> "name1" -> Signal(Plus(Ref("name1") , Ref("name2")))
    >> "name1" -> Signal(Plus(Minus(Ref("name1"), Literal(7.0)) + Ref("name2")))*/
  /*'StackOverflowError' cyclic reference*/
  eval( signalMap1.values.head(), signalMap1 )    //> res65: Double = -1.0125250313857296E9
  eval( signalMap1.values.drop( 5 ).head(), signalMap1 )
                                                  //> res66: Double = -1.0125250313857296E9
  eval( signalMap1.values.last(), signalMap1 )    //> res67: Double = -1.0125250322184876E9
  eval(
    Ref( "name1" ),
    Map( "name1" -> Signal(
      Ref( "name1" )
    )
    )
  )                                               //> res68: Double = NaN

  eval(
    Ref( "name1" ),
    Map( "name1" -> Signal(
      Plus( Ref( "name1" ), Literal( 7.0 ) )
    )
    )
  )                                               //> res69: Double = NaN

  val validExprMap1: Map[ String, Signal[ Expr ] ] = Map(
    "a" -> Signal( Literal( 7.0 ) ),
    "b" -> Signal( Plus( Ref( "a" ), Literal( 3.0 ) ) ),
    "c" -> Signal( Minus( Ref( "b" ), Ref( "a" ) ) )
  )                                               //> validExprMap1  : Map[String,week2.Signal[week2.testCalculator.Expr]] = Map
                                                  //| (a -> week2.Signal@322d26, b -> week2.Signal@12b07fd, c -> week2.Signal@bf
                                                  //| bc86)

  eval( Ref( "a" ), validExprMap1 )               //> res70: Double = 7.0
  eval( Ref( "b" ), validExprMap1 )               //> res71: Double = 10.0
  eval( Ref( "c" ), validExprMap1 )               //> res72: Double = 3.0

  /*'StackOverflowError' cyclic reference*/
  eval( signalMap1.values.drop( 7 ).head(), signalMap1 )
                                                  //> res73: Double = -1.0125250313857296E9

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