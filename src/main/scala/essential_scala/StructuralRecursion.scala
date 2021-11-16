package essential_scala

object StructuralRecursion extends App {

  // Sum Type using Polymorphism
  sealed trait A {
    def f: F
  }
  final case class B() extends A {
    def f: F = ???
  }
  final case class C() extends A {
    def f: F = ???
  }

  // The Product Type Polymorphism
  case class APolymorphism(b: B, c: C) {
    def f: F = ???
  }

  // The Sum Type Pattern Matching -> Menos codigo

  sealed trait ASum
  case class BSum() extends ASum
  case class CSum() extends ASum

  trait F
  def f(a: ASum): F =
    a match {
      case BSum() => ???
      case CSum() => ???
    }

  // Product Type using Pattern Matching -> Menos codigo

  case class AProduct(b: BSum, c: CSum)
  def f(a: AProduct): F =
    a match {
      case AProduct(b, c) => ???
    }

  // El codigo toma la forma de la informacion.

  // 4.5.6.1 Traffic Lights

  /* In the previous section we implemented a TrafficLight data type.

      Using polymorphism and then using pattern matching implement a method called next which returns the next
      TrafficLight in the standard Red -> Green -> Yellow -> Red cycle. Do you think it is better to
      implement this method inside or outside the class? If inside, would you use pattern matching or
      polymorphism? Why?

   */

  object Polymorphism {
    sealed trait TrafficLight {
      def next: TrafficLight
    }
    case object Red extends TrafficLight {
      def next: TrafficLight =
        Green
    }
    case object Green extends TrafficLight {
      def next: TrafficLight =
        Yellow
    }
    case object Yellow extends TrafficLight {
      def next: TrafficLight =
        Red
    }
  }

  object PatterMatching {
    sealed trait TrafficLight {
      def next: TrafficLight = this match {
        case Red => Green
        case Green => Yellow
        case Yellow => Red
      }
    }
    case object Red extends TrafficLight
    case object Green extends TrafficLight
    case object Yellow extends TrafficLight
  }

  // 4.5.6.1 Traffic Lights
  // In the last section we created a Calculation data type like so:
  sealed trait Calculation
  final case class Success(result: Int) extends Calculation
  final case class Failure(reason: String) extends Calculation

  /**
   * We’re now going to write some methods that use a Calculation to perform a larger calculation.
   * These methods will have a somewhat unusual shape—this is a precursor to things we’ll be
   * exploring soon—but if you follow the patterns you will be fine.
   * Create a Calculator object. On Calculator define methods + and - that accept a Calculation and an Int,
   * and return a new Calculation. Here are some examples
   */
  object Calculator {
    def +(c1: Calculation, n: Int): Calculation =
      c1 match {
        case Success(value) => Success(value + n)
        case f: Failure => f
      }
    def -(c1: Calculation, n: Int): Calculation =
      c1 match {
        case Success(value) => Success(value - n)
        case f: Failure => f
      }
    def /(c1: Calculation, n: Int): Calculation =
      c1 match {
        case Success(value) =>
          n match {
            case 0 => Failure("Division by zero")
            case _ => Success(value / n) }
        case f: Failure => f
      }
  }

  assert(Calculator.+(Success(1), 1) == Success(2))
  assert(Calculator.-(Success(1), 1) == Success(0))
  assert(Calculator.+(Failure("Badness"), 1) == Failure("Badness"))

  /**
   * Now write a division method that fails if the divisor is 0. The following tests should pass.
   * Note the behavior for the last test. This indicates “fail fast” behavior.
   * If a calculation has already failed we keep that failure and don’t process any more data even if,
   * as is the case in the test, doing so would lead to another failure.
   */

  assert(Calculator./(Success(4), 2) == Success(2))
  assert(Calculator./(Success(4), 0) == Failure("Division by zero"))
  assert(Calculator./(Failure("Badness"), 0) == Failure("Badness"))

  // 4.5.6.3 Email
  /**
    Recall the Visitor trait we looked at earlier: a website Visitor is either Anonymous or a signed-in User.
    Now imagine we wanted to add the ability to send emails to visitors.
    We can only email signed-in users, and sending an email requires a lot of knowledge about SMTP settings,
    MIME headers, and so on. Would an email method be better implemented using
    polymorphism on the Visitor trait or using pattern matching in an EmailService object? Why?
   */

  // polymorphism porque aplica unicamente para un caso por lo que es una funcionalidad especifica de ese caso.
  // Pero lo ideal es abstraerlo por completo a un servicio de emails

  // 4.6.3.1 A List of Methods

  sealed trait IntList {
    def length: Int = this match {
      case End => 0
      case Pair(head, tail) => 1 + tail.length
    }
  }
  case object End extends IntList
  final case class Pair(head: Int, tail: IntList) extends IntList

  /*
    define a method length that returns the length of the list.
    There is test data below you can use to check your solution.
    For this exercise it is best to use pattern matching in the base trait.
   */

  val example = Pair(1, Pair(2, Pair(3, End)))

  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End.length == 0)

  // 4.6.3.2 The Forest of Trees

  /**
    A binary tree of integers can be defined as follows:
    A Tree is a Node with a left and right Tree or a Leaf with an element of type Int.
    Implement this algebraic data type.
   */
  // Implement sum and double on Tree using polymorphism and pattern matching.

  sealed trait Tree {
    def sum: Int = this match {
      case Node(left, right) => left.sum + right.sum
      case Leaf(value) => value
    }
    def double: Tree = this match {
      case Node(left, right) => Node(left.double, right.double)
      case Leaf(value) => Leaf(value * 2)
    }
  }
  final case class Node(left: Tree, right: Tree) extends Tree {
    override def sum: Int = left.sum + right.sum

    override def double: Tree = Node(left.double, right.double)
  }
  final case class Leaf(value: Int) extends Tree {
    override def sum: Int = value

    override def double: Tree = Leaf(value * 2)
  }

  // 4.7.0.1 A Calculator

  /*

  In this exercise we’ll implement a simple interpreter for programs containing only numeric operations.
  We start by defining some types to represent the expressions we’ll be operating on. In the compiler literature this is known as an abstract syntax tree.
  Our representation is:
    - An Expression is an Addition, Subtraction, or a Number;
    - An Addition has a left and right Expression;
    - A Subtraction has a left and right Expression; or
    - A Number has a value of type Double.

    Implement this in Scala.

   */

  sealed trait Expression {
    def eval: Calculation =
      this match {
        case Addition(l, r) =>
          l.eval match {
            case Failure(reason) => Failure(reason)
            case Success1(r1) =>
              r.eval match {
                case Failure(reason) => Failure(reason)
                case Success1(r2) => Success1(r1 + r2)
              }
          }
        case Subtraction(l, r) =>
          l.eval match {
            case Failure(reason) => Failure(reason)
            case Success1(r1) =>
              r.eval match {
                case Failure(reason) => Failure(reason)
                case Success1(r2) => Success1(r1 - r2)
              }
          }
        case Division(l, r) =>
          l.eval match {
            case Failure(reason) => Failure(reason)
            case Success1(r1) =>
              r.eval match {
                case Failure(reason) => Failure(reason)
                case Success1(r2) =>
                  if(r2 == 0)
                    Failure("Division by zero")
                  else
                    Success1(r1 / r2)
              }
          }
        case SquareRoot(v) =>
          v.eval match {
            case Success1(r) =>
              if(r < 0)
                Failure("Square root of negative number")
              else
                Success1(Math.sqrt(r))
            case Failure(reason) => Failure(reason)
          }
        case Number(v) => Success1(v)
      }
  }
  final case class Addition(left: Expression, right: Expression) extends Expression
  final case class Subtraction(left: Expression, right: Expression) extends Expression
  final case class Number(value: Double) extends Expression

  /*
    Now implement a method eval that converts an Expression to a Double.
    Use polymorphism or pattern matching as you see fit. Explain your choice of implementation method.
   */

  /*
    We’re now going to add some expressions that call fail: division and square root.
    Start by extending the abstract syntax tree to include representations for Division and SquareRoot.
   */
  final case class SquareRoot(value: Expression) extends Expression
  final case class Division(left: Expression, right: Expression) extends Expression
  /*
    Now we’re going to change eval to represent that a computation can fail.
    (Double uses NaN to indicate a computation failed, but we want to be helpful to
    the user and tell them why the computation failed.) Implement an appropriate algebraic data type.
   */

  final case class Success1(result: Double) extends Calculation
  assert(Addition(SquareRoot(Number(-1.0)), Number(2.0)).eval == Failure("Square root of negative number"))
  assert(Addition(SquareRoot(Number(4.0)), Number(2.0)).eval == Success1(4.0))
  assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))

  // 4.7.0.2 JSON
  /*
    In the calculator exercise we gave you the algebraic data type representation.
    In this exercise we want you to design the algebraic data type yourself.
    We’re going to work in what is hopefully a familiar domain: JSON.

    Design an algebraic data type to represent JSON. Don’t go directly to code.
    Start by sketching out the design in terms of logical ands and ors—the building
    blocks of algebraic data types. You might find it useful to use a notation similar to BNF.
    For example, we could represent the Expression data type from the previous exercise as follows:

    Expression ::= Addition left:Expression right:Expression
             | Subtraction left:Expression right:Expression
             | Division left:Expression right:Expression
             | SquareRoot value:Expression
             | Number value:Int

    This simplified notation allows us to concentrate on the structure of the algebraic
    data type without worrying about the intricacies of Scala syntax.
    Note you’ll need a sequence type to model JSON, and we haven’t looked at Scala’s collection library yet.
    However we have seen how to implement a list as an algebraic data type.

    Here are some examples of JSON you’ll need to be able to represent
    ["a string", 1.0, true]
    {
      "a": [1,2,3],
      "b": ["a","b","c"]
      "c": { "doh":true, "ray":false, "me":1 }
    }
   */
  sealed trait Json {
    def print: String = {
      def quote(srt: String ): String =
        '"'.toString ++ srt ++ '"'.toString
      def seqToJson(seq: SeqCell): String =
        seq match {
          case SeqCell(h, t @ SeqCell(_, _)) => s"${h.print} ${seqToJson(t)}"
          case SeqCell(head, SeqEnd) => head.print
        }
      def objectToJson(obj: ObjectCell): String =
        obj match {
          case ObjectCell(k, v, t @ ObjectCell(_, _, _)) =>
            s"${quote(k)}: ${v.print}, ${objectToJson(t)}"
          case ObjectCell(k, v, ObjectEnd) =>
            s"${quote(k)}: ${v.print}"
        }

      this match {
        case JsNumber(value) => value.toString
        case JsString(value) => quote(value)
        case JsBoolean(value) => value.toString
        case JsNull => "null"
        case s @ SeqCell(_, _) => "[" ++ seqToJson(s) ++ "]"
        case SeqEnd => "[]"
        case o @ ObjectCell(_, _, _) => "{" ++ objectToJson(o) ++ "}"
        case ObjectEnd => "{}"
      }
    }
  }

  final case class JsNumber(value: Double) extends Json
  final case class JsString(value: String) extends Json
  final case class JsBoolean(value: Boolean) extends Json
  case object JsNull extends Json

  sealed trait JsSequence extends Json

  final case class SeqCell(head: Json, tail: JsSequence) extends JsSequence
  case object SeqEnd extends JsSequence

  sealed trait JsObject extends Json

  final case class ObjectCell(key: String, value: Json, tail: JsObject) extends JsObject
  case object ObjectEnd extends JsObject

  println(
    SeqCell(JsString("a string"), SeqCell(JsNumber(1.0), SeqCell(JsBoolean(true), SeqEnd))).print
  )
  println(
    ObjectCell(
      "a", SeqCell(JsNumber(1.0), SeqCell(JsNumber(2.0), SeqCell(JsNumber(3.0), SeqEnd))),
      ObjectCell(
        "b", SeqCell(JsString("a"), SeqCell(JsString("b"), SeqCell(JsString("c"), SeqEnd))),
        ObjectCell(
          "c", ObjectCell("doh", JsBoolean(true),
            ObjectCell("ray", JsBoolean(false),
              ObjectCell("me", JsNumber(1.0), ObjectEnd))),
          ObjectEnd
        )
      )
    ).print
  )
  // 4.7.0.3 Music

  /*
  In the JSON exercise there was a well defined specification to model.
  In this exercise we want to work on modelling skills given a rather fuzzy specification.
  The goal is to model music. You can choose to interpret this how you want,
  making your model as simple or complex as you like. The critical thing is to be able to justify
  the decisions you made, and to understand the limits of your model.

  You might find it easiest to use the BNF notation,
  introduced in the JSON exercise, to write down your model.*/



}
