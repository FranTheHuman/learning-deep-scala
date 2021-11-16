package essential_scala

object Traits extends App {

  /**
   * 4.1.4.1 Cats, and More Cats
   *
   * Demand for Cat Simulator 1.0 is exploding! For v2 we’re going to go beyond
   * the domesঞc cat to model Tigers, Lions, and Panthers in addiঞon to the Cat.
   * Define a trait Feline and then define all the different species as subtypes of
   * Feline. To make things interesঞng, define:
   * • on Feline a colour;
   * • on Feline a sound, which for a cat is "meow" and is "roar"
   * for all other felines;
   * • only Cat has a favourite food; and
   * • Lions have an Int maneSize.
   *
   */

  sealed trait Sound

  case object Meow extends Sound
  case object Roar extends Sound

  trait Feline {
    def colour: String
    def sound: Sound
  }

  trait BigFeline extends Feline {
    def sound: Sound = Roar
  }

  case class Cat(colour: String, sound: Sound = Meow, favouriteFood: String) extends Feline
  case class Tiger(colour: String) extends BigFeline
  case class Lion(colour: String, maneSize: Int) extends BigFeline
  case class Panter(colour: String) extends BigFeline

  // 4.1.4.2 Shaping Up With Traits

  /**
   * Define a trait called Shape and give it three abstract methods:
    * ° sides returns the number of sides;
    * ° perimeter returns the total length of the sides;
    * ° area returns the area.
    *
    * Implement Shape with three classes: Circle, Rectangle, and Square.
    * In each case provide implementations of each of the three methods.
    * Ensure that the main constructor parameters of each shape (e.g. the radius of the circle)
    * are accessible as fields.
    *
    * Tip: The value of π is accessible as math.Pi.
    */

  // 4.1.4.3 Shaping Up 2 (Da Streets)

  /**
   * The solution from the last exercise delivered three distinct types of shape.
   * However, it doesn’t model the relationships between the three correctly.
   * A Square isn’t just a Shape—it’s also a type of Rectangle where the width and height are the same.
   * Refactor the solution to the last exercise so that Square and Rectangle are subtypes of a common type Rectangular.
   */

  sealed trait Shape {
    def sides: Int
    def perimeter: Double
    def area: Double
    def color: Color
  }

  trait Rectangular extends Shape {
    def width: Double
    def height: Double

    override val sides = 4
    override val perimeter = 2 * width + 2 * height
    override val area = width * height
  }

  case class Circle(radius: Double, color: Color) extends Shape {
    val sides = 1
    val perimeter = 2 * math.Pi * radius
    val area = math.Pi * radius * radius
  }
  case class Rectangle(width: Double, height: Double, color: Color) extends Rectangular
  case class Square(size: Double, color: Color) extends Rectangular {
    val width = size
    val height = size
  }

  // 4.2.2.1 Printing Shapes

  /**
     First make Shape a sealed trait. Then write a singleton object called Draw with an apply method that
     takes a Shape as an argument and returns a description of it on the console. For example:
     Draw(Circle(10)) // res1: String = A circle of radius 10.0cm
   */

  object Draw {
    def apply(shape: Shape): String = shape match {
      case Rectangle(w, h, c) => s"A ${drawColor(c)} rectangle of width ${w}cm and height ${h}cm"
      case Square(s, c) => s"A ${drawColor(c)} Square of size ${s}cm"
      case Circle(r, c) => s"A ${drawColor(c)} circle of radius ${r}cm"
    }
    def drawColor(color: Color): String = color match {
      case Red => Red.toString
      case Yellow => Yellow.toString
      case Pink => Pink.toString
      case custom: CustomColor if custom.isDark => "Dark Color"
      case custom: CustomColor if custom.isLight => "Light Color"
    }
  }

  // 4.2.2.2 The Color and the Shape

  /**
    Write a sealed trait Color to make our shapes more interesting.
     - give Color three properties for its RGB values;
     - create three predefined colours: Red, Yellow, and Pink;
     - provide a means for people to produce their own custom Colors with their own RGB values;
     - provide a means for people to tell whether any Color is “light” or “dark”.
    A lot of this exercise is left deliberately open to interpretation.
    The important thing is to practice working with traits, classes, and objects.

    Decisions such as how to model colours and what is considered a light or dark colour can either
    be left up to you or discussed with other class members.

    Edit the code for Shape and its subtypes to add a colour to each shape.

    Finally, update the code for Draw.apply to print the colour of the argument
    as well as its shape and dimensions:

    if the argument is a predefined colour, print that colour by name:
     Draw(Circle(10, Yellow)) // res8: String = A yellow circle of radius 10.0cm
    if the argument is a custom colour rather than a predefined one,
      print the word “light” or “dark” instead.
   */

  sealed trait Color {
    def red: Int
    def green: Int
    def blue: Int

    // https://stackoverflow.com/questions/22603510/is-this-possible-to-detect-a-colour-is-a-light-or-dark-colour
    def isLight: Boolean =
      math.sqrt(0.299 * (red * red) + 0.587 * (green * green) + 0.114 * (blue * blue)) > 127.5
    def isDark: Boolean = !isLight
  }

  case object Red extends Color {
    val red: Int = 255
    val green: Int = 0
    val blue: Int = 0
  }
  case object Yellow extends Color {
    val red: Int = 255
    val green: Int = 255
    val blue: Int = 0
  }
  case object Pink extends Color {
    val red: Int = 255
    val green: Int = 192
    val blue: Int = 203
  }
  case class CustomColor(red: Int, green: Int, blue: Int) extends Color

  assert(Draw(Square(45, Pink)) == "A Pink Square of size 45.0cm")
  assert(Draw(Rectangle(45, 50, CustomColor(240,128,128))) == "A Light Color rectangle of width 45.0cm and height 50.0cm")
  assert(Draw(Circle(60, CustomColor(139,0,0))) == "A Dark Color circle of radius 60.0cm")

  // 4.2.2.3 A Short Division Exercise

  /**
    Good Scala developers don’t just use types to model data. Types are a great way to put artificial
    limitations in place to ensure we don’t make mistakes in our programs.
    In this exercise we will see a simple (if contrived) example of this—using types to prevent
    division by zero errors.
    Dividing by zero is a tricky problem—it can lead to exceptions.
    The JVM has us covered as far as floating point division is concerned but integer division
    is still a problem:
      1.0 / 0.0 // res31: Double = Infinity
      1 / 0 // java.lang.ArithmeticException: / by zero

    Let’s solve this problem once and for all using types!

   Create an object called divide with an apply method that accepts two Ints and returns DivisionResult.
    DivisionResult should be a sealed trait with two subtypes: a Finite type encapsulating the result
    of a valid division, and an Infinite type representing the result of dividing by 0.

   Finally, write some sample code that calls divide, matches on the result, and returns a sensible description.
   */
  sealed trait DivisionResult
  final case class Finite(result: Double) extends DivisionResult
  final case object Infinite extends DivisionResult

  object Divide {
    def apply(x1: Int, x2: Int): DivisionResult = x2 match {
      case 0 => Infinite
      case _ => Finite(x1 / x2)
    }
  }

  assert(Divide(1, 2) == Finite(0))
  assert(Divide(1, 0) == Infinite)
  assert(Divide(10, 5) == Finite(2))


}
