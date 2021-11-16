package essential_scala

object Generics extends App {

  // 5.4.3.1 Exercise: Generic Sum Type
  /*
    Implement a trait Sum[A, B] with two subtypes Left and Right.
    Create type parameters so that Left and Right can wrap up values of two different types.

    Hint: you will need to put both type parameters on all three types. Example usage:
   */
  trait Sum[A, B]
  case class Left[A, B](value: A) extends Sum[A, B]
  case class Right[A, B](value: B) extends Sum[A, B]

  Left[Int, String](1).value // res9: Int = 1
  Right[Int, String]("foo").value  // res10: String = foo
  val sum: Sum[Int, String] = Right("foo") // sum: sum.Sum[Int,String] = Right(foo)
  sum match {
    case Left(x) => x.toString
    case Right(x) => x
  } // res11: String = foo

  // 5.4.4.1 Exercise: Maybe that Was a Mistake

  /*
    Create a generic trait called Maybe of a generic type A with two subtypes,
    Full containing an A, and Empty containing no value. Example usage:
   */
  sealed trait Maybe[A]
  final case class Full[A](value: A) extends Maybe[A]
  final case class Empty[A]() extends Maybe[A]

  val perhaps: Maybe[Int] = Empty[Int]

  // 5.4.6.1 Generics versus Traits

  /*
    Sum types and product types are general concepts that allow us to model almost any kind of data structure.
    We have seen two methods of writing these types—traits and generics.
    When should we consider using each?
   */

  /*

    Inheritance-based approaches—traits and classes—allow us to create permanent data structures with specific
    types and names. We can name every field and method and implement use-case-specific code in each class.
    Inheritance is therefore better suited to modelling significant aspects of our programs that are re-used in
    many areas of our codebase.

   Generic data structures — Tuples, Options, Eithers, and so on—are extremely broad and general purpose.
   There are a wide range of predefined classes in the Scala standard library that we can use to quickly model
   relationships between data in our code. These classes are therefore better suited to quick,
   one-off pieces of data manipulation where defining our own types would introduce unnecessary verbosity to our codebase.

   */

  // 5.4.6.2 Folding Maybe

  // In this section we implemented a sum type for modelling optional data:
  // Implement fold for this type.

  object foldingMaybe {
    sealed trait Maybe[A] {
      def fold[B](empty: B)(f: A => B): B = this match {
        case Full(value) => f(value)
        case Empty() => empty
      }
    }
    final case class Full[A](value: A) extends Maybe[A]
    final case class Empty[A]() extends Maybe[A]
  }

  // 5.4.6.3 Folding Sum

  // In this section we implemented a generic sum type
  object foldingSum {
    sealed trait Sum[A, B] {
      def fold[C](left: A => C, right: B => C): C = this match {
        case Left(value) => left(value)
        case Right(value) => right(value)
      }
    }
    final case class Left[A, B](value: A) extends Sum[A, B]
    final case class Right[A, B](value: B) extends Sum[A, B]
  }
  // Implement fold for Sum.

}
