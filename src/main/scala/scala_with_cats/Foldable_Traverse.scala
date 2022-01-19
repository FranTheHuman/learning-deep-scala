package scala_with_cats

import cats.implicits.catsSyntaxApplicativeId
import cats.{Applicative, Foldable}

class Foldable_Traverse {

  // 7.1.2 Exercise: Reflecting on Folds

  // Try using foldLeft and foldRight with an empty list as the accumulator and :: as the binary operator.
  // What results do you get in each case?

  List(1, 2, 3, 4).foldLeft(List.empty[Int])((a, i) => i :: a)
  // List(1, 2, 3).foldRight(Nil)(_ :: _)
  List(1, 2, 3, 4).foldRight(List.empty[Int])(_ :: _)

  // 7.1.3 Exercise: Scaf-fold-ing Other Methods

  // foldLeft and foldRight are very general methods.
  // We can use them to implement many of the other high-level sequence operations we know.
  // Prove this to yourself by implementing substitutes for List's map, flatMap, filter,
  // and sum methods in terms of foldRight.

  def mapListWithFold[A, B](list: List[A])(f: A => B): List[B] =
    list.foldLeft(List.empty[B]) { (acc, item) => f(item) :: acc }

  def flatMapWithFold[A, B](list: List[A])(f: A => List[B]): List[B] =
    list.foldLeft(List.empty[B]) { (acc, item) => f(item) ++ acc }

  def filterWithFold[A](list: List[A])(f: A => Boolean ): List[A] =
    list.foldLeft(List.empty[A]) { (acc, item) => if(f(item)) item :: acc else acc }

  def sumWithFoldAndNumeric[A](list: List[A])(implicit numeric: Numeric[A]): A =
    list.foldLeft(numeric.zero)(numeric.plus)

  import cats.Monoid

  def sumWithFoldAndMonoid[A](list: List[A])(implicit monoid: Monoid[A]): A =
    list.foldRight(monoid.empty)(monoid.combine)

  // 7.2.2.1 Exercise: Traversing with Vectors

  import cats.syntax.apply._ // for mapN

  def listTraverse[F[_]: Applicative, A, B]
  (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B]
  (list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  // What is the result of the following?

  import cats.instances.vector._ // for Applicative

  listSequence(List(Vector(1, 2), Vector(3, 4)))

  // RESULT:
  // Vector[List[Int]] = Vector( List(1, 3), List(1, 4), List(2, 3), List(2, 4) )

  // What about a list of three parameters?

  listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))

  // RESULT:
  // Vector[List[Int]] = Vector( List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5) ... )

  // 7.2.2.2 Exercise: Traversing with Options

  // Here’s an example that uses Options:

  import cats.instances.option._ // for Applicative

  def process(inputs: List[Int]) =
    listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)

  // What is the return type of this method? What does it produce for the following inputs?

  // : Option[List[Int]]

  process(List(2, 4, 6)) // Some(List(2, 4, 6))
  process(List(1, 2, 3)) // None

  // 7.2.2.3 Exercise: Traversing with Validated

  // Finally, here is an example that uses Validated:

  import cats.data.Validated
  import cats.instances.list._ // for Monoid

  type ErrorsOr[A] = Validated[List[String], A]

  def process_2(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if(n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }

  // What does this method produce for the following inputs?

  process_2(List(2, 4, 6)) // Valid(LList(2, 4, 6))
  process_2(List(1, 2, 3)) // Invalid(List("1 is not even", "3 is not even"))

}
