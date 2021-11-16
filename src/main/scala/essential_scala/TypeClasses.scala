package essential_scala

object TypeClasses extends App {

  // 7.1.6.1 More Orderings

  /*
  Define an Ordering that orders Ints from lowest to highest by absolute value. The following test cases should pass.
   */

  implicit val absOrdering: Ordering[Int] = Ordering.fromLessThan[Int](Math.abs(_) < Math.abs(_))

  assert(List(-4, -1, 0, 2, 3).sorted(absOrdering) == List(0, -1, 2, 3, -4))
  assert(List(-4, -3, -2, -1).sorted(absOrdering) == List(-1, -2, -3, -4))

  // Now make your ordering an implicit value, so the following test cases work.

  assert(List(-4, -1, 0, 2, 3).sorted == List(0, -1, 2, 3, -4))
  assert(List(-4, -3, -2, -1).sorted == List(-1, -2, -3, -4))

  // 7.1.6.2 Rational Orderings

  // Scala doesn’t have a class to represent rational numbers, but we can easily implement one ourselves.

  final case class Rational(numerator: Int, denominator: Int)

  // Implement an Ordering for Rational to order rationals from smallest to largest. The following test case should pass.

  implicit val rationalOrdering: Ordering[Rational] = Ordering.fromLessThan[Rational] { (x, y) =>
    (x.numerator.toDouble / x.denominator.toDouble) <
      (y.numerator.toDouble / y.denominator.toDouble)
  }

 assert(List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted == List(Rational(1, 3), Rational(1, 2), Rational(3, 4)))

  // 7.2.5.1 Ordering Orders
  // Here is a case class to store orders of some arbitrary item.

  final case class Order(units: Int, unitPrice: Double) {
    val totalPrice: Double = units * unitPrice
  }

  val orderByTotalPrice: Ordering[Order] = Ordering.fromLessThan[Order] { (x, y) =>
    x.totalPrice < y.totalPrice
  }

  val orderByUnits: Ordering[Order] = Ordering.fromLessThan[Order] { (x, y) =>
    x.units < y.units
  }

  val orderByUnitsPrice: Ordering[Order] = Ordering.fromLessThan[Order] { (x, y) =>
    x.unitPrice < y.unitPrice
  }
  /*
  We have a requirement to order Orders in three different ways:
    by totalPrice;
    by number of units; and
    by unitPrice.
  Implement and package implicits to provide these orderings, and justify your packaging. */

  // 7.3.4.1 Equality

  /*
      Scala provides two equality predicates: by value (==) and by reference (eq). Nonetheless, we sometimes need additional predicates.
      For instance, we could compare people by just email address if we were validating new user accounts in some web application.
      Implement a trait Equal of some type A, with a method equal that compares two values of type A and returns a Boolean.
      Equal is a type class.
   */

  trait Equal[A] {
    def equal(x: A, y: A): Boolean
  }

  object Equal {
    def apply[A](implicit instance: Equal[A]): Equal[A] = instance
  }

  // Our Person class is
  case class Person(name: String, email: String)

  // Implement instances of Equal that compare for equality by email address only, and by name and email.

  object EmailEqualImplicits {
    implicit object EmailEqual extends Equal[Person] {
      override def equal(x: Person, y: Person): Boolean =
        x.email == y.email
    }
  }

  object EmailNameEqualImplicits {
    implicit object EmailNameEqual extends Equal[Person] {
      override def equal(x: Person, y: Person): Boolean =
        x.email == y.email && x.name == y.name
    }
  }

  // 7.4.4.1 Equality Again
  // In the previous section we defined a trait Equal along with some implementations for Person.

  /*
    Implement an object called Eq with an apply method.
    This method should accept two explicit parameters of type A and an implicit Equal[A].
    It should perform the equality checking using the provided Equal.
    With appropriate implicits in scope, the following code should work
   */

  object Eq {
    def apply[A](x: A, y: A)(implicit e: Equal[A]): Boolean =
      e.equal(x, y)
  }

  import EmailNameEqualImplicits._
  Eq(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com"))

  /*
    Package up the different Equal implementations as implicit values in their own objects,
    and show you can control the implicit selection by changing which object is imported.
   */

  /*
    Now implement an interface on the companion object for Equal using the no-argument apply method pattern.
    The following code should work.
   */
  import EmailNameEqualImplicits._
  Equal[Person].equal(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com"))
}
