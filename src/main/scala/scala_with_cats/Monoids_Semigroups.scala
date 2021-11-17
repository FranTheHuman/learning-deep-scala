package scala_with_cats

object Monoids_Semigroups extends App {

  // 2.3 Exercise: The Truth About Monoids
  /*
    We’ve seen a few examples of monoids but there are plenty more to be found. Consider Boolean.
    How many monoids can you define for this type? For each monoid,
    define the combine and empty operations and convince yourself that the monoid laws hold.
    Use the following definitions as a starting point:
   */

  object TheTruthAboutMonoids {
    trait Semigroup[A] {
      def combine(x: A, y: A): A
    }

    trait Monoid[A] extends Semigroup[A] {
      def empty: A
    }

    object Monoid {
      def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
    }

    implicit val booleanAndMonoid: Monoid[Boolean] =
      new Monoid[Boolean] {
        def combine(a: Boolean, b: Boolean) = a && b
        def empty = true
      }

    implicit val booleanOrMonoid: Monoid[Boolean] =
      new Monoid[Boolean] {
        def combine(a: Boolean, b: Boolean) = a || b
        def empty = false
      }

    implicit val booleanEitherMonoid: Monoid[Boolean] =
      new Monoid[Boolean] {
        def combine(a: Boolean, b: Boolean) = (a && !b) || (!a && b)
        def empty = false
      }

    implicit val booleanXnorMonoid: Monoid[Boolean] =
      new Monoid[Boolean] {
        def combine(a: Boolean, b: Boolean) = (!a && b) && (a && !b)
        def empty = true
      }
  }

  // 2.4 Exercise: All Set for Monoids

  /* What monoids and semigroups are there for sets? */

    object SetMonoids {
      import TheTruthAboutMonoids._

      implicit def setMonoid[A]: Monoid[Set[A]] =
        new Monoid[Set[A]] {
          override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
          override def empty: Set[A] = Set.empty[A]
        }

      val intSetMonoid = Monoid[Set[Int]]
      val strSetMonoid = Monoid[Set[String]]

      intSetMonoid.combine(Set(1, 2), Set(2, 3))
      // res18: Set[Int] = Set(1, 2, 3)
      strSetMonoid.combine(Set("A", "B"), Set("B", "C"))
      // res19: Set[String] = Set("A", "B", "C")
    }

  // 2.5.4 Exercise: Adding All The Things

  /*
    The cutting edge SuperAdder v3.5a-32 is the world’s first choice for adding together numbers.
    The main function in the program has signature def add(items: List[Int]): Int.
    In a tragic accident this code is deleted! Rewrite the method and save the day!
   */

    object SaveTheDay {

      import cats.Monoid
      import cats.instances.int._
      import cats.syntax.semigroup._ // for |+| = combine

      def add_easy(items: List[Int]): Int =
        items.sum // items.foldLeft(0)(_ + _)

      def add_monoid(items: List[Int]): Int =
        items.foldLeft(Monoid[Int].empty)(_ |+| _)

      /*
        Well done! SuperAdder’s market share continues to grow, and now there is demand for additional functionality.
        People now want to add List[Option[Int]]. Change add so this is possible.
        The SuperAdder code base is of the highest quality, so make sure there is no code duplication!
      */

      def superAdd[A](items: List[A])(implicit monoid: Monoid[A]): A =
        items.foldLeft(monoid.empty)(_ |+| _) // (acc, it) => monoid.combine(acc, it)

      def superAdd_2[A: Monoid](items: List[A]): A =
        items.foldLeft(Monoid[A].empty)(_ |+| _)


      superAdd_2(List(1, 2, 3)) // res10: Int = 6
      superAdd_2(List(Some(1), None, Some(2), None, Some(3))) // res11: Option[Int] = Some(6)


      // SuperAdder is entering the POS (point-of-sale, not the other POS) market. Now we want to add up Orders:

      case class Order(totalCost: Double, quantity: Double)

      // We need to release this code really soon so we can’t make any modifications to add. Make it so!

      implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
        override def empty: Order = Order(0, 0)

        override def combine(x: Order, y: Order): Order =
          Order(
            totalCost = x.totalCost + y.totalCost,
            quantity = x.quantity + y.quantity
          )
      }

    }



}
