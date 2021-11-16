package scala_with_cats

object Monoids_Semigroups extends App {

  // 2.3 Exercise: The Truth About Monoids
  /*
    We’ve seen a few examples of monoids but there are plenty more to be found. Consider Boolean.
    How many monoids can you define for this type? For each monoid,
    define the combine and empty operations and convince yourself that the monoid laws hold.
    Use the following definitions as a starting point:
   */
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
  }

  // 2.4 Exercise: All Set for Monoids

  /* What monoids and semigroups are there for sets? */

  // 2.5.4 Exercise: Adding All The Things

  /*
    The cutting edge SuperAdder v3.5a-32 is the world’s first choice for adding together numbers.
    The main function in the program has signature def add(items: List[Int]): Int.
    In a tragic accident this code is deleted! Rewrite the method and save the day!
   */

  /*
    Well done! SuperAdder’s market share continues to grow, and now there is demand for additional functionality.
    People now want to add List[Option[Int]]. Change add so this is possible.
    The SuperAdder code base is of the highest quality, so make sure there is no code duplication!
   */

  // SuperAdder is entering the POS (point-of-sale, not the other POS) market. Now we want to add up Orders:
  case class Order(totalCost: Double, quantity: Double)

  // We need to release this code really soon so we can’t make any modifications to add. Make it so!
}
