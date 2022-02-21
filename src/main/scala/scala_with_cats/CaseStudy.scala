package scala_with_cats

import cats.data.AndThen

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object CaseStudy extends App {

  // ---------- Testing Asynchronous Code ----------

  // 8.1 Abstracting over Type Constructors

  // We need to implement two versions of UptimeClient: an asynchronous one for use in production
  // and a synchronous one for use in our unit tests:

  trait RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int]
  }

  import cats.Id

  trait TestUptimeClient extends UptimeClient[Id] {
    def getUptime(hostname: String): Id[Int]
  }

  // The question is: what result type should we give to the abstract method in UptimeClient?
  // We need to abstract over Future[Int] and Int:

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  // You should now be able to flesh your definition of TestUptimeClient out into a full
  // class based on a Map[String, Int] as before.

  class Final_TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    def getUptime(hostname: String): Int =
      hosts.getOrElse(hostname, 0)
  }

  // 8.2 Abstracting over Monads

  // Let’s turn our attention to UptimeService. We need to rewrite it to abstract over the two types of UptimeClient.
  // We’ll do this in two stages: first we’ll rewrite the class and method signatures, then the method bodies.
  // Starting with the method signatures:

  // comment out the body of getTotalUptime (replace it with ??? to make everything compile);
  // add a type parameter F[_] to UptimeService and pass it on to UptimeClient.

  import cats.instances.list._ // for Traverse
  import cats.syntax.traverse._ // for traverse
  import cats.syntax.functor._ // for map
  import cats.Applicative

  class UptimeService[F[_] : Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  // Now uncomment the body of getTotalUptime. You should get a compilation error similar to the following:

  // <console>:28: error: could not find implicit value for
  //               evidence parameter of type cats.Applicative[F]
  //            hostnames.traverse(client.getUptime).map(_.sum)
  //

  // The problem here is that traverse only works on sequences of values that have an Applicative.
  // In our original code we were traversing a List[Future[Int]]. There is an applicative for Future so that was fine.
  // In this version we are traversing a List[F[Int]]. We need to prove to the compiler that F has an Applicative.
  // Do this by adding an implicit constructor parameter to UptimeService.

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new Final_TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  testTotalUptime()

  // ---------- Map-Reduce ----------

  // 9.2 Implementing foldMap

  // We saw foldMap briefly back when we covered Foldable.
  // It is one of the derived operations that sits on top of foldLeft and foldRight.
  // However, rather than use Foldable, we will re-implement foldMap here ourselves as it will provide useful insight
  // into the structure of map-reduce.

  // Start by writing out the signature of foldMap. It should accept the following parameters:
  // - a sequence of type Vector[A];
  // - a function of type A => B, where there is a Monoid for B;

  // You will have to add implicit parameters or context bounds to complete the type signature.

  import cats.Monoid

  /** Single-threaded map-reduce function.
   * Maps `func` over `values` and reduces using a `Monoid[B]`.
   */
  // def foldMap[A, B: Monoid](a: Vector[A])(f: A => B): Vector[B] = ???

  // Now implement the body of foldMap
    // - start with a sequence of items of type A;
    // - map over the list to produce a sequence of items of type B;
    // - use the Monoid to reduce the items to a single B.

  import cats.syntax.semigroup._ // for |+|

  def foldMap[A, B : Monoid](a: Vector[A])(func: A => B): B =
    a.map(func).foldLeft(Monoid[B].empty)(_ |+| _)

  // 9.3.3 Implementing parallelFoldMap

  // Implement a parallel version of foldMap called parallelFoldMap. Here is the type signature:

  import scala.concurrent.ExecutionContext.Implicits.global

  def parallelFoldMap[A, B : Monoid] (values: Vector[A]) (func: A => B): Future[B] = {

    // 1. Calculate the number of items to pass to each CPU:
    val numCores: Int = Runtime.getRuntime.availableProcessors()
    val groupSize: Int = (1.0 * values.size / numCores).ceil.toInt

    // 2. Create one group for each CPU:
    val groups: Iterator[Vector[A]] = values grouped groupSize

    // 3. Create a future to foldMap each group:
    val futures: Iterator[Future[B]] =
      groups map { group =>
        Future {
          group.foldLeft(Monoid[B].empty)(_ |+| func(_))
        }
      }
    val futures_2: Iterator[Future[B]] =
      groups.map(group => Future(foldMap(group)(func)))

    // 4. foldMap over the groups to calculate a final result:
    Future.sequence(futures) map { iterable =>
      iterable.foldLeft(Monoid[B].empty)(_ |+| _)
    }

  }

  val result: Future[Int] =
    parallelFoldMap((1 to 1000000).toVector)(identity)

  Await.result(result, 1.second)

  // Use the techniques described above to split the work into batches, one batch per CPU.
  // Process each batch in a parallel thread. Refer back to Figure 15 if you need to review the overall algorithm.

  // For bonus points, process the batches for each CPU using your implementation of foldMap from above.

  // 9.3.4 parallelFoldMap with more Cats

  /*
    Although we implemented foldMap ourselves above, the method is also available as part of the
    Foldable type class we discussed in Section 7.1.

    Reimplement parallelFoldMap using Cats’ Foldable and Traverseable type classes.
   */

  import cats.syntax.foldable._  // for combineAll and foldMap

  def parallelFoldMapWithCats[A, B : Monoid] (values: Vector[A]) (func: A => B): Future[B] = {

    // 1. Calculate the number of items to pass to each CPU:
    val numCores: Int = Runtime.getRuntime.availableProcessors()
    val groupSize: Int = (1.0 * values.size / numCores).ceil.toInt

    values
      .grouped(groupSize)
      .toVector
      .traverse(group => Future(group.toVector.foldMap(func)))
      .map(_.combineAll)

  }

  // ---------- Data Validation ----------

  // We need a Semigroup for E. Then we can combine values of E using the combine method or its associated |+| syntax:

  import cats.Semigroup
  import cats.data.Validated
  import cats.syntax.semigroup._ // for |+|
  import cats.syntax.apply._     // for mapN
  import cats.data.Validated._   // for Valid and Invalid

  sealed trait Predicate[E, A] {
    def and(that: Predicate[E, A]): Predicate[E, A] =
      And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] =
      Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)

        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)

        case Or(left, right) =>
          left(a) match {
            case Valid(_)   => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(_)   => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }

  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
  final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

  // 10.4.2 Checks


  // We’ll use Check to represent a structure we build from a Predicate that also allows transformation of its input.
  // Implement Check with the following interface:

  sealed trait Check[E, A, B] {
    import Check._

    def apply(a: A): Validated[E, B] = ???

    def map[C](func: B => C): Check[E, A, C] =
      Map[E, A, B, C](this, func)

    def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] =
      FlatMap[E, A, B, C](this, f)

    def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
      AndThen[E, A, B, C](this, that)
  }

  object Check {

    final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
      def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
        check(in)
          .map(func)
    }


    final case class FlatMap[E, A, B, C](check: Check[E, A, B], func: B => Check[E, A, C]) extends Check[E, A, C] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
        check(a).withEither(_.flatMap(b => func(b)(a).toEither))
    }

    final case class Pure[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
      def apply(in: A)(implicit s: Semigroup[E]): Validated[E, A] =
        pred(in)
    }

    final case class AndThen[E, A, B, C](check1: Check[E, A, B], check2: Check[E, B, C]) extends Check[E, A, C] {
        def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
          check1(a).withEither(a => a.flatMap(b => check2(b).toEither))
      }

    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
        Pure(pred)

  }

  // Go ahead and implement flatMap for Check, and then we’ll see a more generally useful method.

  // Implement andThen now!

  // 10.5 Kleislis

  // ---------- CRDTs ----------

  // 11.1 Eventual Consistency
  // 11.2 The GCounter
  // 11.2.1 Simple Counters
  // 11.2.2 GCounters
  // 11.2.3 Exercise: GCounter Implementation
  // 11.3 Generalisation
  // 11.3.2 Exercise: BoundedSemiLattice Instances
  // 11.3.3 Exercise: Generic GCounter
  // 11.4 Abstracting GCounter to a Type Class
  // 11.5 Abstracting a Key Value Store

}
