package scala_with_cats

import cats.implicits._
import cats.{Monad, Semigroupal}

object Semigroupal_Applicative extends App {

  // 6.3.1.1 Exercise: The Product of Lists

  // Why does product for List produce the Cartesian product? We saw an example above. Here it is again.
  Semigroupal[List].product(List(1, 2), List(3, 4)) // List((1, 3), (1, 4), (2, 3), (2, 4))

  // We can also write this in terms of tupled.
  (List(1, 2), List(3, 4)).tupled // List((1, 3), (1, 4), (2, 3), (2, 4))

  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  def product[F[_]: Monad, A, B](x: F[A], y: F[B]): F[(A, B)] =
    x.flatMap(a => y.map(b => (a, b)))

  // 6.4.0.1 Exercise: Parallel List

  // Does List have a Parallel instance? If so, what does the Parallel instance do?
  (List(1, 2, 3), List(4, 5, 6)).tupled // List((1, 3), (1, 4), (2, 3), (2, 4))
  (List(1, 2, 3), List(4, 5, 6)).parTupled // List((1, 3), (2, 4))


}
