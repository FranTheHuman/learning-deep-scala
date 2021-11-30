package scala_with_cats

object Monads extends App {

  // 4.1.2 Exercise: Getting Func-y

  /*
    Every monad is also a functor. We can define map in the same way for every monad using the existing methods,
    flatMap and pure:
   */

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(a => pure(func(a)))
  }

  // Try defining map yourself now.

  // 4.3.1 Exercise: Monadic Secret Identities

  type Id[A] = A

  // Implement pure, map, and flatMap for Id! What interesting discoveries do you uncover about the implementation?
//
   trait ID extends Monad[Id] {
     override def pure[A](a: A): Id[A] = a

     override def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = ???
   }

    def pure[A](a: A): Id[A] = a
    def map[A, B](initial: Id[A])(func: A => B): Id[B] =
      func(initial)
    def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] =
      func(initial)
}
