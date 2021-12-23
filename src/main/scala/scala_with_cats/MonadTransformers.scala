package scala_with_cats

import cats.data.EitherT

import scala.concurrent.Future
import scala.concurrent.impl.Promise
import scala.util.{Failure, Success}

object MonadTransformers extends App {

  // 5.4 Exercise: Monads: Transform and Roll Out

  /*
  The Autobots, well-known robots in disguise, frequently send messages during battle requesting the power
  levels of their team mates. This helps them coordinate strategies and launch devastating attacks.
  The message sending method looks like this:
   */

  import cats.implicits.catsSyntaxEitherId
  import scala.concurrent.ExecutionContext.Implicits.global

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case None => EitherT.left(Future("Autobot unreachable"))
      case Some(powerLvl) => EitherT.right(Future(powerLvl))
    }

  /*
  Transmissions take time in Earth’s viscous atmosphere, and messages are occasionally lost due to satellite malfunction
  or sabotage by pesky Decepticons8. Responses are therefore represented as a stack of monads:
   */

  // type Response[A] = Future[Either[String, A]]

  /*
  Optimus Prime is getting tired of the nested for comprehensions in his neural matrix.
  Help him by rewriting Response using a monad transformer.
   */

  import cats.data.EitherT

  type Response[A] = EitherT[Future, String, A]

  /*
  Now test the code by implementing getPowerLevel to retrieve data from a set of imaginary allies. Here’s the data we’ll use:
   */

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  /*
    If an Autobot isn’t in the powerLevels map, return an error message reporting that they were unreachable.
    Include the name in the message for good effect.
   */

  /*
    Two autobots can perform a special move if their combined power level is greater than 15. Write a second method,
    canSpecialMove, that accepts the names of two allies and checks whether a special move is possible.
    If either ally is unavailable, fail with an appropriate error message:
   */
  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
   for {
      lvl1 <- getPowerLevel(ally1)
      lvl2 <- getPowerLevel(ally2)
    } yield (lvl1 + lvl2) >= 15

  /*
  Finally, write a method tacticalReport that takes two ally names and prints a message saying whether they can perform a special move:
   */

  import scala.concurrent.Await
  import scala.concurrent.duration._

  def tacticalReport(ally1: String, ally2: String): String = {
    Await.result(canSpecialMove(ally1, ally2).value, 1.second) match {
      case Left(msg) =>
        s"Comms error: $msg"
      case Right(true)  =>
        s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) =>
        s"$ally1 and $ally2 need a recharge."
    }
  }

  // You should be able to use report as follows:
  println(tacticalReport("Jazz", "Bumblebee"))
  // res13: String = "Jazz and Bumblebee need a recharge."
  println(tacticalReport("Bumblebee", "Hot Rod"))
  // res14: String = "Bumblebee and Hot Rod are ready to roll out!"
    println(tacticalReport("Jazz", "Ironhide"))0
  // res15: String = "Comms error: Ironhide unreachable"
}
