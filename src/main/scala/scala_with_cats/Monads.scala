package scala_with_cats

import cats.{Eval, MonadError}

import scala.annotation.tailrec
import scala.util.Try

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

  // ---------------------------------------- EITHER ----------------------------------------

  // 4.4.5 Exercise: What is Best?

  import cats.syntax.either._

  sealed trait LoginError extends Product with Serializable

  final case class UserNotFound(username: String) extends LoginError

  final case class PasswordIncorrect(username: String) extends LoginError

  case object UnexpectedError extends LoginError

  case class User(username: String, password: String)

  type LoginResult = Either[LoginError, User]

  // Choose error-handling behaviour based on type:
  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFound(u) =>
        println(s"User not found: $u")

      case PasswordIncorrect(u) =>
        println(s"Password incorrect: $u")

      case UnexpectedError =>
        println(s"Unexpected error")
    }

  val result1: LoginResult = User("dave", "passw0rd").asRight
  // result1: LoginResult = Right(User("dave", "passw0rd"))
  val result2: LoginResult = UserNotFound("dave").asLeft
  // result2: LoginResult = Left(UserNotFound("dave"))

  result1.fold(handleError, println) // User(dave,passw0rd)
  result2.fold(handleError, println) // User not found: dave

  // Is the error handling strategy in the previous examples well suited for all purposes?
  // What other features might we want from error handling?

  /*
    * Error recovery is important when processing large jobs.
      We don’t want to run a job for a day and then find it failed on the last element.
    * Error reporting is equally important. We need to know what went wrong, not just that something went wrong.
    * In a number of cases, we want to collect all the errors, not just the first one we encountered.
      A typical example is validating a web form.
      It’s a far better experience to report all errors to the user when they submit a form than to report them one at a time.
   */

  // 4.5.4 Exercise: Abstracting

  import cats.implicits.catsSyntaxApplicativeId

  // Implement a method validateAdult with the following signature
  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    me.ensuring(age >= 18, "Age must be greater than or equal to 18").pure(age)

  // When passed an age greater than or equal to 18 it should return that value as a success.
  // Otherwise it should return a error represented as an IllegalArgumentException.

  // Here are some examples of use.

  println(validateAdult[Try](18)) // res7: Try[Int] = Success(18)
  // println(validateAdult[Try](8)) // res8: Try[Int] = Failure(
  //   java.lang.IllegalArgumentException: Age must be greater than or equal to 18
  // )
  type ExceptionOr[A] = Either[Throwable, A]
  // println(validateAdult[ExceptionOr](-1)) // res9: ExceptionOr[Int] = Left(
  //   java.lang.IllegalArgumentException: Age must be greater than or equal to 18
  // )

  // ---------------------------------------- Eval ----------------------------------------

  // 4.6.5 Exercise: Safer Folding using Eval
  // The naive implementation of foldRight below is not stack safe. Make it so using Eval:

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil =>
        acc
    }

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        acc
    }

  // ---------------------------------------- Writer ----------------------------------------

  // 4.7.3 Exercise: Show Your Working
  // Writers are useful for logging operations in multi-threaded environments.
  // Let’s confirm this by computing (and logging) some factorials.

  // The factorial function below computes a factorial and prints out the intermediate steps as it runs.
  // The slowly helper function ensures this takes a while to run, even on the very small examples below:
  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  // Here’s the output—a sequence of monotonically increasing values:
  factorial(5)

  // If we start several factorials in parallel, the log messages can become interleaved on standard out.
  // This makes it difficult to see which messages come from which computation:

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.duration._

  Await.result(Future.sequence(Vector(
    Future(factorial(5)),
    Future(factorial(5))
  )), 5.seconds)

  // Rewrite factorial so it captures the log messages in a Writer.
  // Demonstrate that this allows us to reliably separate the logs for concurrent computations.

  import cats.data.Writer
  import cats.instances.vector._
  import cats.syntax.applicative._

  type Logged[A] = Writer[Vector[String], A]
  42.pure[Logged]

  import cats.syntax.writer._

  Vector("Message").tell

  import cats.instances.vector._ // for Monoid

  41.pure[Logged].map(_ + 1)

  def factorialW(n: Int): Logged[Int] = for {
    result <- if (n == 0) 1.pure[Logged] else slowly(factorialW(n - 1).map(_ * n))
    _ <- Vector(s"fact $n $result").tell
  } yield result

  val (log, res) = factorialW(5).run

  // We can run several factorials in parallel as follows, capturing their logs independently without fear of interleaving:

  Await.result(Future.sequence(Vector(
    Future(factorialW(5)),
    Future(factorialW(5))
  )).map(_.map(_.written)), 5.seconds)

  // ---------------------------------------- Reader ----------------------------------------

  // 4.8.3 Exercise: Hacking on Readers

  // The classic use of Readers is to build programs that accept a configuration as a parameter.
  // Let’s ground this with a complete example of a simple login system.
  // Our configuration will consist of two databases: a list of valid users and a list of their passwords:

  final case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  // Start by creating a type alias DbReader for a Reader that consumes a Db as input.
  // This will make the rest of our code shorter.

  import cats.data.Reader

  type DbReader[A] = Reader[Db, A]

  // Now create methods that generate DbReaders to look up the username for an Int user ID,
  // and look up the password for a String username. The type signatures should be as follows:

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  // Finally create a checkLogin method to check the password for a given user ID.
  // The type signature should be as follows

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
    username <- findUsername(userId)
    result <- username match {
      case Some(user) => checkPassword(user, password)
      case None => false.pure[DbReader]
    }
  } yield result

  // You should be able to use checkLogin as follows:

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  println(checkLogin(1, "zerocool").run(db)) // res7: cats.package.Id[Boolean] = true
  println(checkLogin(4, "davinci").run(db)) // res8: cats.package.Id[Boolean] = false

  /*
    Readers are most useful in situations where:

      * we are constructing a program that can easily be represented by a function;
      * we need to defer injection of a known parameter or set of parameters;
      * we want to be able to test parts of the program in isolation.

   */

  // ---------------------------------------- State ----------------------------------------

  // 4.9.3 Exercise: Post-Order Calculator

  /*

  The State monad allows us to implement simple interpreters for complex expressions,
  passing the values of mutable registers along with the result.
  We can see a simple example of this by implementing a calculator for post-order integer arithmetic expressions.

  In case you haven’t heard of post-order expressions before (don’t worry if you haven’t),
  they are a mathematical notation where we write the operator after its operands.
  So, for example, instead of writing 1 + 2 we would write: 1 2 +

  Although post-order expressions are difficult for humans to read, they are easy to evaluate in code.
  All we need to do is traverse the symbols from left to right, carrying a stack of operands with us as we go:

    when we see a number, we push it onto the stack;

    when we see an operator, we pop two operands off the stack, operate on them, and push the result in their place.

    This allows us to evaluate complex expressions without using parentheses.
    For example, we can evaluate (1 + 2) * 3)as follows:

    1 2 + 3 * // see 1, push onto stack
    2 + 3 *   // see 2, push onto stack
    + 3 *     // see +, pop 1 and 2 off of stack,
              //        push (1 + 2) = 3 in their place
    3 3 *     // see 3, push onto stack
    3 *       // see 3, push onto stack
    *         // see *, pop 3 and 3 off of stack,
              //        push (3 * 3) = 9 in their place

  Let’s write an interpreter for these expressions. We can parse each symbol into a State instance representing a
  transformation on the stack and an intermediate result.
  The State instances can be threaded together using flatMap to produce an interpreter for any sequence of symbols.

  Start by writing a function evalOne that parses a single symbol into an instance of State.
  Use the code below as a template. Don’t worry about error handling for now—if the stack is in the wrong configuration,
  it’s OK to throw an exception.

   */

  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)
  }

  /*
  If this seems difficult, think about the basic form of the State instances you’re returning.
  Each instance represents a functional transformation from a stack to a pair of a stack and a result.
  You can ignore any wider context and focus on just that one step:

  State[List[Int], Int] { oldStack =>
    val newStack = someTransformation(oldStack)
    val result   = someCalculation
    (newStack, result)
  }

    Feel free to write your Stack instances in this form or as sequences of the convenience constructors we saw above.

   */


  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ =>
        sys.error("Nope")
    }

  // evalOne allows us to evaluate single-symbol expressions as follows. We call runA supplying
  // Nil as an initial stack, and call value to unpack the resulting Eval instance:

  evalOne("42").runA(Nil).value // res10: Int = 42

  // We can represent more complex programs using evalOne, map, and flatMap.
  // Note that most of the work is happening on the stack, so we ignore the results of the intermediate steps for evalOne("1") and evalOne("2"):

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans
  // program: cats.data.IndexedStateT[cats.Eval, List[Int], List[Int], Int] = cats.data.IndexedStateT@3afcc7dd

  program.runA(Nil).value // res11: Int = 3
  // Generalise this example by writing an evalAll method that computes the result of a List[String].
  // Use evalOne to process each symbol, and thread the resulting State monads together using flatMap.
  // Your function should have the following signature:

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (a, b) =>
      a.flatMap(_ => evalOne(b))
    }

  // We can use evalAll to conveniently evaluate multi-stage expressions:

  val multistageProgram = evalAll(List("1", "2", "+", "3", "*")) // multistageProgram: CalcState[Int] = cats.data.IndexedStateT@228a6340

  multistageProgram.runA(Nil).value // res13: Int = 9

  // Because evalOne and evalAll both return instances of State, we can thread these results together using flatMap.
  // evalOne produces a simple stack transformation and evalAll produces a complex one,
  // but they’re both pure functions and we can use them in any order as many times as we like:

  val biggerProgram = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  println(biggerProgram.runA(Nil).value)

  // Complete the exercise by implementing an evalInput function that splits an input String into symbols,
  // calls evalAll, and runs the result with an initial stack.

  def evalInput(input: String): Int =
    evalAll(input.split("").toList).runA(Nil).value

  // ---------------------------------------- Custom Monads ----------------------------------------

  // 4.10.1 Exercise: Branching out Further with Monads

  // Let’s write a Monad for our Tree data type from last chapter. Here’s the type again:

  object CustomMonads {
    sealed trait Tree[+A]

    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    final case class Leaf[A](value: A) extends Tree[A]

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
    def leaf[A](value: A): Tree[A] = Leaf(value)

    implicit val treeMonad: cats.Monad[Tree] = new cats.Monad[Tree] {
      override def pure[A](a: A): Tree[A] = Leaf(a)

      override def map[A, B](value: Tree[A])(f: A => B): Tree[B] = value match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(value) => Leaf(f(value))
      }

      override def flatMap[A, B](value: Tree[A])(f: A => Tree[B]): Tree[B] = value match {
        case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
        case Leaf(value) => f(value)
      }

      override def tailRecM[A, B](value: A)(f: A => Tree[Either[A, B]]): Tree[B] =
        flatMap(f(value)) {
          case Left(value) => tailRecM(value)(f)
          case Right(value) => Leaf(value)
        }

      def tailRecM_tailRecursive[A, B](arg: A)(func: A => Tree[Either[A, B]]): Tree[B] = {
        @tailrec
        def loop(open: List[Tree[Either[A, B]]], closed: List[Option[Tree[B]]]): List[Tree[B]] =
          open match {
            case Branch(l, r) :: next =>
              loop(l :: r :: next, None :: closed)

            case Leaf(Left(value)) :: next =>
              loop(func(value) :: next, closed)

            case Leaf(Right(value)) :: next =>
              loop(next, Some(pure(value)) :: closed)

            case Nil =>
              closed.foldLeft(Nil: List[Tree[B]]) { (acc, maybeTree) =>
                maybeTree.map(_ :: acc).getOrElse {
                  val left :: right :: tail = acc
                  branch(left, right) :: tail
                }
              }

          }
        loop(List(func(arg)), Nil).head
      }

    }

    branch(leaf(3), leaf(4))

    import cats.syntax.functor._ // for map
    import cats.syntax.flatMap._ // for flatMap

    branch(leaf(100), leaf(200)).
      flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

    for {
      a <- branch(leaf(100), leaf(200))
      b <- branch(leaf(a - 10), leaf(a + 10))
      c <- branch(leaf(b - 1), leaf(b + 1))
    } yield c

  }

  // Verify that the code works on instances of Branch and Leaf, and that the Monad provides Functor-like behaviour for free.
  // Also verify that having a Monad in scope allows us to use for comprehensions,
  // despite the fact that we haven’t directly implemented flatMap or map on Tree.
  // Don’t feel you have to make tailRecM tail-recursive. Doing so is quite difficult.
  // We’ve included both tail-recursive and non-tail-recursive implementations in the solutions so you can check your work.

}

