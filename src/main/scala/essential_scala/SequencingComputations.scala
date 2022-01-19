package essential_scala

object SequencingComputations extends App {

  // FOLD -> TRABAJAR CON INFORMACION GENERICA PIDIENDO EL DATO GENERICO Y COMO LABURAR CON ESE DATO



  // 5.1.3.1 Generic List

  // Our IntList type was defined as
  /*
  sealed trait IntList
  case object End extends IntList
  final case class Pair(head: Int, tail: IntList) extends IntList*/

  /**
   *  Change the name to LinkedList and make it generic in the type of data
   *  stored in the list.
   * */

  sealed trait LinkedList[A] {
    def length: Int = this match {
      case End() => 0
      case Pair(_, tail) => 1 + tail.length
    }
  }
  final case class End[A]() extends LinkedList[A]
  final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

  // 5.1.3.2 Working With Generic Types
  /*
    There isn’t much we can do with our LinkedList type.
    Remember that types define the available operations,
    and with a generic type like A there isn’t a concrete type to define any available operations.
    (Generic types are made concrete when a class is instantiated,
    which is too late to make use of the information in the definition of the class.)

    However, we can still do some useful things with our LinkedList! Implement length,
    returning the length of the LinkedList. Some test cases are below.
   */

  val example = Pair(1, Pair(2, Pair(3, End())))
  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End().length == 0)

  // 5.2.3.1 A Better Abstraction

  // We started developing an abstraction over sum, length, and product which we sketched out as
  /*
  def fold(end: Int, f: ???): Int =
    this match {
      case End => end
      case Pair(hd, tl) => f(hd, tl.abstraction(end, f))
    }*/

  // Rename this function to fold, which is the name it is usually known as, and finish the implementation.

  object IntList {
    sealed trait IntList {
      def fold(end: Int)(f: (Int, Int) => Int): Int =
        this match {
          case End => end
          case Pair(hd, tl) => f(hd, tl.fold(end)(f))
        }
      def sum: Int = fold(0)((hd, tl) => hd + tl)
      def length: Int = fold(0)((_, tl) => 1 + tl)
      def product: Int = fold(1)((hd, tl) => hd * tl)
    }
    case object End extends IntList
    final case class Pair(head: Int, tail: IntList) extends IntList
  }
  // Now reimplement sum, length, and product in terms of fold.

  // Implement a generalised version of fold and rewrite double in terms of it.
  def genericFold[A, B](end: A)(f: (B, A) => A): A = ???

  // 5.5.1 Map
    // APLICAR UNA FUNCION A CADA DATO DENTRO DE UN CONTEXTO TRANSFORMANDO EL VALOR

    // 5.5.4.1 Mapping Lists
      // Given the following list

    object MappingList {
      sealed trait LinkedList[A] {
        def map[B](fn: A => B): LinkedList[B] =
          this match {
            case Pair(hd, tl) => Pair(fn(hd), tl.map(fn))
            case End() => End[B]()
          }
      }
      case class Pair[A](hd: A, tl: LinkedList[A]) extends LinkedList[A]
      case class End[A]() extends LinkedList[A]
    }

    val list: MappingList.LinkedList[Int] =
      MappingList.Pair(1, MappingList.Pair(2, MappingList.Pair(3, MappingList.End())))
     /*
     double all the elements in the list;
     add one to all the elements in the list; and
     divide by three all the elements in the list. */

     list.map(_ * 2)
     list.map(_ + 1)
     list.map(_ / 3)

    // 5.5.4.2 Mapping Maybe
    // Implement map for Maybe.

    object MappingMaybe {
      sealed trait Maybe[A] {
        def flatMap[B](f: A => Maybe[B]): Maybe[B] = this match {
          case Full(value) => f(value)
          case Empty() => Empty[B]()
        }
        def map[B](f: A => B): Maybe[B] = this match {
          case Full(value) => Full(f(value))
          case Empty() => Empty[B]()
        }
        def mapFlatMap[B](f: A => B): Maybe[B] =
          flatMap(v => Full(f(v)))
      }
      final case class Full[A](value: A) extends Maybe[A]
      final case class Empty[A]() extends Maybe[A]
    }

    // For bonus points, implement map in terms of flatMap.

    // 5.5.2 FlatMap
    // APLICAR UNA SECUENCIA DE OPERACIONES DEJANDOLO DENTRO DEL CONTEXTO

    // 5.5.4.3 Sequencing Computations

    /*
        We’re going to use Scala’s builtin List class for this exercise as it has a flatMap method.

        Given this list

        val list = List(1, 2, 3)
        return a List[Int] containing both all the elements and their negation.
        Order is not important. Hint: Given an element create a list containing it and its negation.
     */

  import MappingMaybe._

    val list1 = List(1, 2, 3)
    list1.flatMap(value => List(value, -value))

  // Given this list

  val list2: List[Maybe[Int]] = List(Full(3), Full(2), Full(1))

  // return a List[Maybe[Int]] containing None for the odd elements. Hint: If x % 2 == 0 then x is even.

  list2.map(maybe => maybe.flatMap[Int] {  x =>
    if (x % 2 == 0) Full(x) else Empty()
  })

    // 5.5.4.4 Sum
  // Recall our Sum type.

  sealed trait Sum[A, B] {
    def fold[C](left: A => C, right: B => C): C =
      this match {
        case Failure(a) => left(a)
        case Success(b) => right(b)
      }
    def map[C](f: B => C): Sum[A, C] = this match {
      case Failure(value) => Failure(value)
      case Success(value) => Success(f(value))
    }
    def flatMap[C](f: B => Sum[A, C]): Sum[A, C] = this match {
      case Failure(value) => Failure(value)
      case Success(value) => f(value)
    }
  }
  final case class Failure[A, B](value: A) extends Sum[A, B]
  final case class Success[A, B](value: B) extends Sum[A, B]

  /*
    To prevent a name collision between the built-in Either,
    rename the Left and Right cases to Failure and Success respectively.
   */

  /*
    Now things are going to get a bit trickier. We are going to implement map and flatMap,
    again using pattern matching in the Sum trait. Start with map.
    The general recipe for map is to start with a type like F[A] and apply a function A => B to get F[B].
    Sum however has two generic type parameters. To make it fit the F[A] pattern we’re going to fix
    one of these parameters and allow map to alter the other one.
    The natural choice is to fix the type parameter associated with Failure and allow map to alter a Success.
    This corresponds to “fail-fast” behaviour. If our Sum has failed, any sequenced computations don’t get run.

    In summary map should have type
    def map[C](f: B => C): Sum[A, C]
   */

  // Now implement flatMap using the same logic as map.
}
