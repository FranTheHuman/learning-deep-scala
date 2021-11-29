package scala_with_cats

import cats.Functor
import scala_with_cats.Functors.Printable

object Functors extends App {

  // 3.5.4 Exercise: Branching out with Functors
  // Write a Functor for the following binary tree data type.
  // Verify that the code works as expected on instances of Branch and Leaf:

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }
  }

  // Branch(Leaf(10), Leaf(20)).map(_ * 2) // error: value map is not a member of repl.Session.App0.Branch[Int]

  /*
    Oops! This falls foul of the same invariance problem we discussed in Section 1.6.1.
    The compiler can find a Functor instance for Tree but not for Branch or Leaf
   */

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)

    def leaf[A](value: A): Tree[A] =
      Leaf(value)
  }

 //  Tree.leaf(100).map(_ * 2)

  // 3.5.5.1 Exercise: Showing off with Contramap

  // Implement the contramap method for Printable above.
  // Start with the following code template and replace the ??? with a working method body:

  trait Printable[A] { self =>
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] =
      new Printable[B] {
        def format(value: B): String =
          self.format(func(value))
      }
  }

  // If you get stuck, think about the types. You need to turn value, which is of type B, into a String.
  // What functions and methods do you have available and in what order do they need to be combined?

  
  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  // For testing purposes, let’s define some instances of Printable for String and Boolean:

  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String =
        s"'${value}'"
    }

  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if(value) "yes" else "no"
    }

  format("hello") // res2: String = "'hello'"
  format(true) // res3: String = "yes"

  // Now define an instance of Printable for the following Box case class.
  // You’ll need to write this as an implicit def as described in Section 1.2.3:

  final case class Box[A](value: A)

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    new Printable[Box[A]] {
      override def format(value: Box[A]): String = p.format(value.value)
    }

  implicit def boxPrintable_2[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](_.value)

  // Rather than writing out the complete definition from scratch (new Printable[Box] etc…),
  // create your instance from an existing instance using contramap.


    // Your instance should work as follows:

    format(Box("hello world"))(boxPrintable_2[String](stringPrintable)) // res4: String = "'hello world'"
    format(Box(true))(boxPrintable_2[Boolean](booleanPrintable)) // res5: String = "yes"

  // If we don’t have a Printable for the type inside the Box, calls to format should fail to compile:

  implicit val integerPrintable: Printable[Int] =
    new Printable[Int] {
      override def format(value: Int): String = value.toString
    }

    format(Box(123))(boxPrintable_2[Int](integerPrintable))
  // error: could not find implicit value for parameter p: repl.Session.App1.Printable[repl.Session.App1.Box[Int]]
  //       def encode(value: B): String =
  //                 ^

}
