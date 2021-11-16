package scala_with_cats

import cats.{Eq, Show}
import cats.syntax.eq._ // for ===

object TypeClasses extends App {

  // 1.4.6 Exercise: Cat Show

  // Re-implement the Cat application from the previous section using Show instead of Printable.

  import cats.Show
  import cats.instances.int._    // for Show
  import cats.instances.string._ // for Show
  import cats.syntax.show._      // for show

  final case class Cat(name: String, age: Int, color: String)

  implicit val catShow: Show[Cat] = Show.show[Cat] { cat =>
    val name  = cat.name.show
    val age   = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }

  println(Cat("Garfield", 38, "ginger and black").show)

  // 1.5.5 Exercise: Equality, Liberty, and Felinity
  //  Implement an instance of Eq for our running Cat example:
  // Use this to compare the following pairs of objects for equality and inequality:

  implicit val catEq: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
    (cat1.name === cat2.name) &&
      (cat1.age === cat2.age) &&
        (cat1.color === cat2.color)
  }

  val cat1 = Cat("Garfield",   38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  cat1 === cat2 // res15: Boolean = false
  cat1 =!= cat2 // res16: Boolean = true

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  import cats.instances.option._ // for Eq

  optionCat1 === optionCat2 // res17: Boolean = false
  optionCat1 =!= optionCat2 // res18: Boolean = true

}
