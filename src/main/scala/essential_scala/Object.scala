package essential_scala

import essential_scala.Object.parts

import scala.util.{Failure, Success, Try}

object Object extends App {

  class Adder(amount: Int) {
    def apply(am: Int): Int = amount + am
  }
  val adder = new Adder(3)
  println(adder(4))

  // -------------------------------------------

  object Adder2 {
    def apply(a: Int): Int = a
  }

  // 3.3.2.1 Friendly Person Factory

  /**
   * Implement a companion object for Person containing an apply method that
   * accepts a whole name as a single string rather than individual first and last
   * names.
   *
   * Tip: you can split a String into an Array of components as follows:
   */

  val parts = "John Doe".split(" ") // parts: Array[String] = Array(John, Doe)
  parts(0) // res3: String = John

  case class Person(firstName: String, lastName: String)
  object Person {
    def apply(completeName: String): Person = {
      Try { completeName.split(" ") } match {
        case Success(parts) if parts.length == 2 => Person(parts(0), parts(1))
        case _ => Person("", "")
      }
    }
  }

  assert( Person("Marce Perez") == new Person("Marce", "Perez") )

  // 3.3.2.2 Extended Body of Work

  /**
   * Write companion objects for Director and Film as follows:
   *
   * • the Director companion object should contain:
   *  – an apply method that accepts the same parameters as the constructor of the class and returns a new Director;
   *  – a method older that accepts two Directors and returns the
   *    oldest of the two.
   *
   * • the Film companion object should contain:
   *  – an apply method that accepts the same parameters as the constructor of the class and returns a new Film;
   *  – a method highestRating that accepts two Films and returns the highest imdbRating of the two;
   *  – a method oldestDirectorAtTheTime that accepts two Films and returns the Director who was oldest at the respective time of filming.
   *
   */

   case class Director(firstName: String, lastName: String, yearOfBirth: Int)
   case class Film(name: String, yearOfRelease: Int, imdbRating: Double, director: Director) {
     def directorsAge: Int = director.yearOfBirth - yearOfRelease
   }

   object Director {
     def apply(firstName: String, lastName: String, yearOfBirth: Int): Director =
       Director(firstName, lastName, yearOfBirth)
     def older(d1: Director, d2: Director): Director =
       if (d1.yearOfBirth < d2.yearOfBirth) d1 else d2
   }

  object Film {
    def apply(name: String, yearOfRelease: Int, imdbRating: Double, director: Director): Film =
      Film(name, yearOfRelease, imdbRating, director)
    def highestRating(f1: Film, f2: Film): Double = {
      val rating1 = f1.imdbRating
      val rating2 = f2.imdbRating
      if (rating1 > rating2) rating1 else rating2
    }
    def oldestDirectorAtTheTime(f1: Film, f2: Film): Director =
      if (f1.directorsAge > f2.directorsAge) f1.director else f2.director
  }

  //  3.3.2.3 Type or Value?

  /**
   * The similarity in naming of classes and companion objects tends to cause confusion for new Scala developers. When reading a block of code it is important
   * to know which parts refer to a class or type and which parts refer to a singleton
   * object or value.
   * This is the inspiration for the new hit quiz, Type or Value?, which we will be
   * piloting below. In each case identify whether the word Film refers to the
   * type or value:
   */

    //  val prestige: Film = bestFilmByChristopherNolan() - TYPE
    // new Film("Last Action Hero", 1993, mcTiernan) - TYPE
    // Film("Last Action Hero", 1993, mcTiernan) - VALUE
    // Film.newer(highPlainsDrifter, thomasCrownAffair) - VALUE
    // Film.type - VALUE

}
