package essential_scala

import essential_scala.CaseClass.Cat
import essential_scala.Object.{Director, Film}

object PatterMatching extends App {

  // 3.5.3.1 Feed the Cats

  /**
   * Define an object ChipShop with a method willServe. This method should accept a Cat and return true
   * if the cat’s favourite food is chips, and false otherwise. Use pattern matching.
   */

  object ChipShop {
    def willServe(cat: Cat): Boolean = cat match {
      case Cat(_, _, "Chips") => true
      case _ => false
    }
  }

  // 3.5.3.2 Get Off My Lawn!

  /**
   * In this exercise we’re going to write a simulator of my Dad, the movie critic. It’s quite simple:
   * any movie directed by Clint Eastwood gets a rating 10.0, any movie directed by John McTiernan gets a 7.0,
   * while any other movie gets a 3.0. Implement an object called Dad with a method rate which accepts a
   * Film and returns a Double. Use pattern matching.
   */

  object Dad {
    def rate(film: Film): Double = film.director match {
      case Director("Clint", "Eastwood", _) => 10.0
      case Director("John", "McTiernan", _) => 7.0
      case _ => 3.0
    }
  }

}
