package katas

import cats.effect.{IO, IOApp}

import scala.language.postfixOps
import scala.language.implicitConversions

/**
 * Your task:
 *  Write a program that prints one line for each number from 1 to 100
 *  For multiples of three print Fizz instead of the number
 *  For the multiples of five print Buzz instead of the number
 *  For numbers which are multiples of both three and five print FizzBuzz instead of the number

    CORE:  a partir de un numero devolvemos ese numero o una palabra

 */

// --------------------------------- SIN CATS

object FizzBuzz {
  def toFizzBuzz(number: Int): String = number match {
    case num if isFizzBuzz(num)  => s"FizzBuzz"
    case num if isFizz(num)      => s"Fizz"
    case num if isBuzz(num)      => s"Buzz"
    case num => s"$num"
  }
  private def isFizz(num: Int): Boolean = num % 3 == 0
  private def isBuzz(num: Int): Boolean = num % 5 == 0
  private def isFizzBuzz(num: Int): Boolean = isFizz(num) && isBuzz(num)
}

object FizzBuzzT {
  import FizzBuzz._
  private def fizzBuzzT(numbers: Range): List[String] = numbers map toFizzBuzz toList
  def makeFizzBuzzLines(numbers: Range) = fizzBuzzT(numbers).mkString("\n")
}

object FizzBuzzApp extends App {
  import FizzBuzzT._
  println(makeFizzBuzzLines((1 to 100)))
}

// --------------------------------- CON CATS

object FizzBuzzCatsApp extends IOApp.Simple {
  import FizzBuzzT._
  val run = IO.println(makeFizzBuzzLines((1 to 100)))
}