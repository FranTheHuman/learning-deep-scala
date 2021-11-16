package essential_scala

object Collections extends App {

  // Sequences
  // Options
  // Maps


  // 6.1.9.2 Animals

  // Create a Seq containing the Strings "cat", "dog", and "penguin". Bind it to the name animals.
  val animals = Seq("cat", "dog", "penguin")
  // Append the element "tyrannosaurus" to animals and prepend the element "mouse".
  val animals2 = "mouse" +: animals :+ "tyrannosaurus"
  // What happens if you prepend the Int 2 to animals? Why? Try it out… were you correct?
  val animalsWithInt = 2 +: animals2


  // 6.1.9.3 Intranet Movie Database

  /*
    Let’s revisit our films and directors example from the Classes chapter.

    The code below is a partial rewrite of the previous sample code in which Films is stored as a field of Director
    instead of the other way around. Copy and paste this into a new Scala worksheet
    and continue with the exercises below:
   */

  case class Film(
                   name: String,
                   yearOfRelease: Int,
                   imdbRating: Double)

  case class Director(
                       firstName: String,
                       lastName: String,
                       yearOfBirth: Int,
                       films: Seq[Film])

  val memento = Film("Memento", 2000, 8.5)
  val darkKnight = Film("Dark Knight", 2008, 9.0)
  val inception = Film("Inception", 2010, 8.8)

  val highPlainsDrifter = Film("High Plains Drifter", 1973, 7.7)
  val outlawJoseyWales = Film("The Outlaw Josey Wales", 1976, 7.9)
  val unforgiven = Film("Unforgiven", 1992, 8.3)
  val granTorino = Film("Gran Torino", 2008, 8.2)
  val invictus = Film("Invictus", 2009, 7.4)

  val predator = Film("Predator", 1987, 7.9)
  val dieHard = Film("Die Hard", 1988, 8.3)
  val huntForRedOctober = Film("The Hunt for Red October", 1990, 7.6)
  val thomasCrownAffair = Film("The Thomas Crown Affair", 1999, 6.8)

  val films = Seq(
    memento,
    darkKnight,
    inception,
    highPlainsDrifter,
    outlawJoseyWales,
    unforgiven,
    granTorino,
    invictus,
    predator,
    dieHard,
    huntForRedOctober,
    thomasCrownAffair
  )

  val eastwood = Director("Clint", "Eastwood", 1930,
    Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus))

  val mcTiernan = Director("John", "McTiernan", 1951, Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair))

  val nolan = Director("Christopher", "Nolan", 1970, Seq(memento, darkKnight, inception))

  val someGuy = Director("Just", "Some Guy", 1990, Seq())

  val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

  /*
  Using this sample code, write implementations of the following methods:

  - Accept a parameter numberOfFilms of type Int — find all directors who have directed more than numberOfFilms:
  - Accept a parameter year of type Int—find a director who was born before that year:
  - Accept two parameters, year and numberOfFilms, and return a list of directors who were born before year who have also directed more than than numberOfFilms:
  - Accept a parameter ascending of type Boolean that defaults to true. Sort the directors by age in the specified order:
  */
  def directorsDirectedMoreThan(numberOfFilms: Int): Seq[Director] =
    directors.filter(_.films.size > numberOfFilms)

  def directorsBornedBeforeThan(year: Int): Seq[Director] =
    directors.filter(_.yearOfBirth < year)

  def directorsDirectedAndBornedThan(numberOfFilms: Int, year: Int): Seq[Director] =
    directorsDirectedMoreThan(numberOfFilms)
      .filter(directorsBornedBeforeThan(year).contains)

  def sortByAge(ascending: Boolean = true): Seq[Director] =
    if(ascending) directors.sortWith((a, b) => a.yearOfBirth < b.yearOfBirth)
    else directors.sortWith((a, b) => a.yearOfBirth > b.yearOfBirth)


  // 6.2.7.1 Heroes of the Silver Screen

  /*
  These exercises re-use the example code from the Intranet Movie Database exercise from the previous section:
    Nolan Films
   Starting with the definition of nolan, create a list containing the names of the films directed by Christopher Nola
   */

  val films_directed_by_nolan = nolan.films.map(_.name)

  // Starting with the definition of directors, create a list containing the names of all films by all directors.

  val all_films = directors.flatMap(_.films.map(_.name))

  /*
  Vintage McTiernan
  Starting with mcTiernan, find the date of the earliest McTiernan film.
  Tip: you can concisely find the minimum of two numbers a and b using math.min(a, b).
   */
  val mcTiernan_earlies_film_1 = mcTiernan.films.sortWith((a, b) => a.yearOfRelease < b.yearOfRelease).headOption
  val mcTiernan_earlies_film_2 = mcTiernan.films.foldLeft(Int.MaxValue) { (current, film) =>
    math.min(current, film.yearOfRelease)
  }
  val mcTiernan_earlies_film_3 = mcTiernan.films.map(_.yearOfRelease).min

  // Starting with directors, find all films sorted by descending IMDB rating
  val sorted_films = directors.flatMap(d => d.films).sortWith((a, b) => a.imdbRating > b.imdbRating)

  // Starting with directors again, find the average score across all films:
  val directors_with_score = directors.map(d =>
    s"${d.firstName} ${d.lastName}" -> d.films.foldLeft(0.0)((sum, film) => (sum + film.imdbRating) / films.length)
  )

  // Starting with directors, print the following for every film: "Tonight only! FILM NAME by DIRECTOR!"
  directors.foreach(d => d.films.foreach(f => println(s"Tonight only! ${f.name} by ${d.firstName} ${d.lastName}!")))

  // Finally, starting with directors again, find the earliest film by any director:
  val earliest_films_by_director = directors.flatMap(d => d.films).sortWith((a,b) => a.yearOfRelease < b.yearOfRelease).headOption
  // someBody.films.map(_.yearOfRelease).min

  // 6.2.7.2 Do-It-Yourself
  /*
  Now we know the essential methods of Seq, we can write our own versions of some other library methods.
  Minimum
  Write a method to find the smallest element of a Seq[Int].
   */
  def smallest_1(list: List[Int]): Int = list.min
  def smallest_2(seq: Seq[Int]): Int = seq.foldLeft(Int.MaxValue)(math.min)

  /*
  Unique
  Given Seq(1, 1, 2, 4, 3, 4) create the sequence containing each number only once.
  Order is not important, so Seq(1, 2, 4, 3) or Seq(4, 3, 2, 1) are equally valid answers.
  Hint: Use contains to check if a sequence contains a value.
   */
  Seq(1, 1, 2, 4, 3, 4).toSet

  /*
  Reverse
  Write a function that reverses the elements of a sequence.
  Your output does not have to use the same concrete implementation as the input. Hint: use foldLeft.
   */
  def reverse[A](seq: Seq[A]): Seq[A] = {
    seq.foldLeft(Seq.empty[A]){ (seq, elt) => elt +: seq }
  }
  Seq(1, 1, 2, 4, 3, 4).reverse

  // Write map in terms of foldRight.
  def map[A, B](seq: Seq[A], f: A => B): Seq[B] =
    seq.foldRight(Seq.empty[B])((a, b) => f(a) +: b)

}

