package essential_scala

object CaseClass extends App {

  // 3.4.5.1 Case Cats
  /**
   * Recall that a Cat has a String colour and food. Define a case class to represent a Cat.
   */
  case class Cat(name: String, colour: String, food: String)

  // 3.4.5.2 Roger Ebert Said it Best… -

  // 3.4.5.3 Case Class Counter
  /**
   * Reimplement Counter as a case class, using copy where appropriate.
   * Additionally initialise count to a default value of 0.
   */
  case class Counter(count: Int = 0) {
    def ++ : Counter = copy(count = count + 1)
    def -- : Counter = copy(count = count - 1)
  }

  // 3.4.5.4 Application, Application, Application


}
