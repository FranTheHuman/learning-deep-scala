package essential_scala

object AlgebraicDataTypes extends App {

  // SE LLAMAN SUM Y PRODUCT POR COMO SE CALCULA LOS VALORES QUE PUEDEN TOMAR LA ESTUCTURA DE DATOS

  // 4.4.4.1 Stop on a Dime

  // A traffic light is red, green, or yellow. Translate this description into Scala code.

  // ---- Sum Type Pattern --- (Is-a / or)
  sealed trait TrafficLight
  case object Red extends TrafficLight
  case object Green extends TrafficLight
  case object Yellow extends TrafficLight

  // 4.4.4.2 Calculator

  // A calculation may succeed (with an Int result) or fail (with a String message). Implement this.

  // ---- Sum Type Pattern ---  (Is-a / or)
  sealed trait Calculation
  final case class Success(result: Int) extends Calculation
  final case class Failure(message: String) extends Calculation

  // 4.4.4.3 Water, Water, Everywhere

  // Bottled water has a size (an Int), a source (which is a well, spring, or tap), and a Boolean carbonated.
  // Implement this in Scala.

  // ---- Sum Type Pattern --- (Is-a / or)
  sealed trait WaterSource
  case object Well extends WaterSource
  case object Spring extends WaterSource
  case object Tap extends WaterSource

  // ---- Product Type Pattern --- (Has-a / And)
  case class BottledWater(size: Int, source: WaterSource, carbonated: Boolean)

}
