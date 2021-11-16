import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.8"
  lazy val catsCore = "org.typelevel" %% "cats-core" % "2.6.1"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "3.2.2"
  lazy val catsEffectTest =  "org.typelevel" %% "munit-cats-effect-3" % "1.0.3" % Test
}