import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.9"
  lazy val catsCore = "org.typelevel" %% "cats-core" % "2.7.0"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "3.3.5"
  lazy val catsFree = "org.typelevel" %% "cats-free" % "2.7.0"
  lazy val catsEffectTest =  "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
  lazy val scalaZ = "org.scalaz" %% "scalaz-core" % "7.3.6"
}