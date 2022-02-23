import Dependencies._

ThisBuild / scalaVersion     := "2.13.6"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "essential_scala"

lazy val root = (project in file("."))
  .settings(
    name := "learning-deep-scala",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      catsCore,
      catsEffect,
      catsFree,
      catsEffectTest,
      scalaZ
    )
  )

initialCommands in console := "import scalaz._, Scalaz._"

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
