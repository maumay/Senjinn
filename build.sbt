ThisBuild / scalaVersion := "2.12.6"
ThisBuild / organization := "xawd"

lazy val senjinn = (project in file("."))
    .settings(
        name := "Senjinn",
        libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    )
