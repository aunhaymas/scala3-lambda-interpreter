ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"
Compile / run / fork := true
Compile / run / connectInput := true

lazy val root = (project in file("."))
  .settings(
    name := "Scala-template",
    //https://docs.scala-lang.org/scala3/book/tools-sbt.html#using-sbt-with-scalatest
    libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.2.20" % Test)
  )
