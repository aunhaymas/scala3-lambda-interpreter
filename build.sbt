ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"

lazy val root = (project in file("."))
  .settings(
    name := "Scala-template",
    //https://docs.scala-lang.org/scala3/book/tools-sbt.html#using-sbt-with-scalatest
    libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.2.20" % Test)
  )
