import sbt._
import Keys._

object Dependencies {

  val `scala-logging` = "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
  val `logback-classic` = "ch.qos.logback" % "logback-classic" % "1.1.3"
  val `scala-parser-combinators` = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
  val `scala-reflect` = "org.scala-lang" % "scala-reflect" % "2.11.7"
  val parboiled = "org.parboiled" %% "parboiled" % "2.1.0"
  val scalatest = "org.scalatest" %% "scalatest" % "2.2.1"
  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.12.2"

  val logging = Seq(`scala-logging`, `logback-classic`)
  val testing = Seq(scalatest, scalacheck) map (_ % Test)

}
