import sbt._
import Keys._

object Dependencies {
  val `log4j-scala`              = "org.apache.logging.log4j"        %% "log4j-api-scala"          % "11.0"
  val `log4j-api`                = "org.apache.logging.log4j"        % "log4j-api"                 % "2.11.1"
  val `log4j-core`               = "org.apache.logging.log4j"        % "log4j-core"                % "2.11.1"
  val `scala-parser-combinators` = "org.scala-lang.modules"          %% "scala-parser-combinators" % "1.1.1"
  val `scala-reflect`            = "org.scala-lang"                  %  "scala-reflect"            % "2.12.6"
  val parboiled                  = "org.parboiled"                   %% "parboiled"                % "2.1.4"
  val scalatest                  = "org.scalatest"                   %% "scalatest"                % "3.0.5"
  val scalacheck                 = "org.scalacheck"                  %% "scalacheck"               % "1.14.0"

  val logging                    = Seq(`log4j-scala`, `log4j-api`, `log4j-core`)
  val testing                    = Seq(scalatest, scalacheck) map (_ % Test)
}
