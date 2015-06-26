import Dependencies._

name := "scalasciidoc"
version := "1.0"
scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  parboiled,
  `scala-reflect`,
  `scala-parser-combinators`,
  `logback-classic`,
  `scala-logging`,
  scalacheck % Test,
  scalatest % Test
)
