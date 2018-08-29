import Dependencies._

name := "scalasciidoc"
version := "1.0"
scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  parboiled,
  `scala-reflect`,
  `scala-parser-combinators`,
  scalacheck % Test,
  scalatest % Test
) ++ logging
