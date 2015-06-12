package de.digitalstep.scalasciidoc

import de.digitalstep.scalasciidoc.model.{RevisionInfo, Author, Header, Title}
import Grammar.header


/**
 * @author gunnar
 */
class ParseHeaderSpec extends AbstractParserSpec(header) {

  property("header with title only") {
    parseSuccess {
      """This is the title
        |=================
        | """.stripMargin
    } shouldEqual Header(
      Title("This is the title", ""),
      Seq(),
      None
    )
  }

  property("header with author") {
    val header = parseSuccess(
      """
        |This is another title
        |=====================
        |a b
        | """.stripMargin.stripPrefix("\n"))

    header shouldEqual Header(
      Title("This is another title", ""),
      Seq(Author("a", "b", None)),
      None
    )
  }

  property("header with author and revision") {
    parseSuccess {
      """This is another title
        |=====================
        |a b
        |v1.0
        | """.stripMargin.stripPrefix("\n").trim
    } shouldEqual Header(
      Title("This is another title", ""),
      Seq(Author("a", "b", None)),
      Some(RevisionInfo(Some("v1.0"), None, None))
    )
  }

  property("header with attributes") {

  }

}
