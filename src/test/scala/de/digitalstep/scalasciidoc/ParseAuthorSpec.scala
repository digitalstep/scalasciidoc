package de.digitalstep.scalasciidoc

import Grammar._
import de.digitalstep.scalasciidoc.model.Author
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import prop._
import Matchers._

/**
 * @author gunnar
 */
class ParseAuthorSpec extends AbstractParserSpec(author) {

  property("parse author with full name and email") {
    parseSuccess {
      "a b <a@b.de>"
    } shouldEqual Author("a", "b", Some("a@b.de"))
  }

  property("parse author with name only") {
    parse(author, "a b").get shouldEqual Author("a", "b", None)
  }

}
