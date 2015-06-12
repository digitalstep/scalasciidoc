package de.digitalstep.scalasciidoc

import Grammar._

/**
 * @author gunnar
 */
class ParsePlainTextLineSpec extends AbstractParserSpec(plainTextLine) {

  property("one character only") {
    parseSuccess("a\n") shouldEqual "a"
  }

  property("A simple text line") {
    parseSuccess {
      """this is the line
        | """.stripMargin
    } shouldEqual "this is the line"
  }

  property("Multiple text lines") {
    parseSuccess {
      """This is the 1st line
        |and the 2nd
        | """.stripMargin
    } shouldEqual "This is the 1st line"
  }

  property("a leading equals sign") {
    a[RuntimeException] shouldBe thrownBy {
      parseSuccess("= This is not a text line\n")
    }
  }

  property("a leading space") {
    a[RuntimeException] shouldBe thrownBy {
      parseSuccess(" This is not a text line\n")
    }
  }

  property("a leading tab") {
    a[RuntimeException] shouldBe thrownBy {
      parseSuccess("\tThis is not a text line\n")
    }
  }

  property("a leading '.'") {
    a[RuntimeException] shouldBe thrownBy {
      parseSuccess(".This is not a text line\n")
    }
  }

}
