package de.digitalstep.scalasciidoc

import Grammar._
import de.digitalstep.scalasciidoc.model.Title

/**
 * @author gunnar
 */
class ParseDocTitleSpec extends AbstractParserSpec(docTitle) {

  property("An underlined document title") {
    parseSuccess {
      """Document Title
        |==============
        | """.stripMargin
    } shouldEqual Title("Document Title", "")
  }

  property("A document title preceded by = ") {
    parseSuccess {
      "= Document Title\n"
    } shouldEqual Title("Document Title", "")
  }

  property("Underlined with too few ='s") {
    a[RuntimeException] shouldBe thrownBy {
      parseSuccess {
        """Title
          |===
          | """.stripMargin
      }
    }
  }

  property("Underlined with too many ='s") {
    a[RuntimeException] shouldBe thrownBy {
      parseSuccess {
        """Title
          |=======
          | """.stripMargin
      }
    }
  }

  property("Title with special characters") {
    parseSuccess {
      "= 燁\n"
    } shouldEqual Title("燁", "")
  }

  property("Underlined title with special characters") {
    parseSuccess {
      """燁
        |==
        | """.stripMargin
    } shouldEqual Title("燁", "")
  }

}

