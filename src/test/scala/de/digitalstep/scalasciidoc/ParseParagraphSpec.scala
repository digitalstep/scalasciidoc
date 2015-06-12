package de.digitalstep.scalasciidoc

import Grammar.paragraph
import de.digitalstep.scalasciidoc.model.Paragraph

/**
 * @author gunnar
 */
class ParseParagraphSpec extends AbstractParserSpec(paragraph) {

  property("single-line text-only paragraph") {
    parseSuccess("asdf\n\n") shouldEqual Paragraph(text = "asdf")
  }

  property("multi-line text-only paragraph") {
    parseSuccess {
      """1st line
        |2nd line
        |
      """.stripMargin
    } shouldEqual Paragraph(text = "1st line\n2nd line")
  }

  property("multi-line with one character each") {
    parseSuccess {
      """1
        |2
      """.stripMargin
    } shouldEqual Paragraph(text = "1\n2")
  }

}
