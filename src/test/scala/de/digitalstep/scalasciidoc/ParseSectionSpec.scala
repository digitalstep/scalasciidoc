package de.digitalstep.scalasciidoc

import de.digitalstep.scalasciidoc.Grammar.section
import de.digitalstep.scalasciidoc.model.{Paragraph, Section}

/**
 * @author gunnar
 */
class ParseSectionSpec extends AbstractParserSpec(section) {

  property("level 1 with simple paragraph") {
    parseSuccess {
      """== Text
        |a
        |""".stripMargin
    } shouldEqual
      Section(
        title = "Text",
        level = 1,
        content = Seq(Paragraph(text = "a"))
      )
  }

  property("level 2 with two paragraphs") {
    parseSuccess {
      """=== Text
        |a
        |
        |b
        |""".stripMargin
    } shouldEqual
      Section(
        title = "Text",
        level = 2,
        content =
          Seq(
            Paragraph(text = "a"),
            Paragraph(text = "b")
          )
      )
  }

  property("level 3 with a titled paragraph") {
    parseSuccess {
      """==== Text
        |a
        |
        |.titled
        |paragraph
        | """.stripMargin
    } shouldEqual
      Section(
        title = "Text",
        level = 3,
        content = Seq(Paragraph(text = "a"))
      )
  }

}
