package de.digitalstep.scalasciidoc

import de.digitalstep.scalasciidoc.Grammar.sectionTitle
import de.digitalstep.scalasciidoc.model.Title

/**
 * @author gunnar
 */
class ParseSectionTitleSpec extends AbstractParserSpec(sectionTitle) {

  property("level 1") {
    parseSuccess("== Text") shouldEqual Title(main = "Text", level = 1)
  }

  property("level 2") {
    parseSuccess("=== Text") shouldEqual Title(main = "Text", level = 2)
  }

  property("level 3") {
    parseSuccess("==== Text") shouldEqual Title(main = "Text", level = 3)
  }

  property("level 4") {
    parseSuccess("===== Text") shouldEqual Title(main = "Text", level = 4)
  }

  property("level 5") {
    parseSuccess("====== Text") shouldEqual Title(main = "Text", level = 5)
  }

}
