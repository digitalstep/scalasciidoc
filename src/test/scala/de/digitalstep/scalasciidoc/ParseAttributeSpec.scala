package de.digitalstep.scalasciidoc

import Grammar.attribute
import de.digitalstep.scalasciidoc.model.Attribute

/**
 * @author gunnar
 */
class ParseAttributeSpec extends AbstractParserSpec(attribute) {

  property("String attribute") {
    parseSuccess {
      ":name: value\n"
    } shouldEqual Attribute("name", "value")
  }

  property("activated attribute") {
    parseSuccess {
      ":name: \n"
    } shouldEqual Attribute.activate("name")
  }

  property("deactivated attribute") {
    parseSuccess {
      ":name!: \n"
    } shouldEqual Attribute.deactivate("name")
  }

}
