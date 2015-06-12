package de.digitalstep.scalasciidoc

import de.digitalstep.scalasciidoc.Grammar.email

/**
 * @author gunnar
 */
class ParseEmailSpec extends AbstractParserSpec(email) {

  property("2 letter TLD") {
    parseSuccess("a@b.de") shouldEqual "a@b.de"
  }

  property("3 letter TLD") {
    parseSuccess("a@b.com") shouldEqual "a@b.com"
  }

  property("4 letter TLD") {
    parseSuccess("a@b.info") shouldEqual "a@b.info"
  }

  property("2 TLDs") {
    parseSuccess("a@b.co.nz") shouldEqual "a@b.co.nz"
  }

  property("host name with many separators") {
    parseSuccess("a@b.c.d.de") shouldEqual "a@b.c.d.de"
  }

  property("user name with many separators") {
    parseSuccess("x.y.z@host.de") shouldEqual "x.y.z@host.de"
  }

  property("user name with special characters") {
    parseSuccess("a-meier@host.de") shouldEqual "a-meier@host.de"
  }

  property("user name with digits") {
    parseSuccess("meier123@host.de") shouldEqual "meier123@host.de"
  }

  property("host name with digits") {
    parseSuccess("a@host123.de") shouldEqual "a@host123.de"
  }

}
