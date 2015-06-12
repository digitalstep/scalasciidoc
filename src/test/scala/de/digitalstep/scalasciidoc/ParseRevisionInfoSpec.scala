package de.digitalstep.scalasciidoc

import Grammar.revisionInfo
import de.digitalstep.scalasciidoc.model.RevisionInfo

/**
 * @author gunnar
 */
class ParseRevisionInfoSpec extends AbstractParserSpec(revisionInfo) {

  property("Maven-style") {
    parseSuccess("1.2.3\n") shouldEqual RevisionInfo(Some("1.2.3"), None, None)
  }

  property("Revision with leading 'v'") {
    parseSuccess("v1.2\n") shouldEqual RevisionInfo(Some("v1.2"), None, None)
  }

  property("Revision, date, and remark") {
    parseSuccess {
      "v1.2, 2015-06-10: Remark\n"
    } shouldEqual RevisionInfo(Some("v1.2"), Some("2015-06-10"), Some("Remark"))
  }

  property("Alternate date format") {
    parseSuccess {
      "Jun 10, 2015\n"
    } shouldEqual RevisionInfo(None, Some("Jun 10, 2015"), None)
  }

  property("Revision with alternate date format") {
    parseSuccess {
      "v1.0, Jun 10, 2015\n"
    } shouldEqual RevisionInfo(Some("v1.0"), Some("Jun 10, 2015"), None)
  }

  property("Revision with alternate date format and remark") {
    parseSuccess {
      "v1.0, Jun 10, 2015: asdf\n"
    } shouldEqual RevisionInfo(Some("v1.0"), Some("Jun 10, 2015"), Some("asdf"))
  }

}
