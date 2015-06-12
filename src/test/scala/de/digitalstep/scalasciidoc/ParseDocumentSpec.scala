package de.digitalstep.scalasciidoc

import de.digitalstep.scalasciidoc.model._
import Grammar._

/**
 * @author gunnar
 */
class ParseDocumentSpec extends AbstractParserSpec(document) {

  property("document with title and text") {
    parseSuccess(getClass.getResource("/title-and-text.adoc")) shouldEqual
      Document(
        header = Some(Header(
          title = Title("Title", ""),
          authors = Seq(),
          revision = None
        )),
        attributes = Map(),
        body = Seq(
          Paragraph(text = "a\nb")
        )
      )
  }

  property("document with complete header and text") {
    parseSuccess(getClass.getResource("/complete-header-with-text.adoc")) shouldEqual
      Document(
        header = Some(Header(
          Title("Title", ""),
          Seq(Author("author", "name", Some("email@somewhere.com"))),
          revision = Some(RevisionInfo(
            revision = Some("v1.0"),
            date = Some("2015-06-10"),
            remark = Some("Remark"))
          )
        )),
        attributes = Map(
          "attr" → Attribute.Value("Attribute value"),
          "bool" → Attribute.True
        ),
        body = Seq(
          Paragraph(text = "asdf")
        )
      )
  }

  property("document without header") {
    parseSuccess("Text\n") shouldEqual Document(None, Map(), Seq(Paragraph(text = "Text")))
  }

  property("document with title and section") {
    parseSuccess(getClass.getResource("/title-and-section1.adoc")) shouldEqual
      Document(
        header = Header("Title"),
        body = Seq(
          Section(
            title = "Section",
            level = 1,
            content = Seq(
              Paragraph(text = "This is a paragraph"))
          )
        )
      )
  }

}
