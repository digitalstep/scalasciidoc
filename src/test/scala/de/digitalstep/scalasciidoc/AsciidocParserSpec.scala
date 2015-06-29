package de.digitalstep.scalasciidoc

import com.typesafe.scalalogging.LazyLogging
import org.parboiled2.ParseError
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import sun.swing.SwingUtilities2.Section

import scala.language.implicitConversions
import scala.util.Failure
import scala.io.Source.fromURL

/**
 * @author gunnar
 */
class AsciidocParserSpec extends PropSpec with PropertyChecks with Matchers with LazyLogging {

  implicit def string2Asciidoc(input: String): AsciidocParser = AsciidocParser(input)

  property("parse author with full name and email") {
    "a b <a@b.de>".Author.run().get shouldEqual AuthorNode("a b", Some("a@b.de"))
  }

  property("parse author with name only") {
    AsciidocParser("a b").Author.run().get shouldEqual AuthorNode("a b", None)
  }

  property("A document title preceded by = ") {
    "= Document Title\n".DocTitle.run().get shouldEqual TitleNode("Document Title", 0)
  }

  property("An underlined document title") {
    """Document Title
      |==============
      | """.stripMargin.DocTitle.run().get shouldEqual TitleNode("Document Title", 0)
  }

  property("Underlined with too few ='s") {
    """Title
      |===
      | """.stripMargin.DocTitle.run() shouldBe a[Failure[_]]
  }

  property("Underlined with too many ='s") {
    """Title
      |=======
      | """.stripMargin.DocTitle.run() shouldBe a[Failure[_]]
  }

  property("Underlined title with special characters") {
    """燁
      |==
      | """.stripMargin.DocTitle.run().get shouldEqual TitleNode("燁", 0)
  }

  property("A document title with special characters") {
    "= 燁\n".DocTitle.run().get shouldEqual TitleNode("燁", 0)
  }

  property("author email with 2 letter TLD") {
    "<a@b.de>".AuthorEmail.run().get shouldEqual "a@b.de"
  }

  property("author email with 3 letter TLD") {
    "<a@b.com>".AuthorEmail.run().get shouldEqual "a@b.com"
  }

  property("author email with 4 letter TLD") {
    "<a@b.info>".AuthorEmail.run().get shouldEqual "a@b.info"
  }

  property("author email with 2 TLDs") {
    "<a@b.co.nz>".AuthorEmail.run().get shouldEqual "a@b.co.nz"
  }

  property("author email with host name with many separators") {
    "<a@b.c.d.de>".AuthorEmail.run().get shouldEqual "a@b.c.d.de"
  }

  property("author email with user name with many separators") {
    "<x.y.z@host.de>".AuthorEmail.run().get shouldEqual "x.y.z@host.de"
  }

  property("author email with user name with special characters") {
    "<a-meier@host.de>".AuthorEmail.run().get shouldEqual "a-meier@host.de"
  }

  property("author email with user name with digits") {
    "<meier123@host.de>".AuthorEmail.run().get shouldEqual "meier123@host.de"
  }

  property("author email with host name with digits") {
    "<a@host123.de>".AuthorEmail.run().get shouldEqual "a@host123.de"
  }

  property("section title level 1") {
    "== Text".SectionTitle1.run().get shouldEqual TitleNode("Text", 1)
  }

  property("section title level 2") {
    "=== Text".SectionTitle2.run().get shouldEqual TitleNode("Text", 2)
  }

  property("section title level 3") {
    "==== Text".SectionTitle3.run().get shouldEqual TitleNode("Text", 3)
  }

  property("section title level 4") {
    "===== Text".SectionTitle4.run().get shouldEqual TitleNode("Text", 4)
  }

  property("section title level 5") {
    "====== Text".SectionTitle5.run().get shouldEqual TitleNode("Text", 5)
  }

  property("revision Maven-style") {
    "1.2.3".RevisionInfo.run().get shouldEqual RevisionInfoNode(Some("1.2.3"), None, None)
  }

  property("Revision with leading 'v'") {
    "v1.2".RevisionInfo.run().get shouldEqual RevisionInfoNode(Some("v1.2"), None, None)
  }

  property("Revision, date, and remark") {
    "v1.2, 2015-06-10: Remark".RevisionInfo.run().get shouldEqual
      RevisionInfoNode(
        Some("v1.2"),
        Some("2015-06-10"),
        Some("Remark"))
  }

  property("Alternate date format") {
    "Jun 10, 2015".RevisionInfo.run().get shouldEqual RevisionInfoNode(None, Some("Jun 10, 2015"), None)
  }

  property("Revision with alternate date format") {
    "v1.0, Jun 10, 2015".RevisionInfo.run().get shouldEqual RevisionInfoNode(Some("v1.0"), Some("Jun 10, 2015"), None)
  }

  property("Revision with alternate date format and remark") {
    "v1.0, Jun 10, 2015: asdf".RevisionInfo.run().get shouldEqual RevisionInfoNode(Some("v1.0"), Some("Jun 10, 2015"), Some("asdf"))
  }

  property("String attribute") {
    ":a: 1\n".Attribute.run().get shouldEqual AttributeNode("a", StringValueNode("1"))
  }

  property("activated attribute") {
    ":name:".Attribute.run().get shouldEqual AttributeNode("name", BooleanValueNode(true))
  }

  property("deactivated attribute") {
    ":name!:".Attribute.run().get shouldEqual AttributeNode("name", BooleanValueNode(false))
  }

  property("multiple string attributes") {
    ":a: 1\n:b: 2".Attributes.run().get shouldEqual Seq(
      AttributeNode("a", StringValueNode("1")),
      AttributeNode("b", StringValueNode("2")))
  }

  property("multiple attributes, mixed value types") {
    ":a: 1\n:b:\n:c!:".Attributes.run().get shouldEqual Seq(
      AttributeNode("a", StringValueNode("1")),
      AttributeNode("b", BooleanValueNode(true)),
      AttributeNode("c", BooleanValueNode(false)))
  }

  property("header with title only") {
    """This is the title
      |=================
      | """.stripMargin.Header.run().get shouldEqual HeaderNode(TitleNode("This is the title", 0), Seq(), None)
  }

  property("header with author") {
    val document: String =
      """This is another title
        |=====================
        |a b
        | """.stripMargin

    document.Header.run().get shouldEqual HeaderNode(
      TitleNode("This is another title", 0),
      Seq(AuthorNode("a b", None)),
      None)
  }

  property("header with author and revision") {
    val document =
      """This is another title
        |=====================
        |a b
        |v1.0
        | """.stripMargin
    document.Header.run().get shouldEqual HeaderNode(
      TitleNode("This is another title", 0),
      Seq(AuthorNode("a b", None)),
      Some(RevisionInfoNode(Some("v1.0"), None, None)))
  }

  property("level 1 with simple paragraph") {
    val document =
      """== Text
        |a
        | """.stripMargin
    document.Section.run().get shouldEqual SectionNode(
      TitleNode("Text", 1),
      Seq(ParagraphNode(title = None, text = "a")),
      Seq())
  }

  property("level 2 with two paragraphs") {
    val document =
      """=== Text
        |a
        |
        |b
        | """.stripMargin

    document.Section.run().get shouldEqual SectionNode(
      TitleNode("Text", 2),
      Seq(
        ParagraphNode(title = None, text = "a"),
        ParagraphNode(title = None, text = "b")),
      Seq())
  }

  property("level 3 with a titled paragraph") {
    val document =
      """==== Text
        |a
        |
        |.titled
        |paragraph
        | """.stripMargin

    document.Section.run().get shouldEqual SectionNode(
      TitleNode("Text", 3),
      Seq(
        ParagraphNode(title = None, text = "a"),
        ParagraphNode(Some("titled"), "paragraph")),
      Seq())
  }

  property("paragraph with two lines") {
    val document =
      """a
        |b
      """.stripMargin

    document.Paragraph.run().get shouldEqual ParagraphNode(None, "a b")
  }

  property("document with title and text") {
    val document = fromURL(getClass.getResource("/title-and-text.adoc")).getLines().mkString("\n")
    document.Document.run().get shouldEqual DocumentNode(
      header = Some(HeaderNode(
        title = TitleNode("Title", 0),
        authors = Seq(),
        revision = None
      )),
      attributes = Seq(),
      body = BodyNode(Seq(
        ParagraphNode(
          title = None,
          text = "a b")
      ))
    )
  }

  property("document with complete header and text") {
    val document = fromURL(getClass.getResource("/complete-header-with-text.adoc")).getLines().mkString("\n")
    document.Document.run().get shouldEqual DocumentNode(
      header = Some(HeaderNode(
        title = TitleNode("Title", 0),
        authors = Seq(AuthorNode("author name", Some("email@somewhere.com"))),
        revision = Some(RevisionInfoNode(
          revision = Some("v1.0"),
          date = Some("2015-06-10"),
          remark = Some("Remark")))
      )),
      attributes = Seq(
        AttributeNode("attr", StringValueNode("Attribute value")),
        AttributeNode("bool", BooleanValueNode(true))),
      body = BodyNode(Seq(
        ParagraphNode(
          title = None,
          text = "asdf")
      ))
    )
  }

  property("Document without header") {
    "Text".Document.run().get shouldEqual DocumentNode(None, Seq(),
      BodyNode(Seq(ParagraphNode(None, "Text"))))
  }

  property("document with title and section") {
    val document = fromURL(getClass.getResource("/title-and-section1.adoc")).getLines().mkString("\n")
    document.Document.run().get shouldEqual DocumentNode(
      header = Some(HeaderNode(
        title = TitleNode("Title", 0),
        authors = Seq(),
        revision = None)),
      attributes = Seq(),
      body = BodyNode(Seq(
        SectionNode(
          title = TitleNode("Section", 1),
          content = Seq(ParagraphNode(None, "This is a paragraph")),
          Seq()
        ))
      )
    )
  }

  property("Complete document with sections and subsections") {
    import Implicits._

    val document = fromURL(this.getClass.getResource("/document-with-sections.adoc")).getLines().mkString("\n")
    document.Document.run().get shouldEqual DocumentNode(
      header = Some(HeaderNode(
        title = TitleNode("Title", 0),
        authors = Seq(AuthorNode("author name", Some("email@somewhere.com"))),
        revision = Some(RevisionInfoNode(Some("v1.0"), Some("2015-06-10"), Some("Remark"))))),
      attributes = Seq(
        "attr" → "Attribute value",
        "bool" → true),
      body = BodyNode(Seq(
        SectionNode(
          title = TitleNode("Section 1", 1),
          content = Seq(
            ParagraphNode(None, "Test1.1"),
            ParagraphNode(Some("Titled"), "Test1.2"),
            ParagraphNode(None, "Test1.3")
          ),
          Seq(
            SectionNode(
              title = TitleNode("Section 2", 2),
              content = Seq(
                ParagraphNode(None, "Test2.1"),
                ParagraphNode(Some("Titled"), "Test2.2")),
              Seq())
          )
        ))
      )
    )
  }

  property("A block macro") {
    "name::target[]".BlockMacro.run().get shouldEqual BlockMacroNode("name", "target", "")
  }

  property("Section with child sections") {
    val document = """== Level 1
                     |Block
                     |
                     |=== Level 2
                     |Block
                   """.stripMargin

    document.Section.run().get shouldEqual SectionNode(
      title = TitleNode("Level 1", 1),
      content = Seq(ParagraphNode(None, "Block")),
      children = Seq(
        SectionNode(TitleNode(
          title = "Level 2", 2),
          content = Seq(ParagraphNode(None, "Block")),
          children = Seq())
      )
    )
  }

}
