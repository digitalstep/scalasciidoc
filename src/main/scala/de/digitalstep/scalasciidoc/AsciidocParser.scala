package de.digitalstep.scalasciidoc

import org.parboiled2._

import scala.language.implicitConversions

sealed trait Node

case class DocumentNode(header: Option[HeaderNode], attributes: Seq[AttributeNode], body: BodyNode) extends Node

case class HeaderNode(title: TitleNode, authors: Seq[AuthorNode], revision: Option[RevisionInfoNode]) extends Node

case class RevisionInfoNode(revision: Option[String], date: Option[String], remark: Option[String]) extends Node

case class AuthorNode(name: String, email: Option[String]) extends Node

case class AttributeNode(name: String, value: AttributeValueNode) extends Node

sealed trait AttributeValueNode extends Node

case class StringValueNode(value: String) extends AttributeValueNode

case class BooleanValueNode(value: Boolean) extends AttributeValueNode

case class BodyNode(sections: Seq[ContentNode]) extends Node

case class TitleNode(title: String, level: Int) extends Node

sealed trait ContentNode extends Node

case class SectionNode(title: TitleNode, content: Seq[ParagraphNode]) extends ContentNode

case class ParagraphNode(title: Option[String], text: String) extends ContentNode

object AsciidocParser {
  def apply(input: ParserInput) = new AsciidocParser(input)
}

/**
 * @author gunnar
 */
class AsciidocParser(val input: ParserInput) extends Parser {

  implicit def wspStr(s: String): Rule0 = rule {
    str(s) ~ zeroOrMore(anyOf(" \t"))
  }

  def InputText = rule {
    WhiteSpace.? ~ Document ~ WhiteSpace.? ~ EOI
  }

  def Document = rule {
    Header.? ~ NewLine.* ~ Attributes ~ NewLine.* ~ Body ~> DocumentNode
  }

  def Header = rule {
    HeaderWithAuthors | HeaderWithTitle
  }

  def HeaderWithTitle: Rule1[HeaderNode] = rule {
    DocTitle ~> ((t: TitleNode) ⇒ HeaderNode(t, Seq(), None)) ~ NewLine
  }

  def HeaderWithAuthors = rule {
    DocTitle ~ Authors ~ RevisionInfo.? ~> HeaderNode
  }

  def DocTitle = rule {
    DocTitlePrefixed | DocTitleUnderlined
  }

  def createTitleNode(s: String) = TitleNode(s, 0)

  def DocTitleUnderlined = rule {
    ToEndOfLine ~> {
      s ⇒ {
        val minimum: Int = s.length - 1
        val maximum: Int = s.length + 1
        (minimum to maximum).times(str("=")) ~ NewLine ~> (() ⇒ push(TitleNode(s, 0)))
      }
    }
  }

  def DocTitlePrefixed = rule {
    "= " ~ (ToEndOfLine ~> (TitleNode(_, 0)))
  }

  def toAuthors(x: AuthorNode, xs: Seq[AuthorNode]) = xs :+ x

  def Authors: Rule1[Seq[AuthorNode]] = rule {
    Author ~ (";" ~ Author).* ~> (toAuthors(_, _)) ~ NewLine
  }

  def Author = rule {
    AuthorName ~ AuthorEmail.? ~> AuthorNode
  }

  def AuthorName = rule {
    capture((!NewLine ~ !"<" ~ ANY).+) ~> (_.trim)
  }

  def AuthorEmail = rule {
    "<" ~ capture((!NewLine ~ !">" ~ ANY).+) ~ ">"
  }

  def RevisionInfo = rule {
    RevisionDateRemark | RevisionDate | DateRemark |
      Revision ~ (NewLine | EOI) ~> (rev ⇒ RevisionInfoNode(Some(rev), None, None)) |
      Date ~ (NewLine | EOI) ~> (date ⇒ RevisionInfoNode(None, Some(date), None))
  }

  def RevisionDateRemark = rule {
    Revision ~ Date ~ Remark ~ (NewLine | EOI) ~> ((rev, date, remark) ⇒
      RevisionInfoNode(Some(rev), Some(date), Some(remark)))
  }

  def RevisionDate = rule {
    Revision ~ Date ~ (NewLine | EOI) ~> ((rev, date) ⇒ RevisionInfoNode(Some(rev), Some(date), None))
  }

  def DateRemark = rule {
    Date ~ Remark ~ (NewLine | EOI) ~> ((date, remark) ⇒
      RevisionInfoNode(None, Some(date), Some(remark)))
  }

  def Revision = rule {
    capture((!"," ~ !NewLine ~ ANY).+) ~ ",".?
  }

  def Date = rule {
    DateIso | DateEnglish
  }

  def DateIso = rule {
    capture((4 times CharPredicate.Digit) ~ "-" ~ (2 times CharPredicate.Digit) ~ "-" ~ (2 times CharPredicate.Digit))
  }

  def DateEnglish = rule {
    capture("Jun 10, 2015")
  }

  def Remark = rule {
    ":" ~ capture((!NewLine ~ ANY).+)
  }

  def Attributes = rule {
    Attribute.*
  }

  def Attribute = rule {
    AttributeName ~ AttributeValue ~> AttributeNode
  }

  def AttributeName = rule {
    ":" ~ capture((!NewLine ~ !":" ~ !"!" ~ ANY).+)
  }

  def AttributeValue = rule {
    StringValue | BooleanValue
  }

  def StringValue = rule {
    ":" ~ ToEndOfLine ~> StringValueNode
  }

  def BooleanValue = rule {
    capture("!").? ~ ":" ~ NewLine ~> ((x: Option[String]) ⇒ BooleanValueNode(x.isEmpty))
  }

  def Body = rule {
    BodyContent.* ~> BodyNode ~ WhiteSpace.?
  }

  def BodyContent = rule {
    Section | Paragraph
  }

  def Section = rule {
    (Section5 | Section4 | Section3 | Section2 | Section1) ~ Paragraph.* ~> SectionNode
  }

  def Section1 = rule {
    "== " ~ (ToEndOfLine ~> (TitleNode(_, 1)))
  }

  def Section2 = rule {
    "=== " ~ (ToEndOfLine ~> (TitleNode(_, 2)))
  }

  def Section3 = rule {
    "==== " ~ (ToEndOfLine ~> (TitleNode(_, 3)))
  }

  def Section4 = rule {
    "===== " ~ (ToEndOfLine ~> (TitleNode(_, 4)))
  }

  def Section5 = rule {
    "====== " ~ (ToEndOfLine ~> (TitleNode(_, 5)))
  }

  def Paragraph: Rule1[ParagraphNode] = rule {
    (ParagraphTitle.? ~ ParagraphText) ~> ParagraphNode ~ NewLine
  }

  def ParagraphTitle: Rule1[String] = rule {
    "." ~ capture("titled") ~ NewLine
  }

  def ParagraphText: Rule1[String] = rule {
    ToEndOfLine.+ ~> ((x: Seq[String]) ⇒ x.mkString(" "))
  }

  def ToEndOfLine = rule {
    capture((!NewLine ~ ANY).+) ~> (s ⇒ s.trim) ~ NewLine
  }

  def NewLine = rule {
    "\n" | "\r\n" | EOI
  }

  def WhiteSpace = rule {
    anyOf(" \n\r\t\f").+
  }

}

object Runner extends App {

  private val input =
    """= Document Title
      |Ab C. Def <ab.c@Def.de>; Gh Ij
      |
      |== Section 1
    """.stripMargin

  val result = new AsciidocParser(input).InputText.run()

  println(result)
}
