package de.digitalstep.scalasciidoc

import de.digitalstep.scalasciidoc.model._
import scala.util.parsing.combinator.RegexParsers
import scala.language.postfixOps

/**
 * @author gunnar
 */
object Grammar extends RegexParsers {

  override val skipWhitespace = false

  final val BLANKS = "[ \t]"
  final val WORD_CLASS = "a-zA-Z0-9_"
  final val ALNUM = "a-zA-Z0-9"
  final val ALPHA = "a-zA-Z"

  def document: Parser[Document] =
    (header <~ "[\\t\\n ]*".r).? ~
      (attribute <~ "\n" <~ "[\\t\\n ]*".r).* ~
      content ^^ {
      case h ~ a ~ b ⇒
        Document(
          header = h,
          attributes = a.map(a ⇒ a.name → a.value).toMap,
          body = b
        )
    }

  def header = docTitle ~ (authorLine ~ revisionInfo.?).? ^^ {
    case docTitle ~ None ⇒ Header(docTitle, Seq(), None)
    case docTitle ~ Some(authors ~ Some(RevisionInfo(None, None, None))) ⇒ Header(docTitle, authors, None)
    case docTitle ~ Some(authors ~ revision) ⇒ Header(docTitle, authors, revision)
  }

  def authorLine = "^".r ~> author ~ ((s"$BLANKS*;$BLANKS*".r ~> author) *) <~ ("\n" | "$".r) ^^ {
    case author ~ authors ⇒ author :: authors
  }

  def revisionInfo =
    "^".r ~> revision <~ "$".r ^^ {
      case rev ⇒ RevisionInfo(Some(rev), None, None)
    } |
      "^".r ~> date <~ "$".r ^^ {
        case date ⇒ RevisionInfo(None, Some(date), None)
      } |
      "^".r ~> revision ~ ("," ~ ws ~> date <~ "$".r) ^^ {
        case rev ~ date ⇒ RevisionInfo(Some(rev), Some(date), None)
      } |
      revision ~ ("," ~ ws ~> date) ~ (":" ~ ws ~> singleLine <~ "\\n".r) ^^ {
        case rev ~ date ~ remark ⇒ RevisionInfo(Some(rev), Some(date), Some(remark))
      }


  revision.? ~ ("," ~ ws ~> date).? ~ (":" ~ ws ~> singleLine).? ^^ {
    case rev ~ date ~ remark ⇒ RevisionInfo(rev, date, remark)
  }

  def revision = s"[$ALNUM\\-_.]+".r

  def date = ("[0-9]{4}".r ~ ("-" ~> "[0-1][0-9]".r) ~ ("-" ~> "[0-3][0-9]".r)) ^^ {
    case y ~ m ~ d ⇒ s"$y-$m-$d"
  } | ("[A-Z][a-z][a-z]".r ~ (blank ~> "[0-3][0-9]".r) ~ ("," ~ ws ~> "[0-9]{4}".r)) ^^ {
    case m ~ d ~ y ⇒ s"$m $d, $y"
  }

  def author = rep1sep(name, blank) ~ (blank ~ "<" ~> email <~ ">").? ^^ {
    case (firstName :: lastName :: Nil) ~ email ⇒ Author(firstName, lastName, email)
  }

  def email = s"[$WORD_CLASS][$WORD_CLASS.%+-]*@[$ALNUM][$ALNUM.-]*\\.[$ALPHA]{2,4}\\b".r

  def attribute = (":" ~> (name ~ "!".?) <~ ":") ~ (" " ~> singleLine).? ^^ {
    case n ~ None ~ Some(t) if t.nonEmpty ⇒ Attribute(n, t)
    case n ~ None ~ _ ⇒ Attribute.activate(n)
    case n ~ Some("!") ~ _ ⇒ Attribute.deactivate(n)
  }

  def docTitle = {
    val underlined = "^".r ~> singleLine ~ (eol ~> "=+".r <~ eol) ^^ {
      case t ~ x if areOfAlmostSameLength(t, x) ⇒ Title(t, "")
    }
    val prefixed = "= " ~> singleLine <~ eol ^^ { case t ⇒ Title(t, "") }

    underlined | prefixed
  }

  def content: Parser[Seq[ContentNode]] = (section | paragraph).*

  def section: Parser[Section] = sectionTitle ~ repsep(paragraph, eol) ^^ { case t ~ p ⇒ Section(t.main, t.level, p) }

  def sectionTitle: Parser[Title] = (
    sectionTitle1 |
      sectionTitle2 |
      sectionTitle3 |
      sectionTitle4 |
      sectionTitle5
    ) <~ eol.* ^^ { case t ⇒ Title(main = t.main, level = t.level) }

  def sectionTitle1: Parser[Title] = "^== ".r ~> singleLine ^^ { case s ⇒ Title(main = s, level = 1) }

  def sectionTitle2: Parser[Title] = "^=== ".r ~> singleLine ^^ { case s ⇒ Title(main = s, level = 2) }

  def sectionTitle3: Parser[Title] = "^==== ".r ~> singleLine ^^ { case s ⇒ Title(main = s, level = 3) }

  def sectionTitle4: Parser[Title] = "^===== ".r ~> singleLine ^^ { case s ⇒ Title(main = s, level = 4) }

  def sectionTitle5: Parser[Title] = "^====== ".r ~> singleLine ^^ { case s ⇒ Title(main = s, level = 5) }

  def paragraph: Parser[Paragraph] = plainTextLine.+ ^^ { case s ⇒ Paragraph(s mkString "\n") }

  def name: Parser[String] = "[a-zA-Z_\\-.]+".r

  def plainTextLine: Parser[String] = "[^= \\t\\n.][^\\n]*".r <~ eol ^^ (_.trim)

  def singleLine: Parser[String] = "[^\\n]*".r ^^ (_.trim)

  /**
   * End of line
   *
   * a newline character with any number of trailing tabs or spaces
   */
  def eol: Parser[String] = "\\n[\\t ]*".r

  def ws = s"$BLANKS*".r

  def blank: Parser[String] = s"$BLANKS+".r ^^ (s ⇒ " ")

  private[this] def areOfAlmostSameLength(a: String, b: String): Boolean =
    a.length == b.length ||
      a.length == b.length + 1 ||
      a.length == b.length - 1

}
