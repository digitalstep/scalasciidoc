package de.digitalstep.scalasciidoc.model

object Paragraph {
  def apply(text: String): Paragraph = Paragraph(None, text)
}

/**
 * @author gunnar
 */
case class Paragraph(title: Option[String], text: String)
  extends ContentNode
