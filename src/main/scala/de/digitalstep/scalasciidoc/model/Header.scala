package de.digitalstep.scalasciidoc.model

object Header {

  def apply(title: String): Option[Header] = Some(new Header(Title(title), Seq(), None))

}

/**
 * @author gunnar
 */
case class Header(title: Title,
                  authors: Seq[Author],
                  revision: Option[RevisionInfo])
  extends Node
