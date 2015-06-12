package de.digitalstep.scalasciidoc.model

/**
 * @author gunnar
 */
case class Section(title: String,
                   level: Int,
                   content: Seq[ContentNode])
  extends ContentNode
