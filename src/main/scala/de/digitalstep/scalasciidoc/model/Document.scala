package de.digitalstep.scalasciidoc.model

/**
 * @author gunnar
 */
case class Document(header: Option[Header],
                    attributes: Map[String, Attribute.AttributeValue] = Map(),
                    body: Seq[ContentNode])
  extends Node
