package de.digitalstep.scalasciidoc.model


object Attribute {

  def apply(name: String, value: String): Attribute = Attribute(name, Value(value))

  def activate(name: String) = Attribute(name, True)

  def deactivate(name: String) = Attribute(name, False)

  sealed trait AttributeValue

  case object True extends AttributeValue

  case object False extends AttributeValue

  case class Value(s: String) extends AttributeValue

}

/**
 * @author gunnar
 */
case class Attribute(name: String, value: Attribute.AttributeValue)
