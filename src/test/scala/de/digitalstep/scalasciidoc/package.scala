package de.digitalstep

import scala.language.implicitConversions

/**
 * @author gunnar
 */
package object scalasciidoc {

  import Grammar._

  def parseTo[T](parser: Parser[T], s: String): T = parse(parser, s) match {
    case Success(parsedObject, _) ⇒ parsedObject
    case error ⇒ throw new IllegalArgumentException(error.toString)
  }

}
