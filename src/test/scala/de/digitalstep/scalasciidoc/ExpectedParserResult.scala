package de.digitalstep.scalasciidoc

import org.scalacheck.Prop
import scala.language.implicitConversions
import Grammar._

/**
 * @author gunnar
 */
trait ExpectedParserResult {

  implicit def parseResultToObject[T](parseResult: ParseResult[T]): T = parseResult match {
    case Success(parsedObject, _) ⇒ parsedObject
    case error ⇒ throw new IllegalArgumentException(error.toString)
  }

}
