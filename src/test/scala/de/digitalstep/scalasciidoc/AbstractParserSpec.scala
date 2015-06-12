package de.digitalstep.scalasciidoc

import java.net.URL

import com.typesafe.scalalogging.LazyLogging
import de.digitalstep.scalasciidoc.Grammar.{parse, Parser}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

import scala.io.Source.fromURL

/**
 * @author gunnar
 */
abstract class AbstractParserSpec[T](parser: Parser[T]) extends PropSpec with PropertyChecks with Matchers with LazyLogging {

  def parseSuccess(s: String): T = {
    logger.debug("Parsing \n---- \n{}----", s)
    parse(parser, s).get
  }

  def parseSuccess(url: URL): T = parseSuccess(fromURL(url).getLines mkString "\n")

}
