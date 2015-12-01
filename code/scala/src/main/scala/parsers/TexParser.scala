package parsers

import org.parboiled2._

import scala.util.Try

object TParser {
  trait Node
  case class FreeTextNode(value: String) extends Node
  case class MathNode(value: String) extends Node
  case class Document(text: Seq[Node])

  class SimpleTexParser(val input: ParserInput) extends Parser {
    def documentRule = rule { oneOrMore(nodeRule) ~ EOI ~> Document }
    def nodeRule = rule { mathNodeRule | freeTextRule }
    def mathNodeRule = rule { "$$" ~ capture(zeroOrMore(!"$$" ~ ANY)) ~ "$$" ~> MathNode }
    def freeTextRule = rule { capture(oneOrMore(!"$$" ~ ANY)) ~> FreeTextNode }
  }

  def parse(doc: String): Try[Document]= new SimpleTexParser(doc).documentRule.run()
}
