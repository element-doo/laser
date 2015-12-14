package parsers

import org.parboiled2.{CharPredicate, Parser, ParserInput}

import scala.util.Try

object RuleParser {
  trait Node
  trait Line extends Node
  case class Document(nodes: Seq[Node])
  case class Block(head: BlockHead,transformers: Seq[Transformer]) extends Node
  case class BlockHead(classes: Seq[Class])
  case class Transformer(from: FromTransformer, to: Option[ToTransformer]) extends Line
  case class FromTransformer(value: String)
  case class ToTransformer(value: String)
  case class Class(value:String)

  class RuleParser(val input: ParserInput) extends Parser {

    def documentRule = rule { zeroOrMore(nodeRule) ~> Document }
    def nodeRule = rule { blockRule | lineRule }

    def blockRule = rule { blockHeadRule  ~ zeroOrMore(lineRule) ~ "}" ~ stripSpace ~ (newLine | EOI) ~> Block}
    def blockHeadRule = rule { (classNameRule * zeroOrMore(" "))  ~ stripSpace ~ "{" ~ stripSpace ~ newLine ~> BlockHead }
    def classNameRule = rule { capture(oneOrMore(CharPredicate.Alpha)) ~> Class}

    def lineRule = rule {
      stripSpace ~ transformerRule ~ (newLine | EOI)
    }

    def fromTransRule = rule {
      stripSpace ~ !"#" ~ capture(oneOrMore(latinExtended)) ~> FromTransformer
    }

    def toTransRule = rule {
      capture(oneOrMore(latinExtended)) ~ stripSpace  ~> ToTransformer
    }

    def newLine = rule { "\r\n" | "\r" | "\n" }
    def stripSpace = rule { zeroOrMore(" ") }

    def transformerRule = rule {
      fromTransRule ~ transformSeparatorRule ~ optional(toTransRule) ~> Transformer
    }

    def latinExtended = rule {// Latin supplement,extendedA, extendedB -> 80 - 24f
      CharPredicate('\u0021' to '\u024f')
    }

    def transformSeparatorRule = rule {
      oneOrMore(" ") ~ "=>" ~ zeroOrMore(" ")
    }
  }

  def parse(doc: String): Try[Document] = new RuleParser(doc.replaceAll("(?m)^[ \t]*\r?\n", "").replaceAll("#.*\\n","")).documentRule.run()

}