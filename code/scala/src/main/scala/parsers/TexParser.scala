package parsers

import org.parboiled2._
import org.parboiled2.CharPredicate._
import parsers.TParser.Node

import scala.util.Try

object TParser {
  trait Node

  case class Document(nodes: Seq[Node]) extends Node

  case class Func(name: String, bArgs: Seq[BlockArg], funcArg: Seq[FuncArg]) extends Node
  case class BlockFunc(tag: String, openNode: Node, closeNode: Node, nested: Seq[Node]) extends Node

  case class BlockArg(value: Seq[Node], tail: String) extends Node  //tail used for formatting preservation
  case class FuncArg(value: Seq[Node], tail: String) extends Node

  case class TextNode(value: String) extends Node
  trait MathNode extends Node
  case class InlineMath(values: Seq[Node]) extends MathNode
  case class BlockMath(values: Seq[Node]) extends MathNode

  //TODO must signal whether the entire input has been consumed, shoud fail if doesnt parse EOI

  class SimpleTexParser(val input: ParserInput) extends Parser {
    //def mathNodeRule = rule { "$$" ~ capture(zeroOrMore(!"$$" ~ ANY)) ~ "$$" ~> MathNode }

    def nodeRule: Rule1[Seq[Node]] = rule {
      oneOrMore(functionRule | blockArgument | argumentRule | mathNodeRule | rawNodeRule )
    }

    def functionRule = rule {
      "\\" ~ capture(optional(functionNameRule)) ~ zeroOrMore(argumentRule) ~ zeroOrMore(blockArgument) ~> ((name:String,arg:Seq[BlockArg],fa:Seq[FuncArg])=> Func(name,arg,fa))
    }

    def functionNameRule = rule {
      oneOrMore(AlphaNum | "_" | "=" | ">" | "<" | "-" | ":" | "^" | "." | "*")
    }

    def argumentRule = rule {
      "[" ~ optional(nodeRule) ~ "]" ~ capture(nts) ~> ((value:Option[Seq[Node]],tail:String) => BlockArg(value.getOrElse(Seq.empty), tail))
    }

    def blockArgument = rule {
      "{" ~ optional(nodeRule) ~ "}" ~ capture(nts) ~>  ((value:Option[Seq[Node]], tail:String) => FuncArg(value.getOrElse(Seq.empty), tail))
    }

    def rawNodeRule = rule {
      capture(oneOrMore(!"\\" ~ !"]" ~ !"[" ~ !"}" ~ !"{" ~ !"$$" ~ !"$" ~ ANY)) ~> TextNode
    }

    def mathNodeRule = rule {
      inlineMathRule | blockMathRule
    }

    def blockMathRule = rule {
      "$$" ~ nodeRule ~ "$$" ~> BlockMath
    }

    def inlineMathRule = rule {
      "$" ~ !"$" ~ nodeRule ~ "$" ~> InlineMath
    }

    def nts = rule {
      zeroOrMore( newLine | " " | "\t")
    }


    def newLine = rule { "\r\n" | "\r" | "\n" }

    def documentRule = rule {
      nodeRule ~> Document
    }
  }

  def parse(doc: String): Try[Document]= {
    val cleaned = doc.replaceAll("""\\([Ss])lika(\w+)?\s?(<\w*>)?\s?([\w\-]+)?\[""","\\\\$1lika$2$3$4[")
                     .replaceAll("""\\([hv])box\s?to\s?(\w+)?\s?\{""","\\\\$1boxto$2{")
                     .replaceAll("""(?<!\\)%.*(\n|%)""","") //strip comments
    new SimpleTexParser(cleaned).documentRule.run()
  }
}


/* CLEAR
slikalijevo <2mm>ben-dahir[3cmx7cm]     //   \\slika(\w+)?\s?(<\w*>)?\s?([\w\-]+)?\[     to     \\slika$1$2$3\[
slika magicni-01rj[3cmx3cm][]
slikalijevo<3mm>bolsic[3cmx4cm][]
slikalijevo aa1-02[3cmx4cm][]
slikalijevo pi-kalkulato
slika <6mm>aa1-04[3cmx4.2cm][]
slika<9mm> figurice[3cmx6.5cm][]
vbox to 0pt
vbox to0pt
hbox to 6cm
 */

/* BLOCKS
\TAG[][]{<- XXX ->}               nested
\begin{TAG}{}   -> \end{TAG}      same level
\TAG            -> \endTAG        same level
 */