package parsers

import org.parboiled2.RuleTrace.OneOrMore
import org.parboiled2._
import org.parboiled2.CharPredicate._

import scala.util.Try

object TParser {
  trait Node
  case class Func(name: String, bArgs: Seq[BlockArg], funcArg: Seq[FuncArg]) extends Node
  case class BlockArg(value: Seq[Node]) extends Node
  case class FuncArg(value: Seq[Node]) extends Node

  case class TextNode(value: String) extends Node

  //TODO must signal whether the entire input has been consumed, shoud fail if doesnt parse EOI

  class SimpleTexParser(val input: ParserInput) extends Parser {
    //def mathNodeRule = rule { "$$" ~ capture(zeroOrMore(!"$$" ~ ANY)) ~ "$$" ~> MathNode }

    def nodeRule: Rule1[Seq[Node]] = rule {
      oneOrMore(functionRule | blockArgument | argumentRule | rawNodeRule )
    }


    //ne consumati trailing NTS!!!!! potreban za rebuild
    def functionRule = rule {
      "\\" ~ capture(optional(functionNameRule)) ~ zeroOrMore(argumentRule) ~ optional(nts) ~ zeroOrMore(blockArgument) ~ optional(nts) ~> ((name:String,arg:Seq[BlockArg],fa:Seq[FuncArg])=> Func(name,arg,fa))
    }

    def functionNameRule = rule {
      oneOrMore(AlphaNum | "_" | "=" | ">" | "<" | "-" | ":" | "^" | ".")
    }

    def argumentRule = rule {
      "[" ~ optional(nodeRule) ~ "]" ~> ((value:Option[Seq[Node]]) => BlockArg(value.getOrElse(Seq.empty)))
    }

    def blockArgument = rule {
      "{" ~ optional(nodeRule) ~ "}" ~>  ((value:Option[Seq[Node]]) => FuncArg(value.getOrElse(Seq.empty)))
    }

    def rawNodeRule = rule {
      capture(oneOrMore(!"\\" ~ !"]" ~ !"[" ~ !"}" ~ !"{" ~ ANY)) ~> TextNode
    }

    def nts = rule {
      zeroOrMore( newLine | " " | "\t")
    }


    def newLine = rule { "\r\n" | "\r" | "\n" }

    def testno = rule {
      oneOrMore(nodeRule)
    }
  }

  def parse(doc: String): Try[Seq[Node]]= {
    val cleaned = doc.replaceAll("""\\([Ss])lika(\w+)?\s?(<\w*>)?\s?([\w\-]+)?\[""","\\\\$1lika$2$3$4[")
                     .replaceAll("""\\([hv])box\s?to\s?(\w+)?\s?\{""","\\\\$1boxto$2{")
                     .replaceAll("""(?<!\\)%.*(\n|%)""","") //strip comments
    new SimpleTexParser(cleaned).nodeRule.run()
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