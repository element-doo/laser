import parsers.TParser.{MathNode, FreeTextNode}
import parsers.{TParser, RuleParser}
import parsers.RuleParser.{Class, Transformer, Block}
import scala.util.Try
import scala.util.matching.Regex
import scalax.io.Resource


case class RuleNode(tag: String, rules: Seq[Rule], children: Seq[RuleNode])
case class Rule(from: Regex, to: String)

object Launcher {
  def main(args: Array[String]): Unit = {
    var rules = Resource.fromClasspath("rules.txt").string.replaceAll("(?m)^[ \t]*\r?\n", "")
    var text = Resource.fromClasspath("full.txt").string

    Try {
      rules = Resource.fromClasspath("rules.txt").string.replaceAll("(?m)^[ \t]*\r?\n", "")
      text = Resource.fromClasspath("full.txt").string
    } getOrElse(sys.error("Could not open file!"))

    val r = RuleParser.parse(rules).get
    val t = TParser.parse(text).get

    val subtrees = r.nodes.map {
      case Block(head,trans) => nodeSubtree(head.classes,nodeToTransformers(trans))
      case Transformer(from,to) => RuleNode("root",Seq(Rule(from.value.r,to.value)),Seq.empty)
      case _ => RuleNode("root",Seq.empty,Seq.empty)
    }

    val emptyRoot = RuleNode("root",Seq.empty,Seq.empty)
    val rulesPerTag = subtrees.foldLeft(emptyRoot)(joinSubtees)

    //TODO auto mapping
    val rootRules = rulesPerTag.rules
    val mathRules = rulesPerTag.children.head.rules

    t.text.foreach(node => {
      node match {
        case FreeTextNode(value) => println(transformText(value,rootRules))
        case MathNode(value) => println(transformText(value,mathRules))
      }
    })


  }

  def transformText(initial: String, rules: Seq[Rule]) = {
    rules.foldLeft(initial)((acc:String,rule: Rule) => rule.from.replaceAllIn(acc,rule.to))
  }

  def joinSubtees(from: RuleNode, to: RuleNode) : RuleNode = {
    if(from.tag==to.tag) {
      RuleNode(from.tag,from.rules++to.rules,from.children++to.children)
    } else {
      val childrenToJoin = from.children.partition(_.tag==to.tag)
      val newChild = childrenToJoin._1.headOption match {
        case Some(child) => joinSubtees(child,to)
        case _ => RuleNode(to.tag,to.rules,to.children)
      }
      RuleNode(from.tag,from.rules,Seq(newChild)++childrenToJoin._2)
    }
  }

  //TODO eeeewwww, dirtydirty hack, remove comment completely from AST, trans should be only parent node of block
  def nodeToTransformers(nodes: Seq[RuleParser.Node]): Seq[Transformer] = {
    nodes.filter(_.isInstanceOf[Transformer]).asInstanceOf[Seq[Transformer]]
  }

  def nodeSubtree(clzz: Seq[RuleParser.Class],transformers: Seq[Transformer]): RuleNode = {
    clzz match {
      case clz +: IndexedSeq() => {
        val name = clz.value
        val rules = transformers.map(trans => Rule(trans.from.value.r, trans.to.value))
        RuleNode(name,rules,Seq.empty)
      }
      case clz +: rest => RuleNode(clz.value,Seq.empty,Seq(nodeSubtree(rest, transformers)))
    }
  }
}