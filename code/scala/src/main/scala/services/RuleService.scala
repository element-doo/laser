/*
package services

import parsers.TParser.{MathNode, FreeTextNode}
import parsers.{TParser, RuleParser}
import parsers.RuleParser.{Comment, Transformer, Block}

import scala.util.matching.Regex

case class RuleNode(tag: String, rules: Seq[Rule], children: Seq[RuleNode])
case class Rule(from: Regex, to: String)

class RuleService {
  def validateRules(text: String) = {
    val parseTree = RuleParser.parse(text).get
    val parsedRules = parseTree.nodes.map({
      case Block(head,trans) => trans.size
      case Transformer(from,to) => from.toString+" -> "+to.toString
      case Comment(cmnt) => 0
    }).mkString("\n")
    println(parsedRules)
    parsedRules
  }


  def transform(from: String,rules: String) = {
    val r = RuleParser.parse(rules.replaceAll("(?m)^[ \t]*\r?\n", "")).get
    val t = TParser.parse(from).get

    val subtrees = r.nodes.map {
      case Block(head,trans) => nodeSubtree(head.classes,nodeToTransformers(trans))
      case Transformer(from,to) => RuleNode("root",Seq(Rule(from.value.r,to.value)),Seq.empty)
      case _ => RuleNode("root",Seq.empty,Seq.empty)
    }

    val emptyRoot = RuleNode("root",Seq.empty,Seq.empty)
    val rulesPerTag = subtrees.foldLeft(emptyRoot)(joinSubtees)

    //TODO auto mapping
    val rootRules = rulesPerTag.rules
    val mathRules = rulesPerTag.children.headOption.map(_.rules).getOrElse(Seq.empty)

    t.text.foldLeft(""){ (acc,curr) => {
      acc + (curr match {
        case FreeTextNode(value) => transformText(value,rootRules)
        case MathNode(value) => transformText(value,mathRules)
      })
    }}
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
*/