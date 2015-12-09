package services

import parsers.RuleParser
import parsers.RuleParser.{Comment, Transformer, Block}
import transform.Structural

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

  def transformTree(originalText: String) = {
    Structural.transform(originalText)
  }
  def transformTerminals(text: String, rules: String) = ???
}
