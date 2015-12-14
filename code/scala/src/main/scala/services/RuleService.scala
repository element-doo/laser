package services

import parsers.RuleParser
import parsers.RuleParser.{Document, Transformer, Block}
import transform.Structural

import scala.util.matching.Regex

case class RuleNode(tag: String, rules: Seq[Rule], children: Seq[RuleNode])
case class Rule(from: Regex, to: String)

class RuleService {
  def validateRules(text: String) = {
    val ruleMap = parseRules(text)
    ruleMap.mkString("\n")
  }

  def transformTree(text: String, rules: String) = {
    Structural.transform(text,parseRules(rules))
  }

  private def parseRules(input: String): Map[String,Seq[Rule]] = {
    val parseTree = RuleParser.parse(input).get.nodes
    val rulesPerPath = parseTree.map({
      case Block(head,trans) => ("root-"+head.classes.map(_.value).mkString("-"),trans.map(t=> Rule(t.from.value.r,t.to.map(_.value).getOrElse(""))))
      case a: Transformer => ("root", Seq(Rule(a.from.value.r,a.to.map(_.value).getOrElse(""))))
    }).groupBy(_._1).map { case (key,value) => (key,value.map(_._2).flatten)}
    rulesPerPath
  }

  private def buildRuleTree(rules: Seq[RuleNode]) = ???
}
