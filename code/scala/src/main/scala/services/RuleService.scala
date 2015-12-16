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
    Structural.transform(text,parseRules(rules++defaultRules))
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


  val defaultRules =
    """
      |global {
      |ss     =>  š
      |dd     =>  đ
      |cc     =>  č
      |ch     =>  ć
      |zz     =>  ž
      |S[Ss]  =>  Š
      |D[Dd]  =>  Đ
      |C[Cc]  =>  Č
      |C[Hh]  =>  Ć
      |Z[Zz]  =>  Ž
      |(s)\|(s)  =>  $1$2
      |(d)\|(d)  =>  $1$2
      |(c)\|(c)  =>  $1$2
      |(c)\|(h)  =>  $1$2
      |(z)\|(z)  =>  $1$2
      |(S)\|([Ss])  =>  $1$2
      |(D)\|([Dd])  =>  $1$2
      |(C)\|([Cc])  =>  $1$2
      |(C)\|([Hh])  =>  $1$2
      |(Z)\|([Zz])  =>  $1$2
      |<<< =>
      |}
    """.stripMargin
}
