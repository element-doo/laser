package transform.rewriters

import parsers.RuleParser.{ToTransformer, FromTransformer, Transformer}
import parsers.TParser._
import services.Rule
import transform.rewriters.NodeRewriter.{Rewriter, Match}


/*trait NodeRewriter[R>:Node,T>:Node]{


  def rewrite(node: R): Option[T]
}*/
object Descender {
  def apply(rules: Map[String,Seq[Rule]]) = new Descender(rules)
}

class Descender(textRules: Map[String,Seq[Rule]]) {

  //TODO extract common descender
  def descend(node: Node, ctx: Seq[String]): Node = {
    node match {
      case InlineMath(value)            => InlineMath(value) //TODO don't rewrite math tags
      case BlockMath(value)            => BlockMath(rewriteText(value,Seq("root-math")))
      case TextNode(value)            => TextNode(rewriteText(value,ctx))
      case BlockArg(vals,tail)        => BlockArg(transformList(vals,ctx),tail)
      case FuncArg(vals,tail)         => FuncArg(transformList(vals,ctx),tail)
      case Func(name,bArgs,fArgs)     => {
        val nestedCtx = ctx:+name
        val pBargs = bArgs.map(barg => BlockArg(transformList(barg.value,nestedCtx),barg.tail))
        val pFargs = fArgs.map(farg => FuncArg(transformList(farg.value,nestedCtx),farg.tail))
        Func(name,pBargs,pFargs)
      }
      case BlockFunc(tag, open,close,vals) => BlockFunc(
        tag
        , open //TODO dont apply rules to tags
        , close
        , transformList(vals, ctx:+tag)
      )
      case Document(nodes)            => Document(transformList(nodes,ctx))
    }
  }

  //matchers, rewriters
  def transformList(nodes: Seq[Node], ctx: Seq[String]): Seq[Node] = {
    //descendaj sam
    val transformedChildren = nodes.map(descend(_,ctx))
    //transformiraj ako treba pa vrati
    rewrite(transformedChildren)
  }

  def rewriteText(value: String,ctx: Seq[String]): String = {
    //rewrite rules, math or plaintext, depending on the class path
    val oRules = textRules.get(ctx.mkString("-"))
    oRules.map(rules => {
      rules.foldLeft(value)((acc:String,rule: Rule) => rule.from.replaceAllIn(acc,rule.to))
    }).getOrElse(value)
  }

  def rewrite(nodes: Seq[Node]): Seq[Node]= {
    //find matcher that matches, transform, rerun again (on the same input or down the tail)
    //if no match, continue down the tail
    val matchingPerMatcher = NodeRewriter.transformations.keys.map(matcher => {
      (NodeRewriter.transformations(matcher).head, matcher(nodes)) //multi :/
    })
    val successefulMatcher = matchingPerMatcher.find({
      case (rewriter: Rewriter,matching: Option[Match]) => matching.isDefined   //first that matched
    })
    successefulMatcher.map({
      case (rewriter, matching) => {
        val rewritenNodes = rewriter(nodes.take(matching.get.consumed),matching.get)
        rewrite(rewritenNodes++nodes.drop(matching.get.consumed))                     //TODO!!! retries other matchers on rewritten rule
      }
    }).getOrElse({
      if(nodes.isEmpty)
        Seq.empty
      else
        nodes.head+:rewrite(nodes.tail)
    }) //recurse down the nodes list, after match and rewrite, rewrite the reminder
  }
}



object NodeRewriter {

  type Input = Seq[Node]
  type MatchingGroup = Seq[Node]
  type Matcher = Seq[Node] => Option[Match]
  case class Match(consumed: Int, groups: Seq[MatchingGroup])
  type Rewriter = (Seq[Node], Match) => Seq[Node]

  val transformations: Map[Matcher,Seq[Rewriter]] = Map(
    (Matchers.ifBlock,              Seq(Rewriters.elseRemoval)),
    (Matchers.function("df",1),       Seq(Rewriters.boldBlock)),
    (Matchers.function("it",1),       Seq(Rewriters.italicBlock)),
    (Matchers.function("df",1),       Seq(Rewriters.remove)),
    (Matchers.function("smallskip",0),Seq(Rewriters.remove)),
    (Matchers.function("bigskip",0),  Seq(Rewriters.remove)),
    (Matchers.innerFunction("it"),    Seq(Rewriters.italicInnerBlock)),
    (Matchers.innerFunction("it"),    Seq(Rewriters.italicInnerBlock)),
    (Matchers.funcItalicBlock,  Seq(Rewriters.funcItalicBlock)),

    (Matchers.blockFunction("align"),  Seq(Rewriters.toBeginEndBlock)),
    (Matchers.blockFunction("gather"),  Seq(Rewriters.toBeginEndBlock)),

    (Matchers.function("break",0), Seq(Rewriters.remove)),
    (Matchers.function("newpage",0),Seq(Rewriters.remove)),
    (Matchers.function("Prelomi",0),Seq(Rewriters.remove)),

    (Matchers.function("ujedan",0),Seq(Rewriters.remove)),
    (Matchers.function("udva",0),Seq(Rewriters.remove)),
    (Matchers.function("utri",0),Seq(Rewriters.remove)),
    (Matchers.function("ucetiri",0),Seq(Rewriters.remove)),
    (Matchers.function("upet",0),Seq(Rewriters.remove)),
    (Matchers.function("tudva",0),Seq(Rewriters.remove)),
    (Matchers.function("tutri",0),Seq(Rewriters.remove)),
    (Matchers.function(">",0),Seq(Rewriters.remove))
  )

  //MATCHERI


  object Matchers {
    //NO NESTED IFS!!!!!
    def ifBlock(input: Seq[Node]): Option[Match] = {
      input match {
        case Func(name,_,_)::tail if name.startsWith("if")=> {
          val elseIndex = tail.indexWhere(node => node.isInstanceOf[Func] && node.asInstanceOf[Func].name.equals("else"))
          val fiIndex = tail.drop(elseIndex+1).indexWhere(node => node.isInstanceOf[Func] && node.asInstanceOf[Func].name.equals("fi"))
          //TODO bleh, chain options, FOR comprehens...
          if(elseIndex >= 0 && fiIndex >= 0) {
            Some(Match(elseIndex+fiIndex+3,Seq(tail.take(elseIndex),tail.slice(elseIndex+1,elseIndex+fiIndex+1))))
          } else {
            None
          }
        }
        case _ => None
      }
    }
    def function(name:String, fArgsLen: Int): Matcher =
      (input: Seq[Node]) =>  input match {
        case Func(fName,_,fArgs)+:tail if fName == name && fArgs.size == fArgsLen => {
          Some(Match(1,Seq.empty))
        }
        case _ => None
      }
    def blockFunction(tag: String): Matcher =
      (input: Seq[Node]) =>  input match {
        case BlockFunc(tagVal,open,close,nested)+:tail if tagVal == tag && open.asInstanceOf[Func].funcArg.isEmpty => {
          Some(Match(1,Seq(nested)))
        }
        case _ => None
      }
    def slikaComment(input: Seq[Node]): Option[Match] = {
      //input.head.isInstanceOf[Func]
      //func with no fArgs, 1 barg  ~ X randomNodes ~ 1empty bArg   , transform into 2bArg func
      None
    }
    def innerFunction(name:String): Matcher =
      (input: Seq[Node]) =>  input match {
        case FuncArg(values,trailing)+:tail if values.nonEmpty => {
          values.head match {
            case Func(fName,Seq(),Seq()) if fName==name => Some(Match(1,Seq(values.tail)))  //stripaj zadnji dio italic string \/
            case _ => None
          }
        }
        case _ => None
      }
    def funcItalicBlock(input: Seq[Node]): Option[Match] = {
      input match {
        case Func(name,_,fArgs)::tail if fArgs.nonEmpty => {
          fArgs.last match {
            case FuncArg(values,tail) if values.nonEmpty => {
              values.headOption.map({
                case Func(name,_,_) if name=="it" => Some(Match(1,Seq.empty))
                case _ => None
              }).get
            }
            case _ => None
          }
        }
        case _ => None
      }
    }
    def funcFunc(first: String, seccond: String): Matcher =
      (input: Seq[Node]) => input match {
        case Func(f1,_,_)::Func(f2,_,_)::tail  if first == f1 && seccond == f2 => Some(Match(2,Seq.empty))
        case _ => None
      }
  }


  object Rewriters { //input is the only matched part of global input
    val remove = (in: Input,m: Match) => {
      Seq.empty
    }
    val elseRemoval = (in: Input,m: Match) => {
      m.groups.head
    }   //take only elements nested under if clause
    val boldBlock = (in: Input,m: Match) => { //type guaranteed by matcher
      TextNode("<bold>")+:in.head.asInstanceOf[Func].funcArg.head.value:+TextNode("</bold>")
    }
    val italicBlock = (in: Input,m: Match) => {//type guaranteed by matcher
      TextNode("<it>")+:in.head.asInstanceOf[Func].funcArg.head.value:+TextNode("</it>")
    }
    val italicInnerBlock = (in: Input,m: Match) => {
      TextNode("<it>")+:in.head.asInstanceOf[FuncArg].value.tail:+TextNode("</it>")
    }
    val commentedSlika = (in: Input,m: Match) => in
    val funcItalicBlock = (in: Input, m: Match) => {
      val fIn = in.head.asInstanceOf[Func]
      val lastFArg = fIn.funcArg.last
      Seq(Func(fIn.name,fIn.bArgs,fIn.funcArg.seq.dropRight(1)),FuncArg(lastFArg.value,lastFArg.tail))
    }
    val toBeginEndBlock = (in: Input, m: Match) => {
      val bIn = in.head.asInstanceOf[BlockFunc]
      val opent = Func("begin",Seq.empty,Seq(FuncArg(Seq(TextNode(bIn.tag)),"")))
      val closet = Func("end",Seq.empty,Seq(FuncArg(Seq(TextNode(bIn.tag)),"")))
      Seq(BlockFunc(bIn.tag,opent,closet,bIn.nested))
    }
  }


}

//func(blockArg bez fArga) stringNode blockArg    <----------komentar slike

//rewriter
//structural patterns


//niz od 3 -> niz od 1
//1 i njegovu strukturu

//pattern(input) => na matchanom consumanom inputu odradi pravila za taj pattern
//pattern = Seq[RawNode]
//BFS descend po djeci, kod flattanog krece matching
  //za svaki node sekve, probaj ga matchat s pocetkom nekog patterna, consumaj dalje, do kraja
  //sacuvaj matchinge i tailove za svaki okinuti
  //odredi redosljed po prioritetima
  //nakon primjene svakoga,
  //provrti ponovo matching