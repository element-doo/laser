package transform.rewriters

import parsers.TParser._
import transform.rewriters.NodeRewriter.{Rewriter, Match}


/*trait NodeRewriter[R>:Node,T>:Node]{


  def rewrite(node: R): Option[T]
}*/
object Descender {


  //TODO extract common descender
  def descend(node: Node, ctx: Seq[String]): Node = {
    node match {
      case MathNode(value)            => MathNode(rewriteText(value,ctx))
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
      , descend(open,ctx:+tag)
      , descend(close,ctx:+tag)
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
    value
  }

  def rewrite(nodes: Seq[Node]): Seq[Node]= {
    //bleh redo this
    NodeRewriter.transformations.map({
      case (matcher, rewriters) => (rewriters.head,matcher(nodes)) //multi :/
    }).find(
      {case (rewriter: Rewriter,matching: Option[Match]) => matching.isDefined}
    ).map({
      case (rewriter, matching) => {
        val rewritenNodes = rewriter(nodes.take(matching.get.consumed),matching.get)
        rewritenNodes++nodes.drop(matching.get.consumed)
      }
    }).getOrElse(if(!nodes.isEmpty)rewrite(nodes.tail)else nodes) //recurse down the nodes list, after match and rewrite, rewrite the reminder
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
    (Matchers.function("df"),       Seq(Rewriters.boldBlock)),
    (Matchers.function("it"),       Seq(Rewriters.italicBlock)),
    (Matchers.function("df"),       Seq(Rewriters.remove)),
    (Matchers.function("bigskip"),  Seq(Rewriters.remove)),
    (Matchers.function("smallskip"),Seq(Rewriters.remove)),
    (Matchers.function("medskip"),  Seq(Rewriters.remove))
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
    def function(name:String): Matcher =
      (input: Seq[Node]) =>  input match {
        case Func(fName,_,Seq(farg))+:tail if fName == name => Some(Match(1,Seq.empty))
        case _ => None
      }
    def slikaComment(input: Seq[Node]): Option[Match] = {
      //input.head.isInstanceOf[Func]
      //func with no fArgs, 1 barg  ~ X randomNodes ~ 1empty bArg   , transform into 2bArg func
      None
    }
  }


  object Rewriters { //input is the only matched part of global input
    val remove = (in: Input,m: Match) => Seq.empty
    val elseRemoval = (in: Input,m: Match) => {
      m.groups.head
    }   //take only elements nested under if clause
    val boldBlock = (in: Input,m: Match) => { //type guaranteed by matcher
      TextNode("<bold>")+:in.head.asInstanceOf[Func].funcArg.head.value:+TextNode("</bold>")
    }
    val italicBlock = (in: Input,m: Match) => {//type guaranteed by matcher
      TextNode("<it>")+:in.head.asInstanceOf[Func].funcArg.head.value:+TextNode("</it>")
    }
    val commentedSlika = (in: Input,m: Match) => in
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