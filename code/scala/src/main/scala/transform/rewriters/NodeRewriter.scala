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
      case BlockMath(values)            => BlockMath(transformList(values,Seq("root-math")))
      case TextNode(value)            => TextNode(rewriteText(value,ctx,Some("global")))
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

  def rewriteText(value: String,ctx: Seq[String],include: Option[String]): String = {
    //rewrite rules, math or plaintext, depending on the class path
    val oRules = textRules.get(ctx.mkString("-"))
    val globalRules = include.map(inclusion => "root-"+inclusion).map(key => textRules.get(key)).flatten//textRules.get("root-"+include.get)
    rewrite(value, oRules.getOrElse(Seq.empty)++globalRules.getOrElse(Seq.empty))
  }

  def rewrite(value: String, rules: Seq[Rule]): String = {
    rules.foldLeft(value)((acc:String, rule: Rule) => rule.from.replaceAllIn(acc,rule.to))
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
    (Matchers.innerFunctionPrefix("it"),    Seq(Rewriters.innerBlock("it"))),

    (Matchers.funcItalicBlock,  Seq(Rewriters.funcItalicBlock)),

    (Matchers.blockFunction("align"),  Seq(Rewriters.toBeginEndBlock("align"))),
    (Matchers.blockFunction("gather"),  Seq(Rewriters.toBeginEndBlock("gather"))),
    (Matchers.blockFunction("aligned"),  Seq(Rewriters.toBeginEndBlock("align"))),

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
    (Matchers.function(">",0),Seq(Rewriters.remove)),

    (Matchers.funcFunc("global","lhead"),Seq(Rewriters.remove)),
    (Matchers.funcFunc("global","rhead"),Seq(Rewriters.remove)),

    (Matchers.function("mark",1),Seq(Rewriters.remove)),

    (Matchers.function("BezBroja",0),Seq(Rewriters.remove)),
    (Matchers.function("Headlines",0),Seq(Rewriters.remove)),
    (Matchers.functionPrefix("BROJnaslov="),Seq(Rewriters.remove)),
    (Matchers.function("Primjertrue",0),Seq(Rewriters.remove)),
    (Matchers.function("Primjerfalse",0),Seq(Rewriters.remove)),
    (Matchers.function("OkvirPrimjer",0),Seq(Rewriters.remove)),
    (Matchers.function("vss",0),Seq(Rewriters.remove)),

    //(Matchers.functionPrefix("vbox"),Seq(Rewriters.remove)),
    (Matchers.functionPrefix("vglue"),Seq(Rewriters.remove)),
    (Matchers.functionPrefix("vskip"),Seq(Rewriters.remove)),

    (Matchers.functionPrefix("vbox"),Seq(Rewriters.flattenFunc)),

    (Matchers.blockFunction("tekst"),Seq(Rewriters.flattenBlock)),
    (Matchers.blockFunction("umetak"),Seq(Rewriters.flattenBlock)),
    (Matchers.blockFunction("Umetak"),Seq(Rewriters.flattenBlock)),

    (Matchers.innerFunctionPrefix("rightskip"),Seq(Rewriters.flattenInner)),

    (Matchers.function("BRzadatakp=0",0), Seq(Rewriters.genFun("setCounter",Seq("brojzadatkap","0")))),

    (Matchers.blockFunction("odg"),Seq(Rewriters.remove)),
    (Matchers.blockFunction("Odg"),Seq(Rewriters.remove)),

    (Matchers.inlineMath("{}"),Seq(Rewriters.remove)),

    (Matchers.functionPrefix("pomakslike"),Seq(Rewriters.remove)),
    (Matchers.functionPrefix("epsfxsize"),Seq(Rewriters.remove)),
    (Matchers.function("praznired",0),Seq(Rewriters.remove)),
    (Matchers.function("textBojaPoglavlja",0),Seq(Rewriters.remove)),
    (Matchers.function("textBlack",0),Seq(Rewriters.remove)),

    (Matchers.function("tv",1), Seq(Rewriters.flattenFunc)),
    (Matchers.innerFunctionPrefix("parindent"), Seq(Rewriters.flattenInner)),
    (Matchers.functionPrefix("leftskip"),Seq(Rewriters.remove)),
    (Matchers.functionPrefix("rightskip"),Seq(Rewriters.remove)),
    (Matchers.functionPrefix("vglue"),Seq(Rewriters.remove)),
    (Matchers.functionPrefix("vskip"),Seq(Rewriters.remove)),
    (Matchers.functionPrefix("hskip"),Seq(Rewriters.remove))

  )

  //MATCHERI


  object Matchers {
    //NO NESTED IFS!!!!!
    def ifBlock(input: Seq[Node]): Option[Match] = {
      input match {
        case Func(name,_,_)::tail if name.startsWith("if")=> {
          val elseIndex = tail.indexWhere(node => node.isInstanceOf[Func] && node.asInstanceOf[Func].name.equals("else"))
          val fiIndex = tail.indexWhere(node => node.isInstanceOf[Func] && node.asInstanceOf[Func].name.equals("fi"))
          //TODO bleh, chain options, FOR comprehens...
          if(elseIndex >= 0 && fiIndex >= 0) {
            Some(Match(fiIndex+2,Seq(tail.take(elseIndex),tail.slice(elseIndex+1,fiIndex))))
          } else if (fiIndex > elseIndex) { //only  if\fi blick
            Some(Match(fiIndex+2,Seq(tail.slice(0,fiIndex))))
          } else {
            None
          }
        }
        case _ => None
      }
    }
    def functionPrefix(name:String): Matcher =
      (input: Seq[Node]) =>  input match {
        case Func(fName,_,fArgs)+:tail if fName.startsWith(name) => {
          Some(Match(1,Seq.empty))
        }
        case _ => None
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
      val res = if (input.head.isInstanceOf[Func] && input.head.asInstanceOf[Func].name.toLowerCase().startsWith("slika")) {
        val func = input.head.asInstanceOf[Func]
        val (comment,size) = if (func.bArgs.size == 2) ("",0) else {
          val txts = input.tail.takeWhile(_.isInstanceOf[TextNode])
          val closingBArg = input.tail.drop(txts.size).head
          if(closingBArg.isInstanceOf[BlockArg] && closingBArg.asInstanceOf[BlockArg].value.size==0) {
            val joined = txts.foldLeft("")((a,b:Node) => a+b.asInstanceOf[TextNode].value)
            (joined,txts.size+1)
          } else ("",0)
        }
        Some(Match(1+size,Seq(Seq(TextNode(comment)))))
      } else {
        None
      }
      res
      //func with no fArgs, 1 barg  ~ X randomNodes ~ 1empty bArg   , transform into 2bArg func
    }
    def innerFunctionPrefix(name:String): Matcher =
      (input: Seq[Node]) =>  input match {
        case FuncArg(values,trailing)+:tail if values.nonEmpty => {
          values.head match {
            case Func(fName,Seq(),Seq()) if fName.startsWith(name) => Some(Match(1,Seq(values.tail)))  //stripaj zadnji dio italic string \/
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
    def inlineMath(value: String): Matcher =
      (input: Seq[Node]) => input match {
        case InlineMath(math)::tail if value == math => Some(Match(1,Seq.empty))
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
    val innerBlock = (blockName: String) =>
      (in: Input,m: Match) => {
        TextNode(s"<$blockName>")+:in.head.asInstanceOf[FuncArg].value.tail:+TextNode(s"</$blockName>")
      }
    val commentedSlika = (in: Input,m: Match) => in
    val funcItalicBlock = (in: Input, m: Match) => {
      val fIn = in.head.asInstanceOf[Func]
      val lastFArg = fIn.funcArg.last
      Seq(Func(fIn.name,fIn.bArgs,fIn.funcArg.seq.dropRight(1)),FuncArg(lastFArg.value,lastFArg.tail))
    }
    val toBeginEndBlock = (tag: String) =>
     (in: Input, m: Match) => {
      val bIn = in.head.asInstanceOf[BlockFunc]
      val opent = Func("begin",Seq.empty,Seq(FuncArg(Seq(TextNode(tag)),"")))
      val closet = Func("end",Seq.empty,Seq(FuncArg(Seq(TextNode(tag)),"")))
      Seq(BlockFunc(tag,opent,closet,bIn.nested))
    }
    val flattenBlock = (in: Input, m: Match) => {
      val bIn = in.head.asInstanceOf[BlockFunc]
      bIn.nested
    }
    val flattenFunc = (in: Input, m: Match) => {
      val fIn = in.head.asInstanceOf[Func]
      fIn.funcArg.head.value
    }
    val flattenInner = (in: Input, m: Match) => {
      m.groups.head
    }
    val genFun = (funName: String, fArgs: Seq[String]) =>
      (in: Input, m: Match) => {
        val args1= FuncArg(Seq(TextNode(fArgs.head)), "")
        val args2= FuncArg(Seq(TextNode(fArgs.tail.head)), "")
        Seq(Func(funName,Seq.empty,Seq(args1,args2)))
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