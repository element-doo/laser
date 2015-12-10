package transform

import parsers.TParser
import parsers.TParser.BlockFunc

object Structural {
  import TParser.{Node => RawNode, Func => RawFunc, TextNode => RawText, FuncArg => RawFArg, BlockArg => RawBArg, MathNode => RawMath }



  def transform(originalText: String): String = {
    val pTree = TParser.parse(originalText).get
    //println(join(process(pTree)))
    print(process(pTree.nodes))
  }

  def parse(originalText: String): Seq[RawNode] = {
    process(TParser.parse(originalText).get.nodes)
  }


  private def closingTagLookup(closingTag: String, searchSpace: Seq[RawNode], farg: Option[String]): Option[Int] = {
    searchSpace.indexWhere(node => {
      node match {
        case RawFunc(name,_,Seq()) => (name == closingTag) && farg.isEmpty
        case RawFunc(name, _, Seq(fArg:RawFArg)) => {
          (name == closingTag) && farg.isDefined && (fArg.value.head.asInstanceOf[RawText].value == farg.get) //osigurati da je stvarno rawText
        }
        case _ => false
      }
    }) match {
      case i if i < 0 => None
      case i => Some(i)
    }
  }

  //what to search for given tag
  private def closeTag(node: RawNode): (Option[String], Option[String]) = {
    node match {
      case RawFunc("begin",_,Seq(fArg:RawFArg)) => {
        (Some("end"), Some(fArg.value.head.asInstanceOf[RawText].value))
      }   //klasican begin,end
      case RawFunc(name,_,Seq()) => (Some("end"+name), None)//beginTag,  endTag
      case _ => (None,None)                          //zatvoren sam u sebi
    }
  }


  private def process(rawNodes: Seq[RawNode]): Seq[RawNode] = {
    rawNodes.seq match {
      case head+:tail => {
        val closingTag = closeTag(head)
        val lookAheadIndex = closingTag match {
          case (Some(tag:String), fArg:Option[String]) => closingTagLookup(tag,tail,fArg)//lookahead
          case (None,None) => None
        }
        //some,some  - bengin/end
        //some,none  - beginTAG/endTAG
        //none,none  - selfcontained
        val processedNode = lookAheadIndex.map(index => {
          val contextNodes = tail.take(index)
          val closingNode = tail(index)
          val processedNestedNodes = process(contextNodes)
          val tag = head.asInstanceOf[RawFunc].name
          BlockFunc(tag,head,closingNode,processedNestedNodes)
        }).getOrElse({
          processSingle(head)
        })
        processedNode+:process(tail.drop(lookAheadIndex.map(_+1).getOrElse(0)))
      }
      case Nil => Nil
    }
  }

  private def processSingle(node: RawNode): RawNode = {
    node match {
      case RawBArg(nested,tail)            => RawBArg(process(nested),tail)
      case RawFArg(nested,tail)            => RawFArg(process(nested),tail)
      case text:RawText                    => text
      case math:RawMath                    => math
      case RawFunc(name,bArgs,fArgs)  => {
        val pBargs = bArgs.map(barg => RawBArg(process(barg.value),barg.tail))
        val pFargs = fArgs.map(farg => RawFArg(process(farg.value),farg.tail))
        RawFunc(name,pBargs,pFargs)
      }
    }
  }


  private def print(nodes: Seq[RawNode]): String = {
    def prt(node: RawNode): String = {
      node match {
        case BlockFunc(tag, open,close,children) => {
          prt(open) + print(children) + prt(close)
        }
        case RawFunc(name, bArgs, fArgs) => {
          "\\" + name + print(bArgs) + print(fArgs)
        }
        case RawBArg(children, tail) => {
          "[" + print(children) + "]" +  tail
        }
        case RawFArg(children, tail) => {
          "{" + print(children) + "}" + tail
        }
        case RawMath(value) => "$$" + value + "$$"
        case RawText(value) => value
      }
    }
    nodes.map(prt).mkString
  }

}
