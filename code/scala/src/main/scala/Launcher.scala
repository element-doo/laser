import java.util.concurrent.Executors

import akka.actor.{Props, ActorSystem}
import akka.io.IO
import akka.pattern.ask
import parsers.TParser
import parsers.TParser.{Node, TextNode}
import spray.can.Http
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import api._

import scala.util.Try
import scalax.io.Resource

/*
object Launcher {
  def main(args: Array[String]): Unit = {
    implicit val executionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)
    val system = ActorSystem("LAZR")
    val webService = system.actorOf(Props(classOf[WebService]), "web-service")
    val bind = Http.Bind(
        listener = webService
      , interface = system.settings.config.getString("api.interface")
      , port = system.settings.config.getString("api.port").toInt
    )
    (IO(Http)(system) ? bind) (10 seconds) onSuccess {
      case Http.Bound(address) => println(s"Bound to: $address")
    }
  }
}
*/

object StaticLauncher {
  import TParser.{Node => RawNode, Func => RawFunc, TextNode => RawText, FuncArg => RawFArg, BlockArg => RawBArg }

  trait Ctx
  case class Singleton(node: RawNode) extends Ctx
  case class Flat(nodes: Seq[RawNode]) extends Ctx
  case class Nested(name: String, ctxs: Seq[Ctx]) extends Ctx


  def main (args: Array[String]){
     val pTree = Try {
      val text = Resource.fromClasspath("full.txt").string   //"element/text/origtxt/em1-01.txt"
      val parseTree = TParser.parse(text).get
      val ble = parseTree.drop(parseTree.size - 5)
      parseTree
    } getOrElse(sys.error("Could not open file!"))
    //println(pTree.size)

    /*process(pTree).foreach(ctx => ctx match {
      case Singleton(node) => println("singleton")
      case Nested(name, children ) => println(s"nested $name " +children.size )
      case _ => println("dasda")
    })*/

    //lazy val a = join(process(pTree))
    println(pTree)
    println("=======")
    println(join(process(pTree)))
    printNodes(0,join(process(pTree)))
/*
    println(a)
    println("dasd")
*/
    //shortcircut lookahead over searchSpace for closing tag
    def closingTagLookup(closingTag: String, searchSpace: Seq[RawNode], farg: Option[String]): Option[Int] = {
      searchSpace.indexWhere(node => {
        node match {
          case RawFunc(name,_,Seq()) => (name == closingTag) && farg.isEmpty
          case RawFunc(name, _, Seq(fArg:RawText)) => (name == closingTag) && farg.isDefined && (fArg.value == farg.get)
          case _ => false
        }
      }) match {
        case i if i < 0 => None
        case i => Some(i)
      }
    }

    //what to search for given tag
    def closeTag(node: RawNode): (Option[String], Option[String]) = {
      node match {
        case RawFunc("begin",_,Seq(fArg:RawFArg)) => {
          (Some("end"), Some(fArg.value.head.asInstanceOf[RawText].value))
        }   //klasican begin,end
        case RawFunc(name,_,Seq()) => (Some("end"+name), None)//beginTag,  endTag
        case _ => (None,None)                          //zatvoren sam u sebi
      }
    }

    def mergeContext(headNode: RawNode, ctxNodes: Option[Seq[RawNode]]): Ctx = {
      //ima li lookahead il ne
      //nested ako je func sa fArgs ili option definiran
      headNode match {
        case RawFunc(name,_,fBlocks) => {

          val nestedCtxs = fBlocks.isEmpty match {
            case true if ctxNodes.isDefined => process(ctxNodes.get)
            case false =>  fBlocks.flatMap(block => process(block.value))
            case _ => Seq.empty //ide singleton jer nema contexta unutar sebe
          }
          //TODO DIRTY HACK
          val tagName = if(fBlocks.isDefinedAt(0) && fBlocks(0).isInstanceOf[RawText])
            fBlocks(0).asInstanceOf[RawText].value
          else name
          if(nestedCtxs.isEmpty) Singleton(headNode) else Nested(tagName,nestedCtxs)
        }
        case RawText(value) => Singleton(headNode)
        case RawBArg(values) => Nested("#anonBBlock#", process(values))
        case RawFArg(values) => Nested("#anonFBlock#",process(values))
      }
    }

    def process(rawNodes: Seq[RawNode]): Seq[Ctx] = {
      rawNodes.seq match {
        case head+:tail => {
          val closingTag = closeTag(head)
          val lookAheadIndex = closingTag match {
            case (Some(tag:String), fArg:Option[String]) => closingTagLookup(tag,tail,fArg)//lookahead
            case (None,None) => None
          }
          val lookAheadNodes = lookAheadIndex.map(index => tail.take(index+1))
          val currCtx = mergeContext(head,lookAheadNodes)
          currCtx+:process(tail.drop(lookAheadIndex.map(_+1).getOrElse(0)))
        }
        case Nil => Nil
      }
    }


    def join(ctxs: Seq[Ctx]): Seq[Ctx] = {
      ctxs.seq match {
        case Singleton(node)+:tail => {
          //eeeeeeww, FOLDL this shit
          val singletonNodes = tail.takeWhile(_.isInstanceOf[Singleton])
            .asInstanceOf[Seq[Singleton]]
            .map(_.node)
          val last = tail.indexWhere(!_.isInstanceOf[Singleton])
          Flat(node+:singletonNodes)+:join(tail.drop(last))
        }
        case Nested(name,nestedCtxs)+:tail => Nested(name,join(nestedCtxs))+:join(tail)
        case Nil => Seq.empty
      }
    }

    def printNodes(lvl: Int, ctxs: Seq[Ctx]): Unit = {
      ctxs.seq match {
        case Flat(nodes)+:tail => {
          println("   " * lvl + "****")
          printNodes(lvl,tail)
        }
        case Nested(name,nestedCtxs)+:tail => {
          println("   " * lvl + name)
          printNodes(lvl+1,nestedCtxs)
          printNodes(lvl,tail)
        }
        case Nil => Seq.empty
      }
    }

/*

    def printCloseableTags(pTree: Seq[RawNode]) = {
      pTree.zipWithIndex.foreach(node => {
        val closing = closeTag(node._1)

        closing match {
          case (None,None) => println("closed in it self or no closing")
          case (Some(tag),None) => {
            val index = closingTagLookup(tag,pTree.drop(node._2+1),None)
            println(s"Found: $tag closed at: $index" )
          }
          case (Some(tag),optArg) => {
            val index = closingTagLookup(tag,pTree.drop(node._2+1),optArg)
            println(s"              Shoud be closed: $tag($optArg) at $index")
          }
        }
        if(closing._1.isDefined) {
          val index = closingTagLookup(closing._1.get,pTree.drop(node._2+1),closing._2)
          if(index.isDefined) println(s"closed: $closing at $index")
        }
      })
    }
*/


    //recursive descent, build index of all seen funcnames, dont perform lookup if not present in index



  }
}





