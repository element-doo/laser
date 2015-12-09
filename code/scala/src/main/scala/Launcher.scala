import java.util.concurrent.Executors

import akka.actor.{Props, ActorSystem}
import akka.io.IO
import akka.pattern.ask
import parsers.TParser
import parsers.TParser.{Node, TextNode}
import spray.can.Http
import transform.Structural
import transform.rewriters.Rewriters
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import api._

import scala.util.Try
import scalax.io.Resource


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


object StaticLauncher {

  def main (args: Array[String]){
     val originalText = Try {
      Resource.fromClasspath("simple.txt").string   //"element/text/origtxt/em1-01.txt"
    } getOrElse(sys.error("Could not open file!"))
    val a = Rewriters.ifalica(Structural.parse(originalText))
    println(a)
  }
}





