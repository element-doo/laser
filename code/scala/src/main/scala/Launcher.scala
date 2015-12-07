import java.util.concurrent.Executors

import akka.actor.{Props, ActorSystem}
import akka.io.IO
import akka.pattern.ask
import parsers.TParser
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
  def main (args: Array[String]){
    Try {
      val text = Resource.fromClasspath("simple.txt").string   //"element/text/origtxt/em1-01.txt"
      val parseTree = TParser.parse(text).get
      val ble = parseTree.drop(parseTree.size - 5)
      println(parseTree)
    } getOrElse(sys.error("Could not open file!"))
  }
}