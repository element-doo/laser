import java.io.File
import java.io.File
import java.util.concurrent.Executors

import akka.actor.{Props, ActorSystem}
import akka.io.IO
import akka.pattern.ask
import parsers.TParser
import parsers.TParser.{Document, Node, TextNode}
import services.{Store, RuleService}
import spray.can.Http
import transform.Structural
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import api._


import scala.util.Try
import scalax.file.Path
import scalax.io.Resource


object Launcher {
  def main(args: Array[String]): Unit = {
    implicit val executionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

    val default = Store.readRules
    if(default.isSuccess) {
      println("Stored rules: \n" + default.get)
    } else {
      Store.storeRules("")
    }

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
      Resource.fromClasspath("full.txt").string   //"element/text/origtxt/em1-01.txt"D
    } getOrElse(sys.error("Could not open file!"))
    println(new RuleService().transformTree(originalText,"").toString)
  }
}


object BlehLauncher {

  def main (args: Array[String]){
    val input = //"""{\it dsfsds erw  gdf}""".stripMargin  //radi
    """{\it dsfsds erw ss\/}""".stripMargin  //ne radi
    //"""{\it dsfsds}""".stripMargin   //radi
    //println(TParser.parse(input))
    println(new RuleService().transformTree(input,"").toString)
  }
}





