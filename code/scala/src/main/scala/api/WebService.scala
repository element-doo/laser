package api
import services.{Store, RuleService}
import spray.routing._

import scala.util.Try
import scalax.io.Resource

case class TextSubmit(rules: String, text: String)

class WebService  extends HttpServiceActor {


  def receive = runRoute(detach(){
    assetRoutes ~
    ruleRoutes
  })

  def assetRoutes: Route =
    pathPrefix("") {
      getFromResourceDirectory("assets")
    }


  def apiRoutes: Route =
    pathPrefix("api") {
      path("bleh") {
        get {
          complete("asdasdasd")
        }
      }
    }

  def ruleRoutes: Route =
    pathPrefix("rules") {
      path("validate") {
        post {
          entity(as[String]) { rules =>
            complete(new RuleService().validateRules(rules).toString)
          }
        }
      } ~
      path("transform") {
          post {
            entity(as[TextSubmit]) { ts => {
              complete(new RuleService().transformTree(ts.text,ts.rules).toString)
            }}
          }
      } ~
      path("default") {
        get {
          complete(Store.readRules)
        } ~
        post {
          entity(as[String]) { rules => {
              complete(Store.storeRules(rules))
            }
          }
        }
      }/*~
      path("transform") {
        post {
          entity(as[TextSubmit]) { ts => {
            complete(new RuleService().transformTree(ts.text).toString)
          }}
        }
      }*/
    }
}