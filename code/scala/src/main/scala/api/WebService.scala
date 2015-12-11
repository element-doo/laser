package api
import services.RuleService
import spray.routing._

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
      } /*~
      path("transform") {
        post {
          entity(as[TextSubmit]) { ts => {
            complete(new RuleService().transformTree(ts.text).toString)
          }}
        }
      }*/
    }
}