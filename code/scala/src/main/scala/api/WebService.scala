package api
import services.RuleService
import spray.routing._

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
            formFields("rules", "text") { (rules, text ) =>
              complete {
                 "dasdas"
              }
            }
          }
      }
    }
}
