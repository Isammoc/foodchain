package controllers
import actors.WorldActor
import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import javax.inject.{Inject, Named}
import play.api.mvc.{AbstractController, ControllerComponents}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class MainController @Inject()(
  @Named("world") worldActor: ActorRef,
  cc: ControllerComponents
)(implicit ec: ExecutionContext)
    extends AbstractController(cc) {

  def index = Action.async {
    implicit val timeout = Timeout(1.second) // the first run in dev can take a while :-(
    val futureReport: Future[Any] = worldActor ? WorldActor.ReportCmd(None)
    futureReport.collect{
      case WorldActor.Report(report) =>
        Ok(views.html.index(report))
      case _ =>
        NotFound
    }
  }
}
