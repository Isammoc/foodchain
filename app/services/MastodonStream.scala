package services
import actors.WorldActor
import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import javax.inject.{Inject, Named}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import akka.http.scaladsl.model.headers.RawHeader
import play.api.Configuration
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import akka.pattern.ask
import akka.util.Timeout
import mastodon.{Client, Notification, StatusVisibility}

case class Message(user: String, content: String) {
  val Start =
    """(?i).*start(?:\s+([HC]))?(?:\s+N([0-9]+))?(?:\s+Y([0-9]+))?(?:\s+R([0-9]+))?(?:\s+M([0-9]+))?(?:\s+P([0-9]+))?(?:\s+T([0-9]+))?(?:\s+L([0-9]+))?(?:\s+B([0-9]+))?.*""".r
  val Report = "(?i).*report.*".r

  private def toIntOrZero(value: String) = if (value == null) 0 else value.toInt

  def parse: List[WorldActor.Command] =
    content.split("\\s*<br\\s*/?>\\s*").toList.flatMap {
      case Start(herbivore, nose, eyes, ears, mouth, paws, tail, legs, brain) =>
        Some(
          WorldActor.StartCmd(
            user,
            herbivore == "H",
            toIntOrZero(nose),
            toIntOrZero(eyes),
            toIntOrZero(ears),
            toIntOrZero(mouth),
            toIntOrZero(paws),
            toIntOrZero(tail),
            toIntOrZero(legs),
            toIntOrZero(brain)
          )
        )
      case Report() => Some(WorldActor.ReportCmd(Some(user)))
      case _        => None
    }
}

object Message {
  val reads: Reads[Message] =
    (((__ \ "account") \ "acct").read[String] and
      ((__ \ "status") \ "content").read[String])(Message.apply _)

  def fromString(data: String): JsResult[Message] =
    Json
      .parse(data)
      .validate(Message.reads)
}

class MastodonStream @Inject()(
  @Named("world") worldActor: ActorRef,
  configuration: Configuration
)(implicit actorSystem: ActorSystem, ec: ExecutionContext) {
  private val accessToken =
    configuration.get[String]("world.default.access-token")
  private implicit val mat = ActorMaterializer()

  Client("https://botsin.space", accessToken).streaming.user(processMessage)

  private def send(request: HttpRequest) =
    Http().singleRequest(
      request.addHeader(RawHeader("Authorization", "Bearer " + accessToken))
    )

  private def processMessage(msg: Notification): Unit =
    Message(msg.account.acct, msg.status.content).parse.foreach { cmd =>
      implicit val timeout = Timeout(1.second) // the first run in dev can take a while :-(
      val futureReport: Future[Any] = worldActor ? cmd
      futureReport.collect {
        case WorldActor.Report(report) =>
          Client("https://botsin.space", accessToken)
            .status("@" + msg.account.acct + " " + report, StatusVisibility.Direct)
        case _ =>
      }
    }
}
