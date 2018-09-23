package mastodon

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.sse.ServerSentEvent
import akka.stream.{ActorMaterializer, ThrottleMode}
import akka.stream.alpakka.sse.scaladsl.EventSource
import akka.stream.scaladsl.Sink
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.json

import scala.concurrent.duration._
import scala.concurrent.Future

case class Account(acct: String)
case class Status(id: String, content: String)
case class Notification(account: Account, status: Status)

object Json {
  implicit val accountReads: Reads[Account] =
    (JsPath \ "acct").read[String].map(Account.apply)

  implicit val statusReads: Reads[Status] = (
    (JsPath \ "id").read[String] and
      (JsPath \ "content").read[String]
  )(Status.apply _)

  implicit val notificationReads: Reads[Notification] = ((JsPath \ "account")
    .read[Account] and (JsPath \ "status").read[Status])(Notification.apply _)
}

sealed abstract class StatusVisibility
object StatusVisibility {
  case object Direct extends StatusVisibility
}

case class Client(baseApiUrl: String, accessToken: String) {
  def status(content: String, visibility: StatusVisibility)(
    implicit actorSystem: ActorSystem
  ): Future[HttpResponse] = Http().singleRequest(
    HttpRequest(
      uri = Uri(baseApiUrl)
        .withPath(Path("/api/v1/statuses")),
      method = HttpMethods.POST,
      headers = List(RawHeader("Authorization", "Bearer " + accessToken)),
      entity = FormData(
        Map(
          "status" -> content,
          "visibility" -> fromVisibility(visibility),
          "language" -> "en"
        )
      ).toEntity(HttpCharsets.`UTF-8`)
    )
  )

  private def fromVisibility(visibility: StatusVisibility) = visibility match {
    case StatusVisibility.Direct => "direct"
    case _                       => "global"
  }

  case object streaming {

    def user(f: Notification => Unit)(implicit actorSystem: ActorSystem): Future[Done] = {
      implicit val mat: ActorMaterializer = ActorMaterializer()
      EventSource(
        Uri(baseApiUrl).withPath(Path("/api/v1/streaming/user")),
        send,
        None,
        1.second
      ).throttle(1, 500.milliseconds, 1, ThrottleMode.Shaping)
        .filter(_.eventType.contains("notification"))
        .runWith(Sink.foreach{
          case ServerSentEvent(data, Some("notification"), _, _) =>
            f(json.Json.parse(data).as(Json.notificationReads))
          case _ => // ignore
        })
    }

    private def send(request: HttpRequest)(implicit actorSystem: ActorSystem) =
      Http().singleRequest(
        request.addHeader(RawHeader("Authorization", "Bearer " + accessToken))
      )
  }
}
