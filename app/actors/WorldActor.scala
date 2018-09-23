package actors
import akka.actor.{Actor, Props}
import engine.model.World
import javax.inject.Inject
import play.api.Configuration

import scala.concurrent.duration._

object WorldActor {
  def props: Props = Props[WorldActor]

  case class Report(report: String)
  case object Turn
  sealed abstract class Command
  case class StartCmd(user: String,
                      herbivore: Boolean,
                      nose: Int,
                      eyes: Int,
                      ears: Int,
                      mouth: Int,
                      paws: Int,
                      tail: Int,
                      legs: Int,
                      brain: Int)
    extends Command
  case class ReportCmd(user: Option[String]) extends Command
}

class WorldActor @Inject() (configuration: Configuration) extends Actor {
  import WorldActor._
  import context.dispatcher

  private val delay = configuration.get[FiniteDuration]("world.default.duration")
  private val tick = context.system.scheduler.schedule(delay, delay, self, Turn)
  private var world: World = World.initial

  override def postStop() = tick.cancel()

  override def receive: Receive = {
    case ReportCmd(user) => sender() ! Report(engine.Report(world, user))
    case cmd:StartCmd =>
      val result = world.startSpecies(cmd.user, cmd.herbivore, cmd.nose, cmd.eyes, cmd.ears, cmd.mouth, cmd.paws, cmd.tail, cmd.legs, cmd.brain)
      if(result.isLeft)
        sender() ! Report(result.left.get)
      else {
        world = result.right.get
        sender() ! Report("Started with new species")
      }
    case Turn => world = world.next
  }
}
