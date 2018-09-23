import actors.WorldActor
import com.google.inject.AbstractModule
import play.api.libs.concurrent.AkkaGuiceSupport
import services.MastodonStream

class Module extends AbstractModule with AkkaGuiceSupport {

  override def configure(): Unit = {
    bindActor[WorldActor]("world")
    bind(classOf[MastodonStream]).asEagerSingleton()
  }
}
