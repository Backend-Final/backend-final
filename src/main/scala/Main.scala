import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import org.slf4j.{Logger, LoggerFactory}
import actors.{LikeManager, PostManager, UserManager}

import scala.util.Try

object Main {
  def main(args: Array[String]): Unit = {
    implicit val log: Logger = LoggerFactory.getLogger(getClass)

    val guardianActor = Behaviors.setup[Nothing] { context =>
      val userManagerActor = context.spawn(UserManager(), name = "userManagerActor")
      val postManagerActor = context.spawn(PostManager(), name = "postManagerActor")
      val likeManagerActor = context.spawn(LikeManager(), name = "likeManagerActor")
      context.watch(userManagerActor)
      context.watch(postManagerActor)
      context.watch(likeManagerActor)

      val host = "localhost"
      val port = Try(System.getenv("PORT")).map(_.toInt).getOrElse(9000)
      log.info("Server started")

      val router = new TwitterRouter(userManagerActor, postManagerActor, likeManagerActor)(context.system)
      Server.startHttpServer(router.route, host, port)(context.system)

      Behaviors.empty
    }
    ActorSystem[Nothing](guardianActor, "TwitterApp")
  }
}