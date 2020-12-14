import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import org.slf4j.{Logger, LoggerFactory}
import actors.UserManager

import scala.util.Try

object Main {
  def main(args: Array[String]): Unit = {
    implicit val log: Logger = LoggerFactory.getLogger(getClass);

    val guardianActor = Behaviors.setup[Nothing] { context =>
      val userManagerActor = context.spawn(UserManager(), name = "userManagerActor")
      context.watch(userManagerActor)

      val host = "localhost"
      val port = Try(System.getenv("PORT")).map(_.toInt).getOrElse(9000)
      log.info("Server started")

      val router = new TwitterRouter(userManagerActor)(context.system)
      Server.startHttpServer(router.route, host, port)(context.system)

      Behaviors.empty
    }
    val system = ActorSystem[Nothing](guardianActor, "TwitterApp")
  }
}