import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import org.slf4j.{Logger, LoggerFactory}


import scala.util.Try

object Main {
  def main(args: Array[String]): Unit = {
    implicit val log: Logger = LoggerFactory.getLogger(getClass);
    implicit val system = ActorSystem(Behaviors.empty, "final");
    implicit val executionContext = system.executionContext;

    val users: Seq[User] = Seq()
//    val posts: Seq[Post] = Seq()
//    val likes: Seq[Like] = Seq()
    val usersRepository = new InMemoryUsersRepository(users)(executionContext)
    val router = new MyRouter(usersRepository)(system, executionContext)
//    val router = new MyRouter(usersRepository, postsRepository)(system, executionContext)
    val host = "localhost"
//    val host = "0.0.0.0"
    val port = Try(System.getenv("PORT")).map(_.toInt).getOrElse(9000)
    log.info("Server started")
    Server.startHttpServer(router.route, host, port)(system, executionContext)
  }
}