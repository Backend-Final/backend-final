import akka.actor.typed.ActorSystem
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import akka.http.scaladsl.server.{Directives, Route}

import scala.concurrent.ExecutionContext
import io.circe.generic.auto._

trait Router {
  def route: Route
}

class MyRouter(val usersRepository: InMemoryUsersRepository)(implicit system: ActorSystem[_],  ex:ExecutionContext)
  extends  Router
    with  Directives {

  def users = {
    pathPrefix("users") {
      concat(
        pathEnd {
          concat(
            get {
              complete(usersRepository.all())
            },
            post {
              entity(as[CreateUser]) { createUser =>
                complete(usersRepository.registerUser(createUser))
              }
            }
          )
        },
        path(Segment) { id =>
          concat(
            get {
              complete(usersRepository.getUser(id))
            },
            put {
              entity(as[UpdateUser]) { updateUser =>
                complete(usersRepository.updateUser(id, updateUser))
              }
            },
            delete {
              complete(usersRepository.deleteUser(id))
            }
          )
        }
      )
    }
  }

  override def route = {
    concat(
      users
    )
  }
}




