import akka.actor.typed.ActorSystem
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import akka.http.scaladsl.server.{Directives, Route}

import scala.concurrent.ExecutionContext
import io.circe.generic.auto._

trait Router {
  def route: Route
}

class MyRouter(val usersRepository: InMemoryUsersRepository, val postRepository: InMemoryPostRepository)(implicit system: ActorSystem[_],  ex:ExecutionContext)
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
  def posts = {
    pathPrefix("posts") {
      concat(
        pathEnd {
          concat(
            get {
              complete(postRepository.all())
            },
            post {
              entity(as[CreatePost]) { createPost =>
                complete(postRepository.createPost(createPost))
              }
            }
          )
        },
        path(Segment) { id =>
          concat(
              get {
                complete(postRepository.getPost(id))
              },
              post {
                entity(as[UpdatePost]) { updatedPost =>
                  complete(postRepository.updatePost(id, updatedPost))
                }
              },
              delete {
                complete(postRepository.deletePost(id))
              },
            concat(
              path("like"){
                post{
                  entity(as[UpdatePost]){ post=>
                    complete(postRepository.likePost(id,post))
                  }
                }
              }
            )
          )
        }
      )
    }
  }


  override def route = {
    concat(
      users,
      posts
    )
  }
}




