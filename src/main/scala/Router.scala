import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext
import io.circe.generic.auto._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import akka.http.scaladsl.server.{Directives, Route}

trait Router {
  def route: Route
}

class MyRouter(val usersRepository: InMemoryUsersRepository, val postRepository: InMemoryPostRepository)(implicit system: ActorSystem[_],  ex:ExecutionContext)
  extends  Router
    with  Directives
    with ValidatorDirectives {

  def users = {
    pathPrefix("users") {
      concat(
        pathEndOrSingleSlash {
          concat(
            get {
              complete(usersRepository.all())
            },
            post {
              entity(as[CreateUser]) { createUser =>
                validateWith(CreateUserValidator)(createUser) {
                  onComplete(usersRepository.doesUserExist(createUser.username, createUser.email)) {
                    case Success(error: Option[APIError]) => {
                      error match {
                        case Some(x: APIError) => {
                          complete(HttpResponse(x.status, entity = HttpEntity(ContentTypes.`application/json`,
                            s"""{"message": "${x.msg}"}""")))
                        }
                        case None => complete(usersRepository.registerUser(createUser))
                      }
                    }
                  }
                }
              }
            }
          )
        },
        path(Segment) { id =>
          concat(
            get {
              onComplete(usersRepository.checkUserById(id)) {
                case Success(error: Option[APIError]) => {
                  error match {
                    case Some(x: APIError) => {
                      complete(HttpResponse(x.status, entity = HttpEntity(ContentTypes.`application/json`,
                        s"""{"message": "${x.msg}"}""")))
                    }
                    case None => complete(usersRepository.getUser(id))
                  }
                }
              }
            },
            put {
              onComplete(usersRepository.checkUserById(id)) {
                case Success(error: Option[APIError]) => {
                  error match {
                    case Some(x: APIError) => {
                      complete(HttpResponse(x.status, entity = HttpEntity(ContentTypes.`application/json`,
                        s"""{"message": "${x.msg}"}""")))
                    }
                    case None => entity(as[UpdateUser]) { updateUser =>
                      validateWith(UpdateUserValidator)(updateUser) {
                        onComplete(usersRepository.doesUserExist(updateUser.username.getOrElse(""), updateUser.email.getOrElse(""))) {
                          case Success(error: Option[APIError]) => {
                            error match {
                              case Some(x: APIError) => {
                                complete(HttpResponse(x.status, entity = HttpEntity(ContentTypes.`application/json`,
                                  s"""{"message": "${x.msg}"}""")))
                              }
                              case None => complete(usersRepository.updateUser(id, updateUser))
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            },
            delete {
              onComplete(usersRepository.checkUserById(id)) { //Need to wrap not found logic into function
                case Success(error: Option[APIError]) => {
                  error match {
                    case Some(x: APIError) => {
                      complete(HttpResponse(x.status, entity = HttpEntity(ContentTypes.`application/json`,
                        s"""{"message": "${x.msg}"}""")))
                    }
                    case None => complete(usersRepository.deleteUser(id))
                  }
                }
              }
            }
          )
        }
      )
    }
  }
  def posts = {
    pathPrefix("posts") {
      concat(
        pathEndOrSingleSlash {
          concat(
            get {
              complete(postRepository.allPosts())
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
              put {
                entity(as[UpdatePost]) { updatedPost =>
                  complete(postRepository.updatePost(id, updatedPost))
                }
              },
              delete {
                complete(postRepository.deletePost(id))
              },
          )
        },
        path(Segment/"like"){ id =>
          concat(
              post {
                entity(as[CreateLike]) { createLike =>
                  complete(postRepository.likePost(id, createLike.user_id))
                }
              }
          )
        },
        path(Segment/"dislike"){id =>
          concat(
            post {
              entity(as[CreateLike]) { createLike =>
                complete(postRepository.dislikePost(id, createLike.user_id))
              }
            }
          )
        }
      )
    }
  }
  def likes = {
    pathPrefix("likes"){
      concat(
        get{
          complete(postRepository.allLikes())
        }
      )
    }
  }


  override def route = {
    concat(
      users,
      posts,
      likes
    )
  }
}




