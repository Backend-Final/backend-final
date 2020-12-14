import actors.UserManager
import actors.UserManager._
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.util.Timeout
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.generic.auto._

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._
import akka.http.scaladsl.server.{Directives, Route}
import models.{User, CreateUserModel, UpdateUserModel}
import http.{APIError, CreateUserValidator, UpdateUserValidator, ValidatorDirectives}
trait Router {
  def route: Route
}

class TwitterRouter(userManagerActor: ActorRef[UserManager.Command])(implicit val system: ActorSystem[_]) extends  Router
  with  Directives
  with ValidatorDirectives  {

  private implicit val timeout = Timeout(20.seconds)

  def getUserList(): Future[Seq[models.User]] = userManagerActor.ask(GetUserList)
  def getUser(userId: String): Future[models.User] = userManagerActor.ask(GetUser(userId, _))
  def createUser(user: CreateUserModel): Future[models.User] = userManagerActor.ask(CreateUser(user, _))
  def updateUser(userId: String, user: UpdateUserModel): Future[models.User] = userManagerActor.ask(UpdateUser(userId, user, _))
  def deleteUser(userId: String): Future[ActionPerformed] = userManagerActor.ask(DeleteUser(userId, _))
  def checkUserExistenceById(userId: String): Future[Option[APIError]] = userManagerActor.ask(CheckUserById(userId, _))

  val usersRoutes: Route = pathPrefix("users") {
    concat(
      pathEndOrSingleSlash {
        concat(
          get {
            complete(getUserList())
          },
          post {
            entity(as[CreateUserModel]) { user =>
              validateWith(CreateUserValidator)(user) {
                complete(createUser(user))
              }
            }
          }
        )
      },
      path(Segment) { id =>
        concat(
          get {
            onComplete(checkUserExistenceById(id)) {
              case Success(error: Option[APIError]) => {
                error match {
                  case Some(x: APIError) =>
                    complete(HttpResponse(x.status, entity = HttpEntity(ContentTypes.`application/json`,
                      s"""{"message": "${x.msg}"}""")))
                  case None => complete(getUser(id))
                }
              }
            }
          },
          put {
            entity(as[UpdateUserModel]) { user =>
              onComplete(checkUserExistenceById(id)) {
                case Success(error: Option[APIError]) => {
                  error match {
                    case Some(x: APIError) =>
                      complete(HttpResponse(x.status, entity = HttpEntity(ContentTypes.`application/json`,
                        s"""{"message": "${x.msg}"}""")))
                    case None => complete(updateUser(id, user))
                  }
                }
              }
//              validateWith(UpdateUserValidator)(user) {
//                onComplete(checkUserExistenceById(id)) {
//                  case Success(error: Option[APIError]) => {
//                    error match {
//                      case Some(x: APIError) =>
//                        complete(HttpResponse(x.status, entity = HttpEntity(ContentTypes.`application/json`,
//                          s"""{"message": "${x.msg}"}""")))
//                      case None => complete(updateUser(id, user))
//                    }
//                  }
//                }
//              }
            }
          },
          delete {
            onComplete(checkUserExistenceById(id)) {
              case Success(error: Option[APIError]) => {
                error match {
                  case Some(x: APIError) =>
                    complete(HttpResponse(x.status, entity = HttpEntity(ContentTypes.`application/json`,
                      s"""{"message": "${x.msg}"}""")))
                  case None => complete(deleteUser(id))
                }
              }
            }
          }
        )
      }
    )
  }

  override def route = {
    concat(
      usersRoutes,
    )
  }
}
