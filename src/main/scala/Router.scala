import actors.UserManager
import actors.UserManager._
import actors.PostManager
import actors.PostManager._
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
import models.{CreatePostModel, CreateUserModel, UpdatePostModel, UpdateUserModel, User}
import http.{APIError, CreatePostValidator, CreateUserValidator, UpdateUserValidator, ValidatorDirectives}
trait Router {
  def route: Route
}

class TwitterRouter(userManagerActor: ActorRef[UserManager.Command], postManagerActor: ActorRef[PostManager.Command])(implicit val system: ActorSystem[_]) extends  Router
  with  Directives
  with ValidatorDirectives  {

  private implicit val timeout = Timeout(20.seconds)

  def getUserList(): Future[Seq[models.User]] = userManagerActor.ask(GetUserList)
  def getUser(userId: String): Future[models.User] = userManagerActor.ask(GetUser(userId, _))
  def createUser(user: CreateUserModel): Future[models.User] = userManagerActor.ask(CreateUser(user, _))
  def updateUser(userId: String, user: UpdateUserModel): Future[models.User] = userManagerActor.ask(UpdateUser(userId, user, _))
  def deleteUser(userId: String): Future[ActionPerformedUser] = userManagerActor.ask(DeleteUser(userId, _))
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


  def getPostList(): Future[Seq[models.Post]] = postManagerActor.ask(GetPostList)
  def getPost(postId:String): Future[models.Post] = postManagerActor.ask(GetPost(postId, _))
  def createPost(post:CreatePostModel):Future[models.Post] = postManagerActor.ask(CreatePost(post, _))
  def updatePost(postId:String, post:UpdatePostModel): Future[models.Post] = postManagerActor.ask(UpdatePost(postId, post, _))
  def deletePost(postId:String): Future[ActionPerformedPost] = postManagerActor.ask(DeletePost(postId,_))
  def checkPostExistenceById(postId:String): Future[Option[APIError]] = postManagerActor.ask(CheckPostById(postId, _))

  def likePost(postId:String, userId:String): Future[models.Post] = postManagerActor.ask(LikePost(postId,userId, _))
  def disLikePost(postId:String): Future[models.Post] = postManagerActor.ask(DislikePost(postId, _))
  def getLikeList():Future[Seq[models.Like]] = postManagerActor.ask(GetLikeList)

  val postsRoutes: Route = pathPrefix("posts") {
    concat(
      pathEndOrSingleSlash {
        concat(
          get {
            complete(getPostList())
          },
          post {
            entity(as[CreatePostModel]) { post =>
              validateWith(CreatePostValidator)(post) {
                complete(createPost(post))
              }
            }
          }
        )
      },
      path(Segment) { id =>
        concat(
          get {
            onComplete(checkPostExistenceById(id)) {
              case Success(error: Option[APIError]) => {
                error match {
                  case Some(x: APIError) =>
                    complete(HttpResponse(x.status, entity = HttpEntity(ContentTypes.`application/json`,
                      s"""{"message": "${x.msg}"}""")))
                  case None => complete(getPost(id))
                }
              }
            }
          },
          put {
            entity(as[UpdatePostModel]) { post =>
              onComplete(checkPostExistenceById(id)) {
                case Success(error: Option[APIError]) => {
                  error match {
                    case Some(x: APIError) =>
                      complete(HttpResponse(x.status, entity = HttpEntity(ContentTypes.`application/json`,
                        s"""{"message": "${x.msg}"}""")))
                    case None => complete(updatePost(id, post))
                  }
                }
              }
            }
          },
          delete {
            onComplete(checkPostExistenceById(id)) {
              case Success(error: Option[APIError]) => {
                error match {
                  case Some(x: APIError) =>
                    complete(HttpResponse(x.status, entity = HttpEntity(ContentTypes.`application/json`,
                      s"""{"message": "${x.msg}"}""")))
                  case None => complete(deletePost(id))
                }
              }
            }
          }
        )
      },
      path(Segment/"like") { id =>
        concat(
          put {
            entity(as[models.CreateLike]){like =>
              onComplete(checkPostExistenceById(id)) {
                case Success(error: Option[APIError]) => {
                  error match {
                    case Some(x: APIError) =>
                      complete(HttpResponse(x.status, entity = HttpEntity(ContentTypes.`application/json`,
                        s"""{"message": "${x.msg}"}""")))
                    case None => complete(likePost(id,like.user_id))
                  }
                }
              }
            }

          }
        )
      },
      path(Segment/"dislike"){id=>
        concat(
          put{
            onComplete(checkPostExistenceById(id)) {
              case Success(error: Option[APIError]) => {
                error match {
                  case Some(x: APIError) =>
                    complete(HttpResponse(x.status, entity = HttpEntity(ContentTypes.`application/json`,
                      s"""{"message": "${x.msg}"}""")))
                  case None => complete(disLikePost(id))
                }
              }
            }
          }
        )
      }
    )
  }
  val likesRoutes: Route = pathPrefix("likes") {
    concat(
      pathEndOrSingleSlash {
        concat(
          get {
            complete(getLikeList())
          }
        )
      }
    )
  }


  override def route = {
    concat(
      usersRoutes,
      postsRoutes,
      likesRoutes
    )
  }
}
