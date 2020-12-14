package actors

import java.util.UUID

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.http.scaladsl.model.{DateTime, StatusCodes}
import http.APIError
import models.{Post, CreatePostModel, UpdatePostModel}
object PostManager {
  sealed trait Command

  final case class GetPostList(replyTo: ActorRef[Seq[Post]]) extends Command
  final case class GetFilteredPostList(userId: String, replyTo: ActorRef[Seq[Post]]) extends Command
  final case class GetPost(postId: String, replyTo: ActorRef[Post]) extends Command
  final case class CreatePost(post: CreatePostModel, replyTo: ActorRef[Post]) extends Command
  final case class UpdatePost(postId: String, post: UpdatePostModel, replyTo: ActorRef[Post]) extends Command
  final case class DeletePost(postId: String, replyTo: ActorRef[ActionPerformed]) extends Command
  final case class CheckPostById(postId: String, replyTo: ActorRef[Option[APIError]]) extends Command

  final case class ActionPerformed(message: String)

  var posts: Seq[Post] = Seq()

  private def userManager(): Behavior[Command] =
    Behaviors.receiveMessagePartial {
      case GetPostList(replyTo) =>
        replyTo ! users
        Behaviors.same
      case GetUser(userId, replyTo) =>
        val user = users.find(user => user.id == userId)
        user match {
          case Some(x) =>
            replyTo ! x
        }
        Behaviors.same
      case CreateUser(data, replyTo) =>
        val user = User(
          id = UUID.randomUUID().toString,
          username = data.username,
          email = data.email,
          password = data.password,
          name = data.name,
          surname = data.surname,
          age = data.age
        )

        users = users :+ user
        replyTo ! user
        Behaviors.same
      case UpdateUser(userId, data, replyTo) =>
        val updatedUser = users.find(user => user.id == userId)
        updatedUser match {
          case Some(x: User) =>
            var userToUpdate = x
            userToUpdate = userToUpdate.copy(username = data.username.getOrElse(x.username))
            userToUpdate = userToUpdate.copy(email = data.email.getOrElse(x.email))
            userToUpdate = userToUpdate.copy(name = data.name.getOrElse(x.name))
            userToUpdate = userToUpdate.copy(surname = data.surname.getOrElse(x.surname))
            userToUpdate = userToUpdate.copy(password = data.password.getOrElse(x.password))
            userToUpdate = userToUpdate.copy(age = data.age.getOrElse(x.age))
            users = users.map(user => {
              if (user.id == userToUpdate.id) {
                userToUpdate
              } else {
                user
              }
            })
            replyTo ! userToUpdate
        }
        Behaviors.same
      case DeleteUser(userId, replyTo) =>
        val deletedUser = users.find(user => user.id == userId)
        users = users.filter(user => user.id != userId)
        deletedUser match {
          case Some(_) =>
            replyTo ! ActionPerformed(s"user with id ${userId} deleted successfully")
        }
        Behaviors.same
      case CheckUserById(userId, replyTo) =>
        val user = users.find(user => user.id == userId)
        user match {
          case Some(x) =>
            replyTo ! None
          case None =>
            replyTo ! Some(APIError(status = StatusCodes.NotFound, msg = s"User with id=${userId} does not exist in Database"))
        }
        Behaviors.same
    }


  def apply(): Behavior[Command] = userManager()
}
