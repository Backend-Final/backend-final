package actors

import java.util.UUID

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.http.scaladsl.model.{DateTime, StatusCodes}
import http.APIError
import models.{AbstractUser, CreateUserModel, UpdateUserModel, User}
object UserManager {
  sealed trait Command

  final case class GetUserList(replyTo: ActorRef[Seq[User]]) extends Command
  final case class GetUser(userId: String, replyTo: ActorRef[User]) extends Command
  final case class CreateUser(user: CreateUserModel, replyTo: ActorRef[User]) extends Command
  final case class UpdateUser(userId: String, user: UpdateUserModel, replyTo: ActorRef[User]) extends Command
  final case class DeleteUser(userId: String, replyTo: ActorRef[ActionPerformed]) extends Command
  final case class CheckUserById(userId: String, replyTo: ActorRef[Option[APIError]]) extends Command

  final case class ActionPerformed(message: String)
  final case class UserList(users: Seq[User])

  var users: Seq[User] = Seq(
    User("1", "asuleiman", "asuleyman2403@gmail.com", "test1234", "Assyl", "Suleiman", 20)
  )

  private def userManager(): Behavior[Command] =
    Behaviors.receiveMessagePartial {
      case GetUserList(replyTo) =>
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
