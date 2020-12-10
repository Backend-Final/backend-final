import java.util.UUID
import akka.http.scaladsl.model.StatusCode
import akka.http.scaladsl.model.StatusCodes
import scala.concurrent.{ExecutionContext, Future}

trait UserRepository {
  def all(): Future[Seq[User]]

  def registerUser(user: CreateUser): Future[User]

  def getUser(id: String): Future[Option[User]]

  def updateUser(id: String, user: UpdateUser): Future[Option[User]]

  def deleteUser(id: String): Future[Option[User]]

  def doesUserExist(username: String, email: String): Future[Option[APIError]]

  def checkUserById(id: String): Future[Option[APIError]]
}

class InMemoryUsersRepository(initial:Seq[User] = Seq.empty)(implicit  ex:ExecutionContext) extends UserRepository {
  private var users: Vector[User] = initial.toVector

  override def all(): Future[Seq[User]] = Future.successful(users)

  override def registerUser(data: CreateUser): Future[User] = Future.successful {
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
    user
  }

  override def getUser(id: String): Future[Option[User]] = Future.successful {
    users.find(user => user.id == id)
  }

  override def deleteUser(id: String): Future[Option[User]] = Future.successful {
    val deletedUser = users.find(user => user.id == id)
    users = users.filter(user => user.id != id)
    deletedUser
  }

  override def updateUser(id: String, data: UpdateUser): Future[Option[User]] = Future.successful {
    val updatedUser = users.find(user => user.id == id)
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
        Some(userToUpdate)
      case None => updatedUser
    }
  }

  override def doesUserExist(username: String, email: String): Future[Option[APIError]] = Future.successful {
    val user = users.find(user => user.username == username || user.email == email)

    user match {
      case Some(x: User) => Some(APIError(status = StatusCodes.Conflict, msg = s"Such user with " +
        s"username:${username} or email: ${email} is already in database"))
      case None => None
    }
  }

  override def checkUserById(id: String): Future[Option[APIError]] = Future.successful {
    val user = users.find(user => user.id == id)
    user match {
      case Some(x: User) => None
      case None => Some(APIError(status = StatusCodes.NotFound, msg = s"Such user with " +
        s"id:${id} does not exist in database"))
    }
  }
}