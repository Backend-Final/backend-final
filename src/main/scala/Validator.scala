import akka.http.scaladsl.model.StatusCodes

trait Validator[T] {
  def validate(t: T): Option[APIError]
}

object CreateUserValidator extends Validator[CreateUser] {
  private val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

  def validate(createUser: CreateUser): Option[APIError] =
    if (createUser.username.isEmpty) {
      Some(APIError(status = StatusCodes.BadRequest, msg = "username is required"))
    }
    else if (!emailRegex.findFirstMatchIn(createUser.email).isDefined) {
      Some(APIError(status = StatusCodes.BadRequest, msg = "email is not valid"))
    } else if(createUser.password.isEmpty) {
      Some(APIError(status = StatusCodes.BadRequest, msg = "password is required"))
    }
    else if(createUser.password.length < 8) {
      Some(APIError(status = StatusCodes.BadRequest, msg = "password has to contain 8 or more characters"))
    }
    else if(createUser.name.isEmpty) {
      Some(APIError(status = StatusCodes.BadRequest, msg = "name is required"))
    }
    else if(createUser.surname.isEmpty) {
      Some(APIError(status = StatusCodes.BadRequest, msg = "surname is required"))
    }
    else if(createUser.age <= 0) {
      Some(APIError(status = StatusCodes.BadRequest, msg = "age has to be a positive integer"))
    }
    else
      None
}

object UpdateUserValidator extends Validator[UpdateUser] { //FIX ME: Validator fails when a field is None
  private val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

  def validate(updateUser: UpdateUser): Option[APIError] = {
    updateUser.username match {
      case Some(x: String) => {
        if (updateUser.username.isEmpty) {
          Some(APIError(status = StatusCodes.BadRequest, msg = "username is required"))
        }
      }
    }

    updateUser.email match {
      case Some(x: String) => {
        if (updateUser.email.isEmpty) {
          Some(APIError(status = StatusCodes.BadRequest, msg = "email is required"))
        } else if (!emailRegex.findFirstMatchIn(x).isDefined) {
          Some(APIError(status = StatusCodes.BadRequest, msg = "email is not valid"))
        }
      }
    }

    updateUser.password match {
      case Some(x: String) => {
        if (x.isEmpty) {
          Some(APIError(status = StatusCodes.BadRequest, msg = "password is required"))
        } else if(x.length < 8) {
          Some(APIError(status = StatusCodes.BadRequest, msg = "password has to contain 8 or more characters"))
        }
      }
    }

    updateUser.name match {
      case Some(x: String) => {
        if (x.isEmpty) {
          Some(APIError(status = StatusCodes.BadRequest, msg = "name is required"))
        }
      }
    }

    updateUser.surname match {
      case Some(x: String) => {
        if (x.isEmpty) {
          Some(APIError(status = StatusCodes.BadRequest, msg = "surname is required"))
        }
      }
    }

    updateUser.age match {
      case Some(x: Int) => {
        if (x <= 0) {
          Some(APIError(status = StatusCodes.BadRequest, msg = "age has to be a positive integer"))
        }
      }
    }
    None
  }
}