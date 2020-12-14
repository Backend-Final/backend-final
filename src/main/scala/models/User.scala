package models

sealed trait AbstractUser
case class User(id: String, username: String, email: String, password: String, name: String, surname: String, age: Int) extends AbstractUser
case class CreateUserModel(username: String, email: String, password: String, name: String, surname: String, age: Int) extends AbstractUser
case class UpdateUserModel(username: Option[String], email: Option[String], password: Option[String], name: Option[String],
                      surname: Option[String], age: Option[Int]) extends AbstractUser