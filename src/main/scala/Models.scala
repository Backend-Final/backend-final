

case class User(id: String, username: String, email: String, password: String, name: String, surname: String, age: Int)
case class CreateUser(username: String, email: String, password: String, name: String, surname: String, age: Int)
case class UpdateUser(username: Option[String], email: Option[String], password: Option[String], name: Option[String],
                      surname: Option[String], age: Option[Int])


case class Post(id: String, title: String, content: String, like_count: Int, user_id: String)
case class CreatePost(title:String, content:String, user_id: String)
case class UpdatePost(title:Option[String], content:Option[String], like_count:Option[Int])
//case class Comment(id: String, post_id: String, user_id: String, content: String)
case class Like(id: String, post_id: String, user_id: String)
