package models

case class Post(id: String, title: String, content: String, like_count: Int, user_id: String)
case class CreatePostModel(title:String, content:String, user_id: String)
case class UpdatePostModel(title:Option[String], content:Option[String])
