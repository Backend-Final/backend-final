package models

case class Like(id: String, post_id: String, user_id: String)
case class CreateLike(user_id:String, post_id: String)
case class PostIdWrapper(post_id: String)