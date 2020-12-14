package models

import akka.http.scaladsl.model.DateTime

case class Like(id: String, post_id: String, user_id: String, time:DateTime)
case class CreateLike(user_id:String)