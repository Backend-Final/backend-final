import java.util.UUID

import scala.concurrent.{ExecutionContext, Future}

trait LikeRepository {
  def all(): Future[Seq[Like]]

//  def toLikePost (id:String, post:Post): Future[Unit]
  //
  //  def getLike(id:String): Future[Option[Like]]
  //
  //  def updateLike(id: String, like: UpdateLike): Future[Option[Like]]
  //
  //  def deleteLike(id: String): Future[untitledOption[Like]]

}

class InMemoryLikeRepository(initial:Seq[Like] = Seq.empty)(implicit ex: ExecutionContext) extends LikeRepository{
  private var likes: Vector[Like] = initial.toVector

  override def all(): Future[Seq[Like]] = Future.successful(likes)


//  override def toLikePost(id:String): Future[Option[Post]] = Future.successful{
//    val updatedPost = likes.find(post => post.post_id == id)
//    updatedPost match {
//      case Some(x: Post) =>
//        var postToUpdate = x
//        postToUpdate = postToUpdate.copy(like_count = x.like_count + 1)
//        postToUpdate = postToUpdate.copy(like_count = data.like_count.getOrElse(x.like_count))
//
//        posts = posts.map(post => {
//          if (post.id == postToUpdate.id) {
//            postToUpdate
//          } else {
//            post
//          }
//        })
//        updatedPost
//      case None => updatedPost
//
//  }
//
}