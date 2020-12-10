import akka.http.scaladsl.model.DateTime

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

trait LikeRepository {
  def all(): Future[Seq[Like]]
//  def createLike(data:CreateLike):Future[Like]
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

//  override def createLike(data:CreateLike): Future[Like]=Future.successful{
//    val like = Like(
//      id = UUID.randomUUID().toString,
//      post_id = data.post_id,
//      user_id = data.user_id,
//      time = DateTime.now
//    )
//    likes = likes:+like
//    like
//  }
//  val post = posts.find(post => post.id == id)
//  post match {
//    case Some(x:Post) =>
//      var toLikePost = x
//      toLikePost = toLikePost.copy(like_count = x.like_count+1)
//      posts = posts.map(post => {
//        if (post.id == toLikePost.id) {
//          toLikePost
//        } else {
//          post
//        }
//      })
//      post
//    case None => post
//  }

}