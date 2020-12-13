import akka.http.scaladsl.model.{DateTime, StatusCodes}

import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

trait PostRepository {
  def allPosts(): Future[Seq[Post]]
  def allLikes():Future[Seq[Like]]
  def createPost (post: CreatePost): Future[Post]
  def getPost(id:String): Future[Option[Post]]
  def updatePost(id: String, post: UpdatePost): Future[Option[Post]]
  def deletePost(id: String): Future[Option[Post]]
  def likePost(post_id:String, user_id: String): Future[Option[Post]]
  def dislikePost(post_id:String, user_id: String):Future[Option[Post]]
  def checkPostNotExist(title:String): Future[Option[APIError]]
  def checkPostExist(title:String): Future[Option[APIError]]

}

class InMemoryPostRepository(initial1:Seq[Post] = Seq.empty, initial2:Seq[Like]=Seq.empty)(implicit ex: ExecutionContext) extends PostRepository {
  private var posts: Vector[Post] = initial1.toVector
  private var likes: Vector[Like] = initial2.toVector


  override def allPosts(): Future[Seq[Post]] = Future.successful(posts)
  override def allLikes(): Future[Seq[Like]] = Future.successful(likes)

  override def createPost(data: CreatePost): Future[Post] = Future.successful {

    val post = Post(
      id = UUID.randomUUID().toString,
      title = data.title,
      content = data.content,
      like_count = 0,
      user_id = data.user_id
    )
    posts = posts :+ post
    post
  }

  override def getPost(id: String): Future[Option[Post]] = Future.successful {
    posts.find(post => post.id == id)
  }

  override def updatePost(id: String, data: UpdatePost): Future[Option[Post]] = Future.successful {
    val updatedPost = posts.find(post => post.id == id)
    updatedPost match {
      case Some(x: Post) =>
        var postToUpdate = x
        postToUpdate = postToUpdate.copy(title = data.title.getOrElse(x.title))
        postToUpdate = postToUpdate.copy(content = data.content.getOrElse(x.content))

        posts = posts.map(post => {
          if (post.id == postToUpdate.id) {
            postToUpdate
          } else {
            post
          }
        })
        Some(postToUpdate)
      case None => updatedPost
    }
  }

  override def deletePost(id: String): Future[Option[Post]] = Future.successful {
    val deletedPost = posts.find(_.id == id)
    posts = posts.filter(post=>post.id!=id)
    deletedPost
  }

  override def likePost(post_id: String, user_id: String): Future[Option[Post]] = Future.successful{
    val post = posts.find(post => post.id == post_id)

    post match {
      case Some(x:Post) =>
        var toLikePost = x
        toLikePost = toLikePost.copy(like_count = x.like_count + 1)
        val newLike = Like(
          id = UUID.randomUUID().toString,
          post_id = toLikePost.id,
          user_id = user_id,
          time = DateTime.now
        )
        likes = likes :+ newLike
        posts = posts.map(post => {
          if (post.id == toLikePost.id) {
            toLikePost
          } else {
            post
          }
        })
        Some(toLikePost)
      case None => post
    }
  }

  override def dislikePost(post_id: String, user_id: String): Future[Option[Post]] = Future.successful{
    val post = posts.find(post => post.id == post_id)

    val like = likes.find(like => like.post_id == post_id)
    likes = likes.filter(like => like.post_id != post_id)

    post match {
      case Some(x:Post) =>
        var toLikePost = x
        if (toLikePost.like_count > 0) {
          toLikePost = toLikePost.copy(like_count = x.like_count - 1)
        }
        posts = posts.map(post => {
          if (post.id == toLikePost.id) {
            toLikePost
          } else {
            post
          }
        })
        Some(toLikePost)
      case None => post
    }
  }

  override def checkPostNotExist(id: String): Future[Option[APIError]] = Future.successful{
    val post = posts.find(post=>post.id == id)
    post match {
      case Some(x:Post)=>None
      case None => Some(APIError(status = StatusCodes.NotFound, msg = s"Such post with " +
        s"id:${id} does not exist in the database"))
    }
  }
  override def checkPostExist(title: String): Future[Option[APIError]] = Future.successful{
    val post = posts.find(post=>post.title == title)
    post match {
      case Some(x:Post)=>Some(APIError(status = StatusCodes.Conflict, msg = s"Such post with " +
        s"title:${title} already exist in the database"))
      case None => None
    }
  }


}
