import akka.http.scaladsl.model.DateTime

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
  def likePost(id:String): Future[Option[Post]]
  def dislikePost(id:String):Future[Option[Post]]

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
        postToUpdate = postToUpdate.copy(like_count = data.like_count.getOrElse(x.like_count))

        posts = posts.map(post => {
          if (post.id == postToUpdate.id) {
            postToUpdate
          } else {
            post
          }
        })
        updatedPost
      case None => updatedPost
    }

  }


  override def deletePost(id: String): Future[Option[Post]] = Future.successful {
    val deletedPost = posts.find(_.id == id)
    posts = posts.filter(post=>post.id!=id)
    deletedPost
  }
  override def likePost(id: String): Future[Option[Post]] = Future.successful{
    val post = posts.find(post => post.id == id)

    val newLike = Like(
      id = UUID.randomUUID().toString,
      post_id = post.getOrElse(id).toString,
      user_id = UUID.randomUUID().toString, // исправить на норм user_id
      time = DateTime.now
    )
    likes = likes:+newLike

    post match {
      case Some(x:Post) =>
        var toLikePost = x
        toLikePost = toLikePost.copy(like_count = x.like_count+1)
        posts = posts.map(post => {
          if (post.id == toLikePost.id) {
            toLikePost
          } else {
            post
          }
        })
        post
      case None => post
    }
  }

  override def dislikePost(id: String): Future[Option[Post]] = Future.successful{
    val post = posts.find(post => post.id == id)

    val like = likes.find(like=>like.post_id == id)
    likes = likes.filter(like=>like.post_id!=id)

    post match {
      case Some(x:Post) =>
        var toLikePost = x
        toLikePost = toLikePost.copy(like_count = x.like_count-1)
        posts = posts.map(post => {
          if (post.id == toLikePost.id) {
            toLikePost
          } else {
            post
          }
        })
        post
      case None => post
    }

  }

}
