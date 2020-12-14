package actors

import java.util.UUID

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.http.scaladsl.model.{DateTime, StatusCodes}
import http.APIError
import models.{Post, Like, CreatePostModel, UpdatePostModel}
object PostManager {
  sealed trait Command

  final case class GetPostList(replyTo: ActorRef[Seq[Post]]) extends Command
  final case class GetFilteredPostList(userId: String, replyTo: ActorRef[Seq[Post]]) extends Command
  final case class GetPost(postId: String, replyTo: ActorRef[Post]) extends Command
  final case class CreatePost(post: CreatePostModel, replyTo: ActorRef[Post]) extends Command
  final case class UpdatePost(postId: String, post: UpdatePostModel, replyTo: ActorRef[Post]) extends Command
  final case class DeletePost(postId: String, replyTo: ActorRef[ActionPerformedPost]) extends Command
  final case class CheckPostById(postId: String, replyTo: ActorRef[Option[APIError]]) extends Command

  final case class LikePost(postId:String, userId:String, replyTo: ActorRef[Post]) extends Command
  final case class DislikePost(postId:String, replyTo:ActorRef[Post]) extends Command
  final case class GetLikeList(replyTo:ActorRef[Seq[Like]]) extends Command

  final case class ActionPerformedPost(message: String)

  var posts: Seq[Post] = Seq()
  var likes: Seq[Like] = Seq()

  private def postManager(): Behavior[Command] =
    Behaviors.receiveMessagePartial {
      case GetPostList(replyTo) =>
        replyTo ! posts
        Behaviors.same
      case GetPost(postId, replyTo) =>
        val post = posts.find(post => post.id == postId)
        post match {
          case Some(x) =>
            replyTo ! x
        }
        Behaviors.same
      case CreatePost(data, replyTo) =>
        val post = Post(
          id = UUID.randomUUID().toString,
          title = data.title,
          content = data.content,
          like_count = 0,
          user_id = data.user_id
        )
        posts = posts:+ post
        replyTo ! post
        Behaviors.same

      case UpdatePost(postId, data, replyTo) =>
        val updatedPost = posts.find(post => post.id == postId)
        updatedPost match {
          case Some(x: Post) =>
            var postToUpdate = x
            postToUpdate = postToUpdate.copy(title = data.title.getOrElse(x.title))
            posts = posts.map(post => {
              if (post.id == postToUpdate.id) {
                postToUpdate
              } else {
                post
              }
            })
            replyTo ! postToUpdate
        }
        Behaviors.same


      case DeletePost(postId, replyTo) =>
        val deletedPost = posts.find(post => post.id == postId)
        posts = posts.filter(post => post.id != postId)
        deletedPost match {
          case Some(_) =>
            replyTo ! ActionPerformedPost(s"post with id ${postId} deleted successfully")
        }
        Behaviors.same

      case CheckPostById(postId, replyTo) =>
        val post = posts.find(post => post.id == postId)
        post match {
          case Some(x) =>
            replyTo ! None
          case None =>
            replyTo ! Some(APIError(status = StatusCodes.NotFound, msg = s"Post with id=${postId} does not exist in Database"))
        }
        Behaviors.same

      case LikePost(postId,userId, replyTo) =>
        val post = posts.find(post=> post.id == postId)
        val newLike = Like(
          id = UUID.randomUUID().toString,
          post_id = postId,
          user_id = userId,
          time = DateTime.now
        )
        likes = likes:+ newLike
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
            replyTo ! toLikePost
        }
        Behaviors.same

      case DislikePost(postId, replyTo) =>
        val post = posts.find(post=> post.id == postId)
        likes = likes.filter(like=>like.post_id!=postId)
        post match {
          case Some(x:Post) =>
            var toDisLikePost = x
            toDisLikePost = toDisLikePost.copy(like_count = x.like_count-1)
            posts = posts.map(post => {
              if (post.id == toDisLikePost.id) {
                toDisLikePost
              } else {
                post
              }
            })
            replyTo ! toDisLikePost
        }
        Behaviors.same
      case GetLikeList(replyTo)=>
        replyTo ! likes
        Behaviors.same
    }

  def apply(): Behavior[Command] = postManager()
}
