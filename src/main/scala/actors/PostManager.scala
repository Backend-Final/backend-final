package actors

import java.util.UUID

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.http.scaladsl.model.StatusCodes
import http.APIError
import models.{Post, CreatePostModel, UpdatePostModel}
object PostManager {
  sealed trait Command

  final case class GetPostList(replyTo: ActorRef[Seq[Post]]) extends Command
  final case class GetUsersPost(userId: String, replyTo: ActorRef[Seq[Post]]) extends Command
  final case class GetFilteredPostList(userId: String, replyTo: ActorRef[Seq[Post]]) extends Command
  final case class GetPost(postId: String, replyTo: ActorRef[Post]) extends Command
  final case class CreatePost(post: CreatePostModel, replyTo: ActorRef[Post]) extends Command
  final case class UpdatePost(postId: String, post: UpdatePostModel, replyTo: ActorRef[Post]) extends Command
  final case class DeletePost(postId: String, replyTo: ActorRef[ActionPerformedPost]) extends Command
  final case class CheckPostById(postId: String, replyTo: ActorRef[Option[APIError]]) extends Command
  final case class IncrementLikeCount(postId: String, replyTo: ActorRef[ActionPerformedPost]) extends Command
  final case class DecrementLikeCount(postId: String, replyTo: ActorRef[ActionPerformedPost]) extends Command

  final case class ActionPerformedPost(message: String)

  var posts: Seq[Post] = Seq()

  private def postManager(): Behavior[Command] =
    Behaviors.receiveMessagePartial {
      case GetPostList(replyTo) =>
        replyTo ! posts
        Behaviors.same
      case GetUsersPost(userId, replyTo) =>
        replyTo ! posts.filter(post => post.user_id == userId)
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
            postToUpdate = postToUpdate.copy(content = data.content.getOrElse(x.content))
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

      case IncrementLikeCount(postId, replyTo) =>
        val post = posts.find(post => post.id == postId)
        post match {
          case Some(x: Post) =>
            val postToUpdate = x.copy(like_count = x.like_count + 1)
            posts = posts.map(post => {
              if (post.id == postToUpdate.id) {
                postToUpdate
              } else {
                post
              }
            })
            replyTo ! ActionPerformedPost(message = s"Post with id=${postId} is liked")
            Behaviors.same
        }

      case DecrementLikeCount(postId, replyTo) =>
        val post = posts.find(post => post.id == postId)
        post match {
          case Some(x: Post) =>
            var like_count = x.like_count;
            if (like_count > 0) {
              like_count = like_count - 1;
            }
            val postToUpdate = x.copy(like_count = like_count)
            posts = posts.map(post => {
              if (post.id == postToUpdate.id) {
                postToUpdate
              } else {
                post
              }
            })
            replyTo ! ActionPerformedPost(message = s"Post with id=${postId} is disliked")
            Behaviors.same
        }
    }

  def apply(): Behavior[Command] = postManager()
}
