package actors

import java.util.UUID

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.http.scaladsl.model.StatusCodes
import http.APIError
import models.Like
object LikeManager {
  sealed trait Command

  final case class GetLikes(replyTo: ActorRef[Seq[Like]]) extends Command
  final case class GetPostsLikes(postId: String, replyTo: ActorRef[Seq[Like]]) extends Command
  final case class CreateLike(postId: String, userId: String, replyTo: ActorRef[Like]) extends Command
  final case class DeleteLike(likeId: String, replyTo: ActorRef[Like]) extends Command
  final case class CheckLikeById(likeId: String, replyTo: ActorRef[Option[APIError]]) extends Command

  final case class LikeActionPerformed(message: String)

  var likes: Seq[Like] = Seq()

  private def likeManager(): Behavior[Command] =
    Behaviors.receiveMessagePartial {
      case GetLikes(replyTo) =>
        replyTo ! likes
        Behaviors.same
      case GetPostsLikes(postId, replyTo) =>
        replyTo ! likes.filter(like => like.post_id == postId)
        Behaviors.same
      case CreateLike(postId, userId, replyTo) =>
        val like = Like(post_id = postId, user_id = userId, id = UUID.randomUUID().toString)
        likes = likes :+ like
        replyTo ! like
        Behaviors.same

      case DeleteLike(likeId, replyTo) =>
        val like = likes.find(like => like.id == likeId)
        likes = likes.filter(like => like.id != likeId)
        like match {
          case Some(x) =>
            replyTo ! x
        }
        Behaviors.same
      case CheckLikeById(likeId, replyTo) =>
        val like = likes.find(like => like.id == likeId)
        like match {
          case Some(_) =>
            replyTo ! None
          case None =>
            replyTo ! Some(APIError(StatusCodes.NotFound, msg = s"Like with id=$likeId is not found"))
        }
        Behaviors.same
    }

  def apply(): Behavior[Command] = likeManager()
}
