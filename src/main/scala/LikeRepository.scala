import akka.http.scaladsl.model.DateTime

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

trait LikeRepository {
  def all(): Future[Seq[Like]]
}

class InMemoryLikeRepository(initial:Seq[Like] = Seq.empty)(implicit ex: ExecutionContext) extends LikeRepository{
  private var likes: Vector[Like] = initial.toVector
  override def all(): Future[Seq[Like]] = Future.successful(likes)

}