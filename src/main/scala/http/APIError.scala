package http

import akka.http.scaladsl.model.StatusCode
import akka.http.scaladsl.model.StatusCodes

case class APIError(status: StatusCode, msg: String);
