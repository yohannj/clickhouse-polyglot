package com.amendil

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object ConcurrencyUtils {

  def executeInSequence[T, U](elements: Seq[T], fn: (T) => Future[U])(implicit ec: ExecutionContext): Future[Seq[U]] =
    elements match
      case head :: tail => fn(head).flatMap(res => executeInSequence(tail, fn).map(l => l :+ res))
      case _            => Future.successful(Seq.empty)

}
