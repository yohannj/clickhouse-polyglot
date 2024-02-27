package com.amendil

import scala.concurrent.{ExecutionContext, Future}

object ConcurrencyUtils {

  def executeInParallelOnlySuccess[T, U](elements: Seq[T], fn: (T) => Future[U])(
      implicit ec: ExecutionContext
  ): Future[Seq[U]] =
    val futures = elements.map { el =>
      fn(el).map[Option[U]](Some(_)).recover(_ => None)
    }
    Future.sequence(futures).map(_.flatten)

  def executeInSequence[T, U](elements: Seq[T], fn: (T) => Future[U])(implicit ec: ExecutionContext): Future[Seq[U]] =
    elements match
      case head :: tail => fn(head).flatMap(res => executeInSequence(tail, fn).map(l => l :+ res))
      case _            => Future.successful(Seq.empty)

  def executeUntilSuccess[T](elements: Seq[T], fn: (T) => Future[_])(implicit ec: ExecutionContext): Future[Boolean] =
    elements match
      case head :: tail => fn(head).map(_ => true).recoverWith(_ => executeUntilSuccess(tail, fn))
      case _            => Future.successful(false)

}
