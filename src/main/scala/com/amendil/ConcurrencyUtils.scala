package com.amendil

import scala.concurrent.{ExecutionContext, Future}

object ConcurrencyUtils {

  /**
    * Execute all calls in parallel, with the ability to partition queries to handle a max concurrency.
    * Results of failing queries are skipped.
    *
    * @param maxConcurrency if 0, execute all calls in parallel
    */
  def executeInParallelOnlySuccess[T, U](elements: Seq[T], fn: (T) => Future[U], maxConcurrency: Int)(
      implicit ec: ExecutionContext
  ): Future[Seq[U]] =
    val futures = elements
      .grouped(maxConcurrency)
      .toSeq
      .map { subElements =>
        executeInSequence(
          subElements,
          el => fn(el).map[Option[U]](Some(_)).recover(_ => None)
        )
      }

    Future.sequence(futures).map(_.flatten.flatten)

  /**
    * Execute all calls in parallel.
    * Results of failing queries are skipped.
    */
  def executeInParallelOnlySuccess[T, U](elements: Seq[T], fn: (T) => Future[U])(
      implicit ec: ExecutionContext
  ): Future[Seq[U]] =
    val futures = elements.map { el =>
      fn(el).map[Option[U]](Some(_)).recover(_ => None)
    }
    Future.sequence(futures).map(_.flatten)

  def executeInSequence[T, U](elements: Seq[T], fn: (T) => Future[U])(implicit ec: ExecutionContext): Future[Seq[U]] =
    elements match
      case Seq(head, tail @ _*) => fn(head).flatMap(res => executeInSequence(tail, fn).map(l => l :+ res))
      case _                    => Future.successful(Seq.empty)

  def executeInSequenceUntilSuccess[T](elements: Seq[T], fn: (T) => Future[_])(
      implicit ec: ExecutionContext
  ): Future[Boolean] =
    elements match
      case Seq(head, tail @ _*) => fn(head).map(_ => true).recoverWith(_ => executeInSequenceUntilSuccess(tail, fn))
      case _                    => Future.successful(false)

}
