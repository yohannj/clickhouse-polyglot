package com.amendil

import scala.concurrent.{ExecutionContext, Future}

object ConcurrencyUtils:

  /**
    * Take the element and call sequentially and in order the provided asychronous functions.
    *
    * The argument of the first function will be the provided element.
    * The argument of the following functions will be the result of the previous function.
    *
    * @return The result of the very last function, if no function has been provided, returns the provided element
    */
  def executeChain[T, U](element: T, fns: Seq[(T) => Future[T]])(implicit ec: ExecutionContext): Future[T] =
    fns match
      case Seq(fn, tail @ _*) => fn(element).flatMap(executeChain(_, tail))
      case _                  => Future.successful(element)

  /**
    * The maxConcurrency is handled by grouping the elements into maxConcurrency partition (or bucket).
    * Each partition execute the function on its group of elements sequentially.
    * Partitions are executed in parallel.
    *
    * Whenever a computation fails, all partitions stop and the Future.Failure generated by that computation is returned.
    *
    * The order of the elements is kept. This function can be seen like an asynchronous `map` operation.
    *
    * @return A sequence of the same size as the provided elements
    * @throws IllegalArgumentException when maxConcurrency is less than 1
    */
  def executeInParallel[T, U](elements: Seq[T], fn: (T) => Future[U], maxConcurrency: Int)(
      implicit ec: ExecutionContext
  ): Future[Seq[U]] =
    val partitionSize =
      if elements.size == 0 || maxConcurrency < 1 then 1
      else Math.ceil(elements.size.toFloat / maxConcurrency).toInt

    val futures = elements
      .grouped(partitionSize)
      .toSeq
      .map { (subElements: Seq[T]) =>
        executeInSequence(subElements, el => fn(el))
      }

    Future.sequence(futures).map(_.flatten)

  /**
    * The maxConcurrency is handled by grouping the elements into maxConcurrency partition (or bucket).
    * Each partition execute the function on its group of elements sequentially.
    * Partitions are executed in parallel.
    *
    * Results of failing queries are skipped.
    *
    * The order of the elements is kept. This function can be seen like an asynchronous `collect` operation.
    *
    * @return A sequence which maximum size is the number of provided elements
    * @throws IllegalArgumentException when maxConcurrency is less than 1
    */
  def executeInParallelOnlySuccess[T, U](elements: Seq[T], fn: (T) => Future[U], maxConcurrency: Int)(
      implicit ec: ExecutionContext
  ): Future[Seq[U]] =
    val partitionSize =
      if elements.size == 0 || maxConcurrency < 1 then 1
      else Math.ceil(elements.size.toFloat / maxConcurrency).toInt

    val futures = elements
      .grouped(partitionSize)
      .toSeq
      .map { (subElements: Seq[T]) =>
        executeInSequence(
          subElements,
          el => fn(el).map[Option[U]](Some(_)).recover(_ => None)
        )
      }

    Future.sequence(futures).map(_.flatten.flatten)

  /**
    * Executes all calls in parallel.
    *
    * Results of failing queries are skipped.
    *
    * The order of the elements is kept. This function can be seen like an asynchronous `collect` operation.
    *
    * @return A sequence which maximum size is the number of provided elements
    */
  def executeInParallelOnlySuccess[T, U](elements: Seq[T], fn: (T) => Future[U])(
      implicit ec: ExecutionContext
  ): Future[Seq[U]] =
    val futures = elements.map { el =>
      fn(el).map[Option[U]](Some(_)).recover(_ => None)
    }
    Future.sequence(futures).map(_.flatten)

  /**
    * Executes all calls sequentially.
    * This is done by call one function and flatMap to call the next one.
    *
    * Whenever a computation fails its Future.Failure is returned.
    *
    * The order of the elements is kept. This function can be seen like an synchronous `map` operation, that is not blocking its thread.
    *
    * @return A sequence of the same size as the provided elements
    */
  def executeInSequence[T, U](elements: Seq[T], fn: (T) => Future[U])(implicit ec: ExecutionContext): Future[Seq[U]] =
    elements match
      case Seq(head, tail @ _*) => fn(head).flatMap(res => executeInSequence(tail, fn).map(l => res +: l))
      case _                    => Future.successful(Seq.empty)

  /**
    * Executes all calls sequentially.
    * This is done by call one function and flatMap to call the next one.
    *
    * Results of failing queries are skipped.
    *
    * The order of the elements is kept. This function can be seen like an synchronous `collect` operation, that is not blocking its thread.
    *
    * @return A sequence of the same size as the provided elements
    */
  def executeInSequenceOnlySuccess[T, U](elements: Seq[T], fn: (T) => Future[U])(
      implicit ec: ExecutionContext
  ): Future[Seq[U]] =
    elements match
      case Seq(head, tail @ _*) =>
        fn(head)
          .flatMap(res => executeInSequenceOnlySuccess(tail, fn).map(l => l :+ res))
          .recoverWith(_ => executeInSequenceOnlySuccess(tail, fn))
      case _ => Future.successful(Seq.empty)

  /**
    * Executes all calls sequentially until receiving a Future.Success
    * This is done by call one function and recoverWith to call the next one.
    *
    * @return A Future.Success containing true if we found a successful call, false otherwise
    */
  def executeInSequenceUntilSuccess[T](elements: Seq[T], fn: (T) => Future[_])(
      implicit ec: ExecutionContext
  ): Future[Unit] =
    elements match
      case Seq(head, tail @ _*) => fn(head).map(_ => (): Unit).recoverWith(_ => executeInSequenceUntilSuccess(tail, fn))
      case _                    => Future.failed(new Exception("Executed all elements, but none worked"))
