package com.amendil.http

import java.util.concurrent.{CompletableFuture, Executors, ScheduledExecutorService, TimeUnit}
import scala.compat.java8.FutureConverters._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Random

object Retry {
  private val scheduledExecutor: ScheduledExecutorService = Executors.newScheduledThreadPool(1)
  private val random = Random()

  private[http] def retryWithExponentialBackoff[T](
      asyncCall: () => CompletableFuture[T],
      shouldRetry: T => Boolean,
      attempt: Int = 0,
      previousWaitTimeMs: Long = 0L
  )(implicit ec: ExecutionContext): Future[T] =
    toScala { asyncCall() }.flatMap { res =>
      if shouldRetry(res) then
        val waitTimeMs: Long = (Math.min(previousWaitTimeMs * 1.15, 60000) + random.nextInt(200)).toLong
        val p = Promise[T]()
        scheduledExecutor.schedule(
          () => p.completeWith(retryWithExponentialBackoff(asyncCall, shouldRetry, attempt + 1, waitTimeMs)),
          waitTimeMs,
          TimeUnit.MILLISECONDS
        )

        p.future
      else Future.successful(res)
    }
}
