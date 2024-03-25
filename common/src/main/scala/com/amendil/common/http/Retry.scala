package com.amendil.common.http

import java.util.concurrent.{CompletableFuture, Executors, ScheduledExecutorService, TimeUnit}
import scala.compat.java8.FutureConverters._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Random

object Retry {
  private val scheduledExecutor: ScheduledExecutorService = Executors.newScheduledThreadPool(1)
  private val random = Random()

  /**
    * Call asyncCall.
    * When the future is a success and shouldRetry returns True, call asyncCall again.
    * When the future is a success and shouldRetry returns False, return the future.
    * When the future is a failure, return the future.
    *
    * asyncCall will be called up to maxNumberOfAttempts times.
    * After that, this method will return whatever future has been generated.
    *
    * When re-calling asyncCall, introduce a delay that increases exponentially up to 1 minute.
    * That delay includes a jitter of 200ms. It spreads the load over time on the target when this method is used concurrently.
    * https://en.wikipedia.org/wiki/Exponential_backoff
    *
    * @param asyncCall Asynchronous call that we try to make.
    * @param shouldRetry Lambda that takes the successful result of the Future and determine if it should be replayed or not
    * @param maxNumberOfAttempts Maximum number of time asyncCall should be called.
    * @param attempt Interval value to know the current number of time asyncCall was called.
    * @param previousWaitTimeMs Interval value to exponentially increase the delay between calls to asyncCall
    * @return The result of asyncCall
    */
  private[http] def retryWithExponentialBackoff[T](
      asyncCall: () => CompletableFuture[T],
      shouldRetry: T => Boolean,
      maxNumberOfAttempts: Int,
      attempt: Int = 0,
      previousWaitTimeMs: Long = 0L
  )(using ec: ExecutionContext): Future[T] =
    toScala { asyncCall() }.flatMap { res =>
      if shouldRetry(res) && attempt >= maxNumberOfAttempts then
        val waitTimeMs: Long = (Math.min(previousWaitTimeMs * 1.15, 59800) + random.nextInt(200)).toLong
        val p = Promise[T]()
        scheduledExecutor.schedule(
          () =>
            p.completeWith(
              retryWithExponentialBackoff(asyncCall, shouldRetry, maxNumberOfAttempts, attempt + 1, waitTimeMs)
            ),
          waitTimeMs,
          TimeUnit.MILLISECONDS
        )

        p.future
      else Future.successful(res)
    }
}
