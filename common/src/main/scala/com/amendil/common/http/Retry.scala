package com.amendil.common.http

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Random, Success}

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
    * @param shouldRetryOnFailure Lambda that takes the exception of the Future and determine if it should be replayed or not
    * @param maxNumberOfAttempts Maximum number of time asyncCall should be called.
    * @param attempt Interval value to know the current number of time asyncCall was called.
    * @param previousWaitTimeMs Interval value to exponentially increase the delay between calls to asyncCall
    * @return The result of asyncCall
    */
  private[http] def retryWithExponentialBackoff[T](
      asyncCall: () => Future[T],
      shouldRetry: T => Boolean,
      shouldRetryOnFailure: Exception => Boolean,
      maxNumberOfAttempts: Int,
      attempt: Int = 0,
      previousWaitTimeMs: Long = 0L
  )(using ec: ExecutionContext): Future[T] =
    lazy val retriedValue = {
      val waitTimeMs: Long = (Math.min(previousWaitTimeMs * 1.15, 59800) + random.nextInt(200)).toLong
      val p = Promise[T]()
      scheduledExecutor.schedule(
        () =>
          p.completeWith(
            retryWithExponentialBackoff(
              asyncCall,
              shouldRetry,
              shouldRetryOnFailure,
              maxNumberOfAttempts,
              attempt + 1,
              waitTimeMs
            )
          ),
        waitTimeMs,
        TimeUnit.MILLISECONDS
      )

      p.future
    }

    asyncCall().transformWith {
      case Failure(e: Exception) =>
        if shouldRetryOnFailure(e) && attempt <= maxNumberOfAttempts then retriedValue
        else Future.failed(e)
      case Failure(t: Throwable) => Future.failed(t)
      case Success(res) =>
        if shouldRetry(res) && attempt <= maxNumberOfAttempts then retriedValue
        else Future.successful(res)
    }
}
