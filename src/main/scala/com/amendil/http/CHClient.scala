package com.amendil.http

import com.amendil.bo.CHResponse
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import java.net.{HttpURLConnection, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.http.HttpClient.Version
import java.net.http.HttpResponse.BodyHandlers
import java.nio.charset.StandardCharsets
import java.time.Duration
import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}
import scala.compat.java8.FutureConverters._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Random

class CHClient(port: Int)(implicit ec: ExecutionContext):
  private val url = s"http://localhost:$port"
  private val scheduledExecutor: ScheduledExecutorService = Executors.newScheduledThreadPool(1)
  private val random = Random()

  private val mapper = JsonMapper.builder().addModule(DefaultScalaModule).build()

  private val client: HttpClient =
    HttpClient
      .newBuilder()
      .connectTimeout(Duration.ofMinutes(1))
      .version(Version.HTTP_1_1)
      .build()

  def execute(query: String): Future[CHResponse] =
    execute(
      HttpRequest
        .newBuilder()
        .uri(new URI(s"$url"))
        .POST(
          HttpRequest.BodyPublishers.ofString(
            s"$query SETTINGS ${CHClient.settings} FORMAT JSONCompact;"
          )
        )
        .setHeader("Content-Type", "application/x-www-form-urlencoded")
        .build()
    )

  private def execute(req: HttpRequest, attempt: Int = 0, previousWaitTime: Long = 0L): Future[CHResponse] =
    toScala {
      client.sendAsync(req, BodyHandlers.ofInputStream())
    }.flatMap { r =>
      if (r.statusCode() == HttpURLConnection.HTTP_OK) {
        Future.successful(mapper.readValue(r.body(), classOf[CHResponse]))
      } else {
        val message = new String(r.body().readAllBytes(), StandardCharsets.UTF_8)

        if (message.contains("Too many simultaneous queries.")) {
          val waitTime = (Math.min(previousWaitTime * 1.15, 60000) + random.nextInt(200)).toLong
          val p = Promise[CHResponse]()
          scheduledExecutor.schedule(
            () => p.completeWith(execute(req, attempt + 1, waitTime)),
            waitTime,
            TimeUnit.MILLISECONDS
          )

          p.future
        } else {
          Future.failed(new Exception(s"ClickHouse query failed: $message"))
        }
      }
    }

object CHClient {
  private val settings: String = Seq(
    "allow_suspicious_low_cardinality_types=1",
    "allow_experimental_object_type=1",
    "allow_experimental_nlp_functions=1",
    "allow_experimental_funnel_functions=1"
  ).mkString(", ")
}
