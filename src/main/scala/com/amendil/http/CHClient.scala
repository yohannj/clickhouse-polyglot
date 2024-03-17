package com.amendil.http

import com.amendil.entities.CHResponse
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import java.io.InputStream
import java.net.{HttpURLConnection, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.http.HttpClient.Version
import java.net.http.HttpResponse.BodyHandlers
import java.nio.charset.StandardCharsets
import java.time.Duration
import scala.concurrent.{ExecutionContext, Future}

class CHClient(url: String)(implicit ec: ExecutionContext):

  private val jsonMapper = JsonMapper.builder().addModule(DefaultScalaModule).build()

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
    val clickHouseHttpResponseF: Future[HttpResponse[InputStream]] =
      Retry.retryWithExponentialBackoff(
        () => client.sendAsync(req, BodyHandlers.ofInputStream()),
        shouldRetry = r => {
          if r.statusCode() == HttpURLConnection.HTTP_OK then false
          else
            val message = new String(r.body().readAllBytes(), StandardCharsets.UTF_8)
            val shouldRetry = message.contains("Too many simultaneous queries.")
            shouldRetry
        },
        maxNumberOfAttempts = Int.MaxValue // Never stops querying ClickHouse
      )

    clickHouseHttpResponseF.flatMap { (httpResponse: HttpResponse[InputStream]) =>
      if httpResponse.statusCode() == HttpURLConnection.HTTP_OK then
        Future.successful(jsonMapper.readValue(httpResponse.body(), classOf[CHResponse]))
      else
        val message = new String(httpResponse.body().readAllBytes(), StandardCharsets.UTF_8)
        Future.failed(new Exception(s"ClickHouse query failed: $message"))
    }

object CHClient:
  private val settings: String = Seq(
    "allow_suspicious_low_cardinality_types=1",
    "allow_experimental_object_type=1",
    "allow_experimental_nlp_functions=1",
    "allow_experimental_funnel_functions=1",
    "log_queries=0",
    "decimal_check_overflow=0"
  ).mkString(", ")
