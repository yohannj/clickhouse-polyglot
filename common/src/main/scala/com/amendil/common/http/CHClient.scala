package com.amendil.common.http

import com.amendil.common.entities.CHResponse
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import java.net.{HttpURLConnection, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.http.HttpClient.Version
import java.net.http.HttpResponse.BodyHandlers
import java.nio.charset.StandardCharsets
import java.time.Duration
import scala.compat.java8.FutureConverters._
import scala.concurrent.{ExecutionContext, Future}

class CHClient(url: String)(using ec: ExecutionContext):

  private val jsonMapper = JsonMapper.builder().addModule(DefaultScalaModule).build()

  private val client: HttpClient =
    HttpClient
      .newBuilder()
      .connectTimeout(Duration.ofMinutes(1))
      .version(Version.HTTP_1_1)
      .build()

  /**
    * Query ClickHouse on its HTTP interface.
    *
    * Some settings are added to allow experimental features and suspicious behaviors.
    * An additional setting is added to avoid logging the call into ClickHouse's `system.query_log` table.
    *
    * @param query Select query to send to ClickHouse
    * @return A Future containing the result of the query.
    */
  def execute(query: String): Future[CHResponse] =
    executeAndParseJson(
      HttpRequest
        .newBuilder()
        .uri(URI(s"$url"))
        .POST(
          HttpRequest.BodyPublishers.ofString(
            // JSONCompact is an efficient format (small network footprint + quick to parse)
            s"$query SETTINGS ${CHClient.settings} FORMAT JSONCompact;"
          )
        )
        .setHeader("Content-Type", "application/x-www-form-urlencoded")
        .build()
    )

  /**
    * Query ClickHouse on its HTTP interface.
    *
    * Some settings are added to allow experimental features and suspicious behaviors.
    * An additional setting is added to avoid logging the call into ClickHouse's `system.query_log` table.
    *
    * @param query Select query to send to ClickHouse
    * @return Future.successful when ClickHouse executed successfully the query, Future.failure otherwise.
    */
  def executeNoResult(query: String): Future[Unit] =
    executeNoResult(
      HttpRequest
        .newBuilder()
        .uri(URI(s"$url"))
        .POST(
          HttpRequest.BodyPublishers.ofString(
            s"$query SETTINGS ${CHClient.settings};"
          )
        )
        .setHeader("Content-Type", "application/x-www-form-urlencoded")
        .build()
    )

  private def executeAndParseJson(req: HttpRequest): Future[CHResponse] =
    execute(req).flatMap { (statusCode: Int, body: String) =>
      if statusCode == HttpURLConnection.HTTP_OK then Future.successful(jsonMapper.readValue(body, classOf[CHResponse]))
      else Future.failed(Exception(s"ClickHouse query failed: $body"))
    }

  private def executeNoResult(req: HttpRequest): Future[Unit] =
    execute(req).map { (statusCode: Int, body: String) =>
      if statusCode == HttpURLConnection.HTTP_OK then (): Unit
      else throw Exception(s"ClickHouse query failed: $body")
    }

  /**
    * @return A tuple containing the HTTP Status code and the body as a String
    */
  private def execute(req: HttpRequest): Future[(Int, String)] =
    Retry.retryWithExponentialBackoff(
      () =>
        toScala { client.sendAsync(req, BodyHandlers.ofInputStream()) }.map { r =>
          (r.statusCode(), String(r.body().readAllBytes(), StandardCharsets.UTF_8))
        },
      shouldRetry = (statusCode: Int, body: String) => {
        if statusCode == HttpURLConnection.HTTP_OK then false
        else
          // When there is an error, sometime it's because too many queries are being sent by ClickHouse.
          // As the query may be invalid, we must retry the call!
          val shouldRetry = body.contains("Too many simultaneous queries.")
          shouldRetry
      },
      maxNumberOfAttempts = Int.MaxValue // Never stops querying ClickHouse, we need 100% accuracy
    )

object CHClient:
  private val settings: String = Seq(
    "allow_experimental_funnel_functions=1",
    "allow_experimental_object_type=1",
    "allow_experimental_nlp_functions=1",
    "allow_introspection_functions=1",
    "allow_suspicious_low_cardinality_types=1",
    "decimal_check_overflow=0",
    "log_queries=0"
  ).mkString(", ")
