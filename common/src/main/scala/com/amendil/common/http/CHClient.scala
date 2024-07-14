package com.amendil.common.http

import com.amendil.common.entities.{CHResponse, CHSetting, CHSettingWithValue}
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import java.net.{HttpURLConnection, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.http.HttpClient.Version
import java.net.http.HttpResponse.BodyHandlers
import java.nio.charset.StandardCharsets
import java.time.Duration
import scala.compat.java8.FutureConverters.*
import scala.concurrent.{ExecutionContext, Future}

trait CHClient:
  protected val unlockFunctionsSettings = CHClient.unlockFunctionsSettingNames.map(_.apply(true))

  def execute(
      query: String,
      unlockFunctionsSettings: Seq[CHSettingWithValue[Boolean]] = unlockFunctionsSettings
  ): Future[CHResponse]
  def executeNoResult(
      query: String,
      unlockFunctionsSettings: Seq[CHSettingWithValue[Boolean]] = unlockFunctionsSettings
  ): Future[Unit]
  def executeNoResultNoSettings(query: String): Future[Unit]

object CHClient:
  val baseSettings = Seq(
    CHSetting.AllowCustomErrorCodeInThrowif(true),
    CHSetting.AllowExperimentalDynamicType(true),
    CHSetting.AllowExperimentalObjectType(true),
    CHSetting.AllowExperimentalVariantType(true),
    CHSetting.AllowGetClientHttpHeader(true),
    CHSetting.AllowSuspiciousLowCardinalityTypes(true),
    CHSetting.DecimalCheckOverflow(false),
    CHSetting.LogQueries(false)
  )

  val unlockFunctionsSettingNames = Seq(
    CHSetting.AllowDeprecatedErrorProneWindowFunctions,
    CHSetting.AllowExperimentalFunnelFunctions,
    CHSetting.AllowExperimentalNlpFunctions,
    CHSetting.AllowIntrospectionFunctions
  )

class CHClientImpl(url: String)(using ExecutionContext) extends CHClient:

  System.setProperty("jdk.httpclient.keepalive.timeout", "10")
  private val jsonMapper = JsonMapper.builder().addModule(DefaultScalaModule).build()

  private lazy val client: HttpClient =
    HttpClient
      .newBuilder()
      .connectTimeout(Duration.ofSeconds(20))
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
  def execute(
      query: String,
      unlockFunctionsSettings: Seq[CHSettingWithValue[Boolean]] = unlockFunctionsSettings
  ): Future[CHResponse] =
    executeAndParseJson(
      HttpRequest
        .newBuilder()
        .uri(URI(s"$url"))
        .POST(
          HttpRequest.BodyPublishers.ofString(
            // JSONCompact is an efficient format (small network footprint + quick to parse)
            s"$query SETTINGS ${(CHClient.baseSettings ++ unlockFunctionsSettings).map(_.asString).mkString(", ")} FORMAT JSONCompact;"
          )
        )
        .setHeader("Content-Type", "application/x-www-form-urlencoded")
        .setHeader("user-agent", "curl/8.4.0")
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
  def executeNoResult(
      query: String,
      unlockFunctionsSettings: Seq[CHSettingWithValue[Boolean]] = unlockFunctionsSettings
  ): Future[Unit] =
    executeNoResult(
      HttpRequest
        .newBuilder()
        .uri(URI(s"$url"))
        .POST(
          HttpRequest.BodyPublishers.ofString(
            s"$query SETTINGS ${(CHClient.baseSettings ++ unlockFunctionsSettings).map(_.asString).mkString(", ")};"
          )
        )
        .setHeader("Content-Type", "application/x-www-form-urlencoded")
        .setHeader("user-agent", "curl/8.4.0")
        .build()
    )

  /**
    * Query ClickHouse on its HTTP interface.
    *
    * This function is aimed at non-SELECT queries
    *
    * @param query Query to send to ClickHouse
    * @return Future.successful when ClickHouse executed successfully the query, Future.failure otherwise.
    */
  def executeNoResultNoSettings(query: String): Future[Unit] =
    executeNoResult(
      HttpRequest
        .newBuilder()
        .uri(URI(s"$url"))
        .POST(
          HttpRequest.BodyPublishers.ofString(query)
        )
        .setHeader("Content-Type", "application/x-www-form-urlencoded")
        .setHeader("user-agent", "curl/8.4.0")
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
      shouldRetry = (statusCode: Int, body: String) =>
        if statusCode == HttpURLConnection.HTTP_OK then false
        else
          // When there is an error, sometime it's because too many queries are being sent by ClickHouse.
          // As the query may be invalid, we must retry the call!
          val shouldRetry = body.contains("Too many simultaneous queries.")
          shouldRetry
      ,
      shouldRetryOnFailure = (e: Exception) =>
        val shouldRetry =
          Seq(
            "java.io.IOException: HTTP/1.1 header parser received no bytes",
            "java.net.ConnectException: Connection reset by peer"
          ).contains(e.getMessage)

        shouldRetry
      ,
      maxNumberOfAttempts = Int.MaxValue // Never stops querying ClickHouse, we need 100% accuracy
    )
