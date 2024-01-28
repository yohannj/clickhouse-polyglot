package com.amendil

import com.amendil.bo.CHResponse
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.typesafe.scalalogging.StrictLogging

import java.io.InputStream
import java.net.{HttpURLConnection, URI}
import java.net.http.{HttpClient, HttpHeaders, HttpRequest, HttpResponse}
import java.net.http.HttpClient.Version
import java.net.http.HttpResponse.BodyHandlers
import java.nio.charset.StandardCharsets
import java.time.Duration
import scala.compat.java8.FutureConverters._
import scala.concurrent.{ExecutionContext, Future}

class CHClient(port: Int)(implicit executionContext: ExecutionContext) extends StrictLogging:
  private val url = s"http://localhost:$port"

  private val mapper = JsonMapper.builder().addModule(DefaultScalaModule).build()

  private val client: HttpClient =
    HttpClient
      .newBuilder()
      .connectTimeout(Duration.ofMinutes(1))
      .version(Version.HTTP_1_1)
      .build()

  def execute(query: String): Future[CHResponse] =
    val r = HttpRequest
      .newBuilder()
      .uri(new URI(s"$url"))
      .POST(HttpRequest.BodyPublishers.ofString(s"$query FORMAT JSONCompact;"))
      .setHeader("Content-Type", "application/x-www-form-urlencoded")
      .build()

    toScala(
      client.sendAsync(r, BodyHandlers.ofInputStream())
    )
      .map { r =>
        if (r.statusCode() == HttpURLConnection.HTTP_OK) {
          mapper.readValue(r.body(), classOf[CHResponse])
        } else {
          val message = new String(r.body().readAllBytes(), StandardCharsets.UTF_8)
          throw new Exception(s"ClickHouse query failed: $message")
        }
      }
