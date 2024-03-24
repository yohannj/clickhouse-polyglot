package com.amendil

import com.typesafe.config.ConfigFactory

object Settings {
  private val config = ConfigFactory.load()

  object ClickHouse {
    private val hostname: String = config.getString("clickhouse.http.host")
    private val port: Int = config.getInt("clickhouse.http.port")
    private val useSsl: Boolean = config.getBoolean("clickhouse.http.ssl")
    private val protocol = if useSsl then "https" else "http"
    val httpUrl: String = s"$protocol://$hostname:$port"

    val maxSupportedConcurrency: Int = config.getInt("clickhouse.concurrency")
  }

  object Fuzzer {
    val supportLowCardinality: Boolean = config.getBoolean("fuzzer.support_lowcardinality")
    val supportNullable: Boolean = config.getBoolean("fuzzer.support_nullable")
  }

}
