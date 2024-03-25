package com.amendil.signature

import com.typesafe.config.ConfigFactory

object Settings {
  private val config = ConfigFactory.load()

  object ClickHouse {
    private val chSettings = config.getConfig("clickhouse")

    private val hostname: String = chSettings.getString("http.host")
    private val port: Int = chSettings.getInt("http.port")
    private val useSsl: Boolean = chSettings.getBoolean("http.ssl")
    private val protocol = if useSsl then "https" else "http"
    val httpUrl: String = s"$protocol://$hostname:$port"

    val maxSupportedConcurrency: Int = chSettings.getInt("concurrency")
  }

  object Fuzzer {
    private val fuzzerSettings = config.getConfig("fuzzer")

    val supportJson: Boolean = fuzzerSettings.getBoolean("support.json")
    val supportLowCardinality: Boolean = fuzzerSettings.getBoolean("support.lowcardinality")
    val supportNullable: Boolean = fuzzerSettings.getBoolean("support.nullable")
  }

}
