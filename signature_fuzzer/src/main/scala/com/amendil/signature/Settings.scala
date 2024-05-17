package com.amendil.signature

import com.typesafe.config.ConfigFactory

object Settings:
  private val config = ConfigFactory.load()

  object ClickHouse:
    private val chSettings = config.getConfig("clickhouse")

    private val hostname: String = chSettings.getString("http.host")
    private val port: Int = chSettings.getInt("http.port")
    private val useSsl: Boolean = chSettings.getBoolean("http.ssl")
    private val protocol = if useSsl then "https" else "http"
    val httpUrl: String = s"$protocol://$hostname:$port"

    /**
      * Maximum number of queries we want to send to ClickHouse concurrently
      */
    val maxSupportedConcurrency: Int = chSettings.getInt("concurrency")

    /**
      * maxSupportedConcurrencyOuterLoop can be used when we have nested loops
      *
      * Instead of having one loop with a parallelism of maxSupportedConcurrency
      * and the other one running sequentially, we can put a smaller parallelism on
      * the outer and inner loops (max two loops can be parallelized).
      *
      * This is useful when the inner loop can have a huge number of elements.
      * We want to avoid running sequentially a big loop.
      *
      * In terms of code, this is viable in the situation below:
      * ```
      * for (...) {   // Use maxSupportedConcurrencyOuterLoop
      *   for (...) { // Use maxSupportedConcurrencyInnerLoop
      *     ...
      *   }
      * }
      * ```
      */
    val maxSupportedConcurrencyOuterLoop: Int = Math.max(1, maxSupportedConcurrency / 5)

    /**
      * maxSupportedConcurrencyInnerLoop can be used when we have nested loops
      *
      * Instead of having one loop with a parallelism of maxSupportedConcurrency
      * and the other one running sequentially, we can put a smaller parallelism on
      * the outer and inner loops (max two loops can be parallelized).
      *
      * This is useful when the inner loop can have a huge number of elements.
      * We want to avoid running sequentially a big loop.
      *
      * In terms of code, this is viable in the situation below:
      * ```
      * for (...) {   // Use maxSupportedConcurrencyOuterLoop
      *   for (...) { // Use maxSupportedConcurrencyInnerLoop
      *     ...
      *   }
      * }
      * ```
      */
    val maxSupportedConcurrencyInnerLoop: Int = maxSupportedConcurrency / maxSupportedConcurrencyOuterLoop

  object Fuzzer:
    private val fuzzerSettings = config.getConfig("fuzzer")

    val supportJson: Boolean = fuzzerSettings.getBoolean("support.json")
    val supportLowCardinality: Boolean = fuzzerSettings.getBoolean("support.lowcardinality")
    val supportNullable: Boolean = fuzzerSettings.getBoolean("support.nullable")

    val skipFuzzingOnArgumentMismatch: Boolean = fuzzerSettings.getBoolean("skip-fuzzing-on-argument-mismatch")
