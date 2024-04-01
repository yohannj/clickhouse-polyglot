package com.amendil.common

import com.typesafe.config.ConfigFactory

object Settings {
  private val config = ConfigFactory.load()

  object Type {
    private val typeSettings = config.getConfig("type")

    val catboostPath: String = typeSettings.getString("catboost-path")
  }

}
