package com.amendil.common

import com.typesafe.config.ConfigFactory

object Settings:
  private val config = ConfigFactory.load()

  object Type:
    private val typeSettings = config.getConfig("type")

    val catboostPath: String = typeSettings.getString("catboost-path")
    lazy val dictionaryNames: Seq[String] = Seq(
      FuzzerDictionaryNames.hierarchyDictionaryName,
      FuzzerDictionaryNames.manyTypesDictionaryName,
      FuzzerDictionaryNames.regexpDictionaryName
    )

    object FuzzerDictionaryNames:
      private val fuzzerDictionaryNamesSettings = config.getConfig("type.fuzzer-dictionary-names")

      val hierarchyDictionaryName: String = fuzzerDictionaryNamesSettings.getString("hierarchy")
      val manyTypesDictionaryName: String = fuzzerDictionaryNamesSettings.getString("many-types")
      val regexpDictionaryName: String = fuzzerDictionaryNamesSettings.getString("regexp")
