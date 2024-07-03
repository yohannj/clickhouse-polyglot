package com.amendil

package object signature:

  val unknownFunctions: Seq[String] = Seq(
    "_CAST",
    "accurateCast",
    "accurateCastOrDefault",
    "accurateCastOrNull",
    "arrayEnumerateDenseRanked",
    "arrayEnumerateUniqRanked",
    "arrayFlatten",
    "arrayZip",
    "catboostEvaluate",
    "sequenceNextNode"
  )
  val unknownFunctionsWithAlias: Seq[(String, String)] = Seq(
    // ("STDDEV_POP", "stddevPop"),
  )
