package com.amendil

import com.amendil.entities.{CHAggregatedType, CHType}

package object fuzz {
  val fnHardcodedOutputType: Map[String, CHType] =
    Map(
      ("JSONExtract", CHAggregatedType.Any),
      ("JSONExtractKeysAndValues", CHAggregatedType.Any)
    )
}
