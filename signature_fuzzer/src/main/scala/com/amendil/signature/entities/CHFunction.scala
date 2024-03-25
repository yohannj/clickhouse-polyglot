package com.amendil.signature.entities

import com.amendil.signature.entities.CHFunction.indent

final case class CHFunction(
    name: String,
    supportedPlatforms: Seq[String],
    signatures: Seq[CHFunctionIO],
    isExperimental: Boolean
):
  def asString(): String =
    if signatures.isEmpty then name
    else
      val signaturesStr =
        signatures
          .map(_.asString())
          .sorted
          .map(s => s"$indent$indent$s")
          .mkString("\n")

      s"""|$name
          |${indent}Supported platforms: ${supportedPlatforms.sorted.mkString(", ")}
          |${indent}Is experimental: ${if isExperimental then "Yes" else "No"}
          |${indent}Signatures:
          |$signaturesStr""".stripMargin

object CHFunction:
  val indent = "    "

  // TODO: Write tests
  def fromCHFunctionFuzzResult(fuzzResult: CHFunctionFuzzResult, platform: String): CHFunction =
    CHFunction(
      name = fuzzResult.name,
      supportedPlatforms = Seq(platform),
      signatures = fuzzResult.functions,
      isExperimental = false // FIXME
    )
