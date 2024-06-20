package com.amendil.common.entities.function

import com.amendil.common.entities.function.CHFunction.indent

final case class CHFunction(
    name: String,
    signatures: Seq[CHFunctionIO],
    modes: Seq[CHFunction.Mode],
    isExperimental: Boolean
):
  def asString(): String =
    if signatures.isEmpty then name
    else
      val signaturesStr =
        signatures
          .map(s => (s.parameters.size, s.arguments.size, s.asString()))
          .sortWith { case ((paramSize1, argSize1, str1), (paramSize2, argSize2, str2)) =>
            if paramSize1 != paramSize2 then paramSize1 < paramSize2
            else if argSize1 != argSize2 then argSize1 < argSize2
            else str1.compareTo(str2) < 0
          }
          .map((_, _, s) => s"$indent$indent$s")
          .mkString("\n")

      s"""|$name
          |${indent}Is experimental: ${if isExperimental then "Yes" else "No"}
          |${indent}Modes: ${modes.mkString(", ")}
          |${indent}Signatures:
          |$signaturesStr""".stripMargin

object CHFunction:
  val indent = "    "

  enum Mode:
    case NoOverWindow extends Mode
    case OverWindow extends Mode
