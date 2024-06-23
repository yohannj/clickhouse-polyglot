package com.amendil.common.entities.function

import com.amendil.common.entities.CHSettingWithValue
import com.amendil.common.entities.function.CHFunction.indent

/**
  * CHFunction provides basic information regarding a function available in ClickHouse.
  * Those information are enough to know how to call it.
  * Name, valid IO signatures, modes
  *
  * @param name Name of the ClickHouse function
  * @param signatures Supported input of the function and the associated output type
  * @param modes Supported context in which to call this method
  * @param settings Mandatory settings to call this method, for example experimental flags
  */
final case class CHFunction(
    name: String,
    signatures: Seq[CHFunctionIO],
    modes: Seq[CHFunction.Mode],
    settings: Seq[CHSettingWithValue[?]]
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
          |${indent}Modes: ${modes.mkString(", ")}
          |${indent}Settings: ${settings.map(_.asString).mkString(", ")}
          |${indent}Signatures:
          |$signaturesStr""".stripMargin

object CHFunction:
  val indent = "    "

  enum Mode:
    /**
      * The function can be called with the syntax `SELECT function(...)
      */
    case NoOverWindow extends Mode

    /**
      * The function can be called with the syntax `SELECT function(...) OVER w1 WINDOW w1 AS ()
      */
    case OverWindow extends Mode
