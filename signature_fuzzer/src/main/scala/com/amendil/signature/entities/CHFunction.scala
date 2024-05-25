package com.amendil.signature.entities

import com.amendil.signature.entities.CHFunction.indent

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
          .map(_.asString())
          .sorted
          .map(s => s"$indent$indent$s")
          .mkString("\n")

      s"""|$name
          |${indent}Is experimental: ${if isExperimental then "Yes" else "No"}
          |${indent}Modes: ${modes.mkString(", ")}
          |${indent}Signatures:
          |$signaturesStr""".stripMargin

object CHFunction:
  val indent = "    "

  // TODO: Write tests
  def fromCHFunctionFuzzResult(fuzzResult: CHFunctionFuzzResult): CHFunction =
    val signatures =
      // productIterator is an internal method in all "case class" to iterate over its constructor arguments
      fuzzResult.productIterator.toSeq.flatMap {
        case s: Seq[?] if s.nonEmpty => // Look for functions that we discovered
          s.head match
            case _: CHFunctionIO => Some(CHFunctionIO.aggregate(s.asInstanceOf[Seq[CHFunctionIO]]))
            case _               => None // This argument is a Sequence but not of functions
        case Some(fn) if fn.isInstanceOf[CHFunctionIO] => Some(Seq(fn.asInstanceOf[CHFunctionIO]))
        case _                                         => None // This argument is not a Sequence of functions
      }.flatten

    CHFunction(
      name = fuzzResult.name,
      signatures = signatures,
      modes = fuzzResult.modes.toSeq.sortWith(_.ordinal < _.ordinal),
      isExperimental = false // FIXME
    )

  enum Mode:
    case NoOverWindow extends Mode
    case OverWindow extends Mode
