package com.amendil.fuzz

import com.amendil.entities._
import com.amendil.http.CHClient
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}

object Fuzzer extends StrictLogging {
  def fuzz(functionName: String)(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    FuzzerSpecialFunctions
      .fuzz(CHFunctionFuzzResult(name = functionName))
      .flatMap(FuzzerNonParametricFunctions.fuzz)

  private[fuzz] def generateCHAbstractTypeCombinations(
      argCount: Int,
      currentArgs: Seq[CHAbstractType] = Seq.empty
  ): Seq[Seq[CHAbstractType]] =
    if (argCount > 0) {
      CHAbstractType.values.toSeq.map { abstractType =>
        generateCHAbstractTypeCombinations(argCount - 1, currentArgs :+ abstractType)
      }.flatten
    } else {
      Seq(currentArgs)
    }

  private[fuzz] def generateCHTypeCombinations(
      abstractTypes: Seq[CHAbstractType],
      currentArgs: Seq[CHType] = Seq.empty
  ): Seq[Seq[CHType]] =
    abstractTypes match {
      case Seq(head, tail @ _*) =>
        head.chTypes
          .map(chType => generateCHTypeCombinations(tail, currentArgs :+ chType))
          .flatten
      case Seq() => Seq(currentArgs)
    }

  private[fuzz] def buildFuzzingValuesArgs(argumentsValues: Seq[Seq[String]]): Seq[String] =
    argumentsValues match
      case Seq()   => throw IllegalArgumentException("Tried to fuzz an argument without any value")
      case Seq(el) => el
      case Seq(head, tail @ _*) =>
        val subChoices = buildFuzzingValuesArgs(tail)
        head.flatMap(el => subChoices.map(subChoice => s"$el, $subChoice"))

}
