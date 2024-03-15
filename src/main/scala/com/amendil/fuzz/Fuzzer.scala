package com.amendil.fuzz

import com.amendil.ConcurrencyUtils.executeChain
import com.amendil.entities._
import com.amendil.http.CHClient

import scala.concurrent.{ExecutionContext, Future}

object Fuzzer:
  def fuzz(functionName: String)(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    functionName match
      case "CAST" | "_CAST" | "accurateCast" | "accurateCastOrNull" =>
        // Cast methods can return any kind of type, depending on the value of a String.
        // This is an edge case not automatically handled by this fuzzer, and it is not worth it to handle it.
        Future.successful(
          CHFunctionFuzzResult(
            name = functionName,
            function2s = Seq(CHFunctionIO.Function2(CHAggregatedType.Any, CHFuzzableType.ClickHouseType, "Any"))
          )
        )
      case "accurateCastOrDefault" =>
        // Cast methods can return any kind of type, depending on the value of a String.
        // This is an edge case not automatically handled by this fuzzer, and it is not worth it to handle it.
        Future.successful(
          CHFunctionFuzzResult(
            name = functionName,
            function2s = Seq(CHFunctionIO.Function2(CHAggregatedType.Any, CHFuzzableType.ClickHouseType, "Any")),
            // FIXME Third argument of the Function3 is not really "Any", but should be of the ClickHouseType chosen
            function3s = Seq(
              CHFunctionIO.Function3(CHAggregatedType.Any, CHFuzzableType.ClickHouseType, CHAggregatedType.Any, "Any")
            )
          )
        )
      case "exponentialMovingAverage" | "exponentialTimeDecayedAvg" | "exponentialTimeDecayedCount" |
          "exponentialTimeDecayedMax" | "exponentialTimeDecayedSum" =>
        // Fuzzing this function leads to OOM

        // TODO Handle these methods.
        // They support many combinations (lowcardinality, nullable), and output type can be nullable
        // Reference: https://clickhouse.com/docs/en/sql-reference/aggregate-functions/reference/exponentialmovingaverage
        // + Introduced window functions exponentialTimeDecayedSum, exponentialTimeDecayedMax, exponentialTimeDecayedCount
        // and exponentialTimeDecayedAvg which are more effective than exponentialMovingAverage for bigger windows.
        // Also more use-cases were covered. #29799 (Vladimir Chebotarev).
        Future.successful(
          CHFunctionFuzzResult(name = functionName)
        )
      case _ =>
        val fuzzingFunctionsWithCost: Seq[(CHFunctionFuzzResult => Future[CHFunctionFuzzResult], Long)] =
          FuzzerSpecialFunctions.fuzzingFunctionWithCost ++
            FuzzerLambdaFunctions.fuzzingFunctionWithCost ++
            FuzzerNonParametricFunctions.fuzzingFunctionWithCost ++
            FuzzerParametricFunctions.fuzzingFunctionWithCost

        val sortedFuzzingFunctions = fuzzingFunctionsWithCost.sortBy(_._2).map(_._1)

        executeChain(
          CHFunctionFuzzResult(name = functionName),
          sortedFuzzingFunctions
        )

  /**
    * @param CHFuzzableAbstractTypeList List of CHFuzzableAbstractType that will be used to generate the combinations
    * @return All combinations of CHFuzzableAbstractType of argCount elements.
    */
  private[fuzz] def generateCHFuzzableAbstractTypeCombinations(
      argCount: Int,
      CHFuzzableAbstractTypeList: Seq[CHFuzzableAbstractType] = CHFuzzableAbstractType.values.toSeq
  ): Seq[Seq[CHFuzzableAbstractType]] =
    generateCHFuzzableAbstractTypeCombinations(
      argCount = argCount,
      currentArgs = Nil,
      CHFuzzableAbstractTypeList = CHFuzzableAbstractTypeList
    )

  private def generateCHFuzzableAbstractTypeCombinations(
      argCount: Int,
      currentArgs: Seq[CHFuzzableAbstractType],
      CHFuzzableAbstractTypeList: Seq[CHFuzzableAbstractType]
  ): Seq[Seq[CHFuzzableAbstractType]] =
    if argCount > 0 then
      CHFuzzableAbstractTypeList.toSeq.map { abstractType =>
        generateCHFuzzableAbstractTypeCombinations(
          argCount - 1,
          currentArgs :+ abstractType,
          CHFuzzableAbstractTypeList
        )
      }.flatten
    else Seq(currentArgs)

  private[fuzz] def generateCHFuzzableTypeCombinations(
      abstractTypes: Seq[CHFuzzableAbstractType],
      currentArgs: Seq[CHFuzzableType] = Seq.empty
  ): Seq[Seq[CHFuzzableType]] =
    abstractTypes match
      case Seq(head, tail @ _*) =>
        head.CHFuzzableTypes
          .map(CHFuzzableType => generateCHFuzzableTypeCombinations(tail, currentArgs :+ CHFuzzableType))
          .flatten
      case Seq() => Seq(currentArgs)

  private[fuzz] def buildFuzzingValuesArgs(argumentsValues: Seq[Seq[String]]): Seq[String] =
    argumentsValues match
      case Seq()   => throw IllegalArgumentException("Tried to fuzz an argument without any value")
      case Seq(el) => el
      case Seq(head, tail @ _*) =>
        val subChoices: Seq[String] = buildFuzzingValuesArgs(tail)
        head.flatMap(el => subChoices.map(subChoice => s"$el, $subChoice"))
