package com.amendil.fuzz

import com.amendil.ConcurrencyUtils.executeChain
import com.amendil.entities._
import com.amendil.http.CHClient

import scala.concurrent.{ExecutionContext, Future}

object Fuzzer:

  // TODO It would be nice to test the hardcoded functions somehow
  def fuzz(functionName: String)(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
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
      case "sequenceNextNode" =>
        // This method has many parameters and arguments. It's not worth to fuzz it.
        import CHFuzzableType._
        import CHSpecialType._

        // format: off
        Future.successful(
          CHFunctionFuzzResult(
            name = functionName,
            specialParametric2Function2Ns =
              for
                (direction, base) <- 
                  Seq(
                    (SequenceDirectionForward, SequenceBaseHead),
                    (SequenceDirectionBackward, SequenceBaseTail)
                  )

                timestamp <-
                  Seq(
                    BooleanType, UInt8, UInt16, UInt32, UInt64, Date, DateTime, LowCardinalityBoolean,
                    LowCardinalityUInt8, LowCardinalityUInt16, LowCardinalityUInt32, LowCardinalityUInt64,
                    LowCardinalityDate, LowCardinalityDateTime, LowCardinalityNullableBoolean, LowCardinalityNullableUInt8,
                    LowCardinalityNullableUInt16, LowCardinalityNullableUInt32, LowCardinalityNullableUInt64,
                    LowCardinalityNullableDate, LowCardinalityNullableDateTime, NullableBoolean, NullableUInt8,
                    NullableUInt16, NullableUInt32, NullableUInt64, NullableDate, NullableDateTime
                  )

              yield {
                CHFunctionIO.Parametric2Function2N(direction, base, timestamp, StringType, BooleanType, "Nullable(String)")
              },
            specialParametric2Function3Ns =
              for
                (direction, base) <- 
                  Seq(
                    (SequenceDirectionForward, SequenceBaseFirstMatch),
                    (SequenceDirectionForward, SequenceBaseLastMatch),
                    (SequenceDirectionBackward, SequenceBaseFirstMatch),
                    (SequenceDirectionBackward, SequenceBaseLastMatch)
                  )

                timestamp <-
                  Seq(
                    BooleanType, UInt8, UInt16, UInt32, UInt64, Date, DateTime, LowCardinalityBoolean,
                    LowCardinalityUInt8, LowCardinalityUInt16, LowCardinalityUInt32, LowCardinalityUInt64,
                    LowCardinalityDate, LowCardinalityDateTime, LowCardinalityNullableBoolean, LowCardinalityNullableUInt8,
                    LowCardinalityNullableUInt16, LowCardinalityNullableUInt32, LowCardinalityNullableUInt64,
                    LowCardinalityNullableDate, LowCardinalityNullableDateTime, NullableBoolean, NullableUInt8,
                    NullableUInt16, NullableUInt32, NullableUInt64, NullableDate, NullableDateTime
                  )

              yield {
                CHFunctionIO.Parametric2Function3N(direction, base, timestamp, StringType, BooleanType, BooleanType, "Nullable(String)")
              }
          )
        )
        // format: on
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
      currentArgs: Seq[CHFuzzableType] = Nil
  ): Seq[Seq[CHFuzzableType]] =
    abstractTypes match
      case Seq(head, tail @ _*) =>
        head.chFuzzableTypes
          .map(chFuzzableType => generateCHFuzzableTypeCombinations(tail, currentArgs :+ chFuzzableType))
          .flatten
      case Seq() => Seq(currentArgs)

  private[fuzz] def buildFuzzingValuesArgs(argumentsValues: Seq[Seq[String]]): Seq[String] =
    argumentsValues match
      case Seq()   => throw IllegalArgumentException("Tried to fuzz an argument without any value")
      case Seq(el) => el
      case Seq(head, tail @ _*) =>
        val subChoices: Seq[String] = buildFuzzingValuesArgs(tail)
        head.flatMap(el => subChoices.map(subChoice => s"$el, $subChoice"))
