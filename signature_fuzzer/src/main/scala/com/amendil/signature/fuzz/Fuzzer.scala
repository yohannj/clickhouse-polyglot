package com.amendil.signature.fuzz

import com.amendil.common.ConcurrencyUtils.executeChain
import com.amendil.common.entities._
import com.amendil.common.entities.CHFuzzableType._
import com.amendil.common.http.CHClient
import com.amendil.signature.entities._

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

  def mergeOutputType(type1: String, type2: String): String =
    val exceptionIfUnknown = IllegalArgumentException(s"Unable to determine higher type for $type1 and $type2")
    if type1 == type2 then type1 // Expects both type to be identical, should be the most obvious use case
    else
      val chFuzzableType1 = CHFuzzableType.getByName(type1)
      val chFuzzableType2 = CHFuzzableType.getByName(type2)

      val mergedType: CHType =
        if chFuzzableType1 == chFuzzableType2 then chFuzzableType1
        else if chFuzzableType1 == BooleanType then
          chFuzzableType2 match
            case UInt8 | UInt16 | UInt32 | UInt64 | UInt128 | UInt256 | Int16 | Int32 | Int64 | Int128 | Int256 =>
              chFuzzableType2
            case _ => throw exceptionIfUnknown
        else if chFuzzableType2 == BooleanType then
          chFuzzableType1 match
            case UInt8 | UInt16 | UInt32 | UInt64 | UInt128 | UInt256 | Int16 | Int32 | Int64 | Int128 | Int256 =>
              chFuzzableType1
            case _ => throw exceptionIfUnknown
        else if chFuzzableType1 == Int8 then
          chFuzzableType2 match
            case UInt8                                   => Int16
            case UInt16                                  => Int32
            case UInt32                                  => Int64
            case UInt64                                  => Int128
            case UInt128 | UInt256                       => Int256
            case Int16 | Int32 | Int64 | Int128 | Int256 => chFuzzableType2
            case _                                       => throw exceptionIfUnknown
        else if chFuzzableType2 == Int8 then
          chFuzzableType1 match
            case UInt8                                   => Int16
            case UInt16                                  => Int32
            case UInt32                                  => Int64
            case UInt64                                  => Int128
            case UInt128 | UInt256                       => Int256
            case Int16 | Int32 | Int64 | Int128 | Int256 => chFuzzableType1
            case _                                       => throw exceptionIfUnknown
        else if chFuzzableType1 == Int16 then
          chFuzzableType2 match
            case UInt8                           => Int16
            case UInt16                          => Int32
            case UInt32                          => Int64
            case UInt64                          => Int128
            case UInt128 | UInt256               => Int256
            case Int32 | Int64 | Int128 | Int256 => chFuzzableType2
            case _                               => throw exceptionIfUnknown
        else if chFuzzableType2 == Int16 then
          chFuzzableType1 match
            case UInt8                           => Int16
            case UInt16                          => Int32
            case UInt32                          => Int64
            case UInt64                          => Int128
            case UInt128 | UInt256               => Int256
            case Int32 | Int64 | Int128 | Int256 => chFuzzableType1
            case _                               => throw exceptionIfUnknown
        else if chFuzzableType1 == Int32 then
          chFuzzableType2 match
            case UInt8 | UInt16          => Int32
            case UInt32                  => Int64
            case UInt64                  => Int128
            case UInt128 | UInt256       => Int256
            case Int64 | Int128 | Int256 => chFuzzableType2
            case _                       => throw exceptionIfUnknown
        else if chFuzzableType2 == Int32 then
          chFuzzableType1 match
            case UInt8 | UInt16          => Int32
            case UInt32                  => Int64
            case UInt64                  => Int128
            case UInt128 | UInt256       => Int256
            case Int64 | Int128 | Int256 => chFuzzableType1
            case _                       => throw exceptionIfUnknown
        else if chFuzzableType1 == Int64 then
          chFuzzableType2 match
            case UInt8 | UInt16 | UInt32 => Int64
            case UInt64                  => Int128
            case UInt128 | UInt256       => Int256
            case Int128 | Int256         => chFuzzableType2
            case _                       => throw exceptionIfUnknown
        else if chFuzzableType2 == Int64 then
          chFuzzableType1 match
            case UInt8 | UInt16 | UInt32 => Int64
            case UInt64                  => Int128
            case UInt128 | UInt256       => Int256
            case Int128 | Int256         => chFuzzableType1
            case _                       => throw exceptionIfUnknown
        else if chFuzzableType1 == Int128 then
          chFuzzableType2 match
            case UInt8 | UInt16 | UInt32 | UInt64 => Int128
            case UInt128 | UInt256                => Int256
            case Int256                           => Int256
            case _                                => throw exceptionIfUnknown
        else if chFuzzableType2 == Int128 then
          chFuzzableType1 match
            case UInt8 | UInt16 | UInt32 | UInt64 => Int128
            case UInt128 | UInt256                => Int256
            case Int256                           => Int256
            case _                                => throw exceptionIfUnknown
        else if chFuzzableType1 == Int256 then Int256
        else if chFuzzableType2 == Int256 then Int256
        // From now on, neither type1 nor type2 can be a signed integer
        else if chFuzzableType1 == UInt8 then
          chFuzzableType2 match
            case UInt16 | UInt32 | UInt64 | UInt128 | UInt256 => chFuzzableType2
            case _                                            => throw exceptionIfUnknown
        else if chFuzzableType2 == UInt8 then
          chFuzzableType1 match
            case UInt16 | UInt32 | UInt64 | UInt128 | UInt256 => chFuzzableType1
            case _                                            => throw exceptionIfUnknown
        else if chFuzzableType1 == UInt16 then
          chFuzzableType2 match
            case UInt32 | UInt64 | UInt128 | UInt256 => chFuzzableType2
            case _                                   => throw exceptionIfUnknown
        else if chFuzzableType2 == UInt16 then
          chFuzzableType1 match
            case UInt32 | UInt64 | UInt128 | UInt256 => chFuzzableType1
            case _                                   => throw exceptionIfUnknown
        else if chFuzzableType1 == UInt32 then
          chFuzzableType2 match
            case UInt64 | UInt128 | UInt256 => chFuzzableType2
            case _                          => throw exceptionIfUnknown
        else if chFuzzableType2 == UInt32 then
          chFuzzableType1 match
            case UInt64 | UInt128 | UInt256 => chFuzzableType1
            case _                          => throw exceptionIfUnknown
        else if chFuzzableType1 == UInt64 then
          chFuzzableType2 match
            case UInt128 | UInt256 => chFuzzableType2
            case _                 => throw exceptionIfUnknown
        else if chFuzzableType2 == UInt64 then
          chFuzzableType1 match
            case UInt128 | UInt256 => chFuzzableType1
            case _                 => throw exceptionIfUnknown
        else if chFuzzableType1 == UInt128 then
          chFuzzableType2 match
            case UInt256 => chFuzzableType2
            case _       => throw exceptionIfUnknown
        else if chFuzzableType2 == UInt128 then
          chFuzzableType1 match
            case UInt256 => chFuzzableType1
            case _       => throw exceptionIfUnknown
        // From now on, neither type1 nor type2 can be an unsigned integer
        else if chFuzzableType1 == Float32 then
          chFuzzableType2 match
            case Float64 => Float64
            case _       => throw exceptionIfUnknown
        else if chFuzzableType2 == Float32 then
          chFuzzableType1 match
            case Float64 => Float64
            case _       => throw exceptionIfUnknown
        // From now on, neither type1 nor type2 can be a float number
        else throw exceptionIfUnknown

      mergedType.name
