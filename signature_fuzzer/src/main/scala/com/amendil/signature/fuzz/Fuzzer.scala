package com.amendil.signature.fuzz

import com.amendil.common.ConcurrencyUtils.executeChain
import com.amendil.common.entities._
import com.amendil.common.entities.CHFuzzableType._
import com.amendil.common.http.CHClient
import com.amendil.signature.entities._
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}

object Fuzzer extends StrictLogging:

  def fuzz(functionName: String)(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val fn = CHFunctionFuzzResult(functionName)
    functionName match
      case "evalMLMethod" =>
        // To be handled at a later time
        // It requires a new type as the first argument must be the result of a "-State" combinator
        // https://clickhouse.com/docs/en/sql-reference/aggregate-functions/reference/stochasticlinearregression
        Future.successful(fn)
      case "__getScalar" =>
        // Internal function
        Future.successful(fn)
      case _ =>
        FuzzerHardcodedFunctions.fuzz(fn).flatMap { res =>
          if res.atLeastOneSignatureFound then Future.successful(res)
          else
            for
              isParametric <- checkIsParametric(fn.name)
              fuzzingFunctionsWithCost: Seq[(CHFunctionFuzzResult => Future[CHFunctionFuzzResult], Long)] =
                if isParametric then FuzzerParametricFunctions.fuzzingFunctionWithCost
                else
                  FuzzerSpecialFunctions.fuzzingFunctionWithCost ++
                    FuzzerLambdaFunctions.fuzzingFunctionWithCost ++
                    FuzzerNonParametricFunctions.fuzzingFunctionWithCost

              sortedFuzzingFunctions = fuzzingFunctionsWithCost.sortBy(_._2).map(_._1)

              fuzzedFn <- executeChain(fn, sortedFuzzingFunctions)
            yield {
              fuzzedFn
            }
        }

  def mergeOutputType(type1: String, type2: String): String =
    lazy val exceptionIfUnknown = {
      val errMsg = s"Unable to determine higher type for $type1 and $type2"
      logger.error(errMsg)
      IllegalArgumentException(errMsg)
    }
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
        else if chFuzzableType1 == Date then
          chFuzzableType2 match
            case Date32 | DateTime | DateTime64 => chFuzzableType2
            case _                              => throw exceptionIfUnknown
        else if chFuzzableType2 == Date then
          chFuzzableType1 match
            case Date32 | DateTime | DateTime64 => chFuzzableType1
            case _                              => throw exceptionIfUnknown
        else if chFuzzableType1 == DateTime then
          chFuzzableType2 match
            case DateTime64 => DateTime64
            case _          => throw exceptionIfUnknown
        else if chFuzzableType2 == Date then
          chFuzzableType1 match
            case DateTime64 => DateTime64
            case _          => throw exceptionIfUnknown
        else throw exceptionIfUnknown

      mergedType.name

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

  private def checkIsParametric(fnName: String)(
      using client: CHClient,
      ec: ExecutionContext
  ): Future[Boolean] =
    client
      .executeNoResult(s"SELECT toTypeName($fnName(1)(1))")
      .map(_ => true) // Coincidence: we used a valid parametric signature for the test
      .recover { err =>
        // Those messages were initially found by looking at `!parameters.empty()` in ClickHouse codebase.
        // The idea is to help detect as early as possible when the function is not parametric for sure.
        // In case we miss an error message, it is expected for the Fuzzer to be unable to determine any signature,
        // that way we receive an error log about it and we can update this list.
        val nonParametricErrorMessages = Seq(
          s"Aggregate function $fnName cannot have parameters",
          s"Executable user defined functions with `executable_pool` type does not support parameters",
          s"Function $fnName cannot be parameterized",
          s"Function $fnName is not parametric",
          s"Incorrect number of parameters for aggregate function $fnName, should be 0",
          s"Parameters are not supported if executable user defined function is not direct"
        )

        !nonParametricErrorMessages.exists(err.getMessage().contains)
      }

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
