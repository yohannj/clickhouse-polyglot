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
      case "evalMLMethod" | "finalizeAggregation" | "initializeAggregation" | "runningAccumulate" |
          "uniqThetaIntersect" | "uniqThetaNot" | "uniqThetaUnion" =>
        // To be handled at a later time
        // It requires a new type being the result of a "-State" combinator
        // https://clickhouse.com/docs/en/sql-reference/aggregate-functions/reference/stochasticlinearregression
        // https://clickhouse.com/docs/en/sql-reference/functions/other-functions#finalizeaggregation
        // https://clickhouse.com/docs/en/sql-reference/functions/other-functions#initializeaggregation
        // https://clickhouse.com/docs/en/sql-reference/functions/other-functions#runningaccumulate
        // https://clickhouse.com/docs/en/sql-reference/functions/uniqtheta-functions#uniqthetaintersect
        // https://clickhouse.com/docs/en/sql-reference/functions/uniqtheta-functions#uniqthetanot
        // https://clickhouse.com/docs/en/sql-reference/functions/uniqtheta-functions#uniqthetaunion
        Future.successful(fn)
      case "isNotDistinctFrom" =>
        // To be handled at a later time
        // It requires supporting JOIN ON
        // https://clickhouse.com/docs/en/sql-reference/statements/select/join#null-values-in-join-keys
        Future.successful(fn)
      case "joinGet" | "joinGetOrNull" =>
        // To be handled at a later time
        // It requires a table created with ENGINE = Join(ANY, LEFT, <join_keys>)
        // https://clickhouse.com/docs/en/sql-reference/functions/other-functions#joinget
        Future.successful(fn)
      case "transactionLatestSnapshot" | "transactionOldestSnapshot" =>
        // To be handled at a later time
        // It requires the experimental "transactions" feature
        // https://clickhouse.com/docs/en/guides/developer/transactional#requirements
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

  def mergeOutputType(type1: CHType, type2: CHType): CHType =
    if type1 == type2 then type1 // Expects both type to be identical, should be the most obvious use case
    else
      val mergedType: CHType =
        if type1 == type2 then type1
        else if type1.isInstanceOf[CHSpecialType.Array] && type2.isInstanceOf[CHSpecialType.Array] then
          CHSpecialType.Array(
            mergeOutputType(
              type1.asInstanceOf[CHSpecialType.Array].innerType,
              type2.asInstanceOf[CHSpecialType.Array].innerType
            )
          )
        else if type1 == BooleanType then
          type2 match
            case UInt8 | UInt16 | UInt32 | UInt64 | UInt128 | UInt256 | Int16 | Int32 | Int64 | Int128 | Int256 =>
              type2
            case _ => CHAggregatedType.Any
        else if type2 == BooleanType then
          type1 match
            case UInt8 | UInt16 | UInt32 | UInt64 | UInt128 | UInt256 | Int16 | Int32 | Int64 | Int128 | Int256 =>
              type1
            case _ => CHAggregatedType.Any
        else if type1 == Int8 then
          type2 match
            case UInt8                                   => Int16
            case UInt16                                  => Int32
            case UInt32                                  => Int64
            case UInt64                                  => Int128
            case UInt128 | UInt256                       => Int256
            case Int16 | Int32 | Int64 | Int128 | Int256 => type2
            case _                                       => CHAggregatedType.Any
        else if type2 == Int8 then
          type1 match
            case UInt8                                   => Int16
            case UInt16                                  => Int32
            case UInt32                                  => Int64
            case UInt64                                  => Int128
            case UInt128 | UInt256                       => Int256
            case Int16 | Int32 | Int64 | Int128 | Int256 => type1
            case _                                       => CHAggregatedType.Any
        else if type1 == Int16 then
          type2 match
            case UInt8                           => Int16
            case UInt16                          => Int32
            case UInt32                          => Int64
            case UInt64                          => Int128
            case UInt128 | UInt256               => Int256
            case Int32 | Int64 | Int128 | Int256 => type2
            case _                               => CHAggregatedType.Any
        else if type2 == Int16 then
          type1 match
            case UInt8                           => Int16
            case UInt16                          => Int32
            case UInt32                          => Int64
            case UInt64                          => Int128
            case UInt128 | UInt256               => Int256
            case Int32 | Int64 | Int128 | Int256 => type1
            case _                               => CHAggregatedType.Any
        else if type1 == Int32 then
          type2 match
            case UInt8 | UInt16          => Int32
            case UInt32                  => Int64
            case UInt64                  => Int128
            case UInt128 | UInt256       => Int256
            case Int64 | Int128 | Int256 => type2
            case _                       => CHAggregatedType.Any
        else if type2 == Int32 then
          type1 match
            case UInt8 | UInt16          => Int32
            case UInt32                  => Int64
            case UInt64                  => Int128
            case UInt128 | UInt256       => Int256
            case Int64 | Int128 | Int256 => type1
            case _                       => CHAggregatedType.Any
        else if type1 == Int64 then
          type2 match
            case UInt8 | UInt16 | UInt32 => Int64
            case UInt64                  => Int128
            case UInt128 | UInt256       => Int256
            case Int128 | Int256         => type2
            case _                       => CHAggregatedType.Any
        else if type2 == Int64 then
          type1 match
            case UInt8 | UInt16 | UInt32 => Int64
            case UInt64                  => Int128
            case UInt128 | UInt256       => Int256
            case Int128 | Int256         => type1
            case _                       => CHAggregatedType.Any
        else if type1 == Int128 then
          type2 match
            case UInt8 | UInt16 | UInt32 | UInt64 => Int128
            case UInt128 | UInt256                => Int256
            case Int256                           => Int256
            case _                                => CHAggregatedType.Any
        else if type2 == Int128 then
          type1 match
            case UInt8 | UInt16 | UInt32 | UInt64 => Int128
            case UInt128 | UInt256                => Int256
            case Int256                           => Int256
            case _                                => CHAggregatedType.Any
        else if type1 == Int256 then Int256
        else if type2 == Int256 then Int256
        // From now on, neither type1 nor type2 can be a signed integer
        else if type1 == UInt8 then
          type2 match
            case UInt16 | UInt32 | UInt64 | UInt128 | UInt256 => type2
            case _                                            => CHAggregatedType.Any
        else if type2 == UInt8 then
          type1 match
            case UInt16 | UInt32 | UInt64 | UInt128 | UInt256 => type1
            case _                                            => CHAggregatedType.Any
        else if type1 == UInt16 then
          type2 match
            case UInt32 | UInt64 | UInt128 | UInt256 => type2
            case _                                   => CHAggregatedType.Any
        else if type2 == UInt16 then
          type1 match
            case UInt32 | UInt64 | UInt128 | UInt256 => type1
            case _                                   => CHAggregatedType.Any
        else if type1 == UInt32 then
          type2 match
            case UInt64 | UInt128 | UInt256 => type2
            case _                          => CHAggregatedType.Any
        else if type2 == UInt32 then
          type1 match
            case UInt64 | UInt128 | UInt256 => type1
            case _                          => CHAggregatedType.Any
        else if type1 == UInt64 then
          type2 match
            case UInt128 | UInt256 => type2
            case _                 => CHAggregatedType.Any
        else if type2 == UInt64 then
          type1 match
            case UInt128 | UInt256 => type1
            case _                 => CHAggregatedType.Any
        else if type1 == UInt128 then
          type2 match
            case UInt256 => type2
            case _       => CHAggregatedType.Any
        else if type2 == UInt128 then
          type1 match
            case UInt256 => type1
            case _       => CHAggregatedType.Any
        // From now on, neither type1 nor type2 can be an unsigned integer
        else if type1 == Float32 then
          type2 match
            case Float64 => Float64
            case _       => CHAggregatedType.Any
        else if type2 == Float32 then
          type1 match
            case Float64 => Float64
            case _       => CHAggregatedType.Any
        // From now on, neither type1 nor type2 can be a float number
        else if type1 == Date then
          type2 match
            case Date32 | DateTime | DateTime64 => type2
            case _                              => CHAggregatedType.Any
        else if type2 == Date then
          type1 match
            case Date32 | DateTime | DateTime64 => type1
            case _                              => CHAggregatedType.Any
        else if type1 == DateTime then
          type2 match
            case DateTime64 => DateTime64
            case _          => CHAggregatedType.Any
        else if type2 == Date then
          type1 match
            case DateTime64 => DateTime64
            case _          => CHAggregatedType.Any
        else CHAggregatedType.Any

      mergedType

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
      case Seq()   => Seq("")
      case Seq(el) => el
      case Seq(head, tail @ _*) =>
        val subChoices: Seq[String] = buildFuzzingValuesArgs(tail)
        head.flatMap(el => subChoices.map(subChoice => s"$el, $subChoice"))

  private def checkIsParametric(fnName: String)(
      using client: CHClient,
      ec: ExecutionContext
  ): Future[Boolean] =
    val hardcodedNonParametricFnNames = Seq("rankCorr")

    if hardcodedNonParametricFnNames.contains(fnName)
    then Future.successful(false)
    else
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
