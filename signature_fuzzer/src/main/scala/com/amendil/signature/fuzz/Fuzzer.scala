package com.amendil.signature.fuzz

import com.amendil.common.ConcurrencyUtils.executeChain
import com.amendil.common.entities.`type`.CHFuzzableType
import com.amendil.common.http.CHClient
import com.amendil.signature.entities.*
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}

object Fuzzer extends StrictLogging:

  def fuzz(fn: CHFunctionFuzzResult)(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    fn.name match
      case "randBinomial" | "hop" | "hopStart" | "hopEnd" | "windowID" =>
        // To be handled at a later time
        // Known issues
        Future.successful(fn)
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
              isParametric <- checkIsParametric(fn)
              fuzzingFunctionsWithCost: Seq[(CHFunctionFuzzResult => Future[CHFunctionFuzzResult], Long)] =
                if isParametric then FuzzerParametricFunctions.fuzzingFunctionWithCost
                else
                  FuzzerSpecialFunctions.fuzzingFunctionWithCost ++
                    FuzzerLambdaFunctions.fuzzingFunctionWithCost ++
                    FuzzerNonParametricFunctions.fuzzingFunctionWithCost

              sortedFuzzingFunctions = fuzzingFunctionsWithCost.sortBy(_._2).map(_._1)

              fuzzedFn <- executeChain(fn, sortedFuzzingFunctions)
            yield fuzzedFn
        }

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
      case Seq(head, tail*) =>
        head.chFuzzableTypes
          .map(chFuzzableType => generateCHFuzzableTypeCombinations(tail, currentArgs :+ chFuzzableType))
          .flatten
      case Seq() => Seq(currentArgs)

  private[fuzz] def buildFuzzingValuesArgs(argumentsValues: Seq[Seq[String]]): Seq[String] =
    argumentsValues match
      case Seq()   => Seq("")
      case Seq(el) => el
      case Seq(head, tail*) =>
        val subChoices: Seq[String] = buildFuzzingValuesArgs(tail)
        head.flatMap(el => subChoices.map(subChoice => s"$el, $subChoice"))

  private def checkIsParametric(fn: CHFunctionFuzzResult)(
      using client: CHClient,
      ec: ExecutionContext
  ): Future[Boolean] =
    val hardcodedNonParametricFnNames = Seq("rankCorr")

    if hardcodedNonParametricFnNames.contains(fn.name) || hardcodedNonParametricFnNames.contains(fn.aliasTo)
    then Future.successful(false)
    else
      client
        .executeNoResult(s"SELECT toTypeName(${fn.name}(1)(1))")
        .map(_ => true) // Coincidence: we used a valid parametric signature for the test
        .recover { err =>
          // Those messages were initially found by looking at `!parameters.empty()` in ClickHouse codebase.
          // The idea is to help detect as early as possible when the function is not parametric for sure.
          // In case we miss an error message, it is expected for the Fuzzer to be unable to determine any signature,
          // that way we receive an error log about it and we can update this list.
          val nonParametricErrorMessages = Seq(
            s"Aggregate function ${fn.name} cannot have parameters",
            s"Aggregate function ${fn.aliasTo} cannot have parameters",
            s"Executable user defined functions with `executable_pool` type does not support parameters",
            s"Function ${fn.name} cannot be parameterized",
            s"Function ${fn.aliasTo} cannot be parameterized",
            s"Function ${fn.name} is not parametric",
            s"Function ${fn.aliasTo} is not parametric",
            s"Incorrect number of parameters for aggregate function ${fn.name}, should be 0",
            s"Incorrect number of parameters for aggregate function ${fn.aliasTo}, should be 0",
            s"Parameters are not supported if executable user defined function is not direct",
            s"Expected one of: token, "
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
