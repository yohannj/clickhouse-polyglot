package com.amendil.signature.fuzz

import com.amendil.common.entities.CHSettingWithValue
import com.amendil.common.entities.`type`.CHFuzzableType
import com.amendil.common.entities.function.CHFunctionIO
import com.amendil.common.helper.ConcurrencyUtils.*
import com.amendil.common.http.CHClient
import com.amendil.signature.Settings
import com.amendil.signature.entities.*
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}

object Fuzzer extends StrictLogging:

  def fuzz(fn: CHFunctionFuzzResult)(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    fn.name match
      case "range" =>
        // Ends up in OOM, to be handled at another time
        Future.successful(fn)
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
    val hardcodedNonParametricFnNames =
      Seq(
        "contingency",
        "cramersV",
        "cramersVBiasCorrected",
        "globalIn",
        "globalInIgnoreSet",
        "globalNotIn",
        "globalNotInIgnoreSet",
        "globalNotNullIn",
        "globalNotNullInIgnoreSet",
        "globalNullIn",
        "globalNullInIgnoreSet",
        "groupArrayIntersect",
        "in",
        "inIgnoreSet",
        "maxIntersections",
        "maxIntersectionsPosition",
        "maxMappedArrays",
        "minMappedArrays",
        "notIn",
        "notInIgnoreSet",
        "notNullIn",
        "notNullInIgnoreSet",
        "nth_value",
        "nullIn",
        "nullInIgnoreSet",
        "rankCorr",
        "sumMappedArrays",
        "sumMapWithOverflow",
        "theilsU"
      )

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

  private[fuzz] def detectMandatorySettingsFromSampleInput(
      fnName: String,
      paramsOpt: Option[String] = None,
      args: String,
      fuzzOverWindow: Boolean,
      sourceTable: Option[String] = None
  )(using client: CHClient, ec: ExecutionContext): Future[Set[CHSettingWithValue[Boolean]]] =
    // Idea: test remove one setting and test if the query still works. If not => the removed settings was mandatory
    val query = Fuzzer.query(fnName, paramsOpt, args, fuzzOverWindow, sourceTable)
    executeInSequence(
      CHClient.unlockFunctionsSettingNames,
      setting =>
        val otherSettings = CHClient.unlockFunctionsSettingNames.filter(_ != setting).map(_.apply(true))

        client.executeNoResult(query, otherSettings).map(_ => None).recover(_ => Some(setting.apply(true)))
    ).map(_.flatten.toSet)

  private[fuzz] def detectMandatorySettingsFromSampleFunction(
      fnName: String,
      sampleFn: CHFunctionIO,
      fuzzOverWindow: Boolean,
      sourceTable: Option[String] = None
  )(using client: CHClient, ec: ExecutionContext): Future[Set[CHSettingWithValue[Boolean]]] =
    for
      (paramsOpt, args) <- findAnyValidInput(fnName, sampleFn, fuzzOverWindow)
      res <- detectMandatorySettingsFromSampleInput(fnName, paramsOpt, args, fuzzOverWindow, sourceTable)
    yield res

  private[fuzz] def parametricQuery(
      fnName: String,
      params: String,
      args: String,
      fuzzOverWindow: Boolean,
      sourceTable: Option[String] = None
  ): String =
    val innerQuery =
      if fuzzOverWindow then
        s"SELECT $fnName($params)($args) OVER w1 as r, toTypeName(r) as type${sourceTable.map(" FROM " + _).getOrElse("")} WINDOW w1 AS ()"
      else s"SELECT $fnName($params)($args) as r, toTypeName(r) as type${sourceTable.map(" FROM " + _).getOrElse("")}"

    s"SELECT if(type = 'UInt8' AND (r::Dynamic = 0 OR r::Dynamic = 1), 'Bool', type) as type FROM ($innerQuery)"

  private[fuzz] def query(
      fnName: String,
      args: String,
      fuzzOverWindow: Boolean,
      sourceTable: Option[String] = None
  ): String =
    val innerQuery =
      if fuzzOverWindow then
        s"SELECT $fnName($args) OVER w1 as r, toTypeName(r) as type${sourceTable.map(" FROM " + _).getOrElse("")} WINDOW w1 AS ()"
      else s"SELECT $fnName($args) as r, toTypeName(r) as type${sourceTable.map(" FROM " + _).getOrElse("")}"

    s"SELECT if(type = 'UInt8' AND (r::Dynamic = 0 OR r::Dynamic = 1), 'Bool', type) as type FROM ($innerQuery)"

  private[fuzz] def testSampleInputWithOverWindow(
      fnName: String,
      paramsOpt: Option[String] = None,
      args: String,
      sourceTable: Option[String] = None
  )(using client: CHClient, ec: ExecutionContext): Future[Boolean] =
    val query = Fuzzer.query(fnName, paramsOpt, args, fuzzOverWindow = true, sourceTable = sourceTable)
    client.executeNoResult(query).map(_ => true).recover(_ => false)

  private[fuzz] def testSampleFunctionWithOverWindow(
      fnName: String,
      sampleFn: CHFunctionIO,
      sourceTable: Option[String] = None
  )(using client: CHClient, ec: ExecutionContext): Future[Boolean] =
    for
      (paramsOpt, args) <- findAnyValidInput(fnName, sampleFn, fuzzOverWindow = false)
      res <- testSampleInputWithOverWindow(fnName, paramsOpt, args, sourceTable)
    yield res

  private def findAnyValidInput(fnName: String, fn: CHFunctionIO, fuzzOverWindow: Boolean)(
      using client: CHClient,
      ec: ExecutionContext
  ): Future[(Option[String], String)] =
    val inputAndQuery =
      if fn.isParametric then
        val fuzzingValuesParams = buildFuzzingValuesArgs(
          fn.parameters.asInstanceOf[Seq[CHFuzzableType]].map(_.fuzzingValues)
        )
        val fuzzingValuesArgs = buildFuzzingValuesArgs(
          fn.arguments.asInstanceOf[Seq[CHFuzzableType]].map(_.fuzzingValues)
        )

        crossJoin(fuzzingValuesParams, fuzzingValuesArgs).map { (params, args) =>
          (Some(params), args, parametricQuery(fnName, params, args, fuzzOverWindow))
        }
      else
        buildFuzzingValuesArgs(fn.arguments.asInstanceOf[Seq[CHFuzzableType]].map(_.fuzzingValues))
          .map(args => (None, args, query(fnName, args, fuzzOverWindow)))

    executeInParallelUntilSuccess(
      inputAndQuery,
      (paramsOpt, args, query) => client.executeNoResult(query).map(_ => (paramsOpt, args)),
      maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
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

  private def query(
      fnName: String,
      paramsOpt: Option[String],
      args: String,
      fuzzOverWindow: Boolean,
      sourceTable: Option[String]
  ): String =
    paramsOpt match
      case None         => query(fnName, args, fuzzOverWindow, sourceTable)
      case Some(params) => parametricQuery(fnName, params, args, fuzzOverWindow, sourceTable)
