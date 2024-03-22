package com.amendil.fuzz

import com.amendil.ConcurrencyUtils._
import com.amendil.Settings
import com.amendil.entities._
import com.amendil.entities.CHFunctionIO._
import com.amendil.fuzz.Fuzzer._
import com.amendil.http.CHClient
import com.typesafe.scalalogging.StrictLogging

import scala.annotation.targetName
import scala.concurrent.{ExecutionContext, Future}

object FuzzerParametricFunctions extends StrictLogging:

  // Remove some types that are obviously not parameters
  private val parametricAbstractType: Seq[CHFuzzableAbstractType] = CHFuzzableAbstractType.values.toSeq.filterNot {
    abstractType =>
      abstractType.fuzzingValues.head.contains("::Array(") ||
      abstractType.fuzzingValues.head.contains("array(") ||
      abstractType.fuzzingValues.head.contains("::Map(") ||
      abstractType.fuzzingValues.head.contains("map(") ||
      abstractType.fuzzingValues.head.contains("::Tuple(") ||
      abstractType.fuzzingValues.head.contains("::Date") ||
      abstractType.fuzzingValues.head.contains("::IPv4") ||
      abstractType.fuzzingValues.head.contains("::IPv6") ||
      abstractType.fuzzingValues.head.contains("::JSON") ||
      abstractType.fuzzingValues.head.contains("::UUID") ||
      abstractType.fuzzingValues.head.contains("::Enum") ||
      abstractType.fuzzingValues.head.contains("::Point") ||
      abstractType.fuzzingValues.head.contains("::Ring") ||
      abstractType.fuzzingValues.head.contains("::Polygon") ||
      abstractType.fuzzingValues.head.contains("::MultiPolygon") ||
      abstractType.fuzzingValues.head.contains("::Interval")
  }

  private[fuzz] def fuzzingFunctionWithCost(
      using client: CHClient,
      ec: ExecutionContext
  ): Seq[((CHFunctionFuzzResult) => Future[CHFunctionFuzzResult], Long)] =
    val paramCount = parametricAbstractType.flatMap(_.chFuzzableTypes).size.toLong
    val argCount = CHFuzzableType.values.size.toLong
    Seq(
      (fuzzFunction1Or0NWithOneParameter, paramCount * argCount),
      (fuzzFunction2Or1NWithOneParameter, paramCount * argCount * argCount),
      (fuzzFunction3Or2NWithOneParameter, paramCount * argCount * argCount * argCount),
      // Functions below MUST happen after the "WithOneParameter", they are then very easy to compute
      (fuzzFunction1WithTwoParameters, paramCount * argCount + 1),
      (fuzzFunction0NWithTwoParameters, paramCount * argCount + 1),
      (fuzzFunction2WithTwoParameters, paramCount * argCount * argCount + 1),
      (fuzzFunction1NWithTwoParameters, paramCount * argCount * argCount + 1),
      (fuzzFunction3WithTwoParameters, paramCount * argCount * argCount * argCount + 1),
      (fuzzFunction2NWithTwoParameters, paramCount * argCount * argCount * argCount + 1),
      // Functions below MUST happen after the "WithTwoParameter", they are then very easy to compute
      (fuzzFunction1WithThreeParameters, paramCount * argCount + 2),
      (fuzzFunction0NWithThreeParameters, paramCount * argCount + 2),
      (fuzzFunction2WithThreeParameters, paramCount * argCount * argCount + 2),
      (fuzzFunction1NWithThreeParameters, paramCount * argCount * argCount + 2),
      (fuzzFunction3WithThreeParameters, paramCount * argCount * argCount * argCount + 2),
      (fuzzFunction2NWithThreeParameters, paramCount * argCount * argCount * argCount + 2),
      // Functions below MUST happen after the "WithThreeParameter", they are then very easy to compute
      (fuzzFunction1WithFourParameters, paramCount * argCount + 3),
      (fuzzFunction0NWithFourParameters, paramCount * argCount + 3),
      (fuzzFunction2WithFourParameters, paramCount * argCount * argCount + 3),
      (fuzzFunction1NWithFourParameters, paramCount * argCount * argCount + 3),
      (fuzzFunction3WithFourParameters, paramCount * argCount * argCount * argCount + 3),
      (fuzzFunction2NWithFourParameters, paramCount * argCount * argCount * argCount + 3),
      // Functions below MUST happen after the "WithFourParameter", they are then very easy to compute
      (fuzzFunction1WithFiveParameters, paramCount * argCount + 4),
      (fuzzFunction0NWithFiveParameters, paramCount * argCount + 4),
      (fuzzFunction2WithFiveParameters, paramCount * argCount * argCount + 4),
      (fuzzFunction1NWithFiveParameters, paramCount * argCount * argCount + 4),
      (fuzzFunction3WithFiveParameters, paramCount * argCount * argCount * argCount + 4),
      (fuzzFunction2NWithFiveParameters, paramCount * argCount * argCount * argCount + 4)
    )

  private def fuzzFunction1Or0NWithOneParameter(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1Or0NWithOneParameter")
    if fn.isLambda || fn.isNonParametric || fn.isSpecialInfiniteFunction then Future.successful(fn)
    else
      for
        functions: Seq[(ParametricFunctionInput, OutputType)] <-
          fuzzParametricFunction(fn.name, paramCount = 1, argCount = 1)
        fnHasInfiniteArgs: Boolean <-
          if functions.isEmpty then Future.successful(true) else testInfiniteArgsFunctions(fn.name, functions.head._1)
      yield {
        def toFn[T <: CHFunctionIO](
            io: (ParametricFunctionInput, OutputType),
            parametricFunctionConstructor: (CHFuzzableType, CHFuzzableType, String) => T
        ): T =
          io._1 match
            case (Seq(param1), Seq(arg1)) => parametricFunctionConstructor(param1, arg1, io._2)
            case _ =>
              throw new Exception(
                s"Expected 1 parameter and 1 argument, but found ${io._1.parameters.size} parameters and ${io._1.arguments.size} arguments"
              )

        if fnHasInfiniteArgs then
          fn.copy(parametric1Function0Ns = functions.map(toFn(_, CHFunctionIO.Parametric1Function0N.apply)))
        else fn.copy(parametric1Function1s = functions.map(toFn(_, CHFunctionIO.Parametric1Function1.apply)))
      }

  private def fuzzFunction2Or1NWithOneParameter(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2Or1NWithOneParameter")
    if fn.isLambda || fn.isNonParametric || fn.isSpecialInfiniteFunction || fn.parametric1Function0Ns.nonEmpty then
      Future.successful(fn)
    else
      for
        functions: Seq[(ParametricFunctionInput, OutputType)] <-
          fuzzParametricFunction(fn.name, paramCount = 1, argCount = 2)
        fnHasInfiniteArgs: Boolean <-
          if functions.isEmpty then Future.successful(true) else testInfiniteArgsFunctions(fn.name, functions.head._1)
      yield {
        def toFn[T <: CHFunctionIO](
            io: (ParametricFunctionInput, OutputType),
            parametricFunctionConstructor: (CHFuzzableType, CHFuzzableType, CHFuzzableType, String) => T
        ): T =
          io._1 match
            case (Seq(param1), Seq(arg1, arg2)) => parametricFunctionConstructor(param1, arg1, arg2, io._2)
            case _ =>
              throw new Exception(
                s"Expected 1 parameter and 2 arguments, but found ${io._1.parameters.size} parameters and ${io._1.arguments.size} arguments"
              )

        if fnHasInfiniteArgs then
          fn.copy(parametric1Function1Ns = functions.map(toFn(_, CHFunctionIO.Parametric1Function1N.apply)))
        else fn.copy(parametric1Function2s = functions.map(toFn(_, CHFunctionIO.Parametric1Function2.apply)))
      }

  private def fuzzFunction3Or2NWithOneParameter(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction3Or2NWithOneParameter")
    if fn.isLambda || fn.isNonParametric || fn.isSpecialInfiniteFunction ||
      fn.parametric1Function0Ns.nonEmpty || fn.parametric1Function1Ns.nonEmpty
    then Future.successful(fn)
    else
      for
        functions: Seq[(ParametricFunctionInput, OutputType)] <-
          fuzzParametricFunction(fn.name, paramCount = 1, argCount = 3)
        fnHasInfiniteArgs: Boolean <-
          if functions.isEmpty then Future.successful(true) else testInfiniteArgsFunctions(fn.name, functions.head._1)
      yield {
        def toFn[T <: CHFunctionIO](
            io: (ParametricFunctionInput, OutputType),
            parametricFunctionConstructor: (CHFuzzableType, CHFuzzableType, CHFuzzableType, CHFuzzableType, String) => T
        ): T =
          io._1 match
            case (Seq(param1), Seq(arg1, arg2, arg3)) => parametricFunctionConstructor(param1, arg1, arg2, arg3, io._2)
            case _ =>
              throw new Exception(
                s"Expected 1 parameter and 3 arguments, but found ${io._1.parameters.size} parameters and ${io._1.arguments.size} arguments"
              )

        if fnHasInfiniteArgs then
          fn.copy(parametric1Function2Ns = functions.map(toFn(_, CHFunctionIO.Parametric1Function2N.apply)))
        else fn.copy(parametric1Function3s = functions.map(toFn(_, CHFunctionIO.Parametric1Function3.apply)))
      }

  private def fuzzFunction1WithTwoParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1WithTwoParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric1Function1s,
      (f: Parametric1Function1, param: CHFuzzableType) =>
        CHFunctionIO.Parametric2Function1(f.paramArg1, param, f.arg1, f.output)
    ).map(res => fn.copy(parametric2Function1s = res))

  private def fuzzFunction1WithThreeParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1WithThreeParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric2Function1s,
      (f: Parametric2Function1, param: CHFuzzableType) =>
        CHFunctionIO.Parametric3Function1(f.paramArg1, f.paramArg2, param, f.arg1, f.output)
    ).map(res => fn.copy(parametric3Function1s = res))

  private def fuzzFunction1WithFourParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1WithFourParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric3Function1s,
      (f: Parametric3Function1, param: CHFuzzableType) =>
        CHFunctionIO.Parametric4Function1(f.paramArg1, f.paramArg2, f.paramArg3, param, f.arg1, f.output)
    ).map(res => fn.copy(parametric4Function1s = res))

  private def fuzzFunction1WithFiveParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1WithFiveParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric4Function1s,
      (f: Parametric4Function1, param: CHFuzzableType) =>
        CHFunctionIO.Parametric5Function1(f.paramArg1, f.paramArg2, f.paramArg3, f.paramArg4, param, f.arg1, f.output)
    ).map(res => fn.copy(parametric5Function1s = res))

  private def fuzzFunction2WithTwoParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2WithTwoParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric1Function2s,
      (f: Parametric1Function2, param: CHFuzzableType) =>
        CHFunctionIO.Parametric2Function2(f.paramArg1, param, f.arg1, f.arg2, f.output)
    ).map(res => fn.copy(parametric2Function2s = res))

  private def fuzzFunction2WithThreeParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2WithThreeParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric2Function2s,
      (f: Parametric2Function2, param: CHFuzzableType) =>
        CHFunctionIO.Parametric3Function2(f.paramArg1, f.paramArg2, param, f.arg1, f.arg2, f.output)
    ).map(res => fn.copy(parametric3Function2s = res))

  private def fuzzFunction2WithFourParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2WithFourParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric3Function2s,
      (f: Parametric3Function2, param: CHFuzzableType) =>
        CHFunctionIO.Parametric4Function2(f.paramArg1, f.paramArg2, f.paramArg3, param, f.arg1, f.arg2, f.output)
    ).map(res => fn.copy(parametric4Function2s = res))

  private def fuzzFunction2WithFiveParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2WithFiveParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric4Function2s,
      (f: Parametric4Function2, param: CHFuzzableType) =>
        CHFunctionIO.Parametric5Function2(
          f.paramArg1,
          f.paramArg2,
          f.paramArg3,
          f.paramArg4,
          param,
          f.arg1,
          f.arg2,
          f.output
        )
    ).map(res => fn.copy(parametric5Function2s = res))

  private def fuzzFunction3WithTwoParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction3WithTwoParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric1Function3s,
      (f: Parametric1Function3, param: CHFuzzableType) =>
        CHFunctionIO.Parametric2Function3(f.paramArg1, param, f.arg1, f.arg2, f.arg3, f.output)
    ).map(res => fn.copy(parametric2Function3s = res))

  private def fuzzFunction3WithThreeParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction3WithThreeParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric2Function3s,
      (f: Parametric2Function3, param: CHFuzzableType) =>
        CHFunctionIO.Parametric3Function3(f.paramArg1, f.paramArg2, param, f.arg1, f.arg2, f.arg3, f.output)
    ).map(res => fn.copy(parametric3Function3s = res))

  private def fuzzFunction3WithFourParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction3WithFourParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric3Function3s,
      (f: Parametric3Function3, param: CHFuzzableType) =>
        CHFunctionIO.Parametric4Function3(
          f.paramArg1,
          f.paramArg2,
          f.paramArg3,
          param,
          f.arg1,
          f.arg2,
          f.arg3,
          f.output
        )
    ).map(res => fn.copy(parametric4Function3s = res))

  private def fuzzFunction3WithFiveParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction3WithFiveParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric4Function3s,
      (f: Parametric4Function3, param: CHFuzzableType) =>
        CHFunctionIO.Parametric5Function3(
          f.paramArg1,
          f.paramArg2,
          f.paramArg3,
          f.paramArg4,
          param,
          f.arg1,
          f.arg2,
          f.arg3,
          f.output
        )
    ).map(res => fn.copy(parametric5Function3s = res))

  private def fuzzFunction0NWithTwoParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction0NWithTwoParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric1Function0Ns,
      (f: Parametric1Function0N, param: CHFuzzableType) =>
        CHFunctionIO.Parametric2Function0N(f.paramArg1, param, f.argN, f.output)
    ).map(res => fn.copy(parametric2Function0Ns = res))

  private def fuzzFunction0NWithThreeParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction0NWithThreeParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric2Function0Ns,
      (f: Parametric2Function0N, param: CHFuzzableType) =>
        CHFunctionIO.Parametric3Function0N(f.paramArg1, f.paramArg2, param, f.argN, f.output)
    ).map(res => fn.copy(parametric3Function0Ns = res))

  private def fuzzFunction0NWithFourParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction0NWithFourParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric3Function0Ns,
      (f: Parametric3Function0N, param: CHFuzzableType) =>
        CHFunctionIO.Parametric4Function0N(f.paramArg1, f.paramArg2, f.paramArg3, param, f.argN, f.output)
    ).map(res => fn.copy(parametric4Function0Ns = res))

  private def fuzzFunction0NWithFiveParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction0NWithFiveParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric4Function0Ns,
      (f: Parametric4Function0N, param: CHFuzzableType) =>
        CHFunctionIO.Parametric5Function0N(f.paramArg1, f.paramArg2, f.paramArg3, f.paramArg4, param, f.argN, f.output)
    ).map(res => fn.copy(parametric5Function0Ns = res))

  private def fuzzFunction1NWithTwoParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1NWithTwoParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric1Function1Ns,
      (f: Parametric1Function1N, param: CHFuzzableType) =>
        CHFunctionIO.Parametric2Function1N(f.paramArg1, param, f.arg1, f.argN, f.output)
    ).map(res => fn.copy(parametric2Function1Ns = res))

  private def fuzzFunction1NWithThreeParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1NWithThreeParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric2Function1Ns,
      (f: Parametric2Function1N, param: CHFuzzableType) =>
        CHFunctionIO.Parametric3Function1N(f.paramArg1, f.paramArg2, param, f.arg1, f.argN, f.output)
    ).map(res => fn.copy(parametric3Function1Ns = res))

  private def fuzzFunction1NWithFourParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1NWithFourParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric3Function1Ns,
      (f: Parametric3Function1N, param: CHFuzzableType) =>
        CHFunctionIO.Parametric4Function1N(f.paramArg1, f.paramArg2, f.paramArg3, param, f.arg1, f.argN, f.output)
    ).map(res => fn.copy(parametric4Function1Ns = res))

  private def fuzzFunction1NWithFiveParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1NWithFiveParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric4Function1Ns,
      (f: Parametric4Function1N, param: CHFuzzableType) =>
        CHFunctionIO.Parametric5Function1N(
          f.paramArg1,
          f.paramArg2,
          f.paramArg3,
          f.paramArg4,
          param,
          f.arg1,
          f.argN,
          f.output
        )
    ).map(res => fn.copy(parametric5Function1Ns = res))

  private def fuzzFunction2NWithTwoParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2NWithTwoParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric1Function2Ns,
      (f: Parametric1Function2N, param: CHFuzzableType) =>
        CHFunctionIO.Parametric2Function2N(f.paramArg1, param, f.arg1, f.arg2, f.argN, f.output)
    ).map(res => fn.copy(parametric2Function2Ns = res))

  private def fuzzFunction2NWithThreeParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2NWithThreeParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric2Function2Ns,
      (f: Parametric2Function2N, param: CHFuzzableType) =>
        CHFunctionIO.Parametric3Function2N(f.paramArg1, f.paramArg2, param, f.arg1, f.arg2, f.argN, f.output)
    ).map(res => fn.copy(parametric3Function2Ns = res))

  private def fuzzFunction2NWithFourParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2NWithFourParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric3Function2Ns,
      (f: Parametric3Function2N, param: CHFuzzableType) =>
        CHFunctionIO.Parametric4Function2N(
          f.paramArg1,
          f.paramArg2,
          f.paramArg3,
          param,
          f.arg1,
          f.arg2,
          f.argN,
          f.output
        )
    ).map(res => fn.copy(parametric4Function2Ns = res))

  private def fuzzFunction2NWithFiveParameters(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2NWithFiveParameters")
    fuzzAddOneParameter(
      fn.name,
      fn.parametric4Function2Ns,
      (f: Parametric4Function2N, param: CHFuzzableType) =>
        CHFunctionIO.Parametric5Function2N(
          f.paramArg1,
          f.paramArg2,
          f.paramArg3,
          f.paramArg4,
          param,
          f.arg1,
          f.arg2,
          f.argN,
          f.output
        )
    ).map(res => fn.copy(parametric5Function2Ns = res))

  /**
    * Test all combination of parametric functions containing paramCount parameters and argCount arguments
    *
    * @param fnName Name of the function to fuzz
    * @return All combinations that worked and their output type
    */
  private def fuzzParametricFunction(
      fnName: String,
      paramCount: Int,
      argCount: Int
  )(
      using client: CHClient,
      ec: ExecutionContext
  ): Future[Seq[(ParametricFunctionInput, OutputType)]] =
    // Build all combinations of parametric fonction input having paramCount parameters and argCount arguments
    // Those combinations are described using AbstractTypes!
    // They are all used to query ClickHouse and we are retrieving here only the ones that succeeded.
    val validCHFuzzableAbstractTypeCombinationsF: Future[Seq[ParametricFunctionAbstractInput]] =
      executeInParallelOnlySuccess(
        crossJoin(
          generateCHFuzzableAbstractTypeCombinations(paramCount, parametricAbstractType),
          generateCHFuzzableAbstractTypeCombinations(argCount)
        ),
        (paramTypes: Seq[CHFuzzableAbstractType], nonParamTypes: Seq[CHFuzzableAbstractType]) => {
          executeInSequenceUntilSuccess(
            crossJoin(
              buildFuzzingValuesArgs(paramTypes.map(_.fuzzingValues)),
              buildFuzzingValuesArgs(nonParamTypes.map(_.fuzzingValues))
            ).map { case (paramArgs, nonParamArgs) => s"SELECT $fnName($paramArgs)($nonParamArgs)" },
            client.execute
          ).map(_ => (paramTypes, nonParamTypes))
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      )

    // Expand abstract types to retrieve all types combinations and their output
    validCHFuzzableAbstractTypeCombinationsF.flatMap:
      (validCHFuzzableAbstractTypeCombinations: Seq[ParametricFunctionAbstractInput]) =>
        if validCHFuzzableAbstractTypeCombinations.isEmpty then Future.successful(Nil)
        else
          assume(
            validCHFuzzableAbstractTypeCombinations
              .groupBy(_.parameters)
              .values
              .map( // For each combination of parameters, we retrieve all the kind of available arguments
                _.map(_.arguments)
              )
              .toSet
              .size == 1, // All combination of parameters should support exactly the same combinations of arguments.
            s"While fuzzing $fnName with $paramCount parameters and $argCount arguments," +
              s" found some combinations of parameters that does not support the same combinations of arguments."
          )

          // As we previously assessed arguments and parameters have no relationship whatsoever
          // We will now fuzz their exact type (non abstract) separately.
          val argumentsAndSqlQuery: Seq[(NonParametricArguments, Seq[String])] =
            validCHFuzzableAbstractTypeCombinations
              .groupBy(_.parameters)
              .values
              .head // We only need one kind of parameters!
              .flatMap { (input: ParametricFunctionAbstractInput) =>
                generateCHFuzzableTypeCombinations(input.arguments)
                  .map((input.parameters, _))
              }
              .map { case (paramAbstractTypes, nonParamTypes) =>
                val queries = crossJoin(
                  buildFuzzingValuesArgs(paramAbstractTypes.map(_.fuzzingValues)),
                  buildFuzzingValuesArgs(nonParamTypes.map(_.fuzzingValues))
                ).map { case (paramArgs, nonParamArgs) => s"SELECT $fnName($paramArgs)($nonParamArgs)" }

                (nonParamTypes, queries)
              }
          val parametersAndSqlQuery: Seq[(ParametricArguments, Seq[String])] =
            validCHFuzzableAbstractTypeCombinations
              .groupBy(_.arguments)
              .values
              .head // We only need one kind of arguments!
              .flatMap { (input: ParametricFunctionAbstractInput) =>
                generateCHFuzzableTypeCombinations(input.parameters)
                  .map((_, input.arguments))
              }
              .map { case (paramTypes, nonParamAbstractTypes) =>
                val queries = crossJoin(
                  buildFuzzingValuesArgs(paramTypes.map(_.fuzzingValues)),
                  buildFuzzingValuesArgs(nonParamAbstractTypes.map(_.fuzzingValues))
                ).map { case (paramArgs, nonParamArgs) => s"SELECT $fnName($paramArgs)($nonParamArgs)" }

                (paramTypes, queries)
              }

          for
            // Fuzz arguments
            outputTypeByArguments: Map[NonParametricArguments, OutputType] <-
              executeInParallelOnlySuccess(
                argumentsAndSqlQuery,
                (nonParamTypes, queries) =>
                  executeInSequenceOnlySuccess(queries, client.execute).map(resp =>
                    (nonParamTypes, resp.map(_.meta.head.`type`).reduce(CHFuzzableType.merge))
                  ),
                maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
              ).map(_.toMap)

            // Fuzz parameters
            validParameters: Seq[ParametricArguments] <-
              executeInParallelOnlySuccess(
                parametersAndSqlQuery,
                (paramTypes, queries) => executeInSequenceUntilSuccess(queries, client.execute).map(_ => paramTypes),
                maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
              )
          yield {
            for
              parameters <- validParameters
              (arguments, outputType) <- outputTypeByArguments.toSeq
            yield {
              ((parameters, arguments), outputType)
            }
          }

  /**
    * For all parametric functions previously found, fuzz if we can add a an additional parameter.
    *
    * @param fnName Name of the method to fuzz
    * @param fnBaseFunctions Parametric functions previously found
    * @param fnConstructor Lambda that builds the new parametric function
    * @return A Future containing all new parametric function
    */
  private def fuzzAddOneParameter[T <: CHFunctionIO, U <: CHFunctionIO](
      fnName: String,
      fnBaseFunctions: Seq[T],
      fnConstructor: (T, CHFuzzableType) => U
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[U]] =
    if fnBaseFunctions.isEmpty then Future.successful(Nil)
    else
      val sampleFunction = fnBaseFunctions.head

      val params = sampleFunction.parameters.map(_.asInstanceOf[CHFuzzableType])
      val args = sampleFunction.arguments.map(_.asInstanceOf[CHFuzzableType])
      testAddOneParameter(
        fnName,
        params,
        args
      ).recoverWith(err =>
        if sampleFunction.hasInfiniteArgument then
          testAddOneParameter(
            fnName,
            params,
            args :+ args.last
          )
        else Future.failed(err)
      ).map { validParameters =>
        for
          param <- validParameters
          f <- fnBaseFunctions
        yield fnConstructor(f, param)
      }

  /**
    * The future generated by this function never fails.
    *
    * @return Future.successful(true) if last argument can be repeated many times, otherwise Future.successful(false)
    */
  private def testInfiniteArgsFunctions(
      fnName: String,
      inputTypes: ParametricFunctionInput
  )(using client: CHClient, ec: ExecutionContext): Future[Boolean] =
    val arguments: NonParametricArguments = inputTypes.arguments
    require(arguments.nonEmpty, "Expected at least one defined argument, but none found.")

    // We shouldn't go to high, to avoid the following error:
    // Maximum number of arguments for aggregate function with Nullable types is 8. (NUMBER_OF_ARGUMENTS_DOESNT_MATCH)
    val argNv1 = Range(0, 4).toSeq.map(_ => Seq(arguments.last.fuzzingValues.head))
    val argNv2 = Range(0, 5).toSeq.map(_ => Seq(arguments.last.fuzzingValues.head))

    val fuzzingValuesParams = buildFuzzingValuesArgs(inputTypes.parameters.map(_.fuzzingValues))
    val fuzzingValuesArgsV1 = buildFuzzingValuesArgs(arguments.map(_.fuzzingValues) ++ argNv1)
    val fuzzingValuesArgsV2 = buildFuzzingValuesArgs(arguments.map(_.fuzzingValues) ++ argNv2)

    executeInSequenceUntilSuccess(
      crossJoin(fuzzingValuesParams, fuzzingValuesArgsV1 ++ fuzzingValuesArgsV2).map { case (params, args) =>
        s"SELECT $fnName($params)($args)"
      },
      client.execute
    ).map(_ => true).recover(_ => false)

  private def testAddOneParameter(
      fnName: String,
      currentParameters: ParametricArguments,
      arguments: NonParametricArguments
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[CHFuzzableType]] =
    val additionalParamTypes = parametricAbstractType.flatMap(_.chFuzzableTypes)
    executeInParallelOnlySuccess(
      additionalParamTypes,
      additionalParamType => {
        val fuzzingValuesParams =
          buildFuzzingValuesArgs((currentParameters :+ additionalParamType).map(_.fuzzingValues))
        val fuzzingValuesArgs = buildFuzzingValuesArgs(arguments.map(_.fuzzingValues))

        executeInSequenceUntilSuccess(
          crossJoin(fuzzingValuesParams, fuzzingValuesArgs).map { case (params, args) =>
            s"SELECT $fnName($params)($args)"
          },
          client.execute
        ).map(_ => additionalParamType)
      },
      maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
    )

  private type ParametricArguments = Seq[CHFuzzableType]
  private type NonParametricArguments = Seq[CHFuzzableType]
  private type OutputType = String

  opaque type ParametricFunctionInput = (Seq[CHFuzzableType], Seq[CHFuzzableType])
  extension (in: ParametricFunctionInput)
    @targetName("ParametricFunctionInput_parameters") def parameters: Seq[CHFuzzableType] = in._1
    @targetName("ParametricFunctionInput_arguments") def arguments: Seq[CHFuzzableType] = in._2

  opaque type ParametricFunctionAbstractInput = (Seq[CHFuzzableAbstractType], Seq[CHFuzzableAbstractType])
  extension (in: ParametricFunctionAbstractInput)
    @targetName("ParametricFunctionAbstractInput_parameters") def parameters: Seq[CHFuzzableAbstractType] = in._1
    @targetName("ParametricFunctionAbstractInput_arguments") def arguments: Seq[CHFuzzableAbstractType] = in._2
