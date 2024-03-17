package com.amendil.fuzz

import com.amendil.ConcurrencyUtils._
import com.amendil.Settings
import com.amendil.entities._
import com.amendil.fuzz.Fuzzer._
import com.amendil.http.CHClient
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}

object FuzzerNonParametricFunctions extends StrictLogging:
  private[fuzz] def fuzzingFunctionWithCost(
      using client: CHClient,
      ec: ExecutionContext
  ): Seq[((CHFunctionFuzzResult) => Future[CHFunctionFuzzResult], Long)] =
    val elemCount: Long = CHFuzzableType.values.size
    Seq(
      (fuzzFunction0, 1L),
      (fuzzFunction1Or0N, elemCount),
      (fuzzFunction2Or1N, elemCount * elemCount),
      (fuzzFunction3Or2N, elemCount * elemCount * elemCount)
      // TODO: Uncomment fuzzFunction4Or3N once all functions are found
      // The combinatory is HUGE, we will have to tests and see if there are possible optimisations
      // Maybe limit it to only functions for which we know of a function3?
      // (fuzzFunction4Or3N, elemCount * elemCount * elemCount * elemCount)
    )

  private def fuzzFunction0(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction0")
    if fn.isLambda then Future.successful(fn)
    else
      client
        .execute(s"SELECT ${fn.name}()")
        .map((resp: CHResponse) =>
          val outputType: String = resp.meta.head.`type`
          fn.copy(function0Opt = Some(CHFunctionIO.Function0(outputType)))
        )
        .recover(_ => fn)

  private def fuzzFunction1Or0N(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1Or0N")
    if fn.isParametric || fn.isSpecialInfiniteFunction then Future.successful(fn)
    else
      for
        functions: Seq[(InputTypes, OutputType)] <- fuzzFiniteArgsFunctions(fn.name, argCount = 1)
        fnHasInfiniteArgs: Boolean <-
          if functions.isEmpty then Future.successful(true) else testInfiniteArgsFunctions(fn.name, functions.head._1)
      yield {
        def toFn[T <: CHFunctionIO](
            io: (InputTypes, OutputType),
            functionConstructor: (CHFuzzableType, String) => T
        ): T =
          io._1 match
            case Seq(arg1) => functionConstructor(arg1, io._2)
            case _         => throw new Exception(s"Expected 1 argument, but found ${io._1.size} arguments")

        if fnHasInfiniteArgs then fn.copy(function0Ns = functions.map(toFn(_, CHFunctionIO.Function0N.apply)))
        else fn.copy(function1s = functions.map(toFn(_, CHFunctionIO.Function1.apply)))
      }

  private def fuzzFunction2Or1N(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2Or1N")
    if fn.isLambda || fn.isParametric || fn.isSpecialInfiniteFunction || fn.function0Ns.nonEmpty then
      Future.successful(fn)
    else
      for
        functions: Seq[(InputTypes, OutputType)] <- fuzzFiniteArgsFunctions(fn.name, argCount = 2)
        fnHasInfiniteArgs: Boolean <-
          if functions.isEmpty then Future.successful(true) else testInfiniteArgsFunctions(fn.name, functions.head._1)
      yield {
        def toFn[T <: CHFunctionIO](
            io: (InputTypes, OutputType),
            functionConstructor: (CHFuzzableType, CHFuzzableType, String) => T
        ): T =
          io._1 match
            case Seq(arg1, arg2) => functionConstructor(arg1, arg2, io._2)
            case _               => throw new Exception(s"Expected 2 argument, but found ${io._1.size} arguments")

        if fnHasInfiniteArgs then fn.copy(function1Ns = functions.map(toFn(_, CHFunctionIO.Function1N.apply)))
        else fn.copy(function2s = functions.map(toFn(_, CHFunctionIO.Function2.apply)))
      }

  private def fuzzFunction3Or2N(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction3Or2N")
    if fn.isLambda || fn.isParametric || fn.isSpecialInfiniteFunction || fn.function0Ns.nonEmpty || fn.function1Ns.nonEmpty ||
      (fn.function1s.filterNot(_.arg1.name.startsWith("Tuple")).nonEmpty && fn.function2s.isEmpty)
    then Future.successful(fn)
    else
      for
        functions: Seq[(InputTypes, OutputType)] <- fuzzFiniteArgsFunctions(fn.name, argCount = 3)
        fnHasInfiniteArgs: Boolean <-
          if functions.isEmpty then Future.successful(true) else testInfiniteArgsFunctions(fn.name, functions.head._1)
      yield {
        def toFn[T <: CHFunctionIO](
            io: (InputTypes, OutputType),
            functionConstructor: (CHFuzzableType, CHFuzzableType, CHFuzzableType, String) => T
        ): T =
          io._1 match
            case Seq(arg1, arg2, arg3) => functionConstructor(arg1, arg2, arg3, io._2)
            case _                     => throw new Exception(s"Expected 3 argument, but found ${io._1.size} arguments")

        if fnHasInfiniteArgs then fn.copy(function2Ns = functions.map(toFn(_, CHFunctionIO.Function2N.apply)))
        else fn.copy(function3s = functions.map(toFn(_, CHFunctionIO.Function3.apply)))
      }

  private def fuzzFunction4Or3N(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction4Or3N")
    if fn.isLambda || fn.isParametric || fn.isSpecialInfiniteFunction || fn.function0Ns.nonEmpty || fn.function1Ns.nonEmpty ||
      fn.function2Ns.nonEmpty || (fn.function1s
        .filterNot(_.arg1.name.startsWith("Tuple"))
        .nonEmpty && fn.function3s.isEmpty)
    then Future.successful(fn)
    else
      for
        functions: Seq[(InputTypes, OutputType)] <- fuzzFiniteArgsFunctions(fn.name, argCount = 4)
        fnHasInfiniteArgs: Boolean <-
          if functions.isEmpty then Future.successful(true) else testInfiniteArgsFunctions(fn.name, functions.head._1)
      yield {
        def toFn[T <: CHFunctionIO](
            io: (InputTypes, OutputType),
            functionConstructor: (CHFuzzableType, CHFuzzableType, CHFuzzableType, CHFuzzableType, String) => T
        ): T =
          io._1 match
            case Seq(arg1, arg2, arg3, arg4) => functionConstructor(arg1, arg2, arg3, arg4, io._2)
            case _                           => throw new Exception(s"Expected 4 argument, but found ${io._1.size} arguments")

        if fnHasInfiniteArgs then fn.copy(function3Ns = functions.map(toFn(_, CHFunctionIO.Function3N.apply)))
        else fn.copy(function4s = functions.map(toFn(_, CHFunctionIO.Function4.apply)))
      }

  private def fuzzFiniteArgsFunctions(
      fnName: String,
      argCount: Int
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[(InputTypes, OutputType)]] =
    // Build all combinations of fonction input having argCount arguments
    // Those combinations are described using AbstractTypes!
    // They are all used to query ClickHouse and we are retrieving here only the ones that succeeded.
    val validCHFuzzableAbstractTypeCombinationsF =
      executeInParallelOnlySuccess(
        generateCHFuzzableAbstractTypeCombinations(argCount),
        (abstractTypes: Seq[CHFuzzableAbstractType]) => {
          executeInSequenceUntilSuccess(
            buildFuzzingValuesArgs(abstractTypes.map(_.fuzzingValues)).map(args => s"SELECT $fnName($args)"),
            client.execute
          ).map(_ => abstractTypes)
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      )

    // Expand abstract types to retrieve all types combinations and their output
    validCHFuzzableAbstractTypeCombinationsF.flatMap { validCHFuzzableAbstractTypeCombinations =>
      val argumentsAndSqlQuery =
        validCHFuzzableAbstractTypeCombinations
          .flatMap(generateCHFuzzableTypeCombinations(_))
          .map { args =>
            val queries = buildFuzzingValuesArgs(args.map(_.fuzzingValues)).map(args => s"SELECT $fnName($args)")

            (args, queries)
          }

      executeInParallelOnlySuccess(
        argumentsAndSqlQuery,
        (args, queries) =>
          executeInSequenceOnlySuccess(queries, client.execute).map(resp =>
            (args, resp.map(_.meta.head.`type`).reduce(CHFuzzableType.merge))
          ),
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      )
    }

  /**
    * The future generated by this function never fails.
    *
    * @return Future.successful(true) if last argument can be repeated many times, otherwise Future.successful(false)
    */
  private def testInfiniteArgsFunctions(
      fnName: String,
      arguments: InputTypes
  )(using client: CHClient, ec: ExecutionContext): Future[Boolean] =
    require(arguments.nonEmpty, "Expected at least one defined argument, but none found.")

    // We shouldn't go to high, to avoid the following error:
    // Maximum number of arguments for aggregate function with Nullable types is 8. (NUMBER_OF_ARGUMENTS_DOESNT_MATCH)
    val argNv1 = Range(0, 5).toSeq.map(_ => Seq(arguments.last.fuzzingValues.head))
    val argNv2 = Range(0, 6).toSeq.map(_ => Seq(arguments.last.fuzzingValues.head))

    val fuzzingValuesArgsV1 = buildFuzzingValuesArgs(arguments.map(_.fuzzingValues) ++ argNv1)
    val fuzzingValuesArgsV2 = buildFuzzingValuesArgs(arguments.map(_.fuzzingValues) ++ argNv2)

    executeInSequenceUntilSuccess(
      (fuzzingValuesArgsV1 ++ fuzzingValuesArgsV2).map { args => s"SELECT $fnName($args)" },
      client.execute
    ).map(_ => true).recover(_ => false)

  private type InputTypes = Seq[CHFuzzableType]
  private type OutputType = String
