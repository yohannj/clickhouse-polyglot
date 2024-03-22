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
    for {
      // Build all combinations of fonction input having argCount arguments
      // Those combinations are described using AbstractTypes!
      // They are all used to query ClickHouse and we are retrieving here only the ones that succeeded.
      abstractInputCombinations: Seq[Seq[CHFuzzableAbstractType]] <-
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

      abstractInputCombinationsWithValidFuzzableTypes: Seq[Seq[(CHFuzzableAbstractType, Seq[CHFuzzableType])]] <-
        filterFuzzableTypePerArgument(fnName, abstractInputCombinations)

      // Expand abstract types to retrieve all types combinations and their output
      inputSignatures <-
        fuzzAbstractTypeToType(
          fnName,
          abstractInputCombinationsWithValidFuzzableTypes.map((_, Seq.empty[CHFuzzableType]))
        )

      res <-
        executeInParallelOnlySuccess(
          inputSignatures,
          inputTypes =>
            val queries =
              buildFuzzingValuesArgs(inputTypes.map(_.fuzzingValues)).map(args => s"SELECT $fnName($args)")

            executeInSequenceOnlySuccess(queries, client.execute).map(resp =>
              (inputTypes, resp.map(_.meta.head.`type`).reduce(CHFuzzableType.merge))
            )
          ,
          maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
        )
    } yield {
      res
    }

  /**
    * For each abstract input, this method checks for each argument which non-abstract type are valid.
    */
  private def filterFuzzableTypePerArgument(
      fnName: String,
      abstractInputs: Seq[Seq[CHFuzzableAbstractType]]
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[Seq[(CHFuzzableAbstractType, Seq[CHFuzzableType])]]] =
    if abstractInputs.nonEmpty then
      if abstractInputs.head.size == 1 then
        Future.successful(
          abstractInputs.map(_.map(abstractType => (abstractType, abstractType.chFuzzableTypes)))
        )
      else
        executeInParallel( // For each abstract input
          abstractInputs,
          (abstractInput: Seq[CHFuzzableAbstractType]) =>
            executeInSequence( // For each argument (identified by its index)
              Range.apply(0, abstractInput.size),
              (idx: Int) =>
                val indexedInput = abstractInput.zipWithIndex
                val fuzzingValuesBeforeColumn = indexedInput.filter(_._2 < idx).map(_._1.fuzzingValues)
                val currentArgumentAbstractType = indexedInput.find(_._2 == idx).get._1
                val fuzzingValuesAfterColumn = indexedInput.filter(_._2 > idx).map(_._1.fuzzingValues)

                executeInSequenceOnlySuccess( // For each possible type of the current argument
                  currentArgumentAbstractType.chFuzzableTypes,
                  (fuzzableType: CHFuzzableType) =>
                    val queries =
                      buildFuzzingValuesArgs(
                        (fuzzingValuesBeforeColumn :+ fuzzableType.fuzzingValues) ++ fuzzingValuesAfterColumn
                      ).map(args => s"SELECT $fnName($args)")

                    executeInSequenceUntilSuccess( // Check if any value of the current possible type is valid
                      queries,
                      client.execute
                    ).map(_ => fuzzableType)
                ).map(filteredFuzzableType => (currentArgumentAbstractType, filteredFuzzableType))
            ),
          maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
        )
    else Future.successful(Nil)

  /**
    * For each signature, it will expand one abstract argument into fuzzable types and
    * call ClickHouse to confirm that signature is valid.
    * i.e. Move one arg at a time from AbstractType to Type.
    *
    * The idea is to prune signatures as early as possible, even though it leads to more calls to ClickHouse.
    * Worst case, the added call will be quite insignificant compared to all the calls that needs to be made anyway.
    * Best case this pruning avoids a lot of calls to ClickHouse while reducing the memory needed to run this program.
    *
    * @param signatures Current valid signatures found, all signatures must have the same number of abstractTypes!
    */
  private def fuzzAbstractTypeToType(
      fnName: String,
      signatures: Seq[(Seq[(CHFuzzableAbstractType, Seq[CHFuzzableType])], Seq[CHFuzzableType])]
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[Seq[CHFuzzableType]]] =
    if signatures.nonEmpty && signatures.head._1.nonEmpty then
      val newSignatures = signatures.flatMap { case (abstractTypes, types) =>
        val newAbstractTypes = abstractTypes.reverse.tail.reverse // Remove last element

        abstractTypes.last._2.map(t => (newAbstractTypes, t +: types))
      }
      executeInParallelOnlySuccess(
        newSignatures,
        signature =>
          val queries = buildFuzzingValuesArgs(
            signature._1.map(_._1.fuzzingValues) ++ signature._2.map(_.fuzzingValues)
          ).map(args => s"SELECT $fnName($args)")

          executeInSequenceUntilSuccess(queries, client.execute).map(_ => signature)
        ,
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).flatMap(fuzzAbstractTypeToType(fnName, _))
    else Future.successful(signatures.map(_._2))

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
