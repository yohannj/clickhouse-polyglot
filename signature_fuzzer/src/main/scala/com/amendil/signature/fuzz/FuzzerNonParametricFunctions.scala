package com.amendil.signature.fuzz

import com.amendil.common.ConcurrencyUtils.*
import com.amendil.common.entities.*
import com.amendil.common.http.CHClient
import com.amendil.signature.Settings
import com.amendil.signature.entities.*
import com.amendil.signature.fuzz.Fuzzer.*
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
    )

  private def fuzzFunction0(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction0")
    if fn.isLambda then Future.successful(fn)
    else
      for
        fn0Opt <-
          client
            .execute(query(fn.name, args = "", fuzzOverWindow = false))
            .map((resp: CHResponse) =>
              val outputType = CHType.getByName(resp.data.head.head.asInstanceOf[String])
              Some(CHFunctionIO.Function0(outputType))
            )
            .recover(_ => None)

        windowedFn0Opt <-
          client
            .execute(query(fn.name, args = "", fuzzOverWindow = true))
            .map((resp: CHResponse) =>
              val outputType = CHType.getByName(resp.data.head.head.asInstanceOf[String])
              Some(CHFunctionIO.Function0(outputType))
            )
            .recover(_ => None)
      yield fn.copy(
        modes = fn.modes ++
          Seq(
            fn0Opt.map(_ => CHFunction.Mode.NoOverWindow),
            windowedFn0Opt.map(_ => CHFunction.Mode.OverWindow)
          ).flatten,
        function0Opt = fn0Opt.orElse(windowedFn0Opt)
      )

  private def fuzzFunction1Or0N(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1Or0N")
    if fn.isSpecialInfiniteFunction then Future.successful(fn)
    else
      fuzzNonParametric(
        fn,
        argCount = 1,
        (io: (InputTypes, OutputType)) => toFn(io, CHFunctionIO.Function1.apply),
        (io: (InputTypes, OutputType)) => toFn(io, CHFunctionIO.Function0N.apply)
      ).map((modes, fn1s, fn0Ns) =>
        logger.trace(s"fuzzFunction1Or0N - fuzz done")
        fn.copy(modes = fn.modes ++ modes, function1s = fn1s, function0Ns = fn0Ns)
      )

  private def fuzzFunction2Or1N(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2Or1N")
    if fn.isLambda || fn.isSpecialInfiniteFunction || fn.function0Ns.nonEmpty then Future.successful(fn)
    else
      fuzzNonParametric(
        fn,
        argCount = 2,
        (io: (InputTypes, OutputType)) => toFn(io, CHFunctionIO.Function2.apply),
        (io: (InputTypes, OutputType)) => toFn(io, CHFunctionIO.Function1N.apply),
        argsOfPreviouslyFoundSignatureOpt = fn.functions
          .find(_.isInstanceOf[CHFunctionIO.Function1])
          .map(f => f.asInstanceOf[CHFunctionIO.Function1].arguments.map(_.asInstanceOf[CHFuzzableType]))
      ).map((modes, fn2s, fn1Ns) =>
        logger.trace(s"fuzzFunction2Or1N - fuzz done")
        fn.copy(modes = fn.modes ++ modes, function2s = fn2s, function1Ns = fn1Ns)
      )

  private def fuzzFunction3Or2N(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction3Or2N")
    if fn.isLambda || fn.isSpecialInfiniteFunction || fn.function0Ns.nonEmpty || fn.function1Ns.nonEmpty ||
      (fn.function1s.filterNot(_.arg1.name.startsWith("Tuple")).nonEmpty && fn.function2s.isEmpty)
    then Future.successful(fn)
    else
      fuzzNonParametric(
        fn,
        argCount = 3,
        (io: (InputTypes, OutputType)) => toFn(io, CHFunctionIO.Function3.apply),
        (io: (InputTypes, OutputType)) => toFn(io, CHFunctionIO.Function2N.apply),
        argsOfPreviouslyFoundSignatureOpt = fn.functions
          .find(_.isInstanceOf[CHFunctionIO.Function2])
          .map(f => f.asInstanceOf[CHFunctionIO.Function2].arguments.map(_.asInstanceOf[CHFuzzableType]))
      ).map((modes, fn3s, fn2Ns) =>
        logger.trace(s"fuzzFunction3Or2N - fuzz done")
        fn.copy(modes = fn.modes ++ modes, function3s = fn3s, function2Ns = fn2Ns)
      )

  private def fuzzNonParametric[U1 <: CHFunctionIO, U2 <: CHFunctionIO](
      fn: CHFunctionFuzzResult,
      argCount: Int,
      fnConstructorFiniteArgs: ((InputTypes, OutputType)) => U1,
      fnConstructorInfiniteArgs: ((InputTypes, OutputType)) => U2,
      argsOfPreviouslyFoundSignatureOpt: Option[Seq[CHFuzzableType]] = None
  )(using client: CHClient, ec: ExecutionContext): Future[(Seq[CHFunction.Mode], Seq[U1], Seq[U2])] =
    val skipFuzzingF =
      if Settings.Fuzzer.skipFuzzingOnArgumentMismatch
      then checkArgMismatch(fn.name, argCount, argsOfPreviouslyFoundSignatureOpt)
      else Future.successful(false)

    skipFuzzingF.flatMap(skipFuzzing =>
      if skipFuzzing then Future.successful((Nil, Nil, Nil))
      else if fn.atLeastOneSignatureFound then
        // We know whether the function requires/supports or not `OVER window` so we can bruteforce only one of the valid values
        fuzzNonParametricSingleMode(
          fn.name,
          argCount,
          fuzzOverWindow = !fn.modes.contains(CHFunction.Mode.NoOverWindow),
          fnConstructorFiniteArgs,
          fnConstructorInfiniteArgs
        ).map((finiteFn, infiniteFn) => (Nil, finiteFn, infiniteFn))
      else
        // We don't yet know if the function requires/supports or not `OVER window` so we need to brute force them all
        // First check without OVER window
        fuzzNonParametricSingleMode(
          fn.name,
          argCount,
          fuzzOverWindow = false,
          fnConstructorFiniteArgs,
          fnConstructorInfiniteArgs
        ).flatMap((finiteFn, infiniteFn) =>
          // Success without OVER window, let's try a sample function with OVER window
          val sampleFn = finiteFn.headOption.orElse(infiniteFn.headOption).get

          val queries =
            buildFuzzingValuesArgs(sampleFn.arguments.asInstanceOf[Seq[CHFuzzableType]].map(_.fuzzingValues))
              .map(args => query(fn.name, args, fuzzOverWindow = true))

          executeInParallelUntilSuccess(queries, client.executeNoResult, Settings.ClickHouse.maxSupportedConcurrency)
            .map(_ => (Seq(CHFunction.Mode.OverWindow, CHFunction.Mode.NoOverWindow), finiteFn, infiniteFn))
            .recover(_ => (Seq(CHFunction.Mode.NoOverWindow), finiteFn, infiniteFn))
        ).recoverWith(_ =>
          // Failure without OVER window, fuzz with OVER window
          fuzzNonParametricSingleMode(
            fn.name,
            argCount,
            fuzzOverWindow = true,
            fnConstructorFiniteArgs,
            fnConstructorInfiniteArgs
          ).map((finiteFn, infiniteFn) => (Seq(CHFunction.Mode.OverWindow), finiteFn, infiniteFn))
            .recover(_ => (Nil, Nil, Nil)) // Nothing worked
        )
    )

  private def fuzzNonParametricSingleMode[U1 <: CHFunctionIO, U2 <: CHFunctionIO](
      fnName: String,
      argCount: Int,
      fuzzOverWindow: Boolean,
      fnConstructorFiniteArgs: ((InputTypes, OutputType)) => U1,
      fnConstructorInfiniteArgs: ((InputTypes, OutputType)) => U2
  )(using client: CHClient, ec: ExecutionContext): Future[(Seq[U1], Seq[U2])] =
    for
      functions: Seq[(InputTypes, OutputType)] <- fuzzFiniteArgsFunctions(fnName, argCount, fuzzOverWindow)

      fnHasInfiniteArgs: Boolean <-
        if functions.isEmpty then Future.successful(false)
        else testInfiniteArgsFunctions(fnName, functions.head._1, fuzzOverWindow)
    yield
      if fnHasInfiniteArgs then (Nil, functions.map(fnConstructorInfiniteArgs))
      else (functions.map(fnConstructorFiniteArgs), Nil)

  private def fuzzFiniteArgsFunctions(
      fnName: String,
      argCount: Int,
      fuzzOverWindow: Boolean
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[(InputTypes, OutputType)]] =
    logger.trace(s"fuzzFiniteArgsFunctions - init")
    for
      // Build all combinations of function input having argCount arguments
      // Those combinations are described using AbstractTypes!
      // They are all used to query ClickHouse and we are retrieving here only the ones that succeeded.
      abstractInputCombinationsNoSpecialType: Seq[Seq[CHFuzzableAbstractType]] <-
        executeInParallelOnlySuccess(
          generateCHFuzzableAbstractTypeCombinations(argCount).filterNot(
            _.exists(_.isInstanceOf[CustomStringBasedAbstractType])
          ),
          (abstractTypes: Seq[CHFuzzableAbstractType]) =>
            executeInParallelUntilSuccess(
              buildFuzzingValuesArgs(abstractTypes.map(_.fuzzingValues)).map(args =>
                query(fnName, args, fuzzOverWindow)
              ),
              client.executeNoResult,
              maxConcurrency = Settings.ClickHouse.maxSupportedConcurrencyInnerLoop
            ).map(_ => abstractTypes),
          maxConcurrency = Settings.ClickHouse.maxSupportedConcurrencyOuterLoop
        )
      _ = logger.trace(
        s"fuzzFiniteArgsFunctions - detected ${abstractInputCombinationsNoSpecialType.size} abstract input combinations (excluding special types)"
      )

      abstractInputCombinations: Seq[Seq[CHFuzzableAbstractType]] <-
        if abstractInputCombinationsNoSpecialType.nonEmpty then
          Future.successful(abstractInputCombinationsNoSpecialType)
        else
          executeInParallelOnlySuccess(
            generateCHFuzzableAbstractTypeCombinations(argCount).filter(
              _.exists(_.isInstanceOf[CustomStringBasedAbstractType])
            ),
            (abstractTypes: Seq[CHFuzzableAbstractType]) =>
              executeInParallelUntilSuccess(
                buildFuzzingValuesArgs(abstractTypes.map(_.fuzzingValues)).map(args =>
                  query(fnName, args, fuzzOverWindow)
                ),
                client.executeNoResult,
                maxConcurrency = Settings.ClickHouse.maxSupportedConcurrencyInnerLoop
              ).map(_ => abstractTypes),
            maxConcurrency = Settings.ClickHouse.maxSupportedConcurrencyOuterLoop
          )
      _ = logger.trace(
        s"fuzzFiniteArgsFunctions - detected ${abstractInputCombinations.size} abstract input combinations"
      )

      res <- fuzzAbstractInputCombinations(fnName, abstractInputCombinations, fuzzOverWindow)
    yield res

  private[fuzz] def fuzzAbstractInputCombinations(
      fnName: String,
      abstractInputCombinations: Seq[Seq[CHFuzzableAbstractType]],
      fuzzOverWindow: Boolean,
      returnFirstOutputTypeFound: Boolean = false
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[(InputTypes, OutputType)]] =
    logger.trace(s"fuzzAbstractInputCombinations - init")
    for
      abstractInputCombinationsWithValidFuzzableTypes: Seq[Seq[(CHFuzzableAbstractType, Seq[CHFuzzableType])]] <-
        filterFuzzableTypePerArgument(fnName, abstractInputCombinations, fuzzOverWindow)

      _ = logger.trace(
        s"fuzzAbstractInputCombinations - found ${abstractInputCombinationsWithValidFuzzableTypes.flatten.map(_._2.size).sum} " +
          s"fuzzable types in the different combinations"
      )

      // Expand abstract types to retrieve all types combinations and their output
      inputSignatures <-
        fuzzAbstractTypeToType(
          fnName,
          abstractInputCombinationsWithValidFuzzableTypes.map((_, Seq.empty[CHFuzzableType])),
          fuzzOverWindow
        )

      _ = logger.trace(s"fuzzAbstractInputCombinations - detected ${inputSignatures.size} input signatures")

      res <-
        executeInParallelOnlySuccess(
          inputSignatures,
          inputTypes =>
            val queries =
              buildFuzzingValuesArgs(inputTypes.map(_.fuzzingValues)).map(args => query(fnName, args, fuzzOverWindow))

            if returnFirstOutputTypeFound then
              executeInParallelUntilSuccess(
                queries,
                client
                  .execute(_)
                  .map(_.data.head.head.asInstanceOf[String]),
                Settings.ClickHouse.maxSupportedConcurrencyInnerLoop
              ).map(outputType => (inputTypes, CHType.getByName(outputType)))
            else
              executeInParallelOnlySuccess(
                queries,
                client
                  .execute(_)
                  .map(_.data.head.head.asInstanceOf[String]),
                Settings.ClickHouse.maxSupportedConcurrencyInnerLoop
              ).map(outputTypes => (inputTypes, outputTypes.map(CHType.getByName).reduce(CHType.mergeOutputType)))
          ,
          maxConcurrency = Settings.ClickHouse.maxSupportedConcurrencyOuterLoop
        )
      _ = logger.trace(s"fuzzAbstractInputCombinations - signatures output detected")
    yield res

  /**
    * Build all combinations of function input having argCount arguments
    * Those combinations are described using AbstractTypes!
    *
    * Then bruteforce them to find the valid ones.
    */
  private def getValidAbstractTypeCombinations(
      fnName: String,
      argCount: Int,
      fuzzOverWindow: Boolean
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[Seq[CHFuzzableAbstractType]]] =
    for
      // For performance reasons, we first exclude custom types that also work as String.
      abstractInputCombinationsNoSpecialType: Seq[Seq[CHFuzzableAbstractType]] <-
        bruteforceAbstractTypeCombinations(
          generateCHFuzzableAbstractTypeCombinations(argCount).filterNot(
            _.exists(_.isInstanceOf[CustomStringBasedAbstractType])
          ),
          fnName,
          fuzzOverWindow
        )

      _ = logger.trace(
        s"bruteforceAbstractTypeCombinations - detected ${abstractInputCombinationsNoSpecialType.size} abstract input combinations (excluding special types)"
      )

      // If nothing was found previously, then it might be because we need a custom types that also work as String.
      abstractInputCombinations: Seq[Seq[CHFuzzableAbstractType]] <-
        if abstractInputCombinationsNoSpecialType.nonEmpty then
          Future.successful(abstractInputCombinationsNoSpecialType)
        else
          bruteforceAbstractTypeCombinations(
            generateCHFuzzableAbstractTypeCombinations(argCount).filter(
              _.exists(_.isInstanceOf[CustomStringBasedAbstractType])
            ),
            fnName,
            fuzzOverWindow
          )

      _ = logger.trace(
        s"bruteforceAbstractTypeCombinations - detected ${abstractInputCombinations.size} abstract input combinations"
      )
    yield abstractInputCombinations

  /**
    * Query ClickHouse for all given combinations and we returning those that succeeded at least once.
    */
  private def bruteforceAbstractTypeCombinations(
      combinations: Seq[Seq[CHFuzzableAbstractType]],
      fnName: String,
      fuzzOverWindow: Boolean
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[Seq[CHFuzzableAbstractType]]] =
    executeInParallelOnlySuccess(
      combinations,
      (abstractTypes: Seq[CHFuzzableAbstractType]) =>
        executeInParallelUntilSuccess(
          buildFuzzingValuesArgs(abstractTypes.map(_.fuzzingValues)).map(args => query(fnName, args, fuzzOverWindow)),
          client.executeNoResult,
          maxConcurrency = Settings.ClickHouse.maxSupportedConcurrencyInnerLoop
        ).map(_ => abstractTypes),
      maxConcurrency = Settings.ClickHouse.maxSupportedConcurrencyOuterLoop
    )

  /**
    * For each abstract input, this method checks for each argument which non-abstract type are valid.
    */
  private def filterFuzzableTypePerArgument(
      fnName: String,
      abstractInputs: Seq[Seq[CHFuzzableAbstractType]],
      fuzzOverWindow: Boolean
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
            executeInParallel( // For each argument (identified by its index)
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
                      ).map(args => query(fnName, args, fuzzOverWindow))

                    executeInSequenceUntilSuccess( // Check if any value of the current possible type is valid
                      queries,
                      client.executeNoResult
                    ).map(_ => fuzzableType)
                ).map { filteredFuzzableType =>
                  if filteredFuzzableType.isEmpty then
                    val errorMsg =
                      s"No individual value found for argument idx $idx, while we know the combination [${abstractInput.mkString(", ")}] is valid."
                    logger.error(errorMsg)
                    throw Exception(errorMsg)

                  (currentArgumentAbstractType, filteredFuzzableType)
                }
              ,
              maxConcurrency = abstractInputs.head.size
            ),
          maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency / abstractInputs.head.size
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
    * Format of a signature is: (left_arguments: abstract type, right_arguments: type)
    */
  private def fuzzAbstractTypeToType(
      fnName: String,
      signatures: Seq[(Seq[(CHFuzzableAbstractType, Seq[CHFuzzableType])], Seq[CHFuzzableType])],
      fuzzOverWindow: Boolean
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[InputTypes]] =
    if signatures.isEmpty || signatures.head._1.isEmpty then Future.successful(signatures.map(_._2))
    else
      logger.trace(
        s"fuzzAbstractTypeToType - init with ${signatures.size} signatures each having " +
          s"${signatures.head._1.size} abstract arguments to fuzz"
      )
      val newSignatures = signatures.flatMap { case (abstractTypes, types) =>
        val newAbstractTypes = abstractTypes.reverse.tail.reverse // Remove last element

        abstractTypes.last._2.map(t => (newAbstractTypes, t +: types))
      }
      logger.trace(
        s"fuzzAbstractTypeToType - signatures expanded, now having ${newSignatures.size} signatures"
      )
      executeInParallelOnlySuccess(
        newSignatures,
        signature =>
          val queries = buildFuzzingValuesArgs(
            signature._1.map(_._1.fuzzingValues) ++ signature._2.map(_.fuzzingValues)
          ).map(args => query(fnName, args, fuzzOverWindow))

          executeInSequenceUntilSuccess(
            queries,
            client.executeNoResult
          ).map(_ => signature)
        ,
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).flatMap(fuzzAbstractTypeToType(fnName, _, fuzzOverWindow))

  /**
    * The future generated by this function never fails.
    *
    * @return Future.successful(true) if last argument can be repeated many times, otherwise Future.successful(false)
    */
  private def testInfiniteArgsFunctions(
      fnName: String,
      arguments: InputTypes,
      fuzzOverWindow: Boolean
  )(using client: CHClient, ec: ExecutionContext): Future[Boolean] =
    logger.trace(s"testInfiniteArgsFunctions - init")
    require(arguments.nonEmpty, "Expected at least one defined argument, but none found.")

    // We shouldn't go to high, to avoid the following error:
    // Maximum number of arguments for aggregate function with Nullable types is 8. (NUMBER_OF_ARGUMENTS_DOESNT_MATCH)
    val argNv1 = Range(0, 5).toSeq.map(_ => Seq(arguments.last.fuzzingValues.head))
    val argNv2 = Range(0, 6).toSeq.map(_ => Seq(arguments.last.fuzzingValues.head))

    val fuzzingValuesArgsV1 = buildFuzzingValuesArgs(arguments.map(_.fuzzingValues) ++ argNv1)
    val fuzzingValuesArgsV2 = buildFuzzingValuesArgs(arguments.map(_.fuzzingValues) ++ argNv2)

    executeInSequenceUntilSuccess(
      (fuzzingValuesArgsV1 ++ fuzzingValuesArgsV2).map(args => query(fnName, args, fuzzOverWindow)),
      client.executeNoResult
    ).map(_ => true).recover(_ => false)

  private def query(fnName: String, args: String, fuzzOverWindow: Boolean): String =
    if fuzzOverWindow then s"SELECT toTypeName($fnName($args) OVER w1) WINDOW w1 AS ()"
    else s"SELECT toTypeName($fnName($args))"

  /**
    * @return true if a NUMBER_OF_ARGUMENTS_DOESNT_MATCH error was returned by ClickHouse, false otherwise
    */
  private def checkArgMismatch(
      fnName: String,
      argCount: Int,
      argsOfPreviouslyFoundSignatureOpt: Option[Seq[CHFuzzableType]]
  )(using client: CHClient, ec: ExecutionContext): Future[Boolean] =
    if fnName.equals("nested") && argCount == 2 then Future.successful(false)
    else if fnName.equals("arrayEnumerateRanked") then Future.successful(false) // Unsure about the expected arg count
    else
      val args = argsOfPreviouslyFoundSignatureOpt match
        case None =>
          Range(0, argCount)
        case Some(argsOfPreviouslyFoundSignature) =>
          argsOfPreviouslyFoundSignature.map(_.fuzzingValues.head) ++ Range(
            0,
            argCount - argsOfPreviouslyFoundSignature.size
          )

      client
        .executeNoResult(
          query(
            fnName,
            args.mkString(", "),
            false
          )
        )
        .map(_ => false)
        .recover { err =>
          Seq(
            "(TOO_FEW_ARGUMENTS_FOR_FUNCTION)",
            "(NUMBER_OF_ARGUMENTS_DOESNT_MATCH)",
            "(TOO_MANY_ARGUMENTS_FOR_FUNCTION)"
          ).exists(err.getMessage().contains)
        }

  private def toFn[T <: CHFunctionIO](
      io: (InputTypes, OutputType),
      functionConstructor: (CHFuzzableType, CHType) => T
  ): T =
    io._1 match
      case Seq(arg1) => functionConstructor(arg1, io._2)
      case _         => throw Exception(s"Expected 1 argument, but found ${io._1.size} arguments")

  private def toFn[T <: CHFunctionIO](
      io: (InputTypes, OutputType),
      functionConstructor: (CHFuzzableType, CHFuzzableType, CHType) => T
  ): T =
    io._1 match
      case Seq(arg1, arg2) => functionConstructor(arg1, arg2, io._2)
      case _               => throw Exception(s"Expected 2 argument, but found ${io._1.size} arguments")

  private def toFn[T <: CHFunctionIO](
      io: (InputTypes, OutputType),
      functionConstructor: (CHFuzzableType, CHFuzzableType, CHFuzzableType, CHType) => T
  ): T =
    io._1 match
      case Seq(arg1, arg2, arg3) => functionConstructor(arg1, arg2, arg3, io._2)
      case _                     => throw Exception(s"Expected 3 argument, but found ${io._1.size} arguments")

  private def toFn[T <: CHFunctionIO](
      io: (InputTypes, OutputType),
      functionConstructor: (CHFuzzableType, CHFuzzableType, CHFuzzableType, CHFuzzableType, CHType) => T
  ): T =
    io._1 match
      case Seq(arg1, arg2, arg3, arg4) => functionConstructor(arg1, arg2, arg3, arg4, io._2)
      case _                           => throw Exception(s"Expected 4 argument, but found ${io._1.size} arguments")

  private type InputTypes = Seq[CHFuzzableType]
  private type OutputType = CHType
