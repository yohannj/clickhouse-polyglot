package com.amendil.signature.fuzz

import com.amendil.common.entities.*
import com.amendil.common.entities.`type`.{CHFuzzableType, CHType}
import com.amendil.common.entities.function.{CHFunction, CHFunctionIO}
import com.amendil.common.helper.*
import com.amendil.common.helper.ConcurrencyUtils.*
import com.amendil.common.http.CHClient
import com.amendil.signature.Settings
import com.amendil.signature.entities.*
import com.amendil.signature.fuzz.Fuzzer.*
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

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
      fuzzNonParametric(
        fn,
        argCount = 0,
        (io: (InputTypes, OutputType)) => toFn(io, CHFunctionIO.Function0.apply),
        (io: (InputTypes, OutputType)) =>
          throw Exception("Found a signature with a repeated argument while we are trying to fuzz 0 arguments")
      ).map((modes, settings, fn0s, _) =>
        logger.trace(s"fuzzFunction0 - fuzz done")
        fn.copy(modes = fn.modes ++ modes, settings = fn.settings ++ settings, function0Opt = fn0s.headOption)
      )

  private def fuzzFunction1Or0N(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1Or0N")
    if (fn.isLambda && fn.lambdaArrayFunction0NOpt.isEmpty && fn.lambdaMapFunction0NOpt.isEmpty && fn.lambdaMapFunction1Opt.isEmpty) || fn.isSpecialRepeatedFunction
    then Future.successful(fn)
    else
      fuzzNonParametric(
        fn,
        argCount = 1,
        (io: (InputTypes, OutputType)) => toFn(io, CHFunctionIO.Function1.apply),
        (io: (InputTypes, OutputType)) => toFn(io, CHFunctionIO.Function0N.apply)
      ).map((modes, settings, fn1s, fn0Ns) =>
        logger.trace(s"fuzzFunction1Or0N - fuzz done")
        fn.copy(modes = fn.modes ++ modes, settings = fn.settings ++ settings, function1s = fn1s, function0Ns = fn0Ns)
      )

  private def fuzzFunction2Or1N(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2Or1N")
    if (fn.isLambda && fn.lambdaArrayFunction1NOpt.isEmpty && fn.lambdaMapFunction1N.isEmpty && fn.lambdaMapFunction2.isEmpty) || fn.isSpecialRepeatedFunction || fn.function0Ns.nonEmpty
    then Future.successful(fn)
    else
      fuzzNonParametric(
        fn,
        argCount = 2,
        (io: (InputTypes, OutputType)) => toFn(io, CHFunctionIO.Function2.apply),
        (io: (InputTypes, OutputType)) => toFn(io, CHFunctionIO.Function1N.apply),
        argsOfPreviouslyFoundSignatureOpt = fn.functions
          .find(_.isInstanceOf[CHFunctionIO.Function1])
          .map(f => f.asInstanceOf[CHFunctionIO.Function1].arguments.map(_.asInstanceOf[CHFuzzableType]))
      ).map((modes, settings, fn2s, fn1Ns) =>
        logger.trace(s"fuzzFunction2Or1N - fuzz done")
        fn.copy(modes = fn.modes ++ modes, settings = fn.settings ++ settings, function2s = fn2s, function1Ns = fn1Ns)
      )

  private def fuzzFunction3Or2N(
      fn: CHFunctionFuzzResult
  )(using CHClient, ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction3Or2N")
    if fn.isLambda || fn.isSpecialRepeatedFunction || fn.function0Ns.nonEmpty || fn.function1Ns.nonEmpty ||
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
      ).map((modes, settings, fn3s, fn2Ns) =>
        logger.trace(s"fuzzFunction3Or2N - fuzz done")
        fn.copy(modes = fn.modes ++ modes, settings = fn.settings ++ settings, function3s = fn3s, function2Ns = fn2Ns)
      )

  private def fuzzNonParametric[U1 <: CHFunctionIO, U2 <: CHFunctionIO](
      fn: CHFunctionFuzzResult,
      argCount: Int,
      fnConstructorFiniteArgs: ((InputTypes, OutputType)) => U1,
      fnConstructorRepeatedArgs: ((InputTypes, OutputType)) => U2,
      argsOfPreviouslyFoundSignatureOpt: Option[Seq[CHFuzzableType]] = None
  )(
      using client: CHClient,
      ec: ExecutionContext
  ): Future[(Set[CHFunction.Mode], Set[CHSettingWithValue[Boolean]], Seq[U1], Seq[U2])] =
    val skipFuzzingF =
      if Settings.Fuzzer.skipFuzzingOnArgumentMismatch
      then checkArgMismatch(fn.name, argCount, argsOfPreviouslyFoundSignatureOpt)
      else Future.successful(false)

    skipFuzzingF.flatMap(skipFuzzing =>
      if skipFuzzing then Future.successful((Set.empty, Set.empty, Nil, Nil))
      else if fn.atLeastOneSignatureFound then
        // We know whether the function requires/supports or not `OVER window` so we can bruteforce only one of the valid values
        fuzzNonParametricSingleMode(
          fn.name,
          argCount,
          fuzzOverWindow = !fn.modes.contains(CHFunction.Mode.NoOverWindow),
          fnConstructorFiniteArgs,
          fnConstructorRepeatedArgs
        ).map((finiteFn, repeatedFn) => (Set.empty, Set.empty, finiteFn, repeatedFn))
      else
        // format: off
        fuzzNonParametricSingleMode(fn.name, argCount, fuzzOverWindow = false, fnConstructorFiniteArgs, fnConstructorRepeatedArgs)
          .transformWith{
            case Success((finiteFn, repeatedFn)) if finiteFn.nonEmpty || repeatedFn.nonEmpty =>
              // Success without OVER window, let's try a sample function with OVER window
              val sampleFn = finiteFn.headOption.orElse(repeatedFn.headOption).get

              testSampleFunctionWithOverWindow(fn.name, sampleFn).map( supportOverWindow =>
                if supportOverWindow
                then (Set(CHFunction.Mode.OverWindow, CHFunction.Mode.NoOverWindow), finiteFn, repeatedFn)
                else (Set(CHFunction.Mode.NoOverWindow), finiteFn, repeatedFn)
              )
            case _ =>
              // Failure without OVER window, fuzz with OVER window
              fuzzNonParametricSingleMode(fn.name, argCount, fuzzOverWindow = true, fnConstructorFiniteArgs, fnConstructorRepeatedArgs)
                .map((finiteFn, repeatedFn) => (Set(CHFunction.Mode.OverWindow), finiteFn, repeatedFn))
                .recover(_ => (Set.empty, Nil, Nil)) // Nothing worked
          }
          .transformWith{
            case Success((modes, finiteFn, repeatedFn)) if finiteFn.nonEmpty || repeatedFn.nonEmpty =>
              val sampleFn = finiteFn.headOption.orElse(repeatedFn.headOption).get
              detectMandatorySettingsFromSampleFunction(fn.name, sampleFn, fuzzOverWindow = !(fn.modes ++ modes).contains(CHFunction.Mode.NoOverWindow))
                .map(settings => (modes, settings, finiteFn, repeatedFn))

            case _ => Future.successful((Set.empty, Set.empty, Nil, Nil))
          }
        // format: on
    )

  private def fuzzNonParametricSingleMode[U1 <: CHFunctionIO, U2 <: CHFunctionIO](
      fnName: String,
      argCount: Int,
      fuzzOverWindow: Boolean,
      fnConstructorFiniteArgs: ((InputTypes, OutputType)) => U1,
      fnConstructorRepeatedArgs: ((InputTypes, OutputType)) => U2
  )(using CHClient, ExecutionContext): Future[(Seq[U1], Seq[U2])] =
    for
      functions: Seq[(InputTypes, OutputType)] <- fuzzFiniteArgsFunctions(fnName, argCount, fuzzOverWindow)

      fnHasRepeatedArgs: Boolean <-
        if functions.isEmpty || argCount == 0 then Future.successful(false)
        else
          testRepeatedArgsFunctions(
            fnName,
            sampleInput = functions.head._1,
            lastArgumentTypes = functions.map(_._1.last).toSet,
            fuzzOverWindow
          )
    yield
      if fnHasRepeatedArgs then (Nil, functions.map(fnConstructorRepeatedArgs))
      else (functions.map(fnConstructorFiniteArgs), Nil)

  private def fuzzFiniteArgsFunctions(
      fnName: String,
      argCount: Int,
      fuzzOverWindow: Boolean
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[(InputTypes, OutputType)]] =
    logger.trace(s"fuzzFiniteArgsFunctions - init")
    for
      abstractInputCombinations: Seq[Seq[CHFuzzableAbstractType]] <-
        getValidAbstractTypeCombinations(fnName, argCount, fuzzOverWindow)

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

      res <- fuzzInputCombinations(fnName, inputSignatures, fuzzOverWindow, returnFirstOutputTypeFound)
    yield res

  private[fuzz] def fuzzInputCombinations(
      fnName: String,
      inputCombinations: Seq[Seq[CHFuzzableType]],
      fuzzOverWindow: Boolean,
      returnFirstOutputTypeFound: Boolean = false
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[(InputTypes, OutputType)]] =
    logger.trace(s"fuzzInputCombinations - init")
    for
      res <-
        executeInParallelOnlySuccess(
          inputCombinations,
          inputTypes =>
            val queries =
              buildFuzzingValuesArgs(inputTypes.map(_.fuzzingValues)).map(args => query(fnName, args, fuzzOverWindow))

            if returnFirstOutputTypeFound then
              executeInParallelUntilSuccess(
                queries,
                client.execute(_).map(_.data.head.head.asInstanceOf[String]),
                Settings.ClickHouse.maxSupportedConcurrencyInnerLoop
              ).map(outputType => (inputTypes, CHTypeParser.getByName(outputType)))
            else
              executeInParallelOnlySuccess(
                queries,
                client.execute(_).map(_.data.head.head.asInstanceOf[String]),
                Settings.ClickHouse.maxSupportedConcurrencyInnerLoop
              ).map(outputTypes =>
                (inputTypes, outputTypes.map(CHTypeParser.getByName).reduce(CHTypeMerger.mergeOutputType))
              )
          ,
          maxConcurrency = Settings.ClickHouse.maxSupportedConcurrencyOuterLoop
        )
      _ = logger.trace(s"fuzzInputCombinations - signatures output detected")
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
  )(using CHClient, ExecutionContext): Future[Seq[Seq[CHFuzzableAbstractType]]] =
    for
      // For performance reasons, we first exclude custom types that also work as String.
      abstractInputCombinationsNoSpecialType: Seq[Seq[CHFuzzableAbstractType]] <-
        bruteforceAbstractTypeCombinations(
          generateCHFuzzableAbstractTypeCombinations(argCount).filterNot(_.exists(_.isInstanceOf[CustomAbstractType])),
          fnName,
          fuzzOverWindow
        )

      _ = logger.trace(
        s"getValidAbstractTypeCombinations - detected ${abstractInputCombinationsNoSpecialType.size} abstract input combinations (excluding special types)"
      )

      // If nothing was found previously, then it might be because we need a custom types that also work as String.
      abstractInputCombinations: Seq[Seq[CHFuzzableAbstractType]] <-
        if abstractInputCombinationsNoSpecialType.nonEmpty then
          Future.successful(abstractInputCombinationsNoSpecialType)
        else
          bruteforceAbstractTypeCombinations(
            generateCHFuzzableAbstractTypeCombinations(argCount).filter(_.exists(_.isInstanceOf[CustomAbstractType])),
            fnName,
            fuzzOverWindow
          )

      _ = logger.trace(
        s"getValidAbstractTypeCombinations - detected ${abstractInputCombinations.size} abstract input combinations"
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
          client.executeNoResult(_),
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
    if abstractInputs.isEmpty then Future.successful(Nil)
    else if abstractInputs.head.size <= 1 then
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
              val fuzzingValuesBeforeColumn = indexedInput.filter(_._2 < idx).map(_._1.exhaustiveFuzzingValues)
              val currentArgumentAbstractType = indexedInput.find(_._2 == idx).get._1
              val fuzzingValuesAfterColumn = indexedInput.filter(_._2 > idx).map(_._1.exhaustiveFuzzingValues)

              executeInSequenceOnlySuccess( // For each possible type of the current argument
                currentArgumentAbstractType.chFuzzableTypes,
                (fuzzableType: CHFuzzableType) =>
                  val queries =
                    buildFuzzingValuesArgs(
                      (fuzzingValuesBeforeColumn :+ fuzzableType.fuzzingValues) ++ fuzzingValuesAfterColumn
                    ).map(args => query(fnName, args, fuzzOverWindow))

                  executeInSequenceUntilSuccess( // Check if any value of the current possible type is valid
                    queries,
                    client.executeNoResult(_)
                  ).map(_ => fuzzableType)
              ).map { filteredFuzzableTypes =>
                if filteredFuzzableTypes.isEmpty then
                  val errorMsg =
                    s"No individual value found for argument idx $idx, while we know the combination [${abstractInput.mkString(", ")}] is valid."
                  logger.error(errorMsg)
                  throw Exception(errorMsg)

                (currentArgumentAbstractType, filteredFuzzableTypes)
              }
            ,
            maxConcurrency = abstractInputs.head.size
          ),
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency / abstractInputs.head.size
      )

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
            signature._1.map(_._1.exhaustiveFuzzingValues) ++ signature._2.map(_.fuzzingValues)
          ).map(args => query(fnName, args, fuzzOverWindow))

          executeInParallelUntilSuccess(
            queries,
            client.executeNoResult(_),
            Settings.ClickHouse.maxSupportedConcurrencyInnerLoop
          ).map(_ => signature)
        ,
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrencyOuterLoop
      ).flatMap(fuzzAbstractTypeToType(fnName, _, fuzzOverWindow))

  /**
    * The future generated by this function never fails.
    *
    * @return Future.successful(true) if last argument can be repeated many times, otherwise Future.successful(false)
    */
  private def testRepeatedArgsFunctions(
      fnName: String,
      sampleInput: InputTypes,
      lastArgumentTypes: Set[CHFuzzableType],
      fuzzOverWindow: Boolean
  )(using client: CHClient, ec: ExecutionContext): Future[Boolean] =
    logger.trace(s"testRepeatedArgsFunctions - init")
    require(sampleInput.nonEmpty, "Expected at least one defined argument, but none found.")

    def buildArgs(lastType: CHFuzzableType): Seq[String] =
      // We shouldn't go to high, to avoid the following error:
      // Maximum number of arguments for aggregate function with Nullable types is 8. (NUMBER_OF_ARGUMENTS_DOESNT_MATCH)
      val argNv1 =
        Range(0, 4).toSeq.map(_ => Seq(sampleInput.last.fuzzingValues.head)) :+ Seq(lastType.fuzzingValues.head)
      val argNv2 =
        Range(0, 5).toSeq.map(_ => Seq(sampleInput.last.fuzzingValues.head)) :+ Seq(lastType.fuzzingValues.head)

      val fuzzingValuesArgsV1 = buildFuzzingValuesArgs(sampleInput.map(_.fuzzingValues) ++ argNv1)
      val fuzzingValuesArgsV2 = buildFuzzingValuesArgs(sampleInput.map(_.fuzzingValues) ++ argNv2)

      fuzzingValuesArgsV1 ++ fuzzingValuesArgsV2

    val resF =
      for
        // Try repeating the last argument N times
        _ <-
          executeInParallelUntilSuccess(
            buildArgs(sampleInput.last).map(args => query(fnName, args, fuzzOverWindow)),
            client.executeNoResult(_),
            Settings.ClickHouse.maxSupportedConcurrency
          )

        otherValidTypes <-
          executeInParallelOnlySuccess(
            // Test all chType that were not found as valid for current argument
            CHFuzzableAbstractType.nonCustomFuzzableTypes.filterNot(lastArgumentTypes),
            chFuzzableType =>
              executeInParallelUntilSuccess(
                buildArgs(chFuzzableType).map(args => query(fnName, args, fuzzOverWindow)),
                client.executeNoResult(_),
                Settings.ClickHouse.maxSupportedConcurrencyInnerLoop
              ).map(_ => chFuzzableType),
            Settings.ClickHouse.maxSupportedConcurrencyOuterLoop
          )
      yield otherValidTypes.isEmpty // If no other types are valid, then current argument is the repeated one.

    resF.recover(_ => false)

  /**
    * @return true if it might be possible to call the function with the provided number of arguments, false otherwise
    */
  private def checkArgMismatch(
      fnName: String,
      argCount: Int,
      argsOfPreviouslyFoundSignatureOpt: Option[Seq[CHFuzzableType]]
  )(using client: CHClient, ec: ExecutionContext): Future[Boolean] =
    if fnName.equals("nth_value") then Future.successful(argCount != 2)
    else if fnName.equals("trim") then Future.successful(argCount != 1)
    else if fnName.equals("ltrim") then Future.successful(argCount != 1)
    else if fnName.equals("rtrim") then Future.successful(argCount != 1)
    else if fnName.equals("hilbertEncode") then Future.successful(argCount == 3)
    else if fnName.equals("mortonEncode") then Future.successful(argCount == 3)
    else if fnName.equals("arrayEnumerateRanked") then Future.successful(false) // Unsure about the expected arg count
    else
      val args = argsOfPreviouslyFoundSignatureOpt match
        case None =>
          Range(0, argCount)
        case Some(argsOfPreviouslyFoundSignature) =>
          argsOfPreviouslyFoundSignature.map(_.fuzzingValues.head) ++
            Range(0, argCount - argsOfPreviouslyFoundSignature.size)

      client
        .executeNoResult(
          query(fnName, args.mkString(", "), false)
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
      functionConstructor: (CHType) => T
  ): T =
    io._1 match
      case Seq() => functionConstructor(io._2)
      case _     => throw Exception(s"Expected 0 arguments, but found ${io._1.size} arguments")

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
