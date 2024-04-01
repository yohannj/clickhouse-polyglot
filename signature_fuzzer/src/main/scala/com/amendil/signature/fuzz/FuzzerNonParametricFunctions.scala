package com.amendil.signature.fuzz

import com.amendil.common.ConcurrencyUtils._
import com.amendil.common.entities._
import com.amendil.common.http.CHClient
import com.amendil.signature.Settings
import com.amendil.signature.entities._
import com.amendil.signature.fuzz.Fuzzer._
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
      for {
        fn0Opt <-
          client
            .execute(query(fn.name, args = "", fuzzOverWindow = false))
            .map((resp: CHResponse) =>
              val outputType: String = resp.data.head.head.asInstanceOf[String]
              Some(CHFunctionIO.Function0(outputType))
            )
            .recover(_ => None)

        windowedFn0Opt <-
          client
            .execute(query(fn.name, args = "", fuzzOverWindow = true))
            .map((resp: CHResponse) =>
              val outputType: String = resp.data.head.head.asInstanceOf[String]
              Some(CHFunctionIO.Function0(outputType))
            )
            .recover(_ => None)
      } yield {
        fn.copy(
          modes = fn.modes ++
            Seq(
              fn0Opt.map(_ => CHFunction.Mode.NoOverWindow),
              windowedFn0Opt.map(_ => CHFunction.Mode.OverWindow)
            ).flatten,
          function0Opt = fn0Opt.orElse(windowedFn0Opt)
        )
      }

  private def fuzzFunction1Or0N(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1Or0N")
    if fn.isSpecialInfiniteFunction then Future.successful(fn)
    else
      executeInSequenceOnlySuccess(
        if fn.atLeastOneSignatureFound then
          // We know whether the function requires/supports or not `OVER window` so we can bruteforce only one of the valid values
          Seq(!fn.modes.contains(CHFunction.Mode.NoOverWindow))
        else
          // We don't yet know if the function requires/supports or not `OVER window` so we need to brute force them all
          Seq(true, false)
        ,
        (fuzzOverWindow: Boolean) =>
          for
            functions: Seq[(InputTypes, OutputType)] <-
              fuzzFiniteArgsFunctions(fn.name, argCount = 1, fuzzOverWindow)

            fnHasInfiniteArgs: Boolean <-
              if functions.isEmpty then Future.successful(false)
              else testInfiniteArgsFunctions(fn.name, functions.head._1, fuzzOverWindow)
          yield {
            if fnHasInfiniteArgs then (fuzzOverWindow, functions.map(toFn(_, CHFunctionIO.Function0N.apply)), Nil)
            else (fuzzOverWindow, Nil, functions.map(toFn(_, CHFunctionIO.Function1.apply)))
          }
      ).map(res =>
        logger.trace(s"fuzzFunction1Or0N - fuzz done")
        if res.isEmpty then fn
        else
          val modes = res
            .filter(r => r._2.nonEmpty || r._3.nonEmpty)
            .map(_._1)
            .map(if _ then CHFunction.Mode.OverWindow else CHFunction.Mode.NoOverWindow)

          val (fn0Ns, fn1s) =
            res.foldLeft(
              (Seq.empty[CHFunctionIO.Function0N], Seq.empty[CHFunctionIO.Function1])
            )((acc, r) => (acc._1 ++ r._2, acc._2 ++ r._3))
          fn.copy(modes = fn.modes ++ modes, function0Ns = fn0Ns, function1s = fn1s)
      )

  private def fuzzFunction2Or1N(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2Or1N")
    if fn.isLambda || fn.isSpecialInfiniteFunction || fn.function0Ns.nonEmpty then Future.successful(fn)
    else
      executeInSequenceOnlySuccess(
        if fn.atLeastOneSignatureFound then
          // We know whether the function requires/supports or not `OVER window` so we can bruteforce only one of the valid values
          Seq(!fn.modes.contains(CHFunction.Mode.NoOverWindow))
        else
          // We don't yet know if the function requires/supports or not `OVER window` so we need to brute force them all
          Seq(true, false)
        ,
        (fuzzOverWindow: Boolean) =>
          for
            functions: Seq[(InputTypes, OutputType)] <-
              fuzzFiniteArgsFunctions(fn.name, argCount = 2, fuzzOverWindow)

            fnHasInfiniteArgs: Boolean <-
              if functions.isEmpty then Future.successful(false)
              else testInfiniteArgsFunctions(fn.name, functions.head._1, fuzzOverWindow)
          yield {
            if fnHasInfiniteArgs then (fuzzOverWindow, functions.map(toFn(_, CHFunctionIO.Function1N.apply)), Nil)
            else (fuzzOverWindow, Nil, functions.map(toFn(_, CHFunctionIO.Function2.apply)))
          }
      ).map(res =>
        logger.trace(s"fuzzFunction2Or1N - fuzz done")
        if res.isEmpty then fn
        else
          val modes = res
            .filter(r => r._2.nonEmpty || r._3.nonEmpty)
            .map(_._1)
            .map(if _ then CHFunction.Mode.OverWindow else CHFunction.Mode.NoOverWindow)

          val (fn1Ns, fn2s) =
            res.foldLeft(
              (Seq.empty[CHFunctionIO.Function1N], Seq.empty[CHFunctionIO.Function2])
            )((acc, r) => (acc._1 ++ r._2, acc._2 ++ r._3))
          fn.copy(modes = fn.modes ++ modes, function1Ns = fn1Ns, function2s = fn2s)
      )

  private def fuzzFunction3Or2N(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction3Or2N")
    if fn.isLambda || fn.isSpecialInfiniteFunction || fn.function0Ns.nonEmpty || fn.function1Ns.nonEmpty ||
      (fn.function1s.filterNot(_.arg1.name.startsWith("Tuple")).nonEmpty && fn.function2s.isEmpty)
    then Future.successful(fn)
    else
      executeInSequenceOnlySuccess(
        if fn.atLeastOneSignatureFound then
          // We know whether the function requires/supports or not `OVER window` so we can bruteforce only one of the valid values
          Seq(!fn.modes.contains(CHFunction.Mode.NoOverWindow))
        else
          // We don't yet know if the function requires/supports or not `OVER window` so we need to brute force them all
          Seq(true, false)
        ,
        (fuzzOverWindow: Boolean) =>
          for
            functions: Seq[(InputTypes, OutputType)] <-
              fuzzFiniteArgsFunctions(fn.name, argCount = 3, fuzzOverWindow)

            fnHasInfiniteArgs: Boolean <-
              if functions.isEmpty then Future.successful(false)
              else testInfiniteArgsFunctions(fn.name, functions.head._1, fuzzOverWindow)
          yield {
            if fnHasInfiniteArgs then (fuzzOverWindow, functions.map(toFn(_, CHFunctionIO.Function2N.apply)), Nil)
            else (fuzzOverWindow, Nil, functions.map(toFn(_, CHFunctionIO.Function3.apply)))
          }
      ).map(res =>
        logger.trace(s"fuzzFunction3Or2N - fuzz done")
        if res.isEmpty then fn
        else
          val modes = res
            .filter(r => r._2.nonEmpty || r._3.nonEmpty)
            .map(_._1)
            .map(if _ then CHFunction.Mode.OverWindow else CHFunction.Mode.NoOverWindow)

          val (fn2Ns, fn3s) =
            res.foldLeft(
              (Seq.empty[CHFunctionIO.Function2N], Seq.empty[CHFunctionIO.Function3])
            )((acc, r) => (acc._1 ++ r._2, acc._2 ++ r._3))
          fn.copy(modes = fn.modes ++ modes, function2Ns = fn2Ns, function3s = fn3s)
      )

  private def fuzzFunction4Or3N(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction4Or3N")
    if fn.isLambda || fn.isSpecialInfiniteFunction || fn.function0Ns.nonEmpty || fn.function1Ns.nonEmpty ||
      fn.function2Ns.nonEmpty || (fn.function1s
        .filterNot(_.arg1.name.startsWith("Tuple"))
        .nonEmpty && fn.function3s.isEmpty)
    then Future.successful(fn)
    else
      executeInSequenceOnlySuccess(
        if fn.atLeastOneSignatureFound then
          // We know whether the function requires/supports or not `OVER window` so we can bruteforce only one of the valid values
          Seq(!fn.modes.contains(CHFunction.Mode.NoOverWindow))
        else
          // We don't yet know if the function requires/supports or not `OVER window` so we need to brute force them all
          Seq(true, false)
        ,
        (fuzzOverWindow: Boolean) =>
          for
            functions: Seq[(InputTypes, OutputType)] <-
              fuzzFiniteArgsFunctions(fn.name, argCount = 4, fuzzOverWindow)

            fnHasInfiniteArgs: Boolean <-
              if functions.isEmpty then Future.successful(false)
              else testInfiniteArgsFunctions(fn.name, functions.head._1, fuzzOverWindow)
          yield {
            if fnHasInfiniteArgs then (fuzzOverWindow, functions.map(toFn(_, CHFunctionIO.Function3N.apply)), Nil)
            else (fuzzOverWindow, Nil, functions.map(toFn(_, CHFunctionIO.Function4.apply)))
          }
      ).map(res =>
        logger.trace(s"fuzzFunction4Or3N - fuzz done")
        if res.isEmpty then fn
        else
          val modes = res
            .filter(r => r._2.nonEmpty || r._3.nonEmpty)
            .map(_._1)
            .map(if _ then CHFunction.Mode.OverWindow else CHFunction.Mode.NoOverWindow)

          val (fn3Ns, fn4s) =
            res.foldLeft(
              (Seq.empty[CHFunctionIO.Function3N], Seq.empty[CHFunctionIO.Function4])
            )((acc, r) => (acc._1 ++ r._2, acc._2 ++ r._3))
          fn.copy(modes = fn.modes ++ modes, function3Ns = fn3Ns, function4s = fn4s)
      )

  private def fuzzFiniteArgsFunctions(
      fnName: String,
      argCount: Int,
      fuzzOverWindow: Boolean
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[(InputTypes, OutputType)]] =
    logger.trace(s"fuzzFiniteArgsFunctions - init")
    for
      // Build all combinations of fonction input having argCount arguments
      // Those combinations are described using AbstractTypes!
      // They are all used to query ClickHouse and we are retrieving here only the ones that succeeded.
      abstractInputCombinations: Seq[Seq[CHFuzzableAbstractType]] <-
        executeInParallelOnlySuccess(
          generateCHFuzzableAbstractTypeCombinations(argCount),
          (abstractTypes: Seq[CHFuzzableAbstractType]) => {
            executeInParallelUntilSuccess(
              buildFuzzingValuesArgs(abstractTypes.map(_.fuzzingValues)).map(args =>
                query(fnName, args, fuzzOverWindow)
              ),
              client.executeNoResult,
              maxConcurrency = Settings.ClickHouse.maxSupportedConcurrencyInnerLoop
            ).map(_ => abstractTypes)
          },
          maxConcurrency = Settings.ClickHouse.maxSupportedConcurrencyOuterLoop
        )
      _ = logger.trace(
        s"fuzzFiniteArgsFunctions - detected ${abstractInputCombinations.size} abstract input combinations"
      )

      abstractInputCombinationsWithValidFuzzableTypes: Seq[Seq[(CHFuzzableAbstractType, Seq[CHFuzzableType])]] <-
        filterFuzzableTypePerArgument(fnName, abstractInputCombinations, fuzzOverWindow)

      _ = logger.trace(
        s"fuzzFiniteArgsFunctions - found ${abstractInputCombinationsWithValidFuzzableTypes.flatten.map(_._2.size).sum} " +
          s"fuzzable types in the different combinations"
      )

      // Expand abstract types to retrieve all types combinations and their output
      inputSignatures <-
        fuzzAbstractTypeToType(
          fnName,
          abstractInputCombinationsWithValidFuzzableTypes.map((_, Seq.empty[CHFuzzableType])),
          fuzzOverWindow
        )

      _ = logger.trace(s"fuzzFiniteArgsFunctions - detected ${inputSignatures.size} input signatures")

      res <-
        executeInParallelOnlySuccess(
          inputSignatures,
          inputTypes =>
            val queries =
              buildFuzzingValuesArgs(inputTypes.map(_.fuzzingValues)).map(args => query(fnName, args, fuzzOverWindow))

            executeInParallelOnlySuccess(
              queries,
              client
                .execute(_)
                .map(_.data.head.head.asInstanceOf[String]),
              Settings.ClickHouse.maxSupportedConcurrencyInnerLoop
            ).map(outputTypes => (inputTypes, outputTypes.reduce(Fuzzer.mergeOutputType)))
          ,
          maxConcurrency = Settings.ClickHouse.maxSupportedConcurrencyOuterLoop
        )
      _ = logger.trace(s"fuzzFiniteArgsFunctions - signatures output detected")
    yield {
      res
    }

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
                      ).map(args => query(fnName, args, fuzzOverWindow))

                    executeInSequenceUntilSuccess( // Check if any value of the current possible type is valid
                      queries,
                      client.executeNoResult
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

  private def toFn[T <: CHFunctionIO](
      io: (InputTypes, OutputType),
      functionConstructor: (CHFuzzableType, String) => T
  ): T =
    io._1 match
      case Seq(arg1) => functionConstructor(arg1, io._2)
      case _         => throw Exception(s"Expected 1 argument, but found ${io._1.size} arguments")

  private def toFn[T <: CHFunctionIO](
      io: (InputTypes, OutputType),
      functionConstructor: (CHFuzzableType, CHFuzzableType, String) => T
  ): T =
    io._1 match
      case Seq(arg1, arg2) => functionConstructor(arg1, arg2, io._2)
      case _               => throw Exception(s"Expected 2 argument, but found ${io._1.size} arguments")

  private def toFn[T <: CHFunctionIO](
      io: (InputTypes, OutputType),
      functionConstructor: (CHFuzzableType, CHFuzzableType, CHFuzzableType, String) => T
  ): T =
    io._1 match
      case Seq(arg1, arg2, arg3) => functionConstructor(arg1, arg2, arg3, io._2)
      case _                     => throw Exception(s"Expected 3 argument, but found ${io._1.size} arguments")

  private def toFn[T <: CHFunctionIO](
      io: (InputTypes, OutputType),
      functionConstructor: (CHFuzzableType, CHFuzzableType, CHFuzzableType, CHFuzzableType, String) => T
  ): T =
    io._1 match
      case Seq(arg1, arg2, arg3, arg4) => functionConstructor(arg1, arg2, arg3, arg4, io._2)
      case _                           => throw Exception(s"Expected 4 argument, but found ${io._1.size} arguments")

  private type InputTypes = Seq[CHFuzzableType]
  private type OutputType = String
