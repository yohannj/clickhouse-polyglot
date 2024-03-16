package com.amendil.fuzz

import com.amendil.ConcurrencyUtils._
import com.amendil.Settings
import com.amendil.entities._
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
      abstractType.fuzzingValues.head.contains("::Map(") ||
      abstractType.fuzzingValues.head.contains("::Tuple(")
  }

  private[fuzz] def fuzzingFunctionWithCost(
      implicit client: CHClient,
      ec: ExecutionContext
  ): Seq[((CHFunctionFuzzResult) => Future[CHFunctionFuzzResult], Long)] =
    val paramCount = parametricAbstractType.flatMap(_.CHFuzzableTypes).size.toLong
    val argCount = CHFuzzableType.values.size.toLong
    Seq(
      (fuzzFunction1Or0NWithOneParameter, paramCount * argCount),
      (fuzzFunction2Or1NWithOneParameter, paramCount * argCount * argCount),
      // Functions below MUST happen after the "WithOneParameter", they are then very easy to compute
      (fuzzFunction1WithTwoParameters, paramCount * argCount + 1),
      (fuzzFunction0NWithTwoParameters, paramCount * argCount + 1),
      (fuzzFunction2WithTwoParameters, paramCount * argCount * argCount + 1),
      (fuzzFunction1NWithTwoParameters, paramCount * argCount * argCount + 1),
      // Functions below MUST happen after the "WithTwoParameter", they are then very easy to compute
      (fuzzFunction1WithThreeParameters, paramCount * argCount + 2),
      (fuzzFunction0NWithThreeParameters, paramCount * argCount + 2),
      (fuzzFunction2WithThreeParameters, paramCount * argCount * argCount + 2),
      (fuzzFunction1NWithThreeParameters, paramCount * argCount * argCount + 2)
    )

  private def fuzzFunction1Or0NWithOneParameter(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
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
            constructor: (CHFuzzableType, CHFuzzableType, String) => T
        ): T =
          io._1 match
            case (Seq(param1), Seq(arg1)) => constructor(param1, arg1, io._2)
            case _                        => throw new Exception(s"Expected 1 type, found ${io.size} types") // FIXME

        if fnHasInfiniteArgs then
          fn.copy(parametric1Function0Ns = functions.map(toFn(_, CHFunctionIO.Parametric1Function0N.apply)))
        else fn.copy(parametric1Function1s = functions.map(toFn(_, CHFunctionIO.Parametric1Function1.apply)))
      }

  private def fuzzFunction2Or1NWithOneParameter(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
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
            constructor: (CHFuzzableType, CHFuzzableType, CHFuzzableType, String) => T
        ): T =
          io._1 match
            case (Seq(param1), Seq(arg1, arg2)) => constructor(param1, arg1, arg2, io._2)
            case _ =>
              throw new Exception(
                s"Expected 1 parameter and 2 arguments, but found ${io._1.parameters.size} parameters and ${io._1.arguments.size} arguments"
              )

        if fnHasInfiniteArgs then
          fn.copy(parametric1Function1Ns = functions.map(toFn(_, CHFunctionIO.Parametric1Function1N.apply)))
        else fn.copy(parametric1Function2s = functions.map(toFn(_, CHFunctionIO.Parametric1Function2.apply)))
      }

  /**
    * For all parametric function with 1 argument and 1 parameter previously found,
    * fuzz if we can add a second parameter.
    *
    * /!\ This method does not try to bruteforce two parameters at once!
    *
    * @return A copy of the given FuzzResult, including all new methods found.
    */
  private def fuzzFunction1WithTwoParameters(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1WithTwoParameters")
    if fn.parametric1Function1s.isEmpty then Future.successful(fn)
    else
      val fnSample = fn.parametric1Function1s.head
      val param1Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val arg1Type = fnSample.arg1.asInstanceOf[CHFuzzableType]

      val param2Types = parametricAbstractType.flatMap(_.CHFuzzableTypes)

      // For each possible type that the second parameter can take
      // Query ClickHouse using only one of the previously found signatures
      executeInParallelOnlySuccess(
        param2Types,
        param2Type => {
          executeInSequenceUntilSuccess(
            crossJoin(param1Type.fuzzingValues, param2Type.fuzzingValues, arg1Type.fuzzingValues)
              .map { case (param1, param2, arg1) => s"SELECT ${fn.name}($param1, $param2)($arg1)" },
            client.execute
          ).map(_ => param2Type)
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map { (validParam2Types: Seq[CHFuzzableType]) =>
        val parametric2Function1s =
          for
            param2Type <- validParam2Types
            f <- fn.parametric1Function1s
          yield CHFunctionIO.Parametric2Function1(f.paramArg1, param2Type, f.arg1, f.output)

        fn.copy(parametric2Function1s = parametric2Function1s)
      }

  /**
    * For all parametric function with 1 argument and 2 parameters previously found,
    * fuzz if we can add a third parameter.
    *
    * /!\ This method does not try to bruteforce three parameters at once!
    *
    * @return A copy of the given FuzzResult, including all new methods found.
    */
  private def fuzzFunction1WithThreeParameters(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1WithThreeParameters")
    if fn.parametric2Function1s.isEmpty then Future.successful(fn)
    else
      val fnSample = fn.parametric2Function1s.head
      val param1Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val param2Type = fnSample.paramArg2.asInstanceOf[CHFuzzableType]
      val arg1Type = fnSample.arg1.asInstanceOf[CHFuzzableType]

      val param3Types = parametricAbstractType.flatMap(_.CHFuzzableTypes)

      // For each possible type that the third parameter can take
      // Query ClickHouse using only one of the previously found signatures
      executeInParallelOnlySuccess(
        param3Types,
        param3Type => {
          executeInSequenceUntilSuccess(
            crossJoin(
              param1Type.fuzzingValues,
              param2Type.fuzzingValues,
              param3Type.fuzzingValues,
              arg1Type.fuzzingValues
            )
              .map { case (param1, param2, param3, arg1) => s"SELECT ${fn.name}($param1, $param2, $param3)($arg1)" },
            client.execute
          ).map(_ => param3Type)
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map { validParam3Types =>
        val parametric3Function1s =
          for
            param3Type <- validParam3Types
            f <- fn.parametric2Function1s
          yield CHFunctionIO.Parametric3Function1(f.paramArg1, f.paramArg2, param3Type, f.arg1, f.output)

        fn.copy(parametric3Function1s = parametric3Function1s)
      }

  /**
    * For all parametric function with 2 arguments and 1 parameter previously found,
    * fuzz if we can add a second parameter.
    *
    * /!\ This method does not try to bruteforce two parameters at once!
    *
    * @return A copy of the given FuzzResult, including all new methods found.
    */
  private def fuzzFunction2WithTwoParameters(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2WithTwoParameters")
    if fn.parametric1Function2s.isEmpty then Future.successful(fn)
    else
      val fnSample = fn.parametric1Function2s.head
      val param1Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val arg1Type = fnSample.arg1.asInstanceOf[CHFuzzableType]
      val arg2Type = fnSample.arg2.asInstanceOf[CHFuzzableType]

      val param2Types = parametricAbstractType.flatMap(_.CHFuzzableTypes)

      // For each possible type that the second parameter can take
      // Query ClickHouse using only one of the previously found signatures
      executeInParallelOnlySuccess(
        param2Types,
        param2Type => {
          executeInSequenceUntilSuccess(
            crossJoin(
              param1Type.fuzzingValues,
              param2Type.fuzzingValues,
              arg1Type.fuzzingValues,
              arg2Type.fuzzingValues
            )
              .map { case (param1, param2, arg1, arg2) => s"SELECT ${fn.name}($param1, $param2)($arg1, $arg2)" },
            client.execute
          ).map(_ => param2Type)
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map { validParam2Types =>
        val parametric2Function2s =
          for
            param2Type <- validParam2Types
            f <- fn.parametric1Function2s
          yield CHFunctionIO.Parametric2Function2(f.paramArg1, param2Type, f.arg1, f.arg2, f.output)

        fn.copy(parametric2Function2s = parametric2Function2s)
      }

  /**
    * For all parametric function with 2 arguments and 2 parameters previously found,
    * fuzz if we can add a third parameter.
    *
    * /!\ This method does not try to bruteforce three parameters at once!
    *
    * @return A copy of the given FuzzResult, including all new methods found.
    */
  private def fuzzFunction2WithThreeParameters(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2WithThreeParameters")
    if fn.parametric2Function2s.isEmpty then Future.successful(fn)
    else
      val fnSample = fn.parametric2Function2s.head
      val param1Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val param2Type = fnSample.paramArg2.asInstanceOf[CHFuzzableType]
      val arg1Type = fnSample.arg1.asInstanceOf[CHFuzzableType]
      val arg2Type = fnSample.arg2.asInstanceOf[CHFuzzableType]

      val param3Types = parametricAbstractType.flatMap(_.CHFuzzableTypes)

      // For each possible type that the third parameter can take
      // Query ClickHouse using only one of the previously found signatures
      executeInParallelOnlySuccess(
        param3Types,
        param3Type => {
          executeInSequenceUntilSuccess(
            crossJoin(
              param1Type.fuzzingValues,
              param2Type.fuzzingValues,
              param3Type.fuzzingValues,
              arg1Type.fuzzingValues,
              arg2Type.fuzzingValues
            )
              .map { case (param1, param2, param3, arg1, arg2) =>
                s"SELECT ${fn.name}($param1, $param2, $param3)($arg1, $arg2)"
              },
            client.execute
          ).map(_ => param3Type)
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map { validParam3Types =>
        val parametric3Function2s =
          for
            param3Type <- validParam3Types
            f <- fn.parametric2Function2s
          yield CHFunctionIO.Parametric3Function2(f.paramArg1, f.paramArg2, param3Type, f.arg1, f.arg2, f.output)

        fn.copy(parametric3Function2s = parametric3Function2s)
      }

  /**
    * For all parametric function with 0N arguments and 1 parameter previously found,
    * fuzz if we can add a second parameter.
    *
    * /!\ This method does not try to bruteforce two parameters at once!
    *
    * @return A copy of the given FuzzResult, including all new methods found.
    */
  private def fuzzFunction0NWithTwoParameters(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction0NWithTwoParameters")
    if fn.parametric1Function0Ns.isEmpty then Future.successful(fn)
    else
      val fnSample = fn.parametric1Function0Ns.head
      val param1Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val argNType = fnSample.argN.asInstanceOf[CHFuzzableType]

      val param2Types = parametricAbstractType.flatMap(_.CHFuzzableTypes)

      // For each possible type that the second parameter can take
      // Query ClickHouse using only one of the previously found signatures
      executeInParallelOnlySuccess(
        param2Types,
        param2Type => {
          executeInSequenceUntilSuccess(
            crossJoin(param1Type.fuzzingValues, param2Type.fuzzingValues, argNType.fuzzingValues)
              .map { case (param1, param2, argN) =>
                (s"SELECT ${fn.name}($param1, $param2)($argN)", s"SELECT ${fn.name}($param1, $param2)($argN, $argN)")
              },
            (query1, query2) => client.execute(query1).recoverWith(_ => client.execute(query2))
          ).map(_ => param2Type)
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map { validParam2Types =>
        val parametric2Function0Ns =
          for
            param2Type <- validParam2Types
            f <- fn.parametric1Function0Ns
          yield CHFunctionIO.Parametric2Function0N(f.paramArg1, param2Type, f.argN, f.output)

        fn.copy(parametric2Function0Ns = parametric2Function0Ns)
      }

  /**
    * For all parametric function with 0N arguments and 2 parameters previously found,
    * fuzz if we can add a third parameter.
    *
    * /!\ This method does not try to bruteforce three parameters at once!
    *
    * @return A copy of the given FuzzResult, including all new methods found.
    */
  private def fuzzFunction0NWithThreeParameters(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction0NWithThreeParameters")
    if fn.parametric2Function0Ns.isEmpty then Future.successful(fn)
    else
      val fnSample = fn.parametric2Function0Ns.head
      val param1Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val param2Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val argNType = fnSample.argN.asInstanceOf[CHFuzzableType]

      val param3Types = parametricAbstractType.flatMap(_.CHFuzzableTypes)

      // For each possible type that the third parameter can take
      // Query ClickHouse using only one of the previously found signatures
      executeInParallelOnlySuccess(
        param3Types,
        param3Type => {
          executeInSequenceUntilSuccess(
            crossJoin(
              param1Type.fuzzingValues,
              param2Type.fuzzingValues,
              param3Type.fuzzingValues,
              argNType.fuzzingValues
            )
              .map { case (param1, param2, param3, argN) =>
                (
                  s"SELECT ${fn.name}($param1, $param2, $param3)($argN)",
                  s"SELECT ${fn.name}($param1, $param2, $param3)($argN, $argN)"
                )
              },
            (query1, query2) => client.execute(query1).recoverWith(_ => client.execute(query2))
          ).map(_ => param3Type)
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map { validParam3Types =>
        val parametric3Function0Ns =
          for
            param3Type <- validParam3Types
            f <- fn.parametric2Function0Ns
          yield CHFunctionIO.Parametric3Function0N(f.paramArg1, f.paramArg2, param3Type, f.argN, f.output)

        fn.copy(parametric3Function0Ns = parametric3Function0Ns)
      }

  /**
    * For all parametric function with 1N arguments and 1 parameter previously found,
    * fuzz if we can add a second parameter.
    *
    * /!\ This method does not try to bruteforce two parameters at once!
    *
    * @return A copy of the given FuzzResult, including all new methods found.
    */
  private def fuzzFunction1NWithTwoParameters(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1NWithTwoParameters")
    if fn.parametric1Function1Ns.isEmpty then Future.successful(fn)
    else
      val fnSample = fn.parametric1Function1Ns.head
      val param1Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val arg1Type = fnSample.arg1.asInstanceOf[CHFuzzableType]
      val argNType = fnSample.argN.asInstanceOf[CHFuzzableType]

      val param2Types = parametricAbstractType.flatMap(_.CHFuzzableTypes)

      // For each possible type that the second parameter can take
      // Query ClickHouse using only one of the previously found signatures
      executeInParallelOnlySuccess(
        param2Types,
        param2Type => {
          executeInSequenceUntilSuccess(
            crossJoin(
              param1Type.fuzzingValues,
              param2Type.fuzzingValues,
              arg1Type.fuzzingValues,
              argNType.fuzzingValues
            )
              .map { case (param1, param2, arg1, argN) =>
                (
                  s"SELECT ${fn.name}($param1, $param2)($arg1, $argN)",
                  s"SELECT ${fn.name}($param1, $param2)($arg1, $argN, $argN)"
                )
              },
            (query1, query2) => client.execute(query1).recoverWith(_ => client.execute(query2))
          ).map(_ => param2Type)
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map { validParam2Types =>
        val parametric2Function1Ns =
          for
            param2Type <- validParam2Types
            f <- fn.parametric1Function1Ns
          yield CHFunctionIO.Parametric2Function1N(f.paramArg1, param2Type, f.arg1, f.argN, f.output)

        fn.copy(parametric2Function1Ns = parametric2Function1Ns)
      }

  /**
    * For all parametric function with 1N arguments and 2 parameters previously found,
    * fuzz if we can add a third parameter.
    *
    * /!\ This method does not try to bruteforce three parameters at once!
    *
    * @return A copy of the given FuzzResult, including all new methods found.
    */
  private def fuzzFunction1NWithThreeParameters(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction1NWithThreeParameters")
    if fn.parametric2Function1Ns.isEmpty then Future.successful(fn)
    else
      val fnSample = fn.parametric2Function1Ns.head
      val param1Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val param2Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val arg1Type = fnSample.arg1.asInstanceOf[CHFuzzableType]
      val argNType = fnSample.argN.asInstanceOf[CHFuzzableType]

      val param3Types = parametricAbstractType.flatMap(_.CHFuzzableTypes)

      // For each possible type that the third parameter can take
      // Query ClickHouse using only one of the previously found signatures
      executeInParallelOnlySuccess(
        param3Types,
        param3Type => {
          executeInSequenceUntilSuccess(
            crossJoin(
              param1Type.fuzzingValues,
              param2Type.fuzzingValues,
              param3Type.fuzzingValues,
              arg1Type.fuzzingValues,
              argNType.fuzzingValues
            )
              .map { case (param1, param2, param3, arg1, argN) =>
                (
                  s"SELECT ${fn.name}($param1, $param2, $param3)($arg1, $argN)",
                  s"SELECT ${fn.name}($param1, $param2, $param3)($arg1, $argN, $argN)"
                )
              },
            (query1, query2) => client.execute(query1).recoverWith(_ => client.execute(query2))
          ).map(_ => param3Type)
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map { validParam3Types =>
        val parametric3Function1Ns =
          for
            param3Type <- validParam3Types
            f <- fn.parametric2Function1Ns
          yield CHFunctionIO.Parametric3Function1N(f.paramArg1, f.paramArg2, param3Type, f.arg1, f.argN, f.output)

        fn.copy(parametric3Function1Ns = parametric3Function1Ns)
      }

  private def fuzzParametricFunction(
      fnName: String,
      paramCount: Int,
      argCount: Int
  )(
      implicit client: CHClient,
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

    validCHFuzzableAbstractTypeCombinationsF.flatMap:
      (validCHFuzzableAbstractTypeCombinations: Seq[ParametricFunctionAbstractInput]) =>
        if validCHFuzzableAbstractTypeCombinations.isEmpty then Future.successful(Nil)
        else
          assume(
            validCHFuzzableAbstractTypeCombinations
              .groupBy(_.parameters)
              .values
              .map(
                _.map(_.arguments)
              ) // For each combination of parameters, we retrieve all the kind of available arguments
              .toSet
              .size == 1, // All combination of parameters should support exactly the same combinations of arguments.
            s"While fuzzing $fnName with $paramCount parameters and $argCount arguments," +
              s" found some combinations of parameters that does not support the same combinations of arguments."
          )

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
            outputTypeByArguments: Map[NonParametricArguments, OutputType] <-
              executeInParallelOnlySuccess(
                argumentsAndSqlQuery,
                (nonParamTypes, queries) =>
                  executeInSequenceOnlySuccess(queries, client.execute).map(resp =>
                    (nonParamTypes, resp.map(_.meta.head.`type`).reduce(CHFuzzableType.merge))
                  ),
                maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
              ).map(_.toMap)
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
    * The future generated by this function never fails.
    *
    * @return Future.successful(true) if last argument can be repeated many times, otherwise Future.successful(false)
    */
  private def testInfiniteArgsFunctions(
      fnName: String,
      inputTypes: ParametricFunctionInput
  )(implicit client: CHClient, ec: ExecutionContext): Future[Boolean] =
    val arguments: NonParametricArguments = inputTypes.arguments
    require(arguments.nonEmpty, "Expected at least one defined argument, but none found.")

    val argNv1 = Range(0, 10).toSeq.map(_ => Seq(arguments.last.fuzzingValues.head))
    val argNv2 = Range(0, 11).toSeq.map(_ => Seq(arguments.last.fuzzingValues.head))

    val fuzzingValuesParams = buildFuzzingValuesArgs(inputTypes.parameters.map(_.fuzzingValues))
    val fuzzingValuesArgsV1 = buildFuzzingValuesArgs(arguments.map(_.fuzzingValues) ++ argNv1)
    val fuzzingValuesArgsV2 = buildFuzzingValuesArgs(arguments.map(_.fuzzingValues) ++ argNv2)

    executeInSequenceUntilSuccess(
      crossJoin(fuzzingValuesParams, fuzzingValuesArgsV1 ++ fuzzingValuesArgsV2).map { case (params, args) =>
        s"SELECT $fnName($params)($args)"
      },
      client.execute
    ).map(_ => true).recover(_ => false)

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
