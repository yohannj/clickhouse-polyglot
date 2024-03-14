package com.amendil.fuzz

import com.amendil.ConcurrencyUtils._
import com.amendil.Settings
import com.amendil.entities._
import com.amendil.fuzz.Fuzzer._
import com.amendil.http.CHClient

import scala.concurrent.{ExecutionContext, Future}

object FuzzerParametricFunctions:

  // Remove some types that are obviously not parameters
  private val parametricAbstractType = CHFuzzableAbstractType.values.toSeq.filterNot { abstractType =>
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
    if fn.isLambda || fn.isNonParametric || fn.isSpecialInfiniteFunction then Future.successful(fn)
    else
      fuzzParametricFunction(fn.name, paramCount = 1, argCount = 1)
        .flatMap { validFunction1IOs =>
          executeInParallel(
            validFunction1IOs,
            (inputTypes, outputType) =>
              val (paramType1, nonParamType1) =
                inputTypes match
                  case (Seq(paramType1), Seq(nonParamType1)) => (paramType1, nonParamType1)
                  case _                                     => throw new Exception(s"Expected 1 type, found ${inputTypes.size} types") // FIXME

              testInfiniteArgsFunctions(
                fn.name,
                paramCHFuzzableTypes = inputTypes._1,
                nonParamCHFuzzableTypes = inputTypes._2
              )
                .map { isInfiniteFunction =>
                  val function: CHFunctionIO.Parametric1Function0N | CHFunctionIO.Parametric1Function1 =
                    if isInfiniteFunction then CHFunctionIO.Parametric1Function0N(paramType1, nonParamType1, outputType)
                    else CHFunctionIO.Parametric1Function1(paramType1, nonParamType1, outputType)

                  function
                }
            ,
            maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
          ).map { case validFunctions: Seq[CHFunctionIO.Parametric1Function0N | CHFunctionIO.Parametric1Function1] =>
            val parametric1Function0Ns = validFunctions.collect { case e: CHFunctionIO.Parametric1Function0N => e }
            val parametric1Function1s = validFunctions.collect { case e: CHFunctionIO.Parametric1Function1 => e }

            fn.copy(parametric1Function0Ns = parametric1Function0Ns, parametric1Function1s = parametric1Function1s)
          }
        }

  private def fuzzFunction2Or1NWithOneParameter(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if fn.isLambda || fn.isNonParametric || fn.isSpecialInfiniteFunction || fn.parametric1Function0Ns.nonEmpty then
      Future.successful(fn)
    else
      fuzzParametricFunction(fn.name, paramCount = 1, argCount = 2)
        .flatMap { validFunction2IOs =>
          executeInParallel(
            validFunction2IOs,
            (inputTypes, outputType) =>
              val (paramType1, nonParamType1, nonParamType2) =
                inputTypes match
                  case (Seq(paramType1), Seq(nonParamType1, nonParamType2)) =>
                    (paramType1, nonParamType1, nonParamType2)
                  case _ => throw new Exception(s"Expected 2 types, but found ${inputTypes.size} types") // FIXME

              testInfiniteArgsFunctions(
                fn.name,
                paramCHFuzzableTypes = inputTypes._1,
                nonParamCHFuzzableTypes = inputTypes._2
              )
                .map { isInfiniteFunction =>
                  val function: CHFunctionIO.Parametric1Function1N | CHFunctionIO.Parametric1Function2 =
                    if isInfiniteFunction then
                      CHFunctionIO.Parametric1Function1N(paramType1, nonParamType1, nonParamType2, outputType)
                    else CHFunctionIO.Parametric1Function2(paramType1, nonParamType1, nonParamType2, outputType)

                  function
                }
            ,
            maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
          ).map { case validFunctions: Seq[CHFunctionIO.Parametric1Function1N | CHFunctionIO.Parametric1Function2] =>
            val parametric1Function1Ns = validFunctions.collect { case e: CHFunctionIO.Parametric1Function1N => e }
            val parametric1Function2s = validFunctions.collect { case e: CHFunctionIO.Parametric1Function2 => e }

            fn.copy(parametric1Function1Ns = parametric1Function1Ns, parametric1Function2s = parametric1Function2s)
          }
        }

  private def fuzzFunction1WithTwoParameters(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if fn.parametric1Function1s.isEmpty then Future.successful(fn)
    else
      val fnSample = fn.parametric1Function1s.head
      val param1Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val arg1Type = fnSample.arg1.asInstanceOf[CHFuzzableType]

      val param2Types = parametricAbstractType.flatMap(_.CHFuzzableTypes)

      executeInParallelOnlySuccess(
        param2Types,
        param2Type => {
          executeInSequenceUntilSuccess(
            crossJoin(param1Type.fuzzingValues, param2Type.fuzzingValues, arg1Type.fuzzingValues)
              .map { case (param1, param2, arg1) => s"SELECT ${fn.name}($param1, $param2)($arg1)" },
            client.execute
          ).map { if _ then Some(param2Type) else None }
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map(_.flatten)
        .map { validParam2Types =>
          val parametric2Function1s =
            for
              param2Type <- validParam2Types
              f <- fn.parametric1Function1s
            yield CHFunctionIO.Parametric2Function1(f.paramArg1, param2Type, f.arg1, f.output)

          fn.copy(parametric2Function1s = parametric2Function1s)
        }

  private def fuzzFunction1WithThreeParameters(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if fn.parametric2Function1s.isEmpty then Future.successful(fn)
    else
      val fnSample = fn.parametric2Function1s.head
      val param1Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val param2Type = fnSample.paramArg2.asInstanceOf[CHFuzzableType]
      val arg1Type = fnSample.arg1.asInstanceOf[CHFuzzableType]

      val param3Types = parametricAbstractType.flatMap(_.CHFuzzableTypes)

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
          ).map { if _ then Some(param3Type) else None }
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map(_.flatten)
        .map { validParam3Types =>
          val parametric3Function1s =
            for
              param3Type <- validParam3Types
              f <- fn.parametric2Function1s
            yield CHFunctionIO.Parametric3Function1(f.paramArg1, f.paramArg2, param3Type, f.arg1, f.output)

          fn.copy(parametric3Function1s = parametric3Function1s)
        }

  private def fuzzFunction2WithTwoParameters(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if fn.parametric1Function2s.isEmpty then Future.successful(fn)
    else
      val fnSample = fn.parametric1Function2s.head
      val param1Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val arg1Type = fnSample.arg1.asInstanceOf[CHFuzzableType]
      val arg2Type = fnSample.arg2.asInstanceOf[CHFuzzableType]

      val param2Types = parametricAbstractType.flatMap(_.CHFuzzableTypes)

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
          ).map { if _ then Some(param2Type) else None }
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map(_.flatten)
        .map { validParam2Types =>
          val parametric2Function2s =
            for
              param2Type <- validParam2Types
              f <- fn.parametric1Function2s
            yield CHFunctionIO.Parametric2Function2(f.paramArg1, param2Type, f.arg1, f.arg2, f.output)

          fn.copy(parametric2Function2s = parametric2Function2s)
        }

  private def fuzzFunction2WithThreeParameters(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if fn.parametric2Function2s.isEmpty then Future.successful(fn)
    else
      val fnSample = fn.parametric2Function2s.head
      val param1Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val param2Type = fnSample.paramArg2.asInstanceOf[CHFuzzableType]
      val arg1Type = fnSample.arg1.asInstanceOf[CHFuzzableType]
      val arg2Type = fnSample.arg2.asInstanceOf[CHFuzzableType]

      val param3Types = parametricAbstractType.flatMap(_.CHFuzzableTypes)

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
          ).map { if _ then Some(param3Type) else None }
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map(_.flatten)
        .map { validParam3Types =>
          val parametric3Function2s =
            for
              param3Type <- validParam3Types
              f <- fn.parametric2Function2s
            yield CHFunctionIO.Parametric3Function2(f.paramArg1, f.paramArg2, param3Type, f.arg1, f.arg2, f.output)

          fn.copy(parametric3Function2s = parametric3Function2s)
        }

  private def fuzzFunction0NWithTwoParameters(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if fn.parametric1Function0Ns.isEmpty then Future.successful(fn)
    else
      val fnSample = fn.parametric1Function0Ns.head
      val param1Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val argNType = fnSample.argN.asInstanceOf[CHFuzzableType]

      val param2Types = parametricAbstractType.flatMap(_.CHFuzzableTypes)

      executeInParallelOnlySuccess(
        param2Types,
        param2Type => {
          executeInSequenceUntilSuccess(
            crossJoin(param1Type.fuzzingValues, param2Type.fuzzingValues, argNType.fuzzingValues)
              .map { case (param1, param2, argN) =>
                (s"SELECT ${fn.name}($param1, $param2)($argN)", s"SELECT ${fn.name}($param1, $param2)($argN, $argN)")
              },
            (query1, query2) => client.execute(query1).recoverWith(_ => client.execute(query2))
          ).map { if _ then Some(param2Type) else None }
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map(_.flatten)
        .map { validParam2Types =>
          val parametric2Function0Ns =
            for
              param2Type <- validParam2Types
              f <- fn.parametric1Function0Ns
            yield CHFunctionIO.Parametric2Function0N(f.paramArg1, param2Type, f.argN, f.output)

          fn.copy(parametric2Function0Ns = parametric2Function0Ns)
        }

  private def fuzzFunction0NWithThreeParameters(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if fn.parametric2Function0Ns.isEmpty then Future.successful(fn)
    else
      val fnSample = fn.parametric2Function0Ns.head
      val param1Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val param2Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val argNType = fnSample.argN.asInstanceOf[CHFuzzableType]

      val param3Types = parametricAbstractType.flatMap(_.CHFuzzableTypes)

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
          ).map { if _ then Some(param3Type) else None }
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map(_.flatten)
        .map { validParam3Types =>
          val parametric3Function0Ns =
            for
              param3Type <- validParam3Types
              f <- fn.parametric2Function0Ns
            yield CHFunctionIO.Parametric3Function0N(f.paramArg1, f.paramArg2, param3Type, f.argN, f.output)

          fn.copy(parametric3Function0Ns = parametric3Function0Ns)
        }

  private def fuzzFunction1NWithTwoParameters(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if fn.parametric1Function1Ns.isEmpty then Future.successful(fn)
    else
      val fnSample = fn.parametric1Function1Ns.head
      val param1Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val arg1Type = fnSample.arg1.asInstanceOf[CHFuzzableType]
      val argNType = fnSample.argN.asInstanceOf[CHFuzzableType]

      val param2Types = parametricAbstractType.flatMap(_.CHFuzzableTypes)

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
          ).map { if _ then Some(param2Type) else None }
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map(_.flatten)
        .map { validParam2Types =>
          val parametric2Function1Ns =
            for
              param2Type <- validParam2Types
              f <- fn.parametric1Function1Ns
            yield CHFunctionIO.Parametric2Function1N(f.paramArg1, param2Type, f.arg1, f.argN, f.output)

          fn.copy(parametric2Function1Ns = parametric2Function1Ns)
        }

  private def fuzzFunction1NWithThreeParameters(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if fn.parametric2Function1Ns.isEmpty then Future.successful(fn)
    else
      val fnSample = fn.parametric2Function1Ns.head
      val param1Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val param2Type = fnSample.paramArg1.asInstanceOf[CHFuzzableType]
      val arg1Type = fnSample.arg1.asInstanceOf[CHFuzzableType]
      val argNType = fnSample.argN.asInstanceOf[CHFuzzableType]

      val param3Types = parametricAbstractType.flatMap(_.CHFuzzableTypes)

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
          ).map { if _ then Some(param3Type) else None }
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map(_.flatten)
        .map { validParam3Types =>
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
  ): Future[Seq[((Seq[CHFuzzableType], Seq[CHFuzzableType]), String)]] =
    val validCHFuzzableAbstractTypeCombinationsF =
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
          ).map(if _ then Some((paramTypes, nonParamTypes)) else None)
        },
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map(_.flatten)

    // Intermediate steps to try to avoid a lot of combinations, if we can filter many args combinations.
    val validCHAbstractParamWithNonAbstractArgsF = validCHFuzzableAbstractTypeCombinationsF.flatMap:
      validCHFuzzableAbstractTypeCombinations =>
        val checksToDo =
          validCHFuzzableAbstractTypeCombinations
            .flatMap { case (paramAbstractTypes, nonParamAbstractTypes) =>
              generateCHFuzzableTypeCombinations(nonParamAbstractTypes).map((paramAbstractTypes, _))
            }
            .flatMap { case (paramAbstractTypes, nonParamTypes) =>
              crossJoin(
                buildFuzzingValuesArgs(paramAbstractTypes.map(_.fuzzingValues)),
                buildFuzzingValuesArgs(nonParamTypes.map(_.fuzzingValues))
              ).map { case (paramArgs, nonParamArgs) => s"SELECT $fnName($paramArgs)($nonParamArgs)" }
                .map(query => (paramAbstractTypes, nonParamTypes, query))
            }

        executeInParallelOnlySuccess(
          checksToDo,
          (paramTypes, nonParamTypes, query) => client.execute(query).map(resp => (paramTypes, nonParamTypes)),
          maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
        )

    validCHAbstractParamWithNonAbstractArgsF
      .flatMap { validCHAbstractParamWithNonAbstractArgs =>
        val checksToDo =
          validCHAbstractParamWithNonAbstractArgs
            .flatMap { case (paramAbstractTypes, nonParamTypes) =>
              generateCHFuzzableTypeCombinations(paramAbstractTypes).map((_, nonParamTypes))
            }
            .flatMap { case (paramTypes, nonParamTypes) =>
              crossJoin(
                buildFuzzingValuesArgs(paramTypes.map(_.fuzzingValues)),
                buildFuzzingValuesArgs(nonParamTypes.map(_.fuzzingValues))
              ).map { case (paramArgs, nonParamArgs) => s"SELECT $fnName($paramArgs)($nonParamArgs)" }
                .map(query => (paramTypes, nonParamTypes, query))
            }

        executeInParallelOnlySuccess(
          checksToDo,
          (paramTypes, nonParamTypes, query) =>
            client.execute(query).map(resp => ((paramTypes, nonParamTypes), resp.meta.head.`type`)),
          maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
        )
      }
      .map { results =>
        results.groupBy(_._1).view.mapValues(_.map(_._2).reduce(CHFuzzableType.merge)).toSeq
      }

  /**
    * The future generated by this function never fails.
    *
    * @return Future.successful(true) if last argument can be repeated many times, otherwise Future.successful(false)
    */
  private def testInfiniteArgsFunctions(
      fnName: String,
      paramCHFuzzableTypes: Seq[CHFuzzableType],
      nonParamCHFuzzableTypes: Seq[CHFuzzableType]
  )(implicit client: CHClient, ec: ExecutionContext): Future[Boolean] =
    require(nonParamCHFuzzableTypes.nonEmpty, "Expected at least one defined argument, but none found.")

    val argNv1 = Range(0, 10).toSeq.map(_ => Seq(nonParamCHFuzzableTypes.last.fuzzingValues.head))
    val argNv2 = Range(0, 11).toSeq.map(_ => Seq(nonParamCHFuzzableTypes.last.fuzzingValues.head))

    val fuzzingValuesParams = buildFuzzingValuesArgs(paramCHFuzzableTypes.map(_.fuzzingValues))
    val fuzzingValuesArgsv1 = buildFuzzingValuesArgs(nonParamCHFuzzableTypes.map(_.fuzzingValues) ++ argNv1)
    val fuzzingValuesArgsv2 = buildFuzzingValuesArgs(nonParamCHFuzzableTypes.map(_.fuzzingValues) ++ argNv2)

    executeInSequenceUntilSuccess(
      crossJoin(fuzzingValuesParams, fuzzingValuesArgsv1 ++ fuzzingValuesArgsv2).map { case (paramArgs, nonParamArgs) =>
        s"SELECT $fnName($paramArgs)($nonParamArgs)"
      },
      client.execute
    )

  private def crossJoin[T, U](s1: Seq[T], s2: Seq[U]): Seq[(T, U)] =
    for
      e1 <- s1
      e2 <- s2
    yield { (e1, e2) }

  private def crossJoin[T, U, V](s1: Seq[T], s2: Seq[U], s3: Seq[V]): Seq[(T, U, V)] =
    for
      e1 <- s1
      e2 <- s2
      e3 <- s3
    yield { (e1, e2, e3) }

  private def crossJoin[T, U, V, W](s1: Seq[T], s2: Seq[U], s3: Seq[V], s4: Seq[W]): Seq[(T, U, V, W)] =
    for
      e1 <- s1
      e2 <- s2
      e3 <- s3
      e4 <- s4
    yield { (e1, e2, e3, e4) }

  private def crossJoin[T, U, V, W, X](
      s1: Seq[T],
      s2: Seq[U],
      s3: Seq[V],
      s4: Seq[W],
      s5: Seq[X]
  ): Seq[(T, U, V, W, X)] =
    for
      e1 <- s1
      e2 <- s2
      e3 <- s3
      e4 <- s4
      e5 <- s5
    yield { (e1, e2, e3, e4, e5) }
