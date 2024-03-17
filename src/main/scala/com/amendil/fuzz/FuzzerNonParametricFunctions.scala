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
      fuzzFiniteArgsFunctions(fn.name, argCount = 1)
        .flatMap { (validFunction1IOs: Seq[(InputTypes, OutputType)]) =>
          executeInParallel(
            validFunction1IOs,
            (inputTypes, outputType) =>
              val inputType =
                inputTypes match
                  case Seq(type1) => type1
                  case _          => throw new Exception(s"Expected 1 type, found ${inputTypes.size} types")

              testInfiniteArgsFunctions(fn.name, inputTypes)
                .map { isInfiniteFunction =>
                  val function: CHFunctionIO.Function0N | CHFunctionIO.Function1 =
                    if isInfiniteFunction then CHFunctionIO.Function0N(inputType, outputType)
                    else CHFunctionIO.Function1(inputType, outputType)

                  function
                }
            ,
            maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
          ).map { case validFunctions: Seq[CHFunctionIO.Function0N | CHFunctionIO.Function1] =>
            val function0Ns = validFunctions.collect { case e: CHFunctionIO.Function0N => e }
            val function1s = validFunctions.collect { case e: CHFunctionIO.Function1 => e }

            fn.copy(function0Ns = function0Ns, function1s = function1s)
          }
        }

  private def fuzzFunction2Or1N(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction2Or1N")
    if fn.isLambda || fn.isParametric || fn.isSpecialInfiniteFunction || fn.function0Ns.nonEmpty then
      Future.successful(fn)
    else
      fuzzFiniteArgsFunctions(fn.name, argCount = 2)
        .flatMap { (validFunction2IOs: Seq[(InputTypes, OutputType)]) =>
          executeInParallel(
            validFunction2IOs,
            (inputTypes, outputType) =>
              val (inputType1, inputType2) =
                inputTypes match
                  case Seq(type1, type2) => (type1, type2)
                  case _                 => throw new Exception(s"Expected 2 types, but found ${inputTypes.size} types")

              testInfiniteArgsFunctions(fn.name, inputTypes)
                .map { isInfiniteFunction =>
                  val function: CHFunctionIO.Function1N | CHFunctionIO.Function2 =
                    if isInfiniteFunction then CHFunctionIO.Function1N(inputType1, inputType2, outputType)
                    else CHFunctionIO.Function2(inputType1, inputType2, outputType)

                  function
                }
            ,
            maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
          ).map { case validFunctions: Seq[CHFunctionIO.Function1N | CHFunctionIO.Function2] =>
            val function1Ns = validFunctions.collect { case e: CHFunctionIO.Function1N => e }
            val function2s = validFunctions.collect { case e: CHFunctionIO.Function2 => e }

            fn.copy(function1Ns = function1Ns, function2s = function2s)
          }
        }

  private def fuzzFunction3Or2N(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzFunction3Or2N")
    if fn.isLambda || fn.isParametric || fn.isSpecialInfiniteFunction || fn.function0Ns.nonEmpty || fn.function1Ns.nonEmpty ||
      (fn.function1s.filterNot(_.arg1.name.startsWith("Tuple")).nonEmpty && fn.function2s.isEmpty)
    then Future.successful(fn)
    else
      fuzzFiniteArgsFunctions(fn.name, argCount = 3)
        .flatMap { (validFunction3IOs: Seq[(InputTypes, OutputType)]) =>
          executeInParallel(
            validFunction3IOs,
            (inputTypes, outputType) =>
              val (inputType1, inputType2, inputType3) =
                inputTypes match
                  case Seq(type1, type2, type3) => (type1, type2, type3)
                  case _                        => throw new Exception(s"Expected 3 types, but found ${inputTypes.size} types")

              testInfiniteArgsFunctions(fn.name, inputTypes)
                .map { isInfiniteFunction =>
                  val function: CHFunctionIO.Function2N | CHFunctionIO.Function3 =
                    if isInfiniteFunction then CHFunctionIO.Function2N(inputType1, inputType2, inputType3, outputType)
                    else CHFunctionIO.Function3(inputType1, inputType2, inputType3, outputType)

                  function
                }
            ,
            maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
          ).map { case validFunctions: Seq[CHFunctionIO.Function2N | CHFunctionIO.Function3] =>
            val function2Ns = validFunctions.collect { case e: CHFunctionIO.Function2N => e }
            val function3s = validFunctions.collect { case e: CHFunctionIO.Function3 => e }

            fn.copy(function2Ns = function2Ns, function3s = function3s)
          }
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
      fuzzFiniteArgsFunctions(fn.name, argCount = 4)
        .flatMap { (validFunction4IOs: Seq[(InputTypes, OutputType)]) =>
          executeInParallel(
            validFunction4IOs,
            (inputTypes, outputType) =>
              val (inputType1, inputType2, inputType3, inputType4) =
                inputTypes match
                  case Seq(type1, type2, type3, type4) => (type1, type2, type3, type4)
                  case _                               => throw new Exception(s"Expected 4 types, but found ${inputTypes.size} types")

              testInfiniteArgsFunctions(fn.name, inputTypes)
                .map { isInfiniteFunction =>
                  val function: CHFunctionIO.Function3N | CHFunctionIO.Function4 =
                    if isInfiniteFunction then
                      CHFunctionIO.Function3N(inputType1, inputType2, inputType3, inputType4, outputType)
                    else CHFunctionIO.Function4(inputType1, inputType2, inputType3, inputType4, outputType)

                  function
                }
            ,
            maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
          ).map { case validFunctions: Seq[CHFunctionIO.Function3N | CHFunctionIO.Function4] =>
            val function3Ns = validFunctions.collect { case e: CHFunctionIO.Function3N => e }
            val function4s = validFunctions.collect { case e: CHFunctionIO.Function4 => e }

            fn.copy(function3Ns = function3Ns, function4s = function4s)
          }
        }

  private type InputTypes = Seq[CHFuzzableType]
  private type OutputType = String
  private def fuzzFiniteArgsFunctions(
      fnName: String,
      argCount: Int
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[(InputTypes, OutputType)]] =
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

    validCHFuzzableAbstractTypeCombinationsF.flatMap { validCHFuzzableAbstractTypeCombinations =>
      val checksToDo =
        validCHFuzzableAbstractTypeCombinations
          .flatMap(generateCHFuzzableTypeCombinations(_))
          .flatMap { CHFuzzableTypesArgs =>
            buildFuzzingValuesArgs(CHFuzzableTypesArgs.map(_.fuzzingValues))
              .map(args => s"SELECT $fnName($args)")
              .map(query => (CHFuzzableTypesArgs, query))
          }

      executeInParallelOnlySuccess(
        checksToDo,
        (CHFuzzableTypesArgs, query) =>
          client.execute(query).map((resp: CHResponse) => (CHFuzzableTypesArgs, resp.meta.head.`type`)),
        maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
      ).map { results =>
        results.groupBy(_._1).view.mapValues(_.map(_._2).reduce(CHFuzzableType.merge)).toSeq
      }
    }

  /**
    * The future generated by this function never fails.
    *
    * @return Future.successful(true) if last argument can be repeated many times, otherwise Future.successful(false)
    */
  private def testInfiniteArgsFunctions(
      fnName: String,
      CHFuzzableTypes: Seq[CHFuzzableType]
  )(using client: CHClient, ec: ExecutionContext): Future[Boolean] =
    require(CHFuzzableTypes.nonEmpty, "Expected at least one defined argument, but none found.")

    val argNv1: Seq[Seq[String]] = Range(0, 10).toSeq.map(_ => Seq(CHFuzzableTypes.last.fuzzingValues.head))
    val argNv2: Seq[Seq[String]] = Range(0, 11).toSeq.map(_ => Seq(CHFuzzableTypes.last.fuzzingValues.head))

    val fuzzingValuesArgsv1 = buildFuzzingValuesArgs(CHFuzzableTypes.map(_.fuzzingValues) ++ argNv1)
    val fuzzingValuesArgsv2 = buildFuzzingValuesArgs(CHFuzzableTypes.map(_.fuzzingValues) ++ argNv2)

    executeInSequenceUntilSuccess(
      (fuzzingValuesArgsv1 ++ fuzzingValuesArgsv2).map(args => s"SELECT $fnName($args)"),
      client.execute
    ).map(_ => true).recover(_ => false)
