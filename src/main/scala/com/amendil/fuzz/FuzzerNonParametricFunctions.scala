package com.amendil.fuzz

import com.amendil.ConcurrencyUtils._
import com.amendil.entities._
import com.amendil.fuzz.Fuzzer._
import com.amendil.http.CHClient

import scala.concurrent.{ExecutionContext, Future}

object FuzzerNonParametricFunctions:
  private[fuzz] def fuzz(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    // Future.successful(fn)
    fuzzFunction0(fn)
      .flatMap(fuzzFunction1Or0N)
      .flatMap(fuzzFunction2Or1N)
      .flatMap(fuzzFunction3Or2N)

  private def fuzzFunction0(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    client
      .execute(s"SELECT ${fn.name}()")
      .map(resp =>
        val outputType = resp.meta.head.`type`
        fn.copy(function0Opt = Some(CHFunctionIO.Function0(outputType)))
      )
      .recover(_ => fn)

  private def fuzzFunction1Or0N(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    fuzzFiniteArgsFunctions(fn.name, argCount = 1)
      .flatMap { validFunction1IOs =>
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
                  if isInfiniteFunction then
                    CHFunctionIO.Function0N(inputType, outputType)
                  else
                    CHFunctionIO.Function1(inputType, outputType)

                function
              }
          ,
          maxConcurrency = 40
        ).map { case validFunctions: Seq[CHFunctionIO.Function0N | CHFunctionIO.Function1] =>
          val function0Ns = validFunctions.collect { case e: CHFunctionIO.Function0N => e }
          val function1s = validFunctions.collect { case e: CHFunctionIO.Function1 => e }

          fn.copy(function0Ns = function0Ns, function1s = function1s)
        }
      }

  private def fuzzFunction2Or1N(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if fn.function0Ns.nonEmpty then
      Future.successful(fn)
    else
      fuzzFiniteArgsFunctions(fn.name, argCount = 2)
        .flatMap { validFunction2IOs =>
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
                    if isInfiniteFunction then
                      CHFunctionIO.Function1N(inputType1, inputType2, outputType)
                    else
                      CHFunctionIO.Function2(inputType1, inputType2, outputType)

                  function
                }
            ,
            maxConcurrency = 40
          ).map { case validFunctions: Seq[CHFunctionIO.Function1N | CHFunctionIO.Function2] =>
            val function1Ns = validFunctions.collect { case e: CHFunctionIO.Function1N => e }
            val function2s = validFunctions.collect { case e: CHFunctionIO.Function2 => e }

            fn.copy(function1Ns = function1Ns, function2s = function2s)
          }
        }

  private def fuzzFunction3Or2N(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if
      fn.function0Ns.nonEmpty || fn.function1Ns.nonEmpty || (
        fn.function1s.filterNot(_.arg1.name.startsWith("Tuple")).nonEmpty &&
          fn.function2s.isEmpty
      )
    then
      Future.successful(fn)
    else
      fuzzFiniteArgsFunctions(fn.name, argCount = 3)
        .flatMap { validFunction3IOs =>
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
                    if isInfiniteFunction then
                      CHFunctionIO.Function2N(inputType1, inputType2, inputType3, outputType)
                    else
                      CHFunctionIO.Function3(inputType1, inputType2, inputType3, outputType)

                  function
                }
            ,
            maxConcurrency = 40
          ).map { case validFunctions: Seq[CHFunctionIO.Function2N | CHFunctionIO.Function3] =>
            val function2Ns = validFunctions.collect { case e: CHFunctionIO.Function2N => e }
            val function3s = validFunctions.collect { case e: CHFunctionIO.Function3 => e }

            fn.copy(function2Ns = function2Ns, function3s = function3s)
          }
        }

  private def fuzzFunction4(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if
      fn.function0Ns.nonEmpty || ((fn.function1s.filterNot(_.arg1.name.startsWith("Tuple")).nonEmpty || fn.function2s
        .filterNot(f => f.arg1.name.startsWith("Tuple") || f.arg2.name.startsWith("Tuple"))
        .nonEmpty) && fn.function3s.isEmpty)
    then
      Future.successful(fn)
    else
      fuzzFiniteArgsFunctions(fn.name, argCount = 4)
        .map { list =>
          val validTypes = list.map { (inputTypes, outputType) =>
            inputTypes match
              case Seq(type1, type2, type3, type4) =>
                CHFunctionIO.Function4(
                  type1,
                  type2,
                  type3,
                  type4,
                  outputType
                )
              case _ => throw new Exception(s"Expected 4 types, but found ${list.size} types")
          }

          fn.copy(function4s = validTypes)
        }

    if
      fn.function0Ns.nonEmpty || fn.function1Ns.nonEmpty || fn.function2Ns.nonEmpty || (
        fn.function1s.filterNot(_.arg1.name.startsWith("Tuple")).nonEmpty &&
          fn.function2s.isEmpty &&
          fn.function3s.isEmpty
      )
    then
      Future.successful(fn)
    else
      fuzzFiniteArgsFunctions(fn.name, argCount = 4)
        .flatMap { validFunction4IOs =>
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
                    else
                      CHFunctionIO.Function4(inputType1, inputType2, inputType3, inputType4, outputType)

                  function
                }
            ,
            maxConcurrency = 40
          ).map { case validFunctions: Seq[CHFunctionIO.Function3N | CHFunctionIO.Function4] =>
            val function3Ns = validFunctions.collect { case e: CHFunctionIO.Function3N => e }
            val function4s = validFunctions.collect { case e: CHFunctionIO.Function4 => e }

            fn.copy(function3Ns = function3Ns, function4s = function4s)
          }
        }

  private def fuzzFiniteArgsFunctions(
      fnName: String,
      argCount: Int
  )(implicit client: CHClient, ec: ExecutionContext): Future[Seq[(Seq[CHType], String)]] =
    val validCHAbstractTypeCombinationsF =
      executeInParallelOnlySuccess(
        generateCHAbstractTypeCombinations(argCount),
        abstractTypes => {
          executeInSequenceUntilSuccess(
            buildFuzzingValuesArgs(abstractTypes.map(_.fuzzingValues)).map(args => s"SELECT $fnName($args)"),
            client.execute
          ).map(if _ then Some(abstractTypes) else None)
        },
        maxConcurrency = 40
      ).map(_.flatten)

    validCHAbstractTypeCombinationsF.flatMap { validCHAbstractTypeCombinations =>
      val checksToDo =
        validCHAbstractTypeCombinations
          .flatMap(generateCHTypeCombinations(_))
          .flatMap { chTypesArgs =>
            buildFuzzingValuesArgs(chTypesArgs.map(_.fuzzingValues))
              .map(args => s"SELECT $fnName($args)")
              .map(query => (chTypesArgs, query))
          }

      executeInParallelOnlySuccess(
        checksToDo,
        (chTypesArgs, query) => client.execute(query).map(resp => (chTypesArgs, resp.meta.head.`type`)),
        maxConcurrency = 40
      ).map { results =>
        results.groupBy(_._1).view.mapValues(_.map(_._2).reduce(CHType.merge)).toSeq
      }
    }

  /**
    * The future generated by this function never fails.
    *
    * @return Future.successful(true) if last argument can be repeated many times, otherwise Future.successful(false)
    */
  private def testInfiniteArgsFunctions(
      fnName: String,
      chTypes: Seq[CHType]
  )(implicit client: CHClient, ec: ExecutionContext): Future[Boolean] =
    require(chTypes.nonEmpty, "Expected at least one defined argument, but none found.")

    val argNv1 = Range(0, 10).toSeq.map(_ => Seq(chTypes.last.fuzzingValues.head))
    val argNv2 = Range(0, 11).toSeq.map(_ => Seq(chTypes.last.fuzzingValues.head))

    val fuzzingValuesArgsv1 = buildFuzzingValuesArgs(chTypes.map(_.fuzzingValues) ++ argNv1)
    val fuzzingValuesArgsv2 = buildFuzzingValuesArgs(chTypes.map(_.fuzzingValues) ++ argNv2)

    executeInSequenceUntilSuccess(
      (fuzzingValuesArgsv1 ++ fuzzingValuesArgsv2).map(args => s"SELECT $fnName($args)"),
      client.execute
    )

