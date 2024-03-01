package com.amendil

import com.amendil.ConcurrencyUtils._
import com.amendil.entities.{CHAbstractType, CHFunctionFuzzResult, CHFunctionIO, CHType}
import com.amendil.http.CHClient
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object Fuzzer extends StrictLogging {
  def fuzz(functionName: String)(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    Fuzzer
      .fuzzFunctionN(CHFunctionFuzzResult(name = functionName))
      // .flatMap(Fuzzer.fuzzFunction0)
      // .flatMap(Fuzzer.fuzzFunction1)
      // .flatMap(Fuzzer.fuzzFunction2)
      .flatMap(Fuzzer.fuzzFunction3)

  private def fuzzFunctionN(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    Future
      .sequence(
        for {
          type1 <- CHAbstractType.values
          arg1 = s"${type1.fuzzingValues.head}"
        } yield {
          client
            .execute(
              s"SELECT ${fn.name}($arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1)"
            )
            .flatMap { _ =>

              val calls = type1.chTypes.map { subType1 =>
                val queries = subType1.fuzzingValues.map { subArg1 =>
                  s"SELECT ${fn.name}($subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1)"
                }

                executeInParallelOnlySuccess(
                  queries,
                  query => client.execute(query).map(_.meta.head.`type`)
                ).map(_.reduce(CHType.merge)).map(outputType => Success((subType1, outputType)))
              }

              Future.sequence(calls)
            }
            .recover(e => Seq(Failure(e)))
        }
      )
      .map { results =>
        val validTypes: Seq[CHFunctionIO.FunctionN] =
          results.flatten.collect { case Success((inputType, outputType)) =>
            CHFunctionIO.FunctionN(
              inputType,
              outputType
            )
          }.toSeq

        fn.copy(functionNs = validTypes)
      }

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

  private def fuzzFunction1(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if (fn.functionNs.nonEmpty) {
      Future.successful(fn)
    } else {
      fuzzFunctionNType(fn.name, 1)
        .map { list =>
          val validTypes = list.map { (inputTypes, outputType) =>
            inputTypes match
              case Seq(type1) => CHFunctionIO.Function1(type1, outputType)
              case _          => throw new Exception(s"Expected 1 type, found ${list.size} types")
          }

          fn.copy(function1s = validTypes)
        }
    }

  private def fuzzFunction2(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if (fn.functionNs.nonEmpty) {
      Future.successful(fn)
    } else {
      fuzzFunctionNType(fn.name, 2)
        .map { list =>
          val validTypes = list.map { (inputTypes, outputType) =>
            inputTypes match
              case Seq(type1, type2) =>
                CHFunctionIO.Function2(type1, type2, outputType)
              case _ => throw new Exception(s"Expected 2 types, found ${list.size} types")
          }

          fn.copy(function2s = validTypes)
        }
    }

  private def fuzzFunction3(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if (
      fn.functionNs.nonEmpty || (fn.function1s
        .filterNot(_.arg1.name.startsWith("Tuple"))
        .nonEmpty && fn.function2s.isEmpty)
    ) {
      Future.successful(fn)
    } else {
      fuzzFunctionNType(fn.name, 3)
        .map { list =>
          val validTypes = list.map { (inputTypes, outputType) =>
            inputTypes match
              case Seq(type1, type2, type3) =>
                CHFunctionIO.Function3(type1, type2, type3, outputType)
              case _ => throw new Exception(s"Expected 3 types, found ${list.size} types")
          }

          fn.copy(function3s = validTypes)
        }
    }

  private def fuzzFunction4(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if (
      fn.functionNs.nonEmpty || ((fn.function1s.filterNot(_.arg1.name.startsWith("Tuple")).nonEmpty || fn.function2s
        .filterNot(f => f.arg1.name.startsWith("Tuple") || f.arg2.name.startsWith("Tuple"))
        .nonEmpty) && fn.function3s.isEmpty)
    ) {
      Future.successful(fn)
    } else {
      fuzzFunctionNType(fn.name, 4)
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
              case _ => throw new Exception(s"Expected 4 types, found ${list.size} types")
          }

          fn.copy(function4s = validTypes)
        }
    }

  private def fuzzFunctionNType(
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
          ).map(if (_) Some(abstractTypes) else None)
        },
        maxConcurrency = 40
      ).map(_.flatten)

    validCHAbstractTypeCombinationsF.flatMap { validCHAbstractTypeCombinations =>
      val checksToDo =
        validCHAbstractTypeCombinations
          .flatMap(generateCHTypeCombinations(_))
          .flatMap { chTypesArgs =>
            generateAllQueries(fnName, chTypesArgs).map(query => (chTypesArgs, query))
          }

      executeInParallelOnlySuccess(
        checksToDo,
        (chTypesArgs, query) => client.execute(query).map(resp => (chTypesArgs, resp.meta.head.`type`)),
        maxConcurrency = 40
      ).map { results =>
        results.groupBy(_._1).view.mapValues(_.map(_._2).reduce(CHType.merge)).toSeq
      }
    }

  private def generateCHAbstractTypeCombinations(
      argCount: Int,
      currentArgs: Seq[CHAbstractType] = Seq.empty
  ): Seq[Seq[CHAbstractType]] =
    if (argCount > 0) {
      CHAbstractType.values.toSeq.map { abstractType =>
        generateCHAbstractTypeCombinations(argCount - 1, currentArgs :+ abstractType)
      }.flatten
    } else {
      Seq(currentArgs)
    }

  private def generateCHTypeCombinations(
      abstractTypes: Seq[CHAbstractType],
      currentArgs: Seq[CHType] = Seq.empty
  ): Seq[Seq[CHType]] =
    abstractTypes match {
      case Seq(head, tail @ _*) =>
        head.chTypes
          .map(chType => generateCHTypeCombinations(tail, currentArgs :+ chType))
          .flatten
      case Seq() => Seq(currentArgs)
    }

  private def generateAllQueries(
      fnName: String,
      args: Seq[CHType]
  ): Seq[String] =
    buildFuzzingValuesArgs(args.map(_.fuzzingValues))
      .map(args => s"SELECT $fnName($args)")

  private def buildFuzzingValuesArgs(argumentsValues: Seq[Seq[String]]): Seq[String] =
    argumentsValues match
      case Seq()   => throw IllegalArgumentException("Tried to fuzz an argument without any value")
      case Seq(el) => el
      case Seq(head, tail @ _*) =>
        val subChoices = buildFuzzingValuesArgs(tail)
        head.flatMap(el => subChoices.map(subChoice => s"$el, $subChoice"))

}
