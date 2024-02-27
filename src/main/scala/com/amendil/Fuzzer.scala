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
      .flatMap(Fuzzer.fuzzFunction0)
      .flatMap(Fuzzer.fuzzFunction1)
    // .flatMap(Fuzzer.fuzzFunction2)
    // .flatMap(Fuzzer.fuzzFunction3)

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
                val queries = subType1.baseFuzzingValues.map { subArg1 =>
                  s"SELECT ${fn.name}($subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1)"
                }

                getOutputType(queries).map(outputTypeOpt =>
                  outputTypeOpt
                    .map(outputType => Success((subType1, outputType)))
                    .getOrElse(
                      Failure(
                        new IllegalArgumentException(
                          s"${subType1.name} is not a valid functionN argument of ${fn.name}"
                        )
                      )
                    )
                )
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
        val outputType = CHType.getByName(resp.meta.head.`type`)
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
    if (fn.functionNs.nonEmpty || (fn.function1s.nonEmpty && fn.function2s.isEmpty)) {
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
    if (fn.functionNs.nonEmpty || ((fn.function1s.nonEmpty || fn.function2s.nonEmpty) && fn.function3s.isEmpty)) {
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
      argCount: Int,
      currentArgs: Seq[CHAbstractType] = Seq.empty
  )(implicit client: CHClient, ec: ExecutionContext): Future[Seq[(Seq[CHType], CHType)]] =
    if (argCount > 0) {
      val checkF = CHAbstractType.values.toSeq.map { abstractType =>
        fuzzFunctionNType(fnName, argCount - 1, currentArgs :+ abstractType)
      }

      Future.sequence(checkF).map(_.flatten.filter(_._1.nonEmpty))
    } else {
      val queries =
        buildFuzzingValuesArgs(currentArgs.map(_.fuzzingValues))
          .map(args => s"SELECT $fnName($args)")

      executeUntilSuccess(queries, client.execute).flatMap { foundAValidQuery =>
        if (foundAValidQuery) {
          fuzzFunctionNType(fnName, currentArgs, Seq.empty)
        } else {
          Future.successful(Seq.empty)
        }
      }
    }

  private def fuzzFunctionNType(
      fnName: String,
      abstractTypes: Seq[CHAbstractType],
      currentArgs: Seq[CHType]
  )(implicit client: CHClient, ec: ExecutionContext): Future[Seq[(Seq[CHType], CHType)]] =
    abstractTypes match {
      case head :: tail =>
        val checkF = head.chTypes.map { chType =>
          fuzzFunctionNType(fnName, tail, currentArgs :+ chType)
        }

        Future.sequence(checkF).map(_.flatten)
      case Seq() =>
        val queries =
          buildFuzzingValuesArgs(currentArgs.map(_.baseFuzzingValues))
            .map(args => s"SELECT $fnName($args)")

        getOutputType(queries).map(outputTypeOpt =>
          outputTypeOpt.map(outputType => Seq((currentArgs, outputType))).getOrElse(Nil)
        )
    }

  private def buildFuzzingValuesArgs(argumentsValues: Seq[Seq[String]]): Seq[String] =
    argumentsValues match
      case Seq()   => throw IllegalArgumentException("Tried to fuzz an argument without any value")
      case Seq(el) => el
      case head :: tail =>
        val subChoices = buildFuzzingValuesArgs(tail)
        head.flatMap(el => subChoices.map(subChoice => s"$el, $subChoice"))

  private def getOutputType(
      queries: Seq[String]
  )(implicit client: CHClient, ec: ExecutionContext): Future[Option[CHType]] =
    executeInParallelOnlySuccess(
      queries,
      query => client.execute(query)
    ).map { responses =>
      if (responses.isEmpty) {
        None
      } else {
        val outputType =
          responses
            .map(resp => CHType.getByName(resp.meta.head.`type`))
            .reduce(CHType.merge)

        Some(outputType)
      }
    }
}
