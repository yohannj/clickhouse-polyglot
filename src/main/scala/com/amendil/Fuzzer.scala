package com.amendil

import com.amendil.bo.CHFunctionFuzzResult
import com.amendil.http.CHClient
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object Fuzzer extends StrictLogging {
  def fuzzFunctionN(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    Future
      .sequence(
        for {
          type1 <- CHAbstractType.values
          arg1 = s"${type1.fuzzingValue}"
        } yield {
          client
            .execute(
              s"SELECT ${fn.name}($arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1)"
            )
            .flatMap { _ =>
              val calls = for {
                subType1 <- type1.chTypes
                subArg1 = subType1.fuzzingValues.head
              } yield {
                client
                  .execute(
                    s"SELECT ${fn.name}($subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1)"
                  )
                  .map(_ => Success(subType1))
                  .recover(Failure(_))
              }

              Future.sequence(calls)
            }
            .recover(e => Seq(Failure(e)))
        }
      )
      .map { results =>
        val validTypes = results.flatten.collect { case Success(value) => value }.toSeq
        fn.copy(functionNTypes = validTypes)
      }

  def fuzzFunction0(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    client
      .execute(s"SELECT ${fn.name}()")
      .map(_ => fn.copy(isFunction0 = true))
      .recover(_ => fn)

  def fuzzFunction1(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if (fn.functionNTypes.nonEmpty) {
      Future.successful(fn)
    } else {
      fuzzFunctionNType(fn.name, 1)
        .map { list =>
          val validTypes = list.map { types =>
            types match
              case Seq(type1) => type1
              case _          => throw new Exception(s"Expected 1 type, found ${list.size} types")
          }

          fn.copy(function1Types = validTypes)
        }
    }

  def fuzzFunction2(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if (fn.functionNTypes.nonEmpty) {
      Future.successful(fn)
    } else {
      fuzzFunctionNType(fn.name, 2)
        .map { list =>
          val validTypes = list.map { types =>
            types match
              case Seq(type1, type2) => (type1, type2)
              case _                 => throw new Exception(s"Expected 2 types, found ${list.size} types")
          }

          fn.copy(function2Types = validTypes)
        }
    }

  def fuzzFunction3(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if (fn.functionNTypes.nonEmpty || (fn.function1Types.nonEmpty && fn.function2Types.isEmpty)) {
      Future.successful(fn)
    } else {
      fuzzFunctionNType(fn.name, 3)
        .map { list =>
          val validTypes = list.map { types =>
            types match
              case Seq(type1, type2, type3) => (type1, type2, type3)
              case _                        => throw new Exception(s"Expected 3 types, found ${list.size} types")
          }

          fn.copy(function3Types = validTypes)
        }
    }

  def fuzzFunction4(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if (
      fn.functionNTypes.nonEmpty || ((fn.function1Types.nonEmpty || fn.function2Types.nonEmpty) && fn.function3Types.isEmpty)
    ) {
      Future.successful(fn)
    } else {
      fuzzFunctionNType(fn.name, 4)
        .map { list =>
          val validTypes = list.map { types =>
            types match
              case Seq(type1, type2, type3, type4) => (type1, type2, type3, type4)
              case _                               => throw new Exception(s"Expected 4 types, found ${list.size} types")
          }

          fn.copy(function4Types = validTypes)
        }
    }

  private def fuzzFunctionNType(
      fnName: String,
      argCount: Int,
      currentArgs: Seq[CHAbstractType] = Seq.empty
  )(implicit client: CHClient, ec: ExecutionContext): Future[Seq[Seq[CHType]]] =
    if (argCount > 0) {
      val checkF = CHAbstractType.values.toSeq.map { abstractType =>
        fuzzFunctionNType(fnName, argCount - 1, currentArgs :+ abstractType)
      }

      Future.sequence(checkF).map(_.flatten.filter(_.nonEmpty))
    } else {
      client
        .execute(s"SELECT $fnName(${currentArgs.map(_.fuzzingValue).mkString(", ")})")
        .flatMap { _ => fuzzFunctionNType(fnName, currentArgs, Seq.empty) }
        .recover(_ => Seq.empty)
    }

  private def fuzzFunctionNType(
      fnName: String,
      abstractTypes: Seq[CHAbstractType],
      currentArgs: Seq[CHType]
  )(implicit client: CHClient, ec: ExecutionContext): Future[Seq[Seq[CHType]]] =
    abstractTypes match {
      case head :: tail =>
        val checkF = head.chTypes.map { chType =>
          fuzzFunctionNType(fnName, tail, currentArgs :+ chType)
        }

        Future.sequence(checkF).map(_.flatten.filter(_.nonEmpty))
      case _ =>
        client
          .execute(s"SELECT $fnName(${currentArgs.map(_.fuzzingValues.head).mkString(", ")})")
          .map { _ => Seq(currentArgs) }
          .recover(_ => Seq.empty)
    }

}
