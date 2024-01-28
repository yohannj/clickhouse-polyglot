package com.amendil

import com.amendil.bo.CHFunctionFuzzResult

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

@main def app: Unit =
  implicit val ec: ExecutionContext = ExecutionContext.global
  implicit val client: CHClient = CHClient(8123)

  val functionNamesF =
    client
      .execute("SELECT name, is_aggregate FROM system.functions")
      .map(_.data.map(_.head).sorted)

  val checksF = for {
    functionNames <- functionNamesF

    checksF = functionNames.map { functionName =>
      for {
        isFunction0 <- Fuzzer.fuzzFunction0(functionName)

        function1Types <- Fuzzer.fuzzFunction1(functionName)
        isFunction1 = function1Types.nonEmpty
      } yield {
        CHFunctionFuzzResult(
          name = functionName,
          isFunction0 = isFunction0,
          isFunction1 = isFunction1
        )
      }
    }

    res <- Future.sequence(checksF)
  } yield {
    res
  }

  println(Await.result(checksF, 200.seconds).filter(c => c.isFunction0 || c.isFunction1).mkString("\n"))
