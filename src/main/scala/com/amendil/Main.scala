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
      Fuzzer
        .fuzzFunctionN(CHFunctionFuzzResult(name = functionName))
        .flatMap(Fuzzer.fuzzFunction0)
        .flatMap(Fuzzer.fuzzFunction1)
        .flatMap(Fuzzer.fuzzFunction2)
        .flatMap(Fuzzer.fuzzFunction3)
    }

    res <- Future.sequence(checksF)
  } yield {
    res
  }

  val r = Await
    .result(checksF, 200.seconds)
    .filter(f =>
      !f.isFunction0 && f.functionNTypes.isEmpty && f.function1Types.isEmpty && f.function2Types.isEmpty && f.function3Types.isEmpty
    )

  print(r.size)
