package com.amendil

import com.amendil.bo.CHFunctionFuzzResult
import com.amendil.http.CHClient
import com.typesafe.scalalogging.Logger

import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.Duration

@main def app: Unit =
  val logger = Logger("Main")
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
  implicit val client: CHClient = CHClient(8123)

  val functionNamesF =
    client
      .execute("SELECT name, is_aggregate FROM system.functions")
      .map(_.data.map(_.head).sorted)

  val checksF = for {
    functionNames <- functionNamesF

    functionCount = functionNames.size
    res <- ConcurrencyUtils.executeInSequence(
      functionNames.zipWithIndex,
      (functionName: String, idx: Int) =>
        if (idx % (functionCount / 20) == 0)
          logger.info(s"${100 * idx / functionCount}%")

        Fuzzer
          .fuzzFunctionN(CHFunctionFuzzResult(name = functionName))
          .flatMap(Fuzzer.fuzzFunction0)
          .flatMap(Fuzzer.fuzzFunction1)
          .flatMap(Fuzzer.fuzzFunction2)
          .flatMap(Fuzzer.fuzzFunction3)
    )
  } yield {
    res
  }

  val r = Await
    .result(checksF, Duration.Inf)
    .filter(f =>
      !f.isFunction0
        && f.functionNTypes.isEmpty
        && f.function1Types.isEmpty
        && f.function2Types.isEmpty
        && f.function3Types.isEmpty
        && f.function4Types.isEmpty
    )

  print(r.size)
