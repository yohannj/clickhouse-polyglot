package com.amendil

import com.amendil.http.CHClient
import com.typesafe.scalalogging.Logger

import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

@main def app: Unit =
  val logger = Logger("Main")
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
  val fuzzEc: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))
  val client: CHClient = CHClient(8123)

  val checkFuzzingValues = Future.sequence(
    (CHType.values.flatMap(_.fuzzingValues) ++ CHAbstractType.values.flatMap(_.fuzzingValues))
      .map(v =>
        client
          .execute(s"SELECT $v")
          .recover(err => logger.error(s"Invalid fuzzing value $v: ${err.getMessage}"))
      )
  )

  val r = Await
    .result(checkFuzzingValues.flatMap(_ => Fuzzer.fuzz()(client, fuzzEc)), Duration.Inf)
    .filter(f =>
      !f.isFunction0
        && f.functionNTypes.isEmpty
        && f.function1Types.isEmpty
        && f.function2Types.isEmpty
        && f.function3Types.isEmpty
        && f.function4Types.isEmpty
    )

  println(r.size)
  println(r.map(_.name).sorted.mkString("\"", "\", \"", "\""))
