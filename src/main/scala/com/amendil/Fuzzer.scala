package com.amendil

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object Fuzzer {
  def fuzzFunction0(functionName: String)(implicit client: CHClient, ec: ExecutionContext): Future[Boolean] =
    client
      .execute(s"SELECT $functionName()")
      .map(_ => true)
      .recover(_ => false)

  def fuzzFunction1(functionName: String)(implicit client: CHClient, ec: ExecutionContext): Future[Seq[CHTypes[_]]] =
    Future
      .sequence(
        CHTypes.values.map { type1 =>
          val arg1 = s"${type1.fuzzingValues.head}::${type1.name}"

          client
            .execute(s"SELECT $functionName($arg1)")
            .map(_ => Success(type1))
            .recover(Failure(_))
        }
      )
      .map {
        _.collect { case Success(value) =>
          value
        }.toSeq
      }

}
