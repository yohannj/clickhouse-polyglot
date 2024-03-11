package com.amendil.fuzz

import com.amendil.entities._
import com.amendil.http.CHClient

import scala.concurrent.{ExecutionContext, Future}

object FuzzerSpecialFunctions:

  private[fuzz] def fuzzingFunctionWithCost(
      implicit client: CHClient,
      ec: ExecutionContext
  ): Seq[((CHFunctionFuzzResult) => Future[CHFunctionFuzzResult], Long)] =
    Seq(
      (fuzzInfiniteAnyTypeFunctions, 1)
    )

  private def fuzzInfiniteAnyTypeFunctions(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val manyCHFuzzableTypes = Seq(
      CHFuzzableType.ArrayDate,
      CHFuzzableType.UInt8,
      CHFuzzableType.StringType,
      CHFuzzableType.DateTime,
      CHFuzzableType.IPv4
    )
    val exprs1 = manyCHFuzzableTypes.map(_.fuzzingValues.head).mkString(",")
    val exprs2 = manyCHFuzzableTypes.reverse.map(_.fuzzingValues.head).mkString(",")

    client
      .execute(s"SELECT ${fn.name}($exprs1), ${fn.name}($exprs2)")
      .map { resp =>
        fn.copy(function0Ns = fn.function0Ns :+ CHFunctionIO.Function0N(CHAggregatedType.Any, resp.meta.head.`type`))
      }
      .recover(_ => fn)
