package com.amendil.fuzz

import com.amendil.entities._
import com.amendil.http.CHClient

import scala.concurrent.{ExecutionContext, Future}

object FuzzerSpecialFunctions {
  private[fuzz] def fuzz(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    Future.successful(fn)
    // val manyCHTypes = Seq(CHType.ArrayDate, CHType.UInt8, CHType.StringType, CHType.DateTime, CHType.IPv4)
    // val exprs1 = manyCHTypes.map(_.fuzzingValues.head).mkString(",")
    // val exprs2 = manyCHTypes.reverse.map(_.fuzzingValues.head).mkString(",")

    // client
    //   .execute(s"SELECT ${fn.name}($exprs1), ${fn.name}($exprs2)")
    //   .map { resp =>
    //     val newFunction = CHFunctionIO.Function0N(CHAggregatedType.Any, resp.meta.head.`type`)
    //     fn.copy(function0Ns = fn.function0Ns :+ newFunction)
    //   }
    //   .recover(_ => fn)
}
