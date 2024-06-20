package com.amendil.signature.fuzz

import com.amendil.common.entities.*
import com.amendil.common.entities.`type`.*
import com.amendil.common.entities.function.{CHFunction, CHFunctionIO}
import com.amendil.common.http.CHClient
import com.amendil.signature.entities.*
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}

object FuzzerSpecialFunctions extends StrictLogging:

  private[fuzz] def fuzzingFunctionWithCost(
      using client: CHClient,
      ec: ExecutionContext
  ): Seq[((CHFunctionFuzzResult) => Future[CHFunctionFuzzResult], Long)] =
    Seq(
      (fuzzRepeatedAnyTypeFunctions, 1)
    )

  /**
    * Detect functions that taking any number of arguments, each using potentially different types.
    * E.g. tuple('foo', 42, now()) and tuple(today(), 'bar', 1, 2, 42) are both valid signatures for `tuple` method
    *
    * This is not necessarily ideal in regards to the output type.
    * E.g. For `tuple`, we might want to know the number of elements in the return tuple (same as in input, with same types)
    */
  private def fuzzRepeatedAnyTypeFunctions(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzRepeatedAnyTypeFunctions")
    val manyCHFuzzableTypes: Seq[CHFuzzableType] = Seq(
      CHFuzzableType.ArrayDate,
      CHFuzzableType.UInt8,
      CHFuzzableType.StringType,
      CHFuzzableType.DateTime,
      CHFuzzableType.IPv4
    )
    val exprs1: String = manyCHFuzzableTypes.map(_.fuzzingValues.head).mkString(",")
    val exprs2: String = manyCHFuzzableTypes.reverse.map(_.fuzzingValues.head).mkString(",")

    client
      .execute(s"SELECT toTypeName(${fn.name}($exprs1)), toTypeName(${fn.name}($exprs2))")
      .map { (resp: CHResponse) =>
        val returnType = CHType.getByName(resp.data.head.head.asInstanceOf[String]) match
          case CHSpecialType.Tuple(
                Seq(
                  CHSpecialType.Array(CHFuzzableType.Date),
                  CHFuzzableType.UInt8,
                  CHFuzzableType.StringType,
                  CHFuzzableType.DateTime,
                  CHFuzzableType.IPv4
                )
              ) =>
            CHSpecialType.TupleN(CHAggregatedType.Any)
          case t => t

        fn.copy(
          modes = fn.modes + CHFunction.Mode.NoOverWindow,
          specialFunction0Ns = Seq(
            CHFunctionIO.Function0N(CHAggregatedType.Any, returnType)
          )
        )
      }
      .recover(_ => fn)
