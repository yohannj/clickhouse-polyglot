package com.amendil.signature.fuzz

import com.amendil.common.entities.{CHResponse, *}
import com.amendil.common.entities.`type`.*
import com.amendil.common.http.CHClient
import com.amendil.signature.entities.*
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}

object FuzzerLambdaFunctions extends StrictLogging:

  private[fuzz] def fuzzingFunctionWithCost(
      using client: CHClient,
      ec: ExecutionContext
  ): Seq[((CHFunctionFuzzResult) => Future[CHFunctionFuzzResult], Long)] =
    Seq(
      (fuzzBlandLambdaFunctionWithArray, 1L),
      (fuzzBooleanLambdaFunctionWithArray, 1L),
      (fuzzBooleanLambdaFunctionWithMap, 1L)
    )

  /**
    * Detect functions that uses a lambda as their first argument.
    *
    * Lambda's return types is neither a Boolean nor the type of the first Array
    * Further arguments of those functions can only be arrays.
    */
  private def fuzzBlandLambdaFunctionWithArray(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzBlandLambdaFunctionWithArray")
    if fn.isSpecialRepeatedFunction then Future.successful(fn)
    else
      client
        .executeNoResult(s"SELECT toTypeName(${fn.name}(x -> today(), ['s']))")
        .map { _ =>
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            lambdaArrayFunction0NOpt = Some(
              CHFunctionIO.LambdaArrayFunction0N(
                CHSpecialType.LambdaType(CHSpecialType.GenericType("T1", CHAggregatedType.Any)),
                argN = CHSpecialType.Array(CHAggregatedType.Any),
                output = CHSpecialType.GenericType("T1", CHAggregatedType.Any)
              )
            )
          )
        }
        .recover(_ => fn)

  /**
    * Detect functions that uses a lambda as their first argument, and arrays afterwards.
    *
    * Lambda's return types is a Boolean.
    */
  private def fuzzBooleanLambdaFunctionWithArray(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzBooleanLambdaFunctionWithArray")
    if fn.isSpecialRepeatedFunction then Future.successful(fn)
    else
      client
        .execute(s"SELECT toTypeName(${fn.name}(x, y -> 1, ['s'], [1]))")
        .map { (resp: CHResponse) =>
          val outputType = CHType.getByName(resp.data.head.head.asInstanceOf[String])

          if outputType == CHFuzzableType.StringType || outputType == CHSpecialType.Array(CHFuzzableType.StringType)
          then
            // Return type is the type of the first Array, so a generic type!
            fn.copy(
              modes = fn.modes + CHFunction.Mode.NoOverWindow,
              lambdaArrayFunction1NOpt = Some(
                CHFunctionIO.LambdaArrayFunction1N(
                  CHSpecialType.LambdaType(CHFuzzableType.BooleanType),
                  arg1 = CHSpecialType.Array(CHSpecialType.GenericType("T1", CHAggregatedType.Any)),
                  argN = CHSpecialType.Array(CHAggregatedType.Any),
                  output =
                    if outputType == CHFuzzableType.StringType then
                      CHSpecialType.GenericType("T1", CHAggregatedType.Any)
                    else CHSpecialType.Array(CHSpecialType.GenericType("T1", CHAggregatedType.Any))
                )
              )
            )
          else
            // Return type is unrelated to the type of the first Array so we keep it as is!
            fn.copy(
              modes = fn.modes + CHFunction.Mode.NoOverWindow,
              lambdaArrayFunction0NOpt = Some(
                CHFunctionIO.LambdaArrayFunction0N(
                  CHSpecialType.LambdaType(CHFuzzableType.BooleanType),
                  argN = CHSpecialType.Array(CHAggregatedType.Any),
                  output = outputType
                )
              )
            )
        }
        .recover(_ => fn)

  /**
    * Detect functions that uses a lambda as their first argument, and a map as their second argument.
    *
    * Lambda's return types is a Boolean.
    */
  private def fuzzBooleanLambdaFunctionWithMap(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzBooleanLambdaFunctionWithMap")
    if fn.isSpecialRepeatedFunction then Future.successful(fn)
    else
      client
        .execute(s"SELECT toTypeName(${fn.name}(x, y -> 1, map(now(), today())))")
        .map { (resp: CHResponse) =>
          val outputType = CHType.getByName(resp.data.head.head.asInstanceOf[String]) match
            case CHFuzzableType.DateTime =>
              // Return type is the type of the keys, so a generic type!
              CHSpecialType.GenericType("T1", CHAggregatedType.MapKey)
            case CHFuzzableType.Date =>
              // Return type is the type of the values, so a generic type!
              CHSpecialType.GenericType("U", CHAggregatedType.Any)
            case CHSpecialType.Map(CHFuzzableType.DateTime, CHFuzzableType.Date) |
                CHSpecialType.Map(CHFuzzableType.DateTime, CHFuzzableType.Date) =>
              // Return type is the same as provided map!
              CHSpecialType.Map(
                CHSpecialType.GenericType("T1", CHAggregatedType.MapKey),
                CHSpecialType.GenericType("U", CHAggregatedType.Any)
              )
            case other =>
              // Let's suppose the returned type never changes depending on the size or types in the map
              other

          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            lambdaMapFunction1Opt = Some(
              CHFunctionIO.LambdaMapFunction1(
                CHSpecialType.LambdaType(CHFuzzableType.BooleanType),
                arg1 = CHSpecialType.Map(
                  CHSpecialType.GenericType("T1", CHAggregatedType.MapKey),
                  CHSpecialType.GenericType("U", CHAggregatedType.Any)
                ),
                output = outputType
              )
            )
          )
        }
        .recover(_ => fn)
