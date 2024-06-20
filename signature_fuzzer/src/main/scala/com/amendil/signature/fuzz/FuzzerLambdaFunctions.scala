package com.amendil.signature.fuzz

import com.amendil.common.ConcurrencyUtils.executeInParallelOnlySuccess
import com.amendil.common.entities.{CHResponse, *}
import com.amendil.common.entities.`type`.*
import com.amendil.common.entities.function.{CHFunction, CHFunctionIO}
import com.amendil.common.http.CHClient
import com.amendil.signature.Settings
import com.amendil.signature.entities.*
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}

object FuzzerLambdaFunctions extends StrictLogging:

  private[fuzz] def fuzzingFunctionWithCost(
      using client: CHClient,
      ec: ExecutionContext
  ): Seq[((CHFunctionFuzzResult) => Future[CHFunctionFuzzResult], Long)] =
    val cost = 1L // We need to compute them first for optimization purpose
    Seq(
      (fuzzBlandLambdaFunctionWithArray, cost),
      (fuzzBooleanLambdaFunctionWithArray, cost),
      (fuzzLambdaFunction1Or0NWithMap, cost),
      (fuzzLambdaFunction2Or1NWithMap, cost)
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
    */
  private def fuzzLambdaFunction1Or0NWithMap(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzLambdaFunction1Or0NWithMap")
    if fn.isSpecialRepeatedFunction then Future.successful(fn)
    else
      val resF =
        for
          resp1 <- client.execute(s"SELECT toTypeName(${fn.name}(x, y -> 1, map(now(), today())))")
          isInfinite <- client
            .execute(s"SELECT toTypeName(${fn.name}(x, y -> 1, map(now(), today()), map(now(), today())))")
            .map(_ => true)
            .recover(_ => false)
          resp2Opt <- client
            .execute(
              s"SELECT toTypeName(${fn.name}(x, y -> ${CHFuzzableType.ArrayUUID.fuzzingValues.head}, map(now(), today())))"
            )
            .map(Some(_))
            .recover(_ => None)
        yield
          // Set default value that can be overriden with generic types
          var lambdaOutputType: CHType =
            resp2Opt match
              case None        => CHFuzzableType.BooleanType
              case Some(value) => CHAggregatedType.Any
          var mapKeyType: CHType = CHAggregatedType.MapKey
          var mapValueType: CHType = CHAggregatedType.Any

          val outputType = CHType.getByName(resp2Opt.getOrElse(resp1).data.head.head.asInstanceOf[String]) match
            case CHSpecialType.Array(CHFuzzableType.UUID) =>
              // Return type is the type of the lambda, so a generic type!
              lambdaOutputType = CHSpecialType.GenericType("T1", CHAggregatedType.Any)
              lambdaOutputType
            case CHFuzzableType.DateTime =>
              // Return type is the type of the keys, so a generic type!
              mapKeyType = CHSpecialType.GenericType("T1", CHAggregatedType.MapKey)
              mapKeyType
            case CHFuzzableType.Date =>
              // Return type is the type of the values, so a generic type!
              mapValueType = CHSpecialType.GenericType("T1", CHAggregatedType.Any)
              mapValueType
            case CHSpecialType.Map(CHFuzzableType.DateTime, CHFuzzableType.Date) =>
              // Return type is the same as provided map!
              mapKeyType = CHSpecialType.GenericType("T1", CHAggregatedType.MapKey)
              mapValueType = CHSpecialType.GenericType("T2", CHAggregatedType.Any)
              CHSpecialType.Map(mapKeyType, mapValueType)
            case other =>
              // Let's suppose the returned type never changes depending on the size or types in the map
              other

          if isInfinite then
            fn.copy(
              modes = fn.modes + CHFunction.Mode.NoOverWindow,
              lambdaMapFunction0NOpt = Some(
                CHFunctionIO.LambdaMapFunction0N(
                  CHSpecialType.LambdaType(lambdaOutputType),
                  argN = CHSpecialType.Map(mapKeyType, mapValueType),
                  output = outputType
                )
              )
            )
          else
            fn.copy(
              modes = fn.modes + CHFunction.Mode.NoOverWindow,
              lambdaMapFunction1Opt = Some(
                CHFunctionIO.LambdaMapFunction1(
                  CHSpecialType.LambdaType(lambdaOutputType),
                  arg1 = CHSpecialType.Map(mapKeyType, mapValueType),
                  output = outputType
                )
              )
            )

      resF.recover(_ => fn)

  /**
    * Detect functions that uses a lambda as their first argument, and a map as their second argument.
    */
  private def fuzzLambdaFunction2Or1NWithMap(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzLambdaFunction2Or1NWithMap")
    if fn.lambdaMapFunction0NOpt.nonEmpty || fn.isSpecialRepeatedFunction then Future.successful(fn)
    else
      val resF =
        for
          resps1 <- executeInParallelOnlySuccess(
            CHFuzzableAbstractType.nonCustomFuzzableTypes,
            t =>
              client
                .execute(s"SELECT toTypeName(${fn.name}(x, y -> 1, ${t.fuzzingValues.head}, map(now(), today())))")
                .map((_, t)),
            maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
          )

          isInfinite <- client
            .execute(
              s"SELECT toTypeName(${fn.name}(x, y -> 1, ${resps1.head._2.fuzzingValues.head}, map(now(), today()), map(now(), today())))"
            )
            .map(_ => true)
            .recover(_ => false)
          resp2Opt <- client
            .execute(
              s"SELECT toTypeName(${fn.name}(x, y -> ${CHFuzzableType.ArrayUUID.fuzzingValues.head}, ${resps1.head._2.fuzzingValues.head}, map(now(), today())))"
            )
            .map(Some(_))
            .recover(_ => None)
        yield
          // Set default value that can be overriden with generic types
          var lambdaOutputType: CHType =
            resp2Opt match
              case None        => CHFuzzableType.BooleanType
              case Some(value) => CHAggregatedType.Any
          var arg1Types = CHType.mergeInputTypes(resps1.map(_._2).toSet)
          var mapKeyType: CHType = CHAggregatedType.MapKey
          var mapValueType: CHType = CHAggregatedType.Any

          val outputType =
            CHType.getByName(resp2Opt.getOrElse(resps1.head._1).data.head.head.asInstanceOf[String]) match
              case CHSpecialType.Array(CHFuzzableType.UUID) =>
                // Return type is the type of the lambda, so a generic type!
                lambdaOutputType = CHSpecialType.GenericType("T1", CHAggregatedType.Any)
                lambdaOutputType
              case CHFuzzableType.DateTime =>
                // Return type is the type of the keys, so a generic type!
                mapKeyType = CHSpecialType.GenericType("T1", CHAggregatedType.MapKey)
                mapKeyType
              case CHFuzzableType.Date =>
                // Return type is the type of the values, so a generic type!
                mapValueType = CHSpecialType.GenericType("T1", CHAggregatedType.Any)
                mapValueType
              case CHSpecialType.Map(CHFuzzableType.DateTime, CHFuzzableType.Date) =>
                // Return type is the same as provided map!
                mapKeyType = CHSpecialType.GenericType("T1", CHAggregatedType.MapKey)
                mapValueType = CHSpecialType.GenericType("T2", CHAggregatedType.Any)
                CHSpecialType.Map(mapKeyType, mapValueType)
              case other =>
                // Let's suppose the returned type never changes depending on the size or types in the map
                other

          if isInfinite then
            val lambdaMapFunction1N = arg1Types.toSeq.map(arg1 =>
              CHFunctionIO.LambdaMapFunction1N(
                CHSpecialType.LambdaType(lambdaOutputType),
                arg1 = arg1,
                argN = CHSpecialType.Map(mapKeyType, mapValueType),
                output = outputType
              )
            )
            fn.copy(
              modes = fn.modes + CHFunction.Mode.NoOverWindow,
              lambdaMapFunction1N = lambdaMapFunction1N
            )
          else
            val lambdaMapFunction2 = arg1Types.toSeq.map(arg1 =>
              CHFunctionIO.LambdaMapFunction2(
                CHSpecialType.LambdaType(lambdaOutputType),
                arg1 = arg1,
                arg2 = CHSpecialType.Map(mapKeyType, mapValueType),
                output = outputType
              )
            )
            fn.copy(
              modes = fn.modes + CHFunction.Mode.NoOverWindow,
              lambdaMapFunction2 = lambdaMapFunction2
            )

      resF.recover(_ => fn)
