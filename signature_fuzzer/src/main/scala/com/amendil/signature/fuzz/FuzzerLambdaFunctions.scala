package com.amendil.signature.fuzz

import com.amendil.common.entities.{CHResponse, _}
import com.amendil.common.http.CHClient
import com.amendil.signature.entities._
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}

object FuzzerLambdaFunctions extends StrictLogging:

  private[fuzz] def fuzzingFunctionWithCost(
      using client: CHClient,
      ec: ExecutionContext
  ): Seq[((CHFunctionFuzzResult) => Future[CHFunctionFuzzResult], Long)] =
    Seq(
      (fuzzLambdaFunction, 1L)
    )

  /**
    * Detect functions that uses a lambda as their first argument.
    * Further arguments of those functions can only be arrays.
    */
  private def fuzzLambdaFunction(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    logger.debug("fuzzLambdaFunction")
    if fn.isSpecialInfiniteFunction then Future.successful(fn)
    else
      // Checks for functions which lambda return types is neither a Boolean nor the type of the first Array
      client
        .executeNoResult(s"SELECT toTypeName(${fn.name}(x -> today(), ['s']))")
        .map { _ =>
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            lambdaFunction0NOpt = Some(
              CHFunctionIO.LambdaFunction0N(
                CHSpecialType.LambdaNType("T"),
                argN = CHSpecialType.Array(CHAggregatedType.Any),
                output = "T"
              )
            )
          )
        }
        .recoverWith { _ =>
          // Checks for functions which lambda return types is a Boolean
          client
            .execute(s"SELECT toTypeName(${fn.name}(x, y -> 1, ['s'], [1]))")
            .map { (resp: CHResponse) =>
              val outputType: String = resp.data.head.head.asInstanceOf[String]

              if outputType == "String" || outputType == "Array(String)" then
                // Return type is the type of the first Array, so a generic type!
                fn.copy(
                  modes = fn.modes + CHFunction.Mode.NoOverWindow,
                  lambdaFunction1NOpt = Some(
                    CHFunctionIO.LambdaFunction1N(
                      CHSpecialType.LambdaNType(CHFuzzableType.BooleanType.name),
                      arg1 = CHSpecialType.Array(CHSpecialType.GenericType("T")),
                      argN = CHSpecialType.Array(CHAggregatedType.Any),
                      output = outputType.replace("String", "T")
                    )
                  )
                )
              else
                // Return type is unrelated to the type of the first Array so we keep it as is!
                fn.copy(
                  modes = fn.modes + CHFunction.Mode.NoOverWindow,
                  lambdaFunction0NOpt = Some(
                    CHFunctionIO.LambdaFunction0N(
                      CHSpecialType.LambdaNType(CHFuzzableType.BooleanType.name),
                      argN = CHSpecialType.Array(CHAggregatedType.Any),
                      output = outputType
                    )
                  )
                )
            }
        }
        .recover(_ => fn)
