package com.amendil.fuzz

import com.amendil.entities._
import com.amendil.http.CHClient

import scala.concurrent.{ExecutionContext, Future}

object FuzzerLambdaFunctions {

  private[fuzz] def fuzzingFunctionWithCost(
      implicit client: CHClient,
      ec: ExecutionContext
  ): Seq[((CHFunctionFuzzResult) => Future[CHFunctionFuzzResult], Long)] =
    Seq(
      (fuzzLambdaFunction, 1L)
    )

  private def fuzzLambdaFunction(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if fn.isParametric || fn.isSpecialInfiniteFunction then Future.successful(fn)
    else
      client
        .execute(s"SELECT ${fn.name}(x -> today(), ['s'])")
        .map { (resp: CHResponse) =>
          fn.copy(lambdaFunction0NOpt =
            Some(
              CHFunctionIO.LambdaFunction0N(
                CHSpecialType.LambdaNType("T"),
                argN = CHSpecialType.Array(CHAggregatedType.Any),
                output = "T"
              )
            )
          )
        }
        .recoverWith { _ =>
          client
            .execute(s"SELECT ${fn.name}(x, y -> 1, ['s'], [1])")
            .map { (resp: CHResponse) =>
              val outputType: String = resp.meta.head.`type`

              if outputType == "String" || outputType == "Array(String)" then
                fn.copy(lambdaFunction1NOpt =
                  Some(
                    CHFunctionIO.LambdaFunction1N(
                      CHSpecialType.LambdaNType(CHFuzzableType.BooleanType.name),
                      arg1 = CHSpecialType.Array(CHSpecialType.GenericType("T")),
                      argN = CHSpecialType.Array(CHAggregatedType.Any),
                      output = outputType.replace("String", "T")
                    )
                  )
                )
              else
                fn.copy(lambdaFunction0NOpt =
                  Some(
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
}
