package com.amendil

import com.amendil.bo.CHFunctionFuzzResult
import com.amendil.http.CHClient
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object Fuzzer extends StrictLogging {
  def fuzzFunctionN(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    Future
      .sequence(
        for {
          type1 <- CHAbstractTypes.values
          arg1 = s"${type1.fuzzingValue}"
        } yield {
          client
            .execute(
              s"SELECT ${fn.name}($arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1, $arg1)"
            )
            .flatMap { _ =>
              val calls = for {
                subType1 <- type1.chTypes
                subArg1 = subType1.fuzzingValues.head
              } yield {
                client
                  .execute(
                    s"SELECT ${fn.name}($subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1, $subArg1)"
                  )
                  .map(_ => Success(subType1))
                  .recover(Failure(_))
              }

              Future.sequence(calls)
            }
            .recover(e => Seq(Failure(e)))
        }
      )
      .map { results =>
        val validTypes = results.flatten.collect { case Success(value) => value }.toSeq
        fn.copy(functionNTypes = validTypes)
      }

  def fuzzFunction0(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    client
      .execute(s"SELECT ${fn.name}()")
      .map(_ => fn.copy(isFunction0 = true))
      .recover(_ => fn)

  def fuzzFunction1(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if (fn.functionNTypes.nonEmpty) {
      Future.successful(fn)
    } else {
      Future
        .sequence(
          for {
            type1 <- CHAbstractTypes.values
            arg1 = s"${type1.fuzzingValue}"
          } yield {
            client
              .execute(
                s"SELECT ${fn.name}($arg1)"
              )
              .flatMap { _ =>
                val calls = for {
                  subType1 <- type1.chTypes
                  subArg1 = subType1.fuzzingValues.head
                } yield {
                  client
                    .execute(
                      s"SELECT ${fn.name}($subArg1)"
                    )
                    .map(_ => Success(subType1))
                    .recover(Failure(_))
                }

                Future.sequence(calls)
              }
              .recover(e => Seq(Failure(e)))
          }
        )
        .map { results =>
          val validTypes = results.flatten.collect { case Success(value) => value }.toSeq
          fn.copy(function1Types = validTypes)
        }
    }

  def fuzzFunction2(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if (fn.functionNTypes.nonEmpty) {
      Future.successful(fn)
    } else {
      Future
        .sequence(
          for {
            type1 <- CHAbstractTypes.values
            arg1 = s"${type1.fuzzingValue}"

            type2 <- CHAbstractTypes.values
            arg2 = s"${type2.fuzzingValue}"
          } yield {
            client
              .execute(
                s"SELECT ${fn.name}($arg1, $arg2)"
              )
              .flatMap { _ =>
                val calls = for {
                  subType1 <- type1.chTypes
                  subArg1 = subType1.fuzzingValues.head

                  subType2 <- type2.chTypes
                  subArg2 = subType2.fuzzingValues.head
                } yield {
                  client
                    .execute(
                      s"SELECT ${fn.name}($subArg1, $subArg2)"
                    )
                    .map(_ => Success((subType1, subType2)))
                    .recover(Failure(_))
                }

                Future.sequence(calls)
              }
              .recover(e => Seq(Failure(e)))
          }
        )
        .map { results =>
          val validTypes = results.flatten.collect { case Success(value) => value }.toSeq
          fn.copy(function2Types = validTypes)
        }
    }

  def fuzzFunction3(
      fn: CHFunctionFuzzResult
  )(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    if (fn.functionNTypes.nonEmpty) {
      Future.successful(fn)
    } else {
      Future
        .sequence(
          for {
            type1 <- CHAbstractTypes.values
            arg1 = s"${type1.fuzzingValue}"

            type2 <- CHAbstractTypes.values
            arg2 = s"${type2.fuzzingValue}"

            type3 <- CHAbstractTypes.values
            arg3 = s"${type3.fuzzingValue}"
          } yield {
            client
              .execute(
                s"SELECT ${fn.name}($arg1, $arg2, $arg3)"
              )
              .flatMap { _ =>
                val calls = for {
                  subType1 <- type1.chTypes
                  subArg1 = subType1.fuzzingValues.head

                  subType2 <- type2.chTypes
                  subArg2 = subType2.fuzzingValues.head

                  subType3 <- type3.chTypes
                  subArg3 = subType3.fuzzingValues.head
                } yield {
                  client
                    .execute(
                      s"SELECT ${fn.name}($subArg1, $subArg2, $subArg3)"
                    )
                    .map(_ => Success((subType1, subType2, subType3)))
                    .recover(Failure(_))
                }

                Future.sequence(calls)
              }
              .recover(e => Seq(Failure(e)))
          }
        )
        .map { results =>
          val validTypes = results.flatten.collect { case Success(value) => value }.toSeq
          fn.copy(function3Types = validTypes)
        }
    }

}
