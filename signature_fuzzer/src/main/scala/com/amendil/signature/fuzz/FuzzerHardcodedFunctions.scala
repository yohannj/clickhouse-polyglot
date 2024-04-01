package com.amendil.signature.fuzz

import com.amendil.common.ConcurrencyUtils._
import com.amendil.common.entities._
import com.amendil.common.http.CHClient
import com.amendil.signature.Settings
import com.amendil.signature.entities._
import com.amendil.signature.entities.CHFunctionIO._
import com.amendil.signature.fuzz.Fuzzer._
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}

object FuzzerHardcodedFunctions extends StrictLogging {

  private[fuzz] def fuzz(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    fn.name match
      case "CAST" | "_CAST" | "accurateCast" | "accurateCastOrNull" => // TODO Test it similarly to "encrypt"
        // Cast methods can return any kind of type, depending on the value of a String.
        // This is an edge case not automatically handled by this fuzzer, and it is not worth it to handle it.
        Future.successful(
          fn.copy(function2s = Seq(CHFunctionIO.Function2(CHAggregatedType.Any, CHFuzzableType.ClickHouseType, "Any")))
        )
      case "accurateCastOrDefault" => // TODO Test it similarly to "encrypt"
        // Cast methods can return any kind of type, depending on the value of a String.
        // This is an edge case not automatically handled by this fuzzer, and it is not worth it to handle it.
        Future.successful(
          fn.copy(
            function2s = Seq(CHFunctionIO.Function2(CHAggregatedType.Any, CHFuzzableType.ClickHouseType, "Any")),
            // FIXME Third argument of the Function3 is not really "Any", but should be of the ClickHouseType chosen
            function3s = Seq(
              CHFunctionIO.Function3(CHAggregatedType.Any, CHFuzzableType.ClickHouseType, CHAggregatedType.Any, "Any")
            )
          )
        )
      case "catboostEvaluate" =>
        // Number and type of arguments depend on the model, which we don't know at compile time.

        Future.successful(
          fn.copy(
            function1Ns = Seq(
              CHFunctionIO.Function1N(
                arg1 = CHFuzzableType.StringType, // Path to model
                argN = CHSpecialType.CatboostParameter,
                output = CHFuzzableType.Float64.name
              )
            )
          )
        )
      case "defaultValueOfTypeName" =>
        executeInSequenceUntilSuccess(
          CHFuzzableType.ClickHouseType.fuzzingValues,
          v => client.executeNoResult(s"SELECT toTypeName(${fn.name}($v))")
        ).map(_ =>
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function1s = Seq(Function1(CHFuzzableType.ClickHouseType, CHAggregatedType.Any.name))
          )
        )
      case "encrypt" | "aes_encrypt_mysql" | "decrypt" | "tryDecrypt" | "aes_decrypt_mysql" =>
        val encryptionMode = (CHFuzzableType.EncryptionMode, CHFuzzableType.EncryptionMode.fuzzingValues)
        val str = (
          CHFuzzableType.StringType,
          Seq(
            "'1234567891012131'", // 16 chars, used as a key and iv
            "'12345678910121314151617181920212'", // 32 chars, used as a key
            "unhex('24E9E4966469')"
          )
        )
        val fixedStr = (
          CHFuzzableType.FixedString,
          Seq(
            "'1234567891012131'::FixedString(16)", // 16 chars, used as a key and iv
            "'12345678910121314151617181920212'::FixedString(32)", // 32 chars, used as a key
            "unhex('24E9E4966469')::FixedString(6)"
          )
        )

        for
          functions3 <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                Seq(encryptionMode),
                Seq(str, fixedStr),
                Seq(str, fixedStr)
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3) => Function3(arg1, arg2, arg3, io._2)
                  case _                     => throw Exception(s"Expected 3 argument, but found ${io._1.size} arguments")
              }
            }

          functions4 <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                Seq(encryptionMode),
                Seq(str, fixedStr),
                Seq(str, fixedStr),
                Seq(str, fixedStr)
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3, arg4) => Function4(arg1, arg2, arg3, arg4, io._2)
                  case _                           => throw Exception(s"Expected 4 argument, but found ${io._1.size} arguments")
              }
            }

          functions5 <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                Seq(encryptionMode),
                Seq(str, fixedStr),
                Seq(str, fixedStr),
                Seq(str, fixedStr),
                Seq(str, fixedStr)
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3, arg4, arg5) => Function5(arg1, arg2, arg3, arg4, arg5, io._2)
                  case _                                 => throw Exception(s"Expected 5 argument, but found ${io._1.size} arguments")
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function3s = functions3,
            function4s = functions4,
            function5s = functions5
          )
        }
      case "sequenceNextNode" => // TODO Test it similarly to "encrypt"
        // This method has many parameters and arguments. It's not worth to fuzz it.
        import CHFuzzableType._
        import CHSpecialType._

        // format: off
        Future.successful(
          fn.copy(
            specialParametric2Function2Ns =
              for
                (direction, base) <- 
                  Seq(
                    (SequenceDirectionForward, SequenceBaseHead),
                    (SequenceDirectionBackward, SequenceBaseTail)
                  )

                timestamp <-
                  Seq(
                    BooleanType, UInt8, UInt16, UInt32, UInt64, Date, DateTime, LowCardinalityBoolean,
                    LowCardinalityUInt8, LowCardinalityUInt16, LowCardinalityUInt32, LowCardinalityUInt64,
                    LowCardinalityDate, LowCardinalityDateTime, LowCardinalityNullableBoolean, LowCardinalityNullableUInt8,
                    LowCardinalityNullableUInt16, LowCardinalityNullableUInt32, LowCardinalityNullableUInt64,
                    LowCardinalityNullableDate, LowCardinalityNullableDateTime, NullableBoolean, NullableUInt8,
                    NullableUInt16, NullableUInt32, NullableUInt64, NullableDate, NullableDateTime
                  )

              yield {
                CHFunctionIO.Parametric2Function2N(direction, base, timestamp, StringType, BooleanType, "Nullable(String)")
              },
            specialParametric2Function3Ns =
              for
                (direction, base) <- 
                  Seq(
                    (SequenceDirectionForward, SequenceBaseFirstMatch),
                    (SequenceDirectionForward, SequenceBaseLastMatch),
                    (SequenceDirectionBackward, SequenceBaseFirstMatch),
                    (SequenceDirectionBackward, SequenceBaseLastMatch)
                  )

                timestamp <-
                  Seq(
                    BooleanType, UInt8, UInt16, UInt32, UInt64, Date, DateTime, LowCardinalityBoolean,
                    LowCardinalityUInt8, LowCardinalityUInt16, LowCardinalityUInt32, LowCardinalityUInt64,
                    LowCardinalityDate, LowCardinalityDateTime, LowCardinalityNullableBoolean, LowCardinalityNullableUInt8,
                    LowCardinalityNullableUInt16, LowCardinalityNullableUInt32, LowCardinalityNullableUInt64,
                    LowCardinalityNullableDate, LowCardinalityNullableDateTime, NullableBoolean, NullableUInt8,
                    NullableUInt16, NullableUInt32, NullableUInt64, NullableDate, NullableDateTime
                  )

              yield {
                CHFunctionIO.Parametric2Function3N(direction, base, timestamp, StringType, BooleanType, BooleanType, "Nullable(String)")
              }
          )
        )
        // format: on
      case _ =>
        Future.successful(fn)

  /**
    * TODO
    *
    * @param fnName
    * @param inputSignatures
    * @param client
    * @param ec
    * @return
    */
  private def fuzzSignatures(
      fnName: String,
      inputSignatures: Seq[InputTypesWithFuzzingValues]
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[(InputTypes, OutputType)]] =
    executeInParallelOnlySuccess(
      inputSignatures,
      inputTypes =>
        val queries =
          buildFuzzingValuesArgs(inputTypes.map(_._2)).map(args => s"SELECT toTypeName($fnName($args))")

        executeInSequenceOnlySuccess(queries, client.execute(_).map(_.data.head.head.asInstanceOf[String])).map(
          outputTypes => (inputTypes.map(_._1), outputTypes.reduce(Fuzzer.mergeOutputType))
        )
      ,
      maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
    )

  private type InputTypesWithFuzzingValues = Seq[(CHFuzzableType, Seq[String])]
  private type InputTypes = Seq[CHFuzzableType]
  private type OutputType = String
}
