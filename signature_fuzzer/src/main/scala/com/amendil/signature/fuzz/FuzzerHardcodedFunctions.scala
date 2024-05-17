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
import scala.util.{Failure, Success}

object FuzzerHardcodedFunctions extends StrictLogging {

  private[fuzz] def fuzz(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    fn.name match
      case "CAST" | "_CAST" | "accurateCast" | "accurateCastOrNull" => // TODO Test it similarly to "encrypt"
        // Cast methods can return any kind of type, depending on the value of a String.
        // This is an edge case not automatically handled by this fuzzer, and it is not worth it to handle it.
        Future.successful(
          fn.copy(function2s =
            Seq(CHFunctionIO.Function2(CHAggregatedType.Any, CHFuzzableType.ClickHouseType, CHAggregatedType.Any))
          )
        )
      case "accurateCastOrDefault" => // TODO Test it similarly to "encrypt"
        // Cast methods can return any kind of type, depending on the value of a String.
        // This is an edge case not automatically handled by this fuzzer, and it is not worth it to handle it.
        Future.successful(
          fn.copy(
            function2s =
              Seq(CHFunctionIO.Function2(CHAggregatedType.Any, CHFuzzableType.ClickHouseType, CHAggregatedType.Any)),
            // FIXME Third argument of the Function3 is not really "Any", but should be of the ClickHouseType chosen
            function3s = Seq(
              CHFunctionIO.Function3(
                CHAggregatedType.Any,
                CHFuzzableType.ClickHouseType,
                CHAggregatedType.Any,
                CHAggregatedType.Any
              )
            )
          )
        )
      case "catboostEvaluate" =>
        // Number and type of arguments depend on the model, which we don't know at compile time.

        Future.successful(
          fn.copy(
            function1Ns = Seq(
              CHFunctionIO.Function1N(
                arg1 = CHFuzzableType.SpecialString, // Path to model
                argN = CHSpecialType.CatboostParameter,
                output = CHFuzzableType.Float64
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
            function1s = Seq(Function1(CHFuzzableType.ClickHouseType, CHAggregatedType.Any))
          )
        )
      case "dictGet" | "dictGetAll" | "dictGetOrNull" =>
        val dictionaryNameType = Seq((CHFuzzableType.DictionaryName, CHFuzzableType.DictionaryName.fuzzingValues))
        val dictionaryColumnsType =
          Seq(
            (
              CHFuzzableType.StringType,
              Seq(
                "'id'",
                "'name'",
                "'version'",
                "'dateValue'",
                "'dateTimeValue'",
                "'float32Value'",
                "'float64Value'",
                "'int16Value'",
                "'int32Value'",
                "'int64Value'",
                "'int8Value'",
                "'iPv4Value'",
                "'iPv6Value'",
                "'stringValue'",
                "'uint16Value'",
                "'uint32Value'",
                "'uint64Value'",
                "'uint8Value'",
                "'uuidValue'"
              )
            ),
            (
              CHFuzzableType.FixedString,
              Seq(
                "'id'::FixedString(20)",
                "'name'::FixedString(20)",
                "'dateValue'::FixedString(20)",
                "'dateTimeValue'::FixedString(20)",
                "'float32Value'::FixedString(20)",
                "'float64Value'::FixedString(20)",
                "'int16Value'::FixedString(20)",
                "'int32Value'::FixedString(20)",
                "'int64Value'::FixedString(20)",
                "'int8Value'::FixedString(20)",
                "'iPv4Value'::FixedString(20)",
                "'iPv6Value'::FixedString(20)",
                "'stringValue'::FixedString(20)",
                "'uint16Value'::FixedString(20)",
                "'uint32Value'::FixedString(20)",
                "'uint64Value'::FixedString(20)",
                "'uint8Value'::FixedString(20)",
                "'uuidValue'::FixedString(20)"
              )
            )
          )
        val idExprType = CHFuzzableAbstractType.values.toSeq
          .filterNot(_.isInstanceOf[CustomStringBasedAbstractType])
          .flatMap(_.chFuzzableTypes)
          .map(t => (t, t.fuzzingValues))
        for attributeNamesTypes <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                dictionaryNameType,
                dictionaryColumnsType,
                idExprType
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3) => arg2
                  case _                     => throw Exception(s"Expected 3 arguments, but found ${io._1.size} arguments")
              }.distinct
            }
        yield {
          val function3s =
            attributeNamesTypes.flatMap(attributeNamesType =>
              Seq(
                Function3(
                  CHFuzzableType.DictionaryName,
                  attributeNamesType,
                  CHAggregatedType.Any,
                  CHAggregatedType.Any
                ),
                Function3(
                  CHFuzzableType.DictionaryName,
                  CHSpecialType.TupleN(attributeNamesType),
                  CHAggregatedType.Any,
                  CHAggregatedType.Any
                )
              )
            )

          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function3s = function3s
          )
        }
      case "dictGetOrDefault" =>
        val dictionaryNameType = Seq((CHFuzzableType.DictionaryName, CHFuzzableType.DictionaryName.fuzzingValues))
        val dictionaryColumnsType =
          Seq(
            (
              CHFuzzableType.StringType,
              Seq(
                "'id'",
                "'name'",
                "'version'",
                "'dateValue'",
                "'dateTimeValue'",
                "'float32Value'",
                "'float64Value'",
                "'int16Value'",
                "'int32Value'",
                "'int64Value'",
                "'int8Value'",
                "'iPv4Value'",
                "'iPv6Value'",
                "'stringValue'",
                "'uint16Value'",
                "'uint32Value'",
                "'uint64Value'",
                "'uint8Value'",
                "'uuidValue'"
              )
            ),
            (
              CHFuzzableType.FixedString,
              Seq(
                "'id'::FixedString(20)",
                "'name'::FixedString(20)",
                "'dateValue'::FixedString(20)",
                "'dateTimeValue'::FixedString(20)",
                "'float32Value'::FixedString(20)",
                "'float64Value'::FixedString(20)",
                "'int16Value'::FixedString(20)",
                "'int32Value'::FixedString(20)",
                "'int64Value'::FixedString(20)",
                "'int8Value'::FixedString(20)",
                "'iPv4Value'::FixedString(20)",
                "'iPv6Value'::FixedString(20)",
                "'stringValue'::FixedString(20)",
                "'uint16Value'::FixedString(20)",
                "'uint32Value'::FixedString(20)",
                "'uint64Value'::FixedString(20)",
                "'uint8Value'::FixedString(20)",
                "'uuidValue'::FixedString(20)"
              )
            )
          )
        val idExprType = CHFuzzableAbstractType.values.toSeq
          .filterNot(_.isInstanceOf[CustomStringBasedAbstractType])
          .flatMap(_.chFuzzableTypes)
          .map(t => (t, t.fuzzingValues))
        val defaultValueType =
          CHFuzzableAbstractType.values.toSeq.flatMap(_.chFuzzableTypes).map(t => (t, t.fuzzingValues))

        for attributeNamesTypes <-
            fuzzSignatures(
              "dictGet",
              crossJoin(
                dictionaryNameType,
                dictionaryColumnsType,
                idExprType
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3) => arg2
                  case _                     => throw Exception(s"Expected 3 arguments, but found ${io._1.size} arguments")
              }.distinct
            }
        yield {
          val function4s =
            attributeNamesTypes.flatMap(attributeNamesType =>
              Seq(
                Function4(
                  CHFuzzableType.DictionaryName,
                  attributeNamesType,
                  CHAggregatedType.Any,
                  CHAggregatedType.Any,
                  CHAggregatedType.Any
                ),
                Function4(
                  CHFuzzableType.DictionaryName,
                  CHSpecialType.TupleN(attributeNamesType),
                  CHAggregatedType.Any,
                  CHAggregatedType.Any,
                  CHAggregatedType.Any
                )
              )
            )

          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function4s = function4s
          )
        }
      case "dictGetDate" | "dictGetDateTime" | "dictGetFloat42" | "dictGetFloat64" | "dictGetIPv4" | "dictGetIPv6" |
          "dictGetInt16" | "dictGetInt32" | "dictGetInt64" | "dictGetInt8" | "dictGetString" | "dictGetUInt16" |
          "dictGetUInt32" | "dictGetUInt64" | "dictGetUInt8" | "dictGetUUID" =>
        val dictionaryNameType = Seq((CHFuzzableType.DictionaryName, CHFuzzableType.DictionaryName.fuzzingValues))
        val dictionaryColumnsType =
          Seq(
            (
              CHFuzzableType.StringType,
              Seq(
                "'id'",
                "'name'",
                "'version'",
                "'dateValue'",
                "'dateTimeValue'",
                "'float32Value'",
                "'float64Value'",
                "'int16Value'",
                "'int32Value'",
                "'int64Value'",
                "'int8Value'",
                "'iPv4Value'",
                "'iPv6Value'",
                "'stringValue'",
                "'uint16Value'",
                "'uint32Value'",
                "'uint64Value'",
                "'uint8Value'",
                "'uuidValue'"
              )
            )
          )
        val idExprType = CHFuzzableAbstractType.values.toSeq
          .filterNot(_.isInstanceOf[CustomStringBasedAbstractType])
          .flatMap(_.chFuzzableTypes)
          .map(t => (t, t.fuzzingValues))
        for function3s <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                dictionaryNameType,
                dictionaryColumnsType,
                idExprType
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3) => Function3(arg1, arg2, arg3, io._2)
                  case _                     => throw Exception(s"Expected 3 arguments, but found ${io._1.size} arguments")
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function3s = function3s
          )
        }
      case "dictGetDateOrDefault" | "dictGetDateTimeOrDefault" | "dictGetFloat32OrDefault" | "dictGetFloat64OrDefault" |
          "dictGetIPv4OrDefault" | "dictGetIPv6OrDefault" | "dictGetInt16OrDefault" | "dictGetInt32OrDefault" |
          "dictGetInt64OrDefault" | "dictGetInt8OrDefault" | "dictGetStringOrDefault" | "dictGetUInt16OrDefault" |
          "dictGetUInt32OrDefault" | "dictGetUInt64OrDefault" | "dictGetUInt8OrDefault" | "dictGetUUIDOrDefault" =>
        val dictionaryNameType = Seq((CHFuzzableType.DictionaryName, CHFuzzableType.DictionaryName.fuzzingValues))
        val dictionaryColumnsType =
          Seq(
            (
              CHFuzzableType.StringType,
              Seq(
                "'id'",
                "'name'",
                "'version'",
                "'dateValue'",
                "'dateTimeValue'",
                "'float32Value'",
                "'float64Value'",
                "'int16Value'",
                "'int32Value'",
                "'int64Value'",
                "'int8Value'",
                "'iPv4Value'",
                "'iPv6Value'",
                "'stringValue'",
                "'uint16Value'",
                "'uint32Value'",
                "'uint64Value'",
                "'uint8Value'",
                "'uuidValue'"
              )
            )
          )
        val idExprType = CHFuzzableAbstractType.values.toSeq
          .filterNot(_.isInstanceOf[CustomStringBasedAbstractType])
          .flatMap(_.chFuzzableTypes)
          .map(t => (t, t.fuzzingValues))
        val defaultValueType =
          CHFuzzableAbstractType.values.toSeq.flatMap(_.chFuzzableTypes).map(t => (t, t.fuzzingValues))

        for
          function3s <-
            fuzzSignatures(
              fn.name.replace("OrDefault", ""),
              crossJoin(
                dictionaryNameType,
                dictionaryColumnsType,
                idExprType
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3) => Function3(arg1, arg2, arg3, io._2)
                  case _                     => throw Exception(s"Expected 3 arguments, but found ${io._1.size} arguments")
              }
            }

          validIdExprType = function3s.map { case Function3(_, _, arg3, _) => arg3 }.distinct
          function4s <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                dictionaryNameType,
                dictionaryColumnsType,
                idExprType.filter(t => validIdExprType.contains(t._1)),
                defaultValueType
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3, arg4) => Function4(arg1, arg2, arg3, arg4, io._2)
                  case _                           => throw Exception(s"Expected 4 arguments, but found ${io._1.size} arguments")
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function4s = function4s
          )
        }
      case "dictHas" =>
        val dictionaryNameType = Seq((CHFuzzableType.DictionaryName, CHFuzzableType.DictionaryName.fuzzingValues))
        val idExprType = CHFuzzableAbstractType.values.toSeq
          .filterNot(_.isInstanceOf[CustomStringBasedAbstractType])
          .flatMap(_.chFuzzableTypes)
          .map(t => (t, t.fuzzingValues))
        for outputTypes <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                dictionaryNameType,
                idExprType
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2) => io._2
                  case _               => throw Exception(s"Expected 2 arguments, but found ${io._1.size} arguments")
              }
            }
        yield {
          if outputTypes.nonEmpty then
            fn.copy(
              modes = fn.modes + CHFunction.Mode.NoOverWindow,
              function2s = Seq(
                Function2(
                  CHFuzzableType.DictionaryName,
                  CHAggregatedType.Any,
                  outputTypes.reduce(Fuzzer.mergeOutputType)
                )
              )
            )
          else fn
        }
      case "dictGetChildren" | "dictGetHierarchy" =>
        val dictionaryNameType = Seq((CHFuzzableType.DictionaryName, CHFuzzableType.DictionaryName.fuzzingValues))
        val idExprType = CHFuzzableAbstractType.values.toSeq
          .filterNot(_.isInstanceOf[CustomStringBasedAbstractType])
          .flatMap(_.chFuzzableTypes)
          .map(t => (t, t.fuzzingValues))
        for function2s <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                dictionaryNameType,
                idExprType
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2) => Function2(arg1, arg2, io._2)
                  case _               => throw Exception(s"Expected 2 arguments, but found ${io._1.size} arguments")
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function2s = function2s
          )
        }
      case "dictGetDescendants" =>
        val dictionaryNameType = Seq((CHFuzzableType.DictionaryName, CHFuzzableType.DictionaryName.fuzzingValues))
        val idExprType = CHFuzzableAbstractType.values.toSeq
          .filterNot(_.isInstanceOf[CustomStringBasedAbstractType])
          .flatMap(_.chFuzzableTypes)
          .map(t => (t, t.fuzzingValues))
        for function2s <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                dictionaryNameType,
                idExprType
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2) => Function2(arg1, arg2, io._2)
                  case _               => throw Exception(s"Expected 2 arguments, but found ${io._1.size} arguments")
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function2s = function2s
          )
        }
      case "dictIsIn" =>
        val dictionaryNameType = Seq((CHFuzzableType.DictionaryName, CHFuzzableType.DictionaryName.fuzzingValues))
        val idExprType = CHFuzzableAbstractType.values.toSeq
          .filterNot(_.isInstanceOf[CustomStringBasedAbstractType])
          .flatMap(_.chFuzzableTypes)
          .map(t => (t, t.fuzzingValues))
        for function3s <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                dictionaryNameType,
                idExprType,
                idExprType
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3) => Function3(arg1, arg2, arg3, io._2)
                  case _                     => throw Exception(s"Expected 3 arguments, but found ${io._1.size} arguments")
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function3s = function3s
          )
        }
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
          function3s <-
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
                  case _                     => throw Exception(s"Expected 3 arguments, but found ${io._1.size} arguments")
              }
            }

          function4s <-
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
                  case _                           => throw Exception(s"Expected 4 arguments, but found ${io._1.size} arguments")
              }
            }

          function5s <-
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
                  case _                                 => throw Exception(s"Expected 5 arguments, but found ${io._1.size} arguments")
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function3s = function3s,
            function4s = function4s,
            function5s = function5s
          )
        }
      case "geoDistance" | "greatCircleAngle" | "greatCircleDistance" =>
        val fuzzType = CHFuzzableAbstractType.Numbers.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))
        for function4s <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                fuzzType,
                fuzzType,
                fuzzType,
                fuzzType
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3, arg4) => Function4(arg1, arg2, arg3, arg4, io._2)
                  case _                           => throw Exception(s"Expected 4 arguments, but found ${io._1.size} arguments")
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function4s = function4s
          )
        }
      case "geohashesInBox" =>
        val fuzzType = CHFuzzableAbstractType.Numbers.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))
        for function5s <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                fuzzType,
                fuzzType,
                fuzzType,
                fuzzType,
                Seq((CHFuzzableType.UInt8, CHFuzzableType.UInt8.fuzzingValues))
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3, arg4, arg5) => Function5(arg1, arg2, arg3, arg4, arg5, io._2)
                  case _                                 => throw Exception(s"Expected 5 arguments, but found ${io._1.size} arguments")
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function5s = function5s
          )
        }
      case "hasColumnInTable" =>
        for
          function3s <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                Seq(
                  (CHFuzzableType.StringType, Seq("'system'")),
                  (CHFuzzableType.FixedString, Seq("'system'::FixedString(6)"))
                ),
                Seq(
                  (CHFuzzableType.StringType, Seq("'numbers'")),
                  (CHFuzzableType.FixedString, Seq("'numbers'::FixedString(7)"))
                ),
                Seq(
                  (CHFuzzableType.StringType, Seq("'number'")),
                  (CHFuzzableType.FixedString, Seq("'number'::FixedString(6)"))
                )
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3) => Function3(arg1, arg2, arg3, io._2)
                  case _                     => throw Exception(s"Expected 3 arguments, but found ${io._1.size} arguments")
              }
            }

          function4s <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                Seq(
                  (CHFuzzableType.StringType, Seq("'127.0.0.1'")),
                  (CHFuzzableType.FixedString, Seq("'127.0.0.1'::FixedString(9)"))
                ),
                Seq(
                  (CHFuzzableType.StringType, Seq("'system'")),
                  (CHFuzzableType.FixedString, Seq("'system'::FixedString(6)"))
                ),
                Seq(
                  (CHFuzzableType.StringType, Seq("'numbers'")),
                  (CHFuzzableType.FixedString, Seq("'numbers'::FixedString(7)"))
                ),
                Seq(
                  (CHFuzzableType.StringType, Seq("'number'")),
                  (CHFuzzableType.FixedString, Seq("'number'::FixedString(6)"))
                )
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3, arg4) => Function4(arg1, arg2, arg3, arg4, io._2)
                  case _                           => throw Exception(s"Expected 4 arguments, but found ${io._1.size} arguments")
              }
            }

          function5s <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                Seq(
                  (CHFuzzableType.StringType, Seq("'127.0.0.1'")),
                  (CHFuzzableType.FixedString, Seq("'127.0.0.1'::FixedString(9)"))
                ),
                Seq(
                  (CHFuzzableType.StringType, Seq("'default'")),
                  (CHFuzzableType.FixedString, Seq("'default'::FixedString(7)"))
                ),
                Seq(
                  (CHFuzzableType.StringType, Seq("'system'")),
                  (CHFuzzableType.FixedString, Seq("'system'::FixedString(6)"))
                ),
                Seq(
                  (CHFuzzableType.StringType, Seq("'numbers'")),
                  (CHFuzzableType.FixedString, Seq("'numbers'::FixedString(7)"))
                ),
                Seq(
                  (CHFuzzableType.StringType, Seq("'number'")),
                  (CHFuzzableType.FixedString, Seq("'number'::FixedString(6)"))
                )
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3, arg4, arg5) => Function5(arg1, arg2, arg3, arg4, arg5, io._2)
                  case _                                 => throw Exception(s"Expected 5 arguments, but found ${io._1.size} arguments")
              }
            }

          function6s <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                Seq(
                  (CHFuzzableType.StringType, Seq("'127.0.0.1'")),
                  (CHFuzzableType.FixedString, Seq("'127.0.0.1'::FixedString(9)"))
                ),
                Seq(
                  (CHFuzzableType.StringType, Seq("'default'")),
                  (CHFuzzableType.FixedString, Seq("'default'::FixedString(7)"))
                ),
                Seq((CHFuzzableType.StringType, Seq("''")), (CHFuzzableType.FixedString, Seq("''::FixedString(0)"))),
                Seq(
                  (CHFuzzableType.StringType, Seq("'system'")),
                  (CHFuzzableType.FixedString, Seq("'system'::FixedString(6)"))
                ),
                Seq(
                  (CHFuzzableType.StringType, Seq("'numbers'")),
                  (CHFuzzableType.FixedString, Seq("'numbers'::FixedString(7)"))
                ),
                Seq(
                  (CHFuzzableType.StringType, Seq("'number'")),
                  (CHFuzzableType.FixedString, Seq("'number'::FixedString(6)"))
                )
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3, arg4, arg5, arg6) => Function6(arg1, arg2, arg3, arg4, arg5, arg6, io._2)
                  case _                                       => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function3s = function3s,
            function4s = function4s,
            function5s = function5s,
            function6s = function6s
          )
        }
      case "kolmogorovSmirnovTest" =>
        val alternativeTypes = Seq((CHFuzzableType.TestAlternative, CHFuzzableType.TestAlternative.fuzzingValues))
        val computationMethodTypes = Seq(
          (CHFuzzableType.PValueComputationMethod, CHFuzzableType.PValueComputationMethod.fuzzingValues)
        )
        val sampleDataTypes = CHFuzzableAbstractType.Numbers.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))
        val sampleIndexTypes = Seq((CHFuzzableType.UInt8, Seq("randNormal(0, 1)")))

        for
          parametric0Function2 <-
            fuzzParametricSignatures(
              fn.name,
              Seq(Nil),
              crossJoin(
                sampleDataTypes,
                sampleIndexTypes
              ).map(_.toList),
              sourceTable = Some("numbers(100)")
            ).map { signatures =>
              signatures.map { io =>
                (io._1, io._2) match
                  case (Seq(), Seq(arg1, arg2)) => Parametric0Function2(arg1, arg2, io._3)
                  case _ =>
                    throw Exception(
                      s"Expected 0 parameters and 2 arguments, but found ${io._1.size} parameters and ${io._2.size} arguments"
                    )
              }
            }

          parametric1Function2 <-
            fuzzParametricSignatures(
              fn.name,
              Seq(alternativeTypes),
              crossJoin(
                sampleDataTypes,
                sampleIndexTypes
              ).map(_.toList),
              sourceTable = Some("numbers(100)")
            ).map { signatures =>
              signatures.map { io =>
                (io._1, io._2) match
                  case (Seq(param1), Seq(arg1, arg2)) => Parametric1Function2(param1, arg1, arg2, io._3)
                  case _ =>
                    throw Exception(
                      s"Expected 1 parameter and 2 arguments, but found ${io._1.size} parameters and ${io._2.size} arguments"
                    )
              }
            }

          parametric2Function2 <-
            fuzzParametricSignatures(
              fn.name,
              crossJoin(
                alternativeTypes,
                computationMethodTypes
              ).map(_.toList),
              crossJoin(
                sampleDataTypes,
                sampleIndexTypes
              ).map(_.toList),
              sourceTable = Some("numbers(100)")
            ).map { signatures =>
              signatures.map { io =>
                (io._1, io._2) match
                  case (Seq(param1, param2), Seq(arg1, arg2)) => Parametric2Function2(param1, param2, arg1, arg2, io._3)
                  case _ =>
                    throw Exception(
                      s"Expected 2 parameters and 2 arguments, but found ${io._1.size} parameters and ${io._2.size} arguments"
                    )
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow + CHFunction.Mode.OverWindow,
            parametric0Function2s = parametric0Function2,
            parametric1Function2s = parametric1Function2,
            parametric2Function2s = parametric2Function2
          )
        }
      case "makeDateTime" =>
        // Actually ends up in OOM
        Future.successful(fn)

      // val fuzzType = CHFuzzableAbstractType.Numbers.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))

      // for
      //   function6s <-
      //     fuzzSignatures(
      //       fn.name,
      //       crossJoin(
      //         fuzzType,
      //         fuzzType,
      //         fuzzType,
      //         fuzzType,
      //         fuzzType,
      //         fuzzType
      //       ).map(_.toList)
      //     ).map { signatures =>
      //       signatures.map { io =>
      //         io._1 match
      //           case Seq(arg1, arg2, arg3, arg4, arg5, arg6) => Function6(arg1, arg2, arg3, arg4, arg5, arg6, io._2)
      //           case _                                       => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
      //       }
      //     }

      //   function7s <-
      //     fuzzSignatures(
      //       fn.name,
      //       crossJoin(
      //         fuzzType,
      //         fuzzType,
      //         fuzzType,
      //         fuzzType,
      //         fuzzType,
      //         fuzzType,
      //         Seq((CHFuzzableType.TimeZone, Seq(CHFuzzableType.TimeZone.fuzzingValues.head)))
      //       ).map(_.toList)
      //     ).map { signatures =>
      //       signatures.map { io =>
      //         io._1 match
      //           case Seq(arg1, arg2, arg3, arg4, arg5, arg6, arg7) => Function7(arg1, arg2, arg3, arg4, arg5, arg6, arg7, io._2)
      //           case _                                             => throw Exception(s"Expected 7 arguments, but found ${io._1.size} arguments")
      //       }
      //     }
      // yield {
      //   fn.copy(
      //     modes = fn.modes + CHFunction.Mode.NoOverWindow,
      //     function6s = function6s,
      //     function7s = function7s
      //   )
      // }
      case "makeDateTime64" =>
        // Actually ends up in OOM
        Future.successful(fn)

      // val fuzzType = CHFuzzableAbstractType.Numbers.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))

      // for
      //   function6s <-
      //     fuzzSignatures(
      //       fn.name,
      //       crossJoin(
      //         fuzzType,
      //         fuzzType,
      //         fuzzType,
      //         fuzzType,
      //         fuzzType,
      //         fuzzType
      //       ).map(_.toList)
      //     ).map { signatures =>
      //       signatures.map { io =>
      //         io._1 match
      //           case Seq(arg1, arg2, arg3, arg4, arg5, arg6) => Function6(arg1, arg2, arg3, arg4, arg5, arg6, io._2)
      //           case _                                       => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
      //       }
      //     }

      //   function7s <-
      //     fuzzSignatures(
      //       fn.name,
      //       crossJoin(
      //         fuzzType,
      //         fuzzType,
      //         fuzzType,
      //         fuzzType,
      //         fuzzType,
      //         fuzzType,
      //         fuzzType
      //       ).map(_.toList)
      //     ).map { signatures =>
      //       signatures.map { io =>
      //         io._1 match
      //           case Seq(arg1, arg2, arg3, arg4, arg5, arg6, arg7) => Function7(arg1, arg2, arg3, arg4, arg5, arg6, arg7, io._2)
      //           case _                                             => throw Exception(s"Expected 7 arguments, but found ${io._1.size} arguments")
      //       }
      //     }

      //   function8s <-
      //       fuzzSignatures(
      //         fn.name,
      //         crossJoin(
      //           fuzzType,
      //           fuzzType,
      //           fuzzType,
      //           fuzzType,
      //           fuzzType,
      //           fuzzType,
      //           fuzzType,
      //           fuzzType
      //         ).map(_.toList)
      //       ).map { signatures =>
      //         signatures.map { io =>
      //           io._1 match
      //             case Seq(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) => Function8(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, io._2)
      //             case _                                                   => throw Exception(s"Expected 8 arguments, but found ${io._1.size} arguments")
      //         }
      //       }

      //   function9s <-
      //       fuzzSignatures(
      //         fn.name,
      //         crossJoin(
      //           fuzzType,
      //           fuzzType,
      //           fuzzType,
      //           fuzzType,
      //           fuzzType,
      //           fuzzType,
      //           fuzzType,
      //           fuzzType,
      //           Seq((CHFuzzableType.TimeZone, Seq(CHFuzzableType.TimeZone.fuzzingValues.head)))
      //         ).map(_.toList)
      //       ).map { signatures =>
      //         signatures.map { io =>
      //           io._1 match
      //             case Seq(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) => Function9(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, io._2)
      //             case _                                                         => throw Exception(s"Expected 9 arguments, but found ${io._1.size} arguments")
      //         }
      //       }
      // yield {
      //   fn.copy(
      //     modes = fn.modes + CHFunction.Mode.NoOverWindow,
      //     function6s = function6s,
      //     function7s = function7s,
      //     function8s = function8s,
      //     function9s = function9s
      //   )
      // }
      case "mannWhitneyUTest" =>
        val alternativeTypes = Seq((CHFuzzableType.TestAlternative, CHFuzzableType.TestAlternative.fuzzingValues))
        val continuityCorrectionTypes =
          CHFuzzableAbstractType.Numbers.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))
        val sampleDataTypes = CHFuzzableAbstractType.Numbers.chFuzzableTypes.map(t =>
          (t, Seq(s"toInt8(randNormal(0, 1))::${if t.name.contains("Decimal") then s"${t.name}(1)" else t.name}"))
        )
        val sampleIndexTypes = Seq((CHFuzzableType.UInt8, Seq("randNormal(0, 1)")))

        for
          parametric0Function2 <-
            fuzzParametricSignatures(
              fn.name,
              Seq(Nil),
              crossJoin(
                sampleDataTypes,
                sampleIndexTypes
              ).map(_.toList),
              sourceTable = Some("numbers(100)")
            ).map { signatures =>
              signatures.map { io =>
                (io._1, io._2) match
                  case (Seq(), Seq(arg1, arg2)) => Parametric0Function2(arg1, arg2, io._3)
                  case _ =>
                    throw Exception(
                      s"Expected 0 parameters and 2 arguments, but found ${io._1.size} parameters and ${io._2.size} arguments"
                    )
              }
            }

          parametric1Function2 <-
            fuzzParametricSignatures(
              fn.name,
              Seq(alternativeTypes),
              crossJoin(
                sampleDataTypes,
                sampleIndexTypes
              ).map(_.toList),
              sourceTable = Some("numbers(100)")
            ).map { signatures =>
              signatures.map { io =>
                (io._1, io._2) match
                  case (Seq(param1), Seq(arg1, arg2)) => Parametric1Function2(param1, arg1, arg2, io._3)
                  case _ =>
                    throw Exception(
                      s"Expected 1 parameter and 2 arguments, but found ${io._1.size} parameters and ${io._2.size} arguments"
                    )
              }
            }

          parametric2Function2 <-
            fuzzParametricSignatures(
              fn.name,
              crossJoin(
                alternativeTypes,
                continuityCorrectionTypes
              ).map(_.toList),
              crossJoin(
                sampleDataTypes,
                sampleIndexTypes
              ).map(_.toList),
              sourceTable = Some("numbers(100)")
            ).map { signatures =>
              signatures.map { io =>
                (io._1, io._2) match
                  case (Seq(param1, param2), Seq(arg1, arg2)) => Parametric2Function2(param1, param2, arg1, arg2, io._3)
                  case _ =>
                    throw Exception(
                      s"Expected 2 parameters and 2 arguments, but found ${io._1.size} parameters and ${io._2.size} arguments"
                    )
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow + CHFunction.Mode.OverWindow,
            parametric0Function2s = parametric0Function2,
            parametric1Function2s = parametric1Function2,
            parametric2Function2s = parametric2Function2
          )
        }
      case "mapApply" => // Any Map working with identity actually
        val typeT = CHSpecialType.GenericType("T")
        val typeU = CHSpecialType.GenericType("U")
        val lambdaIdentityType: (CHSpecialType.LambdaType, Seq[String]) =
          (CHSpecialType.LambdaType(CHSpecialType.Tuple(Seq(typeT, typeU))), Seq("x, y -> (x, y)"))
        val mapType: (CHSpecialType.Map, Seq[String]) = (CHSpecialType.Map(typeT, typeU), Seq("map(now(), today())"))

        for lambdaMapFunction1Opt <-
            fuzzSignatures(
              fn.name,
              Seq(Seq(lambdaIdentityType, mapType))
            ).transform {
              case Failure(_) => Success(None)
              case Success(signatures) =>
                assume(signatures.size == 1)
                val io = signatures.head

                io match
                  case (Seq(arg1, arg2), CHSpecialType.Map(CHFuzzableType.DateTime, CHFuzzableType.Date)) =>
                    Success(Some(LambdaMapFunction1(lambdaIdentityType._1, mapType._1, mapType._1)))
                  case (Seq(arg1, arg2), _) =>
                    Failure(Exception(s"Expected the output type `Map(DateTime, Date)`, but got `${io._2.name}`"))
                  case _ => Failure(Exception(s"Expected 2 arguments, but found ${io._1.size} arguments"))
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            lambdaMapFunction1Opt = lambdaMapFunction1Opt
          )
        }
      case "meanZTest" =>
        val floatNumbersType = CHFuzzableAbstractType.Numbers.chFuzzableTypes
          .filter(_.name.toLowerCase().contains("float"))
          .map(t => (t, t.fuzzingValues))
        val numbersType = CHFuzzableAbstractType.Numbers.chFuzzableTypes.map(t => (t, t.fuzzingValues))

        for parametric3Function2 <-
            fuzzParametricSignatures(
              fn.name,
              crossJoin(
                floatNumbersType,
                floatNumbersType,
                floatNumbersType
              ).map(_.toList),
              crossJoin(
                numbersType,
                numbersType
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                (io._1, io._2) match
                  case (Seq(param1, param2, param3), Seq(arg1, arg2)) =>
                    Parametric3Function2(param1, param2, param3, arg1, arg2, io._3)
                  case _ =>
                    throw Exception(
                      s"Expected 3 parameters and 2 arguments, but found ${io._1.size} parameters and ${io._2.size} arguments"
                    )
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow + CHFunction.Mode.OverWindow,
            parametric3Function2s = parametric3Function2
          )
        }
      case "minSampleSizeContinous" | "minSampleSizeContinuous" =>
        for function5s <-
            FuzzerNonParametricFunctions
              .fuzzAbstractInputCombinations(
                fn.name,
                Seq(
                  Seq(
                    CHFuzzableAbstractType.Numbers,
                    CHFuzzableAbstractType.Numbers,
                    CHFuzzableAbstractType.Numbers,
                    CHFuzzableAbstractType.Numbers,
                    CHFuzzableAbstractType.Numbers
                  )
                ),
                fuzzOverWindow = false,
                returnFirstOutputTypeFound = true
              )
              .map { signatures =>
                signatures.map { io =>
                  io._1 match
                    case Seq(arg1, arg2, arg3, arg4, arg5) => Function5(arg1, arg2, arg3, arg4, arg5, io._2)
                    case _                                 => throw Exception(s"Expected 5 arguments, but found ${io._1.size} arguments")
                }
              }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function5s = function5s
          )
        }
      case "minSampleSizeConversion" =>
        for function4s <-
            FuzzerNonParametricFunctions
              .fuzzAbstractInputCombinations(
                fn.name,
                Seq(
                  Seq(
                    CHFuzzableAbstractType.Numbers,
                    CHFuzzableAbstractType.Numbers,
                    CHFuzzableAbstractType.Numbers,
                    CHFuzzableAbstractType.Numbers
                  )
                ),
                fuzzOverWindow = false,
                returnFirstOutputTypeFound = true
              )
              .map { signatures =>
                signatures.map { io =>
                  io._1 match
                    case Seq(arg1, arg2, arg3, arg4) => Function4(arg1, arg2, arg3, arg4, io._2)
                    case _                           => throw Exception(s"Expected 4 arguments, but found ${io._1.size} arguments")
                }
              }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function4s = function4s
          )
        }
      case "pointInEllipses" =>
        val float64Type: Seq[(CHFuzzableType, Seq[String])] = Seq(
          (CHFuzzableType.Float64, CHFuzzableType.Float64.fuzzingValues)
        )
        for function2Ns <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                float64Type,
                float64Type,
                float64Type,
                float64Type,
                float64Type,
                float64Type
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3, arg4, arg5, arg6) if Set(arg3, arg4, arg5, arg6).size == 1 =>
                    Function2N(arg1, arg2, arg3, io._2)
                  case _ => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function2Ns = function2Ns
          )
        }
      case "proportionsZTest" =>
        val number = CHFuzzableAbstractType.Numbers.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))
        val usevarType = Seq((CHFuzzableType.Usevar, CHFuzzableType.Usevar.fuzzingValues))

        for function6s <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                number, // successes_x
                number, // successes_y
                number, // trials_x
                number, // trials_y
                number, // confidence level (float betzeen 0 and 1)
                usevarType // usevar
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3, arg4, arg5, arg6) => Function6(arg1, arg2, arg3, arg4, arg5, arg6, io._2)
                  case _                                       => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function6s = function6s
          )
        }
      case "reinterpret" =>
        Future.successful(
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function2s = Seq(
              Function2(CHAggregatedType.Any, CHFuzzableType.ClickHouseType, CHAggregatedType.Any)
            )
          )
        )
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
                CHFunctionIO.Parametric2Function2N(direction, base, timestamp, StringType, BooleanType, CHSpecialType.Nullable(CHFuzzableType.StringType))
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
                CHFunctionIO.Parametric2Function3N(direction, base, timestamp, StringType, BooleanType, BooleanType, CHSpecialType.Nullable(CHFuzzableType.StringType))
              }
          )
        )
        // format: on
      case "seriesOutliersDetectTukey" =>
        for
          function1s <-
            FuzzerNonParametricFunctions
              .fuzzAbstractInputCombinations(
                fn.name,
                Seq(
                  Seq(CHFuzzableAbstractType.ArrayNumbers)
                ),
                fuzzOverWindow = false
              )
              .map { signatures =>
                signatures.map { io =>
                  io._1 match
                    case Seq(arg1) => Function1(arg1, io._2)
                    case _         => throw Exception(s"Expected 1 argument, but found ${io._1.size} arguments")
                }
              }

          function4s <-
            FuzzerNonParametricFunctions
              .fuzzAbstractInputCombinations(
                fn.name,
                Seq(
                  Seq(
                    CHFuzzableAbstractType.ArrayNumbers,
                    CHFuzzableAbstractType.Numbers,
                    CHFuzzableAbstractType.Numbers,
                    CHFuzzableAbstractType.Numbers
                  )
                ),
                fuzzOverWindow = false
              )
              .map { signatures =>
                signatures.map { io =>
                  io._1 match
                    case Seq(arg1, arg2, arg3, arg4) => Function4(arg1, arg2, arg3, arg4, io._2)
                    case _                           => throw Exception(s"Expected 4 arguments, but found ${io._1.size} arguments")
                }
              }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function1s = function1s,
            function4s = function4s
          )
        }
      case "toModifiedJulianDay" =>
        for function1s <-
            fuzzSignatures(
              fn.name,
              Seq(
                Seq((CHFuzzableType.StringType, Seq("'2024-01-01'"))),
                Seq((CHFuzzableType.FixedString, Seq("'2024-01-01'::FixedString(10)")))
              )
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1) => Function1(arg1, io._2)
                  case _         => throw Exception(s"Expected 1 argument, but found ${io._1.size} arguments")
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function1s = function1s
          )
        }
      case "timeSlots" =>
        val dateTimeTypes = CHFuzzableAbstractType.DateTime.chFuzzableTypes.map(t => (t, t.fuzzingValues))

        val allNumbersCHTypes = CHFuzzableAbstractType.Numbers.chFuzzableTypes
        val nonExpectedNumbersTypes =
          allNumbersCHTypes.filterNot(t =>
            t.name == "Bool" || t.name.startsWith("UInt") || t.name.startsWith("Int") || t.name.startsWith(
              "Float"
            ) || t.name.startsWith("Decimal")
          )

        assume(
          nonExpectedNumbersTypes.isEmpty,
          s"Expected to handle only signed, unsigned, floating numbers, as well as Decimal. Found: ${nonExpectedNumbersTypes.map(_.name).mkString(", ")}"
        )

        val numberTypes =
          allNumbersCHTypes
            .filter(t => t.name.startsWith("UInt") || t.name.startsWith("Int"))
            .map(t => (t, Seq(s"1::${t.name}"))) ++
            allNumbersCHTypes
              .filter(t => t.name.startsWith("Decimal"))
              .map(t => (t, Seq(s"1::${t.name}(0)")))

        for
          function2s <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                dateTimeTypes,
                numberTypes
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2) => Function2(arg1, arg2, io._2)
                  case _               => throw Exception(s"Expected 2 arguments, but found ${io._1.size} arguments")
              }
            }

          function3s <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                dateTimeTypes,
                numberTypes,
                numberTypes
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2, arg3) => Function3(arg1, arg2, arg3, io._2)
                  case _                     => throw Exception(s"Expected 3 arguments, but found ${io._1.size} arguments")
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function2s = function2s,
            function3s = function3s
          )
        }
      case "variantElement" =>
        val variantColumn = Seq((CHFuzzableType.Variant, Seq("123456::Variant(UInt64, String, Array(UInt64))")))
        val typeColumn = Seq(
          (CHFuzzableType.StringType, Seq("'UInt32'::String", "'UInt64'::String")),
          (CHFuzzableType.FixedString, Seq("'UInt32'::FixedString(6)", "'UInt64'::FixedString(6)"))
        )

        for function2s <-
            fuzzSignatures(
              fn.name,
              crossJoin(
                variantColumn,
                typeColumn
              ).map(_.toList)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1, arg2) => Function2(arg1, arg2, CHAggregatedType.Any)
                  case _               => throw Exception(s"Expected 2 arguments, but found ${io._1.size} arguments")
              }
            }
        yield {
          val function3s = function2s.map { case Function2(arg1, arg2, _) =>
            Function3(arg1, arg2, CHAggregatedType.Any, CHAggregatedType.Any)
          }

          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function2s = function2s,
            function3s = function3s
          )
        }
      case "variantType" =>
        val variantColumn = Seq((CHFuzzableType.Variant, Seq("123456::Variant(UInt64, String, Array(UInt64))")))

        for function1s <-
            fuzzSignatures(
              fn.name,
              Seq(variantColumn)
            ).map { signatures =>
              signatures.map { io =>
                io._1 match
                  case Seq(arg1) => Function1(arg1, CHFuzzableType.Enum8)
                  case _         => throw Exception(s"Expected 1 argument, but found ${io._1.size} arguments")
              }
            }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function1s = function1s
          )
        }
      case "widthBucket" | "width_bucket" =>
        for function4s <-
            FuzzerNonParametricFunctions
              .fuzzAbstractInputCombinations(
                fn.name,
                Seq(
                  Seq(
                    CHFuzzableAbstractType.Numbers,
                    CHFuzzableAbstractType.Numbers,
                    CHFuzzableAbstractType.Numbers,
                    CHFuzzableAbstractType.Numbers
                  )
                ),
                fuzzOverWindow = false
              )
              .map { signatures =>
                signatures.map { io =>
                  io._1 match
                    case Seq(arg1, arg2, arg3, arg4) => Function4(arg1, arg2, arg3, arg4, io._2)
                    case _                           => throw Exception(s"Expected 4 arguments, but found ${io._1.size} arguments")
                }
              }
        yield {
          fn.copy(
            modes = fn.modes + CHFunction.Mode.NoOverWindow,
            function4s = function4s
          )
        }
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
      inputSignatures: Seq[InputTypesWithFuzzingValues],
      sourceTable: Option[String] = None
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[(InputTypes, OutputType)]] =
    executeInParallelOnlySuccess(
      inputSignatures,
      inputTypes =>
        val queries =
          buildFuzzingValuesArgs(inputTypes.map(_._2)).map(args =>
            s"SELECT toTypeName($fnName($args))${sourceTable.map(" FROM " + _).getOrElse("")}"
          )

        executeInSequenceOnlySuccess(queries, client.execute(_).map(_.data.head.head.asInstanceOf[String])).map(
          outputTypes => (inputTypes.map(_._1), outputTypes.map(CHType.getByName).reduce(Fuzzer.mergeOutputType))
        )
      ,
      maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
    )

  /**
    * TODO
    *
    * @param fnName
    * @param inputSignaturesParams
    * @param inputSignaturesArgs
    * @param client
    * @param ec
    * @return
    */
  private def fuzzParametricSignatures(
      fnName: String,
      inputSignaturesParams: Seq[InputTypesWithFuzzingValues],
      inputSignaturesArgs: Seq[InputTypesWithFuzzingValues],
      sourceTable: Option[String] = None
  )(using client: CHClient, ec: ExecutionContext): Future[Seq[(InputTypes, InputTypes, OutputType)]] =
    executeInParallelOnlySuccess(
      crossJoin(inputSignaturesParams, inputSignaturesArgs),
      (inputParams, inputArgs) =>
        val queries =
          crossJoin(
            buildFuzzingValuesArgs(inputParams.map(_._2)),
            buildFuzzingValuesArgs(inputArgs.map(_._2))
          ).map((params, args) =>
            s"SELECT toTypeName($fnName($params)($args))${sourceTable.map(" FROM " + _).getOrElse("")}"
          )

        executeInSequenceOnlySuccess(queries, client.execute(_).map(_.data.head.head.asInstanceOf[String])).map(
          outputTypes =>
            (
              inputParams.map(_._1),
              inputArgs.map(_._1),
              outputTypes.map(CHType.getByName).reduce(Fuzzer.mergeOutputType)
            )
        )
      ,
      maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
    )

  private type InputTypesWithFuzzingValues = Seq[(CHType, Seq[String])]
  private type InputTypes = Seq[CHType]
  private type OutputType = CHType
}
