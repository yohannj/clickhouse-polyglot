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
                arg1 = CHFuzzableType.StringType, // Path to model
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
                  case _                     => throw Exception(s"Expected 3 arguments, but found ${io._1.size} arguments")
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
                  case _                           => throw Exception(s"Expected 4 arguments, but found ${io._1.size} arguments")
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
                  case _                                 => throw Exception(s"Expected 5 arguments, but found ${io._1.size} arguments")
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
      case "geoDistance" | "greatCircleAngle" | "greatCircleDistance" =>
        val fuzzType = CHFuzzableAbstractType.Numbers.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))
        for functions4 <-
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
            function4s = functions4
          )
        }
      case "geohashesInBox" =>
        val fuzzType = CHFuzzableAbstractType.Numbers.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))
        for functions5 <-
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
            function5s = functions5
          )
        }
      case "hasColumnInTable" =>
        for
          functions3 <-
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

          functions4 <-
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

          functions5 <-
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

          functions6 <-
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
            function3s = functions3,
            function4s = functions4,
            function5s = functions5,
            function6s = functions6
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
      //   functions6 <-
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

      //   functions7 <-
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
      //     function6s = functions6,
      //     function7s = functions7
      //   )
      // }
      case "makeDateTime64" =>
        // Actually ends up in OOM
        Future.successful(fn)

      // val fuzzType = CHFuzzableAbstractType.Numbers.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))

      // for
      //   functions6 <-
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

      //   functions7 <-
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

      //   functions8 <-
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

      //   functions9 <-
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
      //     function6s = functions6,
      //     function7s = functions7,
      //     function8s = functions8,
      //     function9s = functions9
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
