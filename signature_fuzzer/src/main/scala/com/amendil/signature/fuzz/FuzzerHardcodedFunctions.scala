package com.amendil.signature.fuzz

import com.amendil.common.Settings as CommonSettings
import com.amendil.common.entities.*
import com.amendil.common.entities.`type`.*
import com.amendil.common.entities.function.{CHFunction, CHFunctionIO}
import com.amendil.common.entities.function.CHFunctionIO.*
import com.amendil.common.helper.*
import com.amendil.common.helper.ConcurrencyUtils.*
import com.amendil.common.http.CHClient
import com.amendil.signature.Settings
import com.amendil.signature.entities.*
import com.amendil.signature.fuzz.Fuzzer.*
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object FuzzerHardcodedFunctions extends StrictLogging {

  private[fuzz] def fuzz(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    fn.name match
      case "CAST" | "_CAST" | "accurateCast" | "accurateCastOrNull" => fuzzCastLike(fn)
      case "accurateCastOrDefault"                                  => fuzzAccurateCastOrDefault(fn)
      case "anova" | "analysisOfVariance"                           => fuzzAnova(fn)
      case "arrayFold"                                              => fuzzArrayFold(fn)
      case "arrayEnumerateDenseRanked" | "arrayEnumerateUniqRanked" => fuzzArrayEnumerateRanked(fn)
      case "arrayFlatten"                                           => fuzzArrayFlatten(fn)
      case "arrayReduce"                                            => fuzzArrayReduce(fn)
      case "arrayReduceInRanges"                                    => fuzzArrayReduceInRanges(fn)
      case "arrayZip"                                               => fuzzArrayZip(fn)
      case "catboostEvaluate"                                       => fuzzCatboostEvaluate(fn)
      case "DATE" | "toDate" | "toDate32"                           => fuzzToDateLike(fn)
      case "defaultValueOfTypeName"                                 => fuzzDefaultValueOfTypeName(fn)
      case "dictGet" | "dictGetAll" | "dictGetOrNull"               => fuzzDictGetLike(fn)
      case "dictGetOrDefault"                                       => fuzzDictGetOrDefault(fn)
      case "dictGetDate" | "dictGetDateTime" | "dictGetFloat32" | "dictGetFloat64" | "dictGetIPv4" | "dictGetIPv6" |
          "dictGetInt16" | "dictGetInt32" | "dictGetInt64" | "dictGetInt8" | "dictGetString" | "dictGetUInt16" |
          "dictGetUInt32" | "dictGetUInt64" | "dictGetUInt8" | "dictGetUUID" =>
        fuzzDictGetSpecificType(fn)
      case "dictGetDateOrDefault" | "dictGetDateTimeOrDefault" | "dictGetFloat32OrDefault" | "dictGetFloat64OrDefault" |
          "dictGetIPv4OrDefault" | "dictGetIPv6OrDefault" | "dictGetInt16OrDefault" | "dictGetInt32OrDefault" |
          "dictGetInt64OrDefault" | "dictGetInt8OrDefault" | "dictGetStringOrDefault" | "dictGetUInt16OrDefault" |
          "dictGetUInt32OrDefault" | "dictGetUInt64OrDefault" | "dictGetUInt8OrDefault" | "dictGetUUIDOrDefault" =>
        fuzzDictGetSpecificTypeOrDefault(fn)
      case "dictHas"                                                                        => fuzzDictHas(fn)
      case "dictGetChildren" | "dictGetHierarchy"                                           => fuzzDictGetHierarchyLike(fn)
      case "dictGetDescendants"                                                             => fuzzDictGetDescendants(fn)
      case "dictIsIn"                                                                       => fuzzDictIsIn(fn)
      case "dynamicElement"                                                                 => fuzzDynamicElement(fn)
      case "encrypt" | "aes_encrypt_mysql" | "decrypt" | "tryDecrypt" | "aes_decrypt_mysql" => fuzzEncryptDecrypt(fn)
      case "extractKeyValuePairs" | "extractKeyValuePairsWithEscaping" | "mapFromString" | "str_to_map" =>
        fuzzExtractKeyValuePairsLike(fn)
      case "geoDistance" | "greatCircleAngle" | "greatCircleDistance" => fuzzGeoDistanceLike(fn)
      case "geohashesInBox"                                           => fuzzGeoHashesInBox(fn)
      case "groupArrayInsertAt"                                       => fuzzGroupArrayInsertAt(fn)
      case "h3PointDistKm" | "h3PointDistM" | "h3PointDistRads"       => fuzzH3Dist(fn)
      case "hasColumnInTable"                                         => fuzzHasColumnInTable(fn)
      case "kolmogorovSmirnovTest"                                    => fuzzKolmogorovSmirnovTest(fn)
      case "makeDateTime"                                             => fuzzMakeDateTime(fn)
      case "makeDateTime64"                                           => fuzzMakeDateTime64(fn)
      case "mannWhitneyUTest"                                         => fuzzMannWhitneyUTest(fn)
      case "map"                                                      => fuzzMap(fn)
      case "mapApply" /* Any Map working with identity actually */    => fuzzMapApply(fn)
      case "meanZTest"                                                => fuzzMeanZTest(fn)
      case "minSampleSizeContinous" | "minSampleSizeContinuous"       => fuzzMinSampleSizeContinous(fn)
      case "minSampleSizeConversion"                                  => fuzzMinSampleSizeConversion(fn)
      case "nested"                                                   => fuzzNested(fn)
      case "pointInEllipses"                                          => fuzzPointInEllipses(fn)
      case "proportionsZTest"                                         => fuzzProportionsZTest(fn)
      case "rankCorr"                                                 => fuzzRankCorr(fn)
      case "reinterpret"                                              => fuzzReinterpret(fn)
      case "s2CapUnion" | "s2RectIntersection" | "s2RectUnion"        => fuzzS2CapUnionLike(fn)
      case "sequenceCount" | "sequenceMatch"                          => fuzzSequenceCountLike(fn)
      case "sequenceNextNode"                                         => fuzzSequenceNextNode(fn)
      case "seriesOutliersDetectTukey"                                => fuzzSeriesOutliersDetectTukey(fn)
      case "toModifiedJulianDay"                                      => fuzzToModifiedJulianDay(fn)
      case "timeSlots"                                                => fuzzTimeSlots(fn)
      case "tupleElement"                                             => fuzzTupleElement(fn)
      case "variantElement"                                           => fuzzVariantElement(fn)
      case "variantType"                                              => fuzzVariantType(fn)
      case "widthBucket" | "width_bucket"                             => fuzzWidthBucket(fn)
      case _                                                          => Future.successful(fn)

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
          outputTypes =>
            (inputTypes.map(_._1), outputTypes.map(CHTypeParser.getByName).reduce(CHTypeMerger.mergeOutputType))
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
              outputTypes.map(CHTypeParser.getByName).reduce(CHTypeMerger.mergeOutputType)
            )
        )
      ,
      maxConcurrency = Settings.ClickHouse.maxSupportedConcurrency
    )

  private type InputTypesWithFuzzingValues = Seq[(CHType, Seq[String])]
  private type InputTypes = Seq[CHType]
  private type OutputType = CHType

  private def fuzzCastLike(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    // Cast methods can return any kind of type, depending on the value of a ClickHouseType.
    // This is an edge case not automatically handled by this fuzzer, and it is not worth it to handle it.
    for
      isValidSignature <- client
        .executeNoResult(s"SELECT toTypeName(${fn.name}(1, 'UInt32'))")
        .map(_ => true)
        .recover(_ => false)
      sampleIO = Function2(CHFuzzableType.UInt8, CHFuzzableType.ClickHouseType, CHAggregatedType.Any)
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      if !isValidSignature then
        logger.error(s"Unexpected result for hardcoded function ${fn.name}, it may not exists anymore. Skipping it.")
        fn
      else
        val modes =
          if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
          else Set(CHFunction.Mode.NoOverWindow)

        fn.copy(
          modes = fn.modes ++ modes,
          settings = settings,
          function2s =
            Seq(CHFunctionIO.Function2(CHAggregatedType.Any, CHFuzzableType.ClickHouseType, CHAggregatedType.Any))
        )

  private def fuzzAccurateCastOrDefault(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    // Cast methods can return any kind of type, depending on the value of a ClickHouseType.
    // This is an edge case not automatically handled by this fuzzer, and it is not worth it to handle it.
    for
      isValidSignature <-
        client
          .executeNoResult(s"SELECT toTypeName(${fn.name}(1, 'UInt32'))")
          .flatMap(_ => client.executeNoResult(s"SELECT toTypeName(${fn.name}(1, 'UInt32', 2::UInt32))"))
          .map(_ => true)
          .recover(_ => false)

      sampleIO = Function2(CHFuzzableType.UInt8, CHFuzzableType.ClickHouseType, CHAggregatedType.Any)
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      if !isValidSignature then
        logger.error(
          s"Unexpected result for hardcoded function ${fn.name}, it may not exists anymore. Skipping it."
        )
        fn
      else
        val modes =
          if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
          else Set(CHFunction.Mode.NoOverWindow)

        fn.copy(
          modes = fn.modes ++ modes,
          settings = settings,
          function2s =
            Seq(CHFunctionIO.Function2(CHAggregatedType.Any, CHFuzzableType.ClickHouseType, CHAggregatedType.Any)),
          function3s = Seq(
            CHFunctionIO.Function3(
              CHAggregatedType.Any,
              CHFuzzableType.ClickHouseType,
              CHSpecialType.GenericType("T1", CHAggregatedType.Any),
              CHSpecialType.GenericType("T1", CHAggregatedType.Any)
            )
          )
        )

  private def fuzzAnova(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val valTypes = CHFuzzableAbstractType.Number.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))
    val groupIndexTypes = Seq(
      (CHFuzzableType.UInt8, Seq("(number % 2)::UInt8")),
      (CHFuzzableType.UInt16, Seq("(number % 2)::UInt16")),
      (CHFuzzableType.UInt32, Seq("(number % 2)::UInt32")),
      (CHFuzzableType.UInt64, Seq("(number % 2)::UInt64")),
      (CHFuzzableType.UInt128, Seq("(number % 2)::UInt128")),
      (CHFuzzableType.UInt256, Seq("(number % 2)::UInt256")),
      (CHFuzzableType.Int8, Seq("(number % 2)::Int8")),
      (CHFuzzableType.Int16, Seq("(number % 2)::Int16")),
      (CHFuzzableType.Int32, Seq("(number % 2)::Int32")),
      (CHFuzzableType.Int64, Seq("(number % 2)::Int64")),
      (CHFuzzableType.Int128, Seq("(number % 2)::Int128")),
      (CHFuzzableType.Int256, Seq("(number % 2)::Int256")),
      (CHFuzzableType.Decimal32, Seq("(number % 2)::Decimal32(1)")),
      (CHFuzzableType.Decimal64, Seq("(number % 2)::Decimal64(1)")),
      (CHFuzzableType.Decimal128, Seq("(number % 2)::Decimal128(1)")),
      (CHFuzzableType.Decimal256, Seq("(number % 2)::Decimal256(1)"))
    )

    for
      function2s <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            valTypes,
            groupIndexTypes
          ).map(_.toList),
          sourceTable = Some("(SELECT arrayJoin([1, 2, 10000000000000000001]) as number)")
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(arg1, arg2) => Function2(arg1, arg2, io._2)
              case _               => throw Exception(s"Expected 2 arguments, but found ${io._1.size} arguments")
          }
        }

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(
        fn.name,
        args = "0, (number % 2)::UInt8",
        sourceTable = Some("(SELECT arrayJoin([1, 2, 10000000000000000001]) as number)")
      )
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(
        fn.name,
        args = "0, (number % 2)::UInt8",
        fuzzOverWindow = false,
        sourceTable = Some("(SELECT arrayJoin([1, 2, 10000000000000000001]) as number)")
      )
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function2s = function2s
      )

  private def fuzzArrayFold(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    Future.successful(
      fn.copy(
        modes = fn.modes + CHFunction.Mode.NoOverWindow,
        lambdaArrayFunction1N1Opt = Some(
          LambdaArrayFunction1N1(
            CHSpecialType.LambdaType(CHSpecialType.GenericType("T1", CHAggregatedType.Any)),
            CHSpecialType.Array(CHAggregatedType.Any),
            CHSpecialType.Array(CHAggregatedType.Any),
            CHSpecialType.GenericType("T1", CHAggregatedType.Any),
            CHSpecialType.GenericType("T1", CHAggregatedType.Any)
          )
        )
      )
    )

  private def fuzzArrayEnumerateRanked(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val anyNonArrayTypes = CHFuzzableAbstractType.nonCustomFuzzableTypes
      .filterNot(_.isArrayType)
      .map(t => (t, t.fuzzingValues))

    val arrTypes = Seq((CHFuzzableType.ArrayInt8, Seq("[1]")))

    for
      isValidSignature <-
        client
          .executeNoResult(s"SELECT toTypeName(${fn.name}([['foo', 'bar'], ['baz', 'bar']], [1, 1]))")
          .flatMap(_ =>
            client.executeNoResult(s"SELECT toTypeName(${fn.name}(1, [['foo', 'bar'], ['baz', 'bar']], [1, 1]))")
          )
          .flatMap(_ =>
            client.executeNoResult(s"SELECT toTypeName(${fn.name}([['foo', 'bar'], ['baz', 'bar']], [1, 1], 1))")
          )
          .flatMap(_ =>
            client.executeNoResult(s"SELECT toTypeName(${fn.name}(1, [['foo', 'bar'], ['baz', 'bar']], [1, 1], 1))")
          )
          .map(_ => true)
          .recover(_ => false)

      sampleIO = Function0N(CHFuzzableType.ArrayString, CHAggregatedType.Any)
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)

      clearDepthTypes <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            anyNonArrayTypes,
            arrTypes
          ).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(arg1, _) => arg1
              case _            => throw Exception(s"Expected 2 arguments, but found ${io._1.size} arguments")
          }
        }

      maxArrayDepthTypes <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            arrTypes,
            anyNonArrayTypes
          ).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(_, arg2) => arg2
              case _            => throw Exception(s"Expected 2 arguments, but found ${io._1.size} arguments")
          }
        }
    yield
      if !isValidSignature then
        logger.error(
          s"Unexpected result for hardcoded function ${fn.name}, it may not exists anymore. Skipping it."
        )
        fn
      else
        val modes =
          if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
          else Set(CHFunction.Mode.NoOverWindow)

        fn.copy(
          modes = fn.modes ++ modes,
          settings = settings,
          function0Ns = Seq(
            CHFunctionIO
              .Function0N(CHSpecialType.Array(CHAggregatedType.Any), CHSpecialType.Array(CHFuzzableType.UInt32))
          ),
          function1Ns = clearDepthTypes.map(clearDepthType =>
            CHFunctionIO.Function1N(
              clearDepthType,
              CHSpecialType.Array(CHAggregatedType.Any),
              CHSpecialType.Array(CHFuzzableType.UInt32)
            )
          ),
          function0N1s = maxArrayDepthTypes.map(maxArrayDepthType =>
            CHFunctionIO.Function0N1(
              CHSpecialType.Array(CHAggregatedType.Any),
              maxArrayDepthType,
              CHSpecialType.Array(CHFuzzableType.UInt32)
            )
          ),
          function1N1s = crossJoin(clearDepthTypes, maxArrayDepthTypes).map((clearDepthType, maxArrayDepthType) =>
            CHFunctionIO.Function1N1(
              clearDepthType,
              CHSpecialType.Array(CHAggregatedType.Any),
              maxArrayDepthType,
              CHSpecialType.Array(CHFuzzableType.UInt32)
            )
          )
        )

  private def fuzzArrayFlatten(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    for
      isValidSignature <-
        client
          .execute(
            s"SELECT toTypeName(arrayFlatten([[[[[1, 2, 3]]]], [[[[2, 3, 4]]]]]::Array(Array(Array(Array(Array(UInt8)))))))"
          )
          .map { res =>
            res.data.head.head == "Array(UInt8)"
          }
          .recover(_ => false)

      sampleIO = Function1(CHFuzzableType.ArrayString, CHAggregatedType.Any)
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      if !isValidSignature then
        logger.error(
          s"Unexpected result for hardcoded function ${fn.name}, it may not exists anymore. Skipping it."
        )
        fn
      else
        val modes =
          if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
          else Set(CHFunction.Mode.NoOverWindow)

        fn.copy(
          modes = fn.modes ++ modes,
          settings = settings,
          function1s = Seq(
            CHFunctionIO.Function1(
              CHSpecialType.Array(CHAggregatedType.Any),
              CHSpecialType.Array(CHAggregatedType.Any)
            )
          )
        )

  private def fuzzArrayReduce(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val aggFnNameType =
      Seq(
        (CHFuzzableType.StringType, Seq("'sum'", "'SumIf'")),
        (CHFuzzableType.FixedString, Seq("'sum'::FixedString(3)", "'SumIf'::FixedString(5)"))
      )
    val arrType = Seq((CHSpecialType.Array(CHAggregatedType.Any), Seq("[3, 5]", "[1, 0]")))
    for
      function1Ns <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            aggFnNameType,
            arrType
          ).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(arg1, argN) => Function1N(arg1, argN, CHAggregatedType.Any)
              case _               => throw Exception(s"Expected 2 arguments, but found ${io._1.size} arguments")
          }
        }

      isRepeatedConfirmation <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            aggFnNameType,
            arrType,
            arrType
          ).map(_.toList)
        ).map(_.nonEmpty)

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(fn.name, args = "'sum', [3, 5]")
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(fn.name, args = "'sum', [3, 5]", fuzzOverWindow = false)
    yield
      if !isRepeatedConfirmation then
        logger.error(s"Unexpected result for hardcoded function ${fn.name}. Skipping it.")
        fn
      else
        val modes =
          if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
          else Set(CHFunction.Mode.NoOverWindow)

        fn.copy(
          modes = fn.modes ++ modes,
          settings = settings,
          function1Ns = function1Ns
        )

  private def fuzzArrayReduceInRanges(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val aggFnNameType =
      Seq(
        (CHFuzzableType.StringType, Seq("'sum'", "'SumIf'")),
        (CHFuzzableType.FixedString, Seq("'sum'::FixedString(3)", "'SumIf'::FixedString(5)"))
      )
    val rangesType =
      Seq(
        (
          CHSpecialType.Array(
            CHSpecialType.Tuple(
              Seq(CHAggregatedType.NonDecimalNorFloatMax64Bits, CHAggregatedType.NonDecimalNorFloatMax64Bits)
            )
          ),
          Seq("[(3, 5), (1, 0)]")
        )
      )
    val arrType = Seq((CHSpecialType.Array(CHAggregatedType.Any), Seq("[3, 5]", "[1, 0]")))
    for
      function2Ns <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            aggFnNameType,
            rangesType,
            arrType
          ).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(arg1, arg2, argN) => Function2N(arg1, arg2, argN, CHAggregatedType.Any)
              case _                     => throw Exception(s"Expected 3 arguments, but found ${io._1.size} arguments")
          }
        }

      isRepeatedConfirmation <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            aggFnNameType,
            rangesType,
            arrType,
            arrType
          ).map(_.toList)
        ).map(_.nonEmpty)

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(fn.name, args = "'sum', [(3, 5), (1, 0)], [3, 5]")
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(
        fn.name,
        args = "'sum', [(3, 5), (1, 0)], [3, 5]",
        fuzzOverWindow = false
      )
    yield
      if !isRepeatedConfirmation then
        logger.error(s"Unexpected result for hardcoded function ${fn.name}. Skipping it.")
        fn
      else
        val modes =
          if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
          else Set(CHFunction.Mode.NoOverWindow)

        fn.copy(
          modes = fn.modes ++ modes,
          settings = settings,
          function2Ns = function2Ns
        )

  private def fuzzArrayZip(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    for
      isValidSignature <-
        client
          .execute(s"SELECT toTypeName(${fn.name}([1::UInt8], [today()::Date], ['116.106.34.242'::IPv4]))")
          .map { res =>
            res.data.head.head == "Array(Tuple(UInt8, Date, IPv4))"
          }
          .recover(_ => false)

      sampleIO = Function1(CHFuzzableType.ArrayString, CHAggregatedType.Any)
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      if !isValidSignature then
        logger.error(
          s"Unexpected result for hardcoded function ${fn.name}, it may not exists anymore. Skipping it."
        )
        fn
      else
        val modes =
          if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
          else Set(CHFunction.Mode.NoOverWindow)
        val genericTypes = Range(1, 10).map(i => CHSpecialType.GenericType(s"T$i", CHAggregatedType.Any))

        fn.copy(
          modes = fn.modes ++ modes,
          settings = settings,
          function1s = Seq(
            CHFunctionIO.Function1(
              CHSpecialType.Array(genericTypes(0)),
              CHSpecialType.Array(CHSpecialType.Tuple(genericTypes.take(1)))
            )
          ),
          function2s = Seq(
            CHFunctionIO.Function2(
              CHSpecialType.Array(genericTypes(0)),
              CHSpecialType.Array(genericTypes(1)),
              CHSpecialType.Array(CHSpecialType.Tuple(genericTypes.take(2)))
            )
          ),
          function3s = Seq(
            CHFunctionIO.Function3(
              CHSpecialType.Array(genericTypes(0)),
              CHSpecialType.Array(genericTypes(1)),
              CHSpecialType.Array(genericTypes(2)),
              CHSpecialType.Array(CHSpecialType.Tuple(genericTypes.take(3)))
            )
          ),
          function4s = Seq(
            CHFunctionIO.Function4(
              CHSpecialType.Array(genericTypes(0)),
              CHSpecialType.Array(genericTypes(1)),
              CHSpecialType.Array(genericTypes(2)),
              CHSpecialType.Array(genericTypes(3)),
              CHSpecialType.Array(CHSpecialType.Tuple(genericTypes.take(4)))
            )
          ),
          function5s = Seq(
            CHFunctionIO.Function5(
              CHSpecialType.Array(genericTypes(0)),
              CHSpecialType.Array(genericTypes(1)),
              CHSpecialType.Array(genericTypes(2)),
              CHSpecialType.Array(genericTypes(3)),
              CHSpecialType.Array(genericTypes(4)),
              CHSpecialType.Array(CHSpecialType.Tuple(genericTypes.take(5)))
            )
          ),
          function6s = Seq(
            CHFunctionIO.Function6(
              CHSpecialType.Array(genericTypes(0)),
              CHSpecialType.Array(genericTypes(1)),
              CHSpecialType.Array(genericTypes(2)),
              CHSpecialType.Array(genericTypes(3)),
              CHSpecialType.Array(genericTypes(4)),
              CHSpecialType.Array(genericTypes(5)),
              CHSpecialType.Array(CHSpecialType.Tuple(genericTypes.take(6)))
            )
          ),
          function7s = Seq(
            CHFunctionIO.Function7(
              CHSpecialType.Array(genericTypes(0)),
              CHSpecialType.Array(genericTypes(1)),
              CHSpecialType.Array(genericTypes(2)),
              CHSpecialType.Array(genericTypes(3)),
              CHSpecialType.Array(genericTypes(4)),
              CHSpecialType.Array(genericTypes(5)),
              CHSpecialType.Array(genericTypes(6)),
              CHSpecialType.Array(CHSpecialType.Tuple(genericTypes.take(7)))
            )
          ),
          function8s = Seq(
            CHFunctionIO.Function8(
              CHSpecialType.Array(genericTypes(0)),
              CHSpecialType.Array(genericTypes(1)),
              CHSpecialType.Array(genericTypes(2)),
              CHSpecialType.Array(genericTypes(3)),
              CHSpecialType.Array(genericTypes(4)),
              CHSpecialType.Array(genericTypes(5)),
              CHSpecialType.Array(genericTypes(6)),
              CHSpecialType.Array(genericTypes(7)),
              CHSpecialType.Array(CHSpecialType.Tuple(genericTypes.take(8)))
            )
          ),
          function9s = Seq(
            CHFunctionIO.Function9(
              CHSpecialType.Array(genericTypes(0)),
              CHSpecialType.Array(genericTypes(1)),
              CHSpecialType.Array(genericTypes(2)),
              CHSpecialType.Array(genericTypes(3)),
              CHSpecialType.Array(genericTypes(4)),
              CHSpecialType.Array(genericTypes(5)),
              CHSpecialType.Array(genericTypes(6)),
              CHSpecialType.Array(genericTypes(7)),
              CHSpecialType.Array(genericTypes(8)),
              CHSpecialType.Array(CHSpecialType.Tuple(genericTypes.take(9)))
            )
          )
        )

  private def fuzzCatboostEvaluate(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    // Number and type of arguments depend on the model, which we don't know at compile time.
    Future.successful(
      fn.copy(
        modes = fn.modes ++ Set(CHFunction.Mode.NoOverWindow),
        function1Ns = Seq(
          CHFunctionIO.Function1N(
            arg1 = CHFuzzableType.SpecialString, // Path to model
            argN = CHSpecialType.CatboostParameter,
            output = CHFuzzableType.Float64
          )
        )
      )
    )

  private def fuzzToDateLike(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    // Second argument (TimeZone) is sometimes accepted as any String, and not check is done to verify the String is a TimeZone.
    // In the automatic signature detectionm it leads us to incorrectly believe the second argument is a String and not a TimeZone.

    val anyType = CHFuzzableAbstractType.nonCustomFuzzableTypes
      .map(t => (t, t.fuzzingValues))

    val stringType = Seq((CHFuzzableType.StringType, Seq("''")))
    val timeZoneType = Seq((CHFuzzableType.TimeZone, Seq(CHFuzzableType.TimeZone.fuzzingValues.head)))

    for
      function1s <-
        fuzzSignatures(
          fn.name,
          anyType.map(Seq(_))
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(arg1) => Function1(arg1, io._2)
              case _         => throw Exception(s"Expected 1 argument, but found ${io._1.size} arguments")
          }
        }

      typesThatDiscardTimezoneArgument <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            anyType,
            stringType
          ).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(arg1, _) => arg1
              case _            => throw Exception(s"Expected 2 arguments, but found ${io._1.size} arguments")
          }
        }

      function2s <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            anyType,
            timeZoneType
          ).map(_.toList)
        ).map { signatures =>
          signatures
            .map { io =>
              io._1 match
                case Seq(arg1, arg2) => Function2(arg1, arg2, io._2)
                case _               => throw Exception(s"Expected 2 arguments, but found ${io._1.size} arguments")
            }
            .filterNot(f => typesThatDiscardTimezoneArgument.contains(f.arg1))
        }

      sampleIO = function1s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function1s = function1s,
        function2s = function2s
      )

  private def fuzzDefaultValueOfTypeName(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    executeInSequenceUntilSuccess(
      CHFuzzableType.ClickHouseType.fuzzingValues,
      v => client.executeNoResult(s"SELECT toTypeName(${fn.name}($v))")
    ).map(_ =>
      fn.copy(
        modes = fn.modes + CHFunction.Mode.NoOverWindow,
        function1s = Seq(Function1(CHFuzzableType.ClickHouseType, CHAggregatedType.Any))
      )
    )

  private def fuzzDictGetLike(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
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
    val idExprType = CHFuzzableAbstractType.nonCustomFuzzableTypes
      .map(t => (t, t.fuzzingValues))
    for
      function3s <-
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

      attributeNamesTypes = function3s.map(_.arg2).distinct

      supportOverWindow <- Fuzzer
        .testSampleInputWithOverWindow(
          fn.name,
          args = s"'${CommonSettings.Type.FuzzerDictionaryNames.manyTypesDictionaryName}', 'dateValue', 1"
        )
        .flatMap(supportOverWindow =>
          if supportOverWindow then Future.successful(supportOverWindow)
          else
            Fuzzer.testSampleInputWithOverWindow(
              fn.name,
              args = s"'${CommonSettings.Type.FuzzerDictionaryNames.regexpDictionaryName}', 'name', 1"
            )
        )
      settings <- Fuzzer
        .detectMandatorySettingsFromSampleInput(
          fn.name,
          args = s"'${CommonSettings.Type.FuzzerDictionaryNames.manyTypesDictionaryName}', 'dateValue', 1",
          fuzzOverWindow = false
        )
        .flatMap(settings =>
          if settings.size <= 1 then Future.successful(settings)
          else
            Fuzzer.detectMandatorySettingsFromSampleInput(
              fn.name,
              args = s"'${CommonSettings.Type.FuzzerDictionaryNames.regexpDictionaryName}', 'name', 1",
              fuzzOverWindow = false
            )
        )
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)
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
        modes = fn.modes ++ modes,
        settings = settings,
        function3s = function3s
      )

  private def fuzzDictGetOrDefault(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
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
    val idExprType = CHFuzzableAbstractType.nonCustomFuzzableTypes
      .map(t => (t, t.fuzzingValues))
    val defaultValueType =
      CHFuzzableAbstractType.nonCustomFuzzableAbstractTypes
        .flatMap(_.chFuzzableTypes)
        .map(t => (t, t.fuzzingValues))

    for
      dictGetFunction3s <-
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
              case Seq(arg1, arg2, arg3) => Function3(arg1, arg2, arg3, io._2)
              case _                     => throw Exception(s"Expected 3 arguments, but found ${io._1.size} arguments")
          }
        }

      attributeNamesTypes = dictGetFunction3s.map(_.arg2).distinct

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(
        fn.name,
        args = s"'${CommonSettings.Type.FuzzerDictionaryNames.manyTypesDictionaryName}', 'dateValue', 1, today()"
      )
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(
        fn.name,
        args = s"'${CommonSettings.Type.FuzzerDictionaryNames.manyTypesDictionaryName}', 'dateValue', 1, today()",
        fuzzOverWindow = false
      )
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)
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
        modes = fn.modes ++ modes,
        settings = settings,
        function4s = function4s
      )

  private def fuzzDictGetSpecificType(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
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
    val idExprType = CHFuzzableAbstractType.nonCustomFuzzableTypes
      .map(t => (t, t.fuzzingValues))
    for
      function3s <-
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

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(
        "dictGetDate",
        args = s"'${CommonSettings.Type.FuzzerDictionaryNames.manyTypesDictionaryName}', 'dateValue', 1"
      )
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(
        "dictGetDate",
        args = s"'${CommonSettings.Type.FuzzerDictionaryNames.manyTypesDictionaryName}', 'dateValue', 1",
        fuzzOverWindow = false
      )
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function3s = function3s
      )

  private def fuzzDictGetSpecificTypeOrDefault(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
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
    val idExprType = CHFuzzableAbstractType.nonCustomFuzzableTypes
      .map(t => (t, t.fuzzingValues))
    val defaultValueType =
      CHFuzzableAbstractType.nonCustomFuzzableAbstractTypes
        .flatMap(_.chFuzzableTypes)
        .map(t => (t, t.fuzzingValues))

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

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(
        "dictGetDateOrDefault",
        args = s"'${CommonSettings.Type.FuzzerDictionaryNames.manyTypesDictionaryName}', 'dateValue', 1, today()"
      )
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(
        "dictGetDateOrDefault",
        args = s"'${CommonSettings.Type.FuzzerDictionaryNames.manyTypesDictionaryName}', 'dateValue', 1, today()",
        fuzzOverWindow = false
      )
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function4s = function4s
      )

  private def fuzzDictHas(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val dictionaryNameType = Seq((CHFuzzableType.DictionaryName, CHFuzzableType.DictionaryName.fuzzingValues))
    val idExprType = CHFuzzableAbstractType.nonCustomFuzzableTypes
      .map(t => (t, t.fuzzingValues))
    for
      function2s <-
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

      outputTypes = function2s.map(_.output)

      sampleIO = function2s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      if outputTypes.nonEmpty then
        fn.copy(
          modes = fn.modes ++ modes,
          settings = settings,
          function2s = Seq(
            Function2(
              CHFuzzableType.DictionaryName,
              CHAggregatedType.Any,
              outputTypes.reduce(CHTypeMerger.mergeOutputType)
            )
          )
        )
      else fn

  private def fuzzDictGetHierarchyLike(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val dictionaryNameType = Seq((CHFuzzableType.DictionaryName, CHFuzzableType.DictionaryName.fuzzingValues))
    val idExprType = CHFuzzableAbstractType.nonCustomFuzzableTypes
      .map(t => (t, t.fuzzingValues))
    for
      function2s <-
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

      sampleIO = function2s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function2s = function2s
      )

  private def fuzzDictGetDescendants(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val dictionaryNameType = Seq((CHFuzzableType.DictionaryName, CHFuzzableType.DictionaryName.fuzzingValues))
    val idExprType = CHFuzzableAbstractType.nonCustomFuzzableTypes
      .map(t => (t, t.fuzzingValues))
    for
      function2s <-
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

      sampleIO = function2s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)
      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function2s = function2s
      )

  private def fuzzDictIsIn(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val dictionaryNameType = Seq((CHFuzzableType.DictionaryName, CHFuzzableType.DictionaryName.fuzzingValues))
    val idExprType = CHFuzzableAbstractType.nonCustomFuzzableTypes
      .map(t => (t, t.fuzzingValues))
    for
      function3s <-
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

      sampleIO = function3s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function3s = function3s
      )

  private def fuzzDynamicElement(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val dynamicType = (CHFuzzableType.Dynamic, CHFuzzableType.Dynamic.fuzzingValues)
    val chTypeType = (CHFuzzableType.ClickHouseType, CHFuzzableType.ClickHouseType.fuzzingValues)

    for
      function2s <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            Seq(dynamicType),
            Seq(chTypeType)
          ).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(arg1, arg2) if io._2 == CHAggregatedType.Any => Function2(arg1, arg2, CHAggregatedType.Any)
              case Seq(_, _)                                        => throw Exception(s"Expected output type to be Nullable, but found ${io._2.name}")
              case _                                                => throw Exception(s"Expected 2 arguments, but found ${io._1.size} arguments")
          }
        }

      sampleIO = function2s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function2s = function2s
      )

  private def fuzzEncryptDecrypt(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
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

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(
        fn.name,
        args = s"'aes-256-ofb', unhex('24E9E4966469'), '12345678910121314151617181920212'"
      )
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(
        fn.name,
        args = s"'aes-256-ofb', unhex('24E9E4966469'), '12345678910121314151617181920212'",
        fuzzOverWindow = false
      )
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function3s = function3s,
        function4s = function4s,
        function5s = function5s
      )

  private def fuzzExtractKeyValuePairsLike(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val strTypes = Seq(
      (CHFuzzableType.StringType, Seq("'foo'::String", "':'::String", "' ,;'::String", "'\"'::String")),
      (
        CHFuzzableType.FixedString,
        Seq("'foo'::FixedString(3)", "':'::FixedString(1)", "' ,;'::FixedString(3)", "'\"'::FixedString(1)")
      )
    )

    for
      function1s <-
        fuzzSignatures(
          fn.name,
          strTypes.map(Seq(_))
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(arg1) => Function1(arg1, io._2)
              case _         => throw Exception(s"Expected 1 argument, but found ${io._1.size} arguments")
          }
        }

      function2s <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            strTypes,
            strTypes
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
            strTypes,
            strTypes,
            strTypes
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
            strTypes,
            strTypes,
            strTypes,
            strTypes
          ).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(arg1, arg2, arg3, arg4) => Function4(arg1, arg2, arg3, arg4, io._2)
              case _                           => throw Exception(s"Expected 4 arguments, but found ${io._1.size} arguments")
          }
        }

      sampleIO = function1s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function1s = function1s,
        function2s = function2s,
        function3s = function3s,
        function4s = function4s
      )

  private def fuzzGeoDistanceLike(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val fuzzType = CHFuzzableAbstractType.Number.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))
    for
      function4s <-
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

      sampleIO = function4s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function4s = function4s
      )

  private def fuzzGeoHashesInBox(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val fuzzType = CHFuzzableAbstractType.Number.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))
    for
      function5s <-
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

      sampleIO = function5s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function5s = function5s
      )

  private def fuzzGroupArrayInsertAt(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val xType =
      CHFuzzableAbstractType.nonCustomFuzzableTypes
        .filterNot(_.name.toLowerCase().contains("enum"))
        .map(t => (t, t.fuzzingValues.take(3)))
    val posAndSizeType =
      Seq(
        (CHFuzzableType.UInt8, Seq("1::UInt8")),
        (CHFuzzableType.UInt16, Seq("256::UInt16")),
        (CHFuzzableType.UInt32, Seq("65536::UInt32"))
      )

    for
      parametric0Function2 <-
        fuzzParametricSignatures(
          fn.name,
          Seq(Nil),
          crossJoin(
            xType,
            posAndSizeType
          ).map(_.toList)
        ).map { signatures =>
          logger.trace(
            s"FuzzerHardcodedFunctions - groupArrayInsertAt - fuzzed ${signatures.size} valid signatures with 0 parameters"
          )
          signatures.map { io =>
            (io._1, io._2) match
              case (Seq(), Seq(arg1, arg2)) => Parametric0Function2(arg1, arg2, io._3)
              case _ =>
                throw Exception(
                  s"Expected 0 parameters and 2 arguments, but found ${io._1.size} parameters and ${io._2.size} arguments"
                )
          }
        }

      validPosTypes = parametric0Function2.map(_.arg2).distinct

      parametric1Function2 <-
        fuzzParametricSignatures(
          fn.name,
          xType.map(Seq(_)),
          crossJoin(
            xType,
            posAndSizeType.filter(_._1 == validPosTypes.head)
          ).map(_.toList)
        ).map { signatures =>
          logger.trace(
            s"FuzzerHardcodedFunctions - groupArrayInsertAt - fuzzed ${signatures.size} valid combinations of defaultX and x"
          )
          signatures.flatMap { io =>
            (io._1, io._2) match
              case (Seq(param1), Seq(arg1, _)) =>
                posAndSizeType.map((arg2, _) => Parametric1Function2(param1, arg1, arg2, io._3))
              case _ =>
                throw Exception(
                  s"Expected 1 parameter and 2 arguments, but found ${io._1.size} parameters and ${io._2.size} arguments"
                )
          }
        }

      validXTypeCombinations = parametric1Function2.map(f => (f.paramArg1, f.arg1)).distinct

      parametric2Function2 <-
        fuzzParametricSignatures(
          fn.name,
          crossJoin(
            xType.filter(_._1 == validXTypeCombinations.head._1),
            posAndSizeType
          ).map(_.toList),
          crossJoin(
            xType.filter(_._1 == validXTypeCombinations.head._2),
            posAndSizeType.filter(_._1 == validPosTypes.head)
          ).map(_.toList)
        ).map { signatures =>
          logger.trace(
            s"FuzzerHardcodedFunctions - groupArrayInsertAt - fuzzed ${signatures.size} valid types for size parameter"
          )
          signatures.flatMap { io =>
            (io._1, io._2) match
              case (Seq(_, param2), Seq(_, _)) =>
                crossJoin(validXTypeCombinations, posAndSizeType.map(_._1)).map { case ((param1, arg1), arg2) =>
                  Parametric2Function2(param1, param2, arg1, arg2, io._3)
                }
              case _ =>
                throw Exception(
                  s"Expected 2 parameters and 2 arguments, but found ${io._1.size} parameters and ${io._2.size} arguments"
                )
          }
        }

      sampleIO = parametric0Function2.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        parametric0Function2s = parametric0Function2,
        parametric1Function2s = parametric1Function2,
        parametric2Function2s = parametric2Function2
      )

  private def fuzzH3Dist(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val float64Type: Seq[(CHFuzzableType, Seq[String])] = Seq(
      (CHFuzzableType.Float64, CHFuzzableType.Float64.fuzzingValues)
    )
    for
      function4s <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            float64Type,
            float64Type,
            float64Type,
            float64Type
          ).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(arg1, arg2, arg3, arg4) => Function4(arg1, arg2, arg3, arg4, io._2)
              case _                           => throw Exception(s"Expected 4 arguments, but found ${io._1.size} arguments")
          }
        }

      sampleIO = function4s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function4s = function4s
      )

  private def fuzzHasColumnInTable(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
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
              case Seq(arg1, arg2, arg3) => Function3(arg1, arg2, arg3, CHFuzzableType.BooleanType)
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
              case Seq(arg1, arg2, arg3, arg4) => Function4(arg1, arg2, arg3, arg4, CHFuzzableType.BooleanType)
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
              case Seq(arg1, arg2, arg3, arg4, arg5) =>
                Function5(arg1, arg2, arg3, arg4, arg5, CHFuzzableType.BooleanType)
              case _ => throw Exception(s"Expected 5 arguments, but found ${io._1.size} arguments")
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
              case Seq(arg1, arg2, arg3, arg4, arg5, arg6) =>
                Function6(arg1, arg2, arg3, arg4, arg5, arg6, CHFuzzableType.BooleanType)
              case _ => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
          }
        }

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(fn.name, args = s"'system', 'numbers', 'number'")
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(
        fn.name,
        args = s"'system', 'numbers', 'number'",
        fuzzOverWindow = false
      )
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function3s = function3s,
        function4s = function4s,
        function5s = function5s,
        function6s = function6s
      )

  private def fuzzKolmogorovSmirnovTest(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val alternativeTypes = Seq((CHFuzzableType.TestAlternative, CHFuzzableType.TestAlternative.fuzzingValues))
    val computationMethodTypes = Seq(
      (CHFuzzableType.PValueComputationMethod, CHFuzzableType.PValueComputationMethod.fuzzingValues)
    )
    val sampleDataTypes = CHFuzzableAbstractType.Number.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))
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
          alternativeTypes.map(Seq(_)),
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

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(
        fn.name,
        paramsOpt = Some(""),
        args = "false, randNormal(0, 1)",
        sourceTable = Some("numbers(100)")
      )
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(
        fn.name,
        paramsOpt = Some(""),
        args = "false, randNormal(0, 1)",
        fuzzOverWindow = false,
        sourceTable = Some("numbers(100)")
      )
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        parametric0Function2s = parametric0Function2,
        parametric1Function2s = parametric1Function2,
        parametric2Function2s = parametric2Function2
      )

  private def fuzzMakeDateTime(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val fuzzType = CHFuzzableAbstractType.Number.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))
    val uint8Type = Seq((CHFuzzableType.UInt8, Seq(CHFuzzableType.UInt8.fuzzingValues.head)))

    for
      arg1Types <-
        fuzzSignatures(
          fn.name,
          crossJoin(fuzzType, uint8Type, uint8Type, uint8Type, uint8Type, uint8Type).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(arg1, _, _, _, _, _) => arg1
              case _                        => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
          }.toSet
        }

      arg2Types <-
        fuzzSignatures(
          fn.name,
          crossJoin(uint8Type, fuzzType, uint8Type, uint8Type, uint8Type, uint8Type).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(_, arg2, _, _, _, _) => arg2
              case _                        => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
          }.toSet
        }

      arg3Types <-
        fuzzSignatures(
          fn.name,
          crossJoin(uint8Type, uint8Type, fuzzType, uint8Type, uint8Type, uint8Type).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(_, _, arg3, _, _, _) => arg3
              case _                        => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
          }.toSet
        }

      arg4Types <-
        fuzzSignatures(
          fn.name,
          crossJoin(uint8Type, uint8Type, uint8Type, fuzzType, uint8Type, uint8Type).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(_, _, _, arg4, _, _) => arg4
              case _                        => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
          }.toSet
        }

      arg5Types <-
        fuzzSignatures(
          fn.name,
          crossJoin(uint8Type, uint8Type, uint8Type, uint8Type, fuzzType, uint8Type).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(_, _, _, _, arg5, _) => arg5
              case _                        => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
          }.toSet
        }

      arg6Types <-
        fuzzSignatures(
          fn.name,
          crossJoin(uint8Type, uint8Type, uint8Type, uint8Type, uint8Type, fuzzType).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(_, _, _, _, _, arg6) => arg6
              case _                        => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
          }.toSet
        }

      isFunction7 <- client
        .executeNoResult(
          s"SELECT toTypeName(${fn.name}(1, 2, 3, 4, 5, 6, ${CHFuzzableType.TimeZone.fuzzingValues.head}))"
        )
        .map(_ => true)
        .recover(_ => false)

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(fn.name, args = "1, 1, 1, 1, 1, 1")
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(
        fn.name,
        args = "1, 1, 1, 1, 1, 1",
        fuzzOverWindow = false
      )
    yield
      if !isFunction7 then
        logger.error(s"Unexpected result for hardcoded function ${fn.name}. Skipping it.")
        fn
      else
        val modes =
          if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
          else Set(CHFunction.Mode.NoOverWindow)

        val (function6s, function7s) =
          crossJoin(
            CHTypeMerger.mergeInputTypes(arg1Types).toSeq,
            CHTypeMerger.mergeInputTypes(arg2Types).toSeq,
            CHTypeMerger.mergeInputTypes(arg3Types).toSeq,
            CHTypeMerger.mergeInputTypes(arg4Types).toSeq,
            CHTypeMerger.mergeInputTypes(arg5Types).toSeq,
            CHTypeMerger.mergeInputTypes(arg6Types).toSeq
          ).map((arg1, arg2, arg3, arg4, arg5, arg6) =>
            (
              Function6(arg1, arg2, arg3, arg4, arg5, arg6, CHFuzzableType.DateTime),
              Function7(arg1, arg2, arg3, arg4, arg5, arg6, CHFuzzableType.TimeZone, CHFuzzableType.DateTime)
            )
          ).unzip

        fn.copy(
          modes = fn.modes ++ modes,
          settings = settings,
          function6s = function6s,
          function7s = function7s
        )

  private def fuzzMakeDateTime64(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val fuzzType = CHFuzzableAbstractType.Number.chFuzzableTypes.map(t => (t, t.fuzzingValues))
    val uint8Type = Seq((CHFuzzableType.UInt8, Seq(CHFuzzableType.UInt8.fuzzingValues.head)))

    for
      arg1Types <-
        fuzzSignatures(
          fn.name,
          crossJoin(fuzzType, uint8Type, uint8Type, uint8Type, uint8Type, uint8Type).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(arg1, _, _, _, _, _) => arg1
              case _                        => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
          }.toSet
        }

      arg2Types <-
        fuzzSignatures(
          fn.name,
          crossJoin(uint8Type, fuzzType, uint8Type, uint8Type, uint8Type, uint8Type).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(_, arg2, _, _, _, _) => arg2
              case _                        => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
          }.toSet
        }

      arg3Types <-
        fuzzSignatures(
          fn.name,
          crossJoin(uint8Type, uint8Type, fuzzType, uint8Type, uint8Type, uint8Type).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(_, _, arg3, _, _, _) => arg3
              case _                        => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
          }.toSet
        }

      arg4Types <-
        fuzzSignatures(
          fn.name,
          crossJoin(uint8Type, uint8Type, uint8Type, fuzzType, uint8Type, uint8Type).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(_, _, _, arg4, _, _) => arg4
              case _                        => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
          }.toSet
        }

      arg5Types <-
        fuzzSignatures(
          fn.name,
          crossJoin(uint8Type, uint8Type, uint8Type, uint8Type, fuzzType, uint8Type).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(_, _, _, _, arg5, _) => arg5
              case _                        => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
          }.toSet
        }

      arg6Types <-
        fuzzSignatures(
          fn.name,
          crossJoin(uint8Type, uint8Type, uint8Type, uint8Type, uint8Type, fuzzType).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(_, _, _, _, _, arg6) => arg6
              case _                        => throw Exception(s"Expected 6 arguments, but found ${io._1.size} arguments")
          }.toSet
        }

      arg7Types <-
        fuzzSignatures(
          fn.name,
          crossJoin(uint8Type, uint8Type, uint8Type, uint8Type, uint8Type, uint8Type, fuzzType).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(_, _, _, _, _, _, arg7) => arg7
              case _                           => throw Exception(s"Expected 7 arguments, but found ${io._1.size} arguments")
          }.toSet
        }

      arg8Types <-
        fuzzSignatures(
          fn.name,
          crossJoin(uint8Type, uint8Type, uint8Type, uint8Type, uint8Type, uint8Type, uint8Type, fuzzType).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(_, _, _, _, _, _, _, arg8) => arg8
              case _                              => throw Exception(s"Expected 8 arguments, but found ${io._1.size} arguments")
          }.toSet
        }

      isFunction9 <- client
        .executeNoResult(
          s"SELECT toTypeName(${fn.name}(1, 2, 3, 4, 5, 6, 7, 8, ${CHFuzzableType.TimeZone.fuzzingValues.head}))"
        )
        .map(_ => true)
        .recover(_ => false)

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(fn.name, args = "1, 1, 1, 1, 1, 1")
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(
        fn.name,
        args = "1, 1, 1, 1, 1, 1",
        fuzzOverWindow = false
      )
    yield
      if !isFunction9 then
        logger.error(s"Unexpected result for hardcoded function ${fn.name}. Skipping it.")
        fn
      else
        val modes =
          if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
          else Set(CHFunction.Mode.NoOverWindow)

        val functions =
          crossJoin(
            CHTypeMerger.mergeInputTypes(arg1Types).toSeq,
            CHTypeMerger.mergeInputTypes(arg2Types).toSeq,
            CHTypeMerger.mergeInputTypes(arg3Types).toSeq,
            CHTypeMerger.mergeInputTypes(arg4Types).toSeq,
            CHTypeMerger.mergeInputTypes(arg5Types).toSeq,
            CHTypeMerger.mergeInputTypes(arg6Types).toSeq,
            CHTypeMerger.mergeInputTypes(arg7Types).toSeq,
            CHTypeMerger.mergeInputTypes(arg8Types).toSeq
          ).map((arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) =>
            (
              Function6(
                arg1,
                arg2,
                arg3,
                arg4,
                arg5,
                arg6,
                CHFuzzableType.DateTime64
              ), // Missing arg7, arg8 so there are duplications
              Function7(
                arg1,
                arg2,
                arg3,
                arg4,
                arg5,
                arg6,
                arg7,
                CHFuzzableType.DateTime64
              ), // Missing arg8 so there are duplications
              Function8(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, CHFuzzableType.DateTime64),
              Function9(
                arg1,
                arg2,
                arg3,
                arg4,
                arg5,
                arg6,
                arg7,
                arg8,
                CHFuzzableType.TimeZone,
                CHFuzzableType.DateTime64
              )
            )
          )

        val function6s = functions.map(_._1).distinct
        val function7s = functions.map(_._2).distinct
        val function8s = functions.map(_._3)
        val function9s = functions.map(_._4)

        fn.copy(
          modes = fn.modes ++ modes,
          settings = settings,
          function6s = function6s,
          function7s = function7s,
          function8s = function8s,
          function9s = function9s
        )

  private def fuzzMannWhitneyUTest(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val alternativeTypes = Seq((CHFuzzableType.TestAlternative, CHFuzzableType.TestAlternative.fuzzingValues))
    val continuityCorrectionTypes =
      CHFuzzableAbstractType.Number.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))
    val sampleDataTypes = CHFuzzableAbstractType.Number.chFuzzableTypes.map(t =>
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
          alternativeTypes.map(Seq(_)),
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

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(
        fn.name,
        paramsOpt = Some(""),
        args = "toInt8(randNormal(0, 1)), randNormal(0, 1)",
        sourceTable = Some("numbers(100)")
      )
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(
        fn.name,
        paramsOpt = Some(""),
        args = "toInt8(randNormal(0, 1)), randNormal(0, 1)",
        sourceTable = Some("numbers(100)"),
        fuzzOverWindow = false
      )
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        parametric0Function2s = parametric0Function2,
        parametric1Function2s = parametric1Function2,
        parametric2Function2s = parametric2Function2
      )

  private def fuzzMap(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    // We should use generic types here,
    val anyType = CHFuzzableAbstractType.nonCustomFuzzableTypes
      .map(t => (t, Seq(t.fuzzingValues.head)))

    for
      function2s <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            anyType,
            anyType
          ).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(arg1, arg2) => Function2(arg1, arg2, io._2)
              case _               => throw Exception(s"Expected 2 arguments, but found ${io._1.size} arguments")
          }
        }

      sampleIO = function2s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      require(
        function2s.map(_.arg1).distinct.size * function2s.map(_.arg2).distinct.size == function2s.size,
        "Expected each map keys to support exactly the same types of values. But it's apparently not the case anymore."
      )

      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)
      val t1 = CHTypeMerger.mergeInputTypes(function2s.map(_.arg1).toSet, supportJson = Settings.Fuzzer.supportJson)
      val t2 = CHTypeMerger.mergeInputTypes(function2s.map(_.arg2).toSet, supportJson = Settings.Fuzzer.supportJson)
      require(t1.size == 1, "Map key type should be aggregatable to a single type")
      require(t2.size == 1, "Map value type should be aggregatable to a single type")

      // This signature is wrong, t1 and t2 should be generic types
      // Yet right now I don't have a good idea to express those parameters must be repeated as a duo
      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function1Ns = Seq(Function1N(t1.head, t2.head, CHSpecialType.Map(t1.head, t2.head)))
      )

  private def fuzzMapApply(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val typeT = CHSpecialType.GenericType("T1", CHAggregatedType.MapKey)
    val typeU = CHSpecialType.GenericType("T2", CHAggregatedType.Any)
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
    yield fn.copy(
      modes = fn.modes + CHFunction.Mode.NoOverWindow,
      lambdaMapFunction1Opt = lambdaMapFunction1Opt
    )

  private def fuzzMeanZTest(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val floatNumberType = CHFuzzableAbstractType.Number.chFuzzableTypes
      .filter(_.name.toLowerCase().contains("float"))
      .map(t => (t, t.fuzzingValues.take(3)))
    val numbersType = CHFuzzableAbstractType.Number.chFuzzableTypes.map(t => (t, t.fuzzingValues.take(3)))

    for
      parametric3Function2 <-
        fuzzParametricSignatures(
          fn.name,
          crossJoin(
            floatNumberType,
            floatNumberType,
            floatNumberType
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

      sampleIO = parametric3Function2.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        parametric3Function2s = parametric3Function2
      )

  private def fuzzMinSampleSizeContinous(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    for
      Seq(baselineTypes, sigmaTypes, mdeTypes, powerTypes, alphaTypes) <-
        ConcurrencyUtils.executeInSequence(
          Range(0, 5), // we are going to test the types of this method's five arguments separately
          argIdx =>
            FuzzerNonParametricFunctions
              .fuzzInputCombinations(
                fn.name,
                CHFuzzableAbstractType.Number.chFuzzableTypes
                  .map(t => Range(0, 5).map(idx => if idx == argIdx then t else CHFuzzableType.UInt8)),
                fuzzOverWindow = false,
                returnFirstOutputTypeFound = true
              )
              .map { signatures =>
                signatures.map { io =>
                  io._1 match
                    case s: Seq[CHFuzzableType] if s.size == 5 => s(argIdx)
                    case _                                     => throw Exception(s"Expected 5 arguments, but found ${io._1.size} arguments")
                }
              }
        )

      function5s <-
        FuzzerNonParametricFunctions
          .fuzzInputCombinations(
            fn.name,
            crossJoin(baselineTypes, sigmaTypes, mdeTypes, powerTypes, alphaTypes).map(_.toList),
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

      sampleIO = function5s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function5s = function5s
      )

  private def fuzzMinSampleSizeConversion(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    for
      Seq(baselineTypes, mdeTypes, powerTypes, alphaTypes) <-
        ConcurrencyUtils.executeInSequence(
          Range(0, 4), // we are going to test the types of this method's four arguments separately
          argIdx =>
            FuzzerNonParametricFunctions
              .fuzzInputCombinations(
                fn.name,
                CHFuzzableAbstractType.Number.chFuzzableTypes
                  .map(t => Range(0, 4).map(idx => if idx == argIdx then t else CHFuzzableType.Float64)),
                fuzzOverWindow = false,
                returnFirstOutputTypeFound = true
              )
              .map { signatures =>
                signatures.map { io =>
                  io._1 match
                    case s: Seq[CHFuzzableType] if s.size == 4 => s(argIdx)
                    case _                                     => throw Exception(s"Expected 4 arguments, but found ${io._1.size} arguments")
                }
              }
        )

      function4s <-
        FuzzerNonParametricFunctions
          .fuzzInputCombinations(
            fn.name,
            crossJoin(baselineTypes, mdeTypes, powerTypes, alphaTypes).map(_.toList),
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

      sampleIO = function4s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function4s = function4s
      )

  private def fuzzNested(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val stringArrayType: Seq[(CHFuzzableType, Seq[String])] = Seq(
      (CHFuzzableType.ArrayString, Seq("['foo']", "['foo', 'bar']")),
      (
        CHFuzzableType.ArrayFixedString,
        Seq("['foo'::FixedString(3)]", "['foo'::FixedString(3), 'bar'::FixedString(3)]")
      )
    )
    for
      function2s <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            stringArrayType,
            stringArrayType
          ).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(arg1, arg2) => Function2(arg1, arg2, io._2)
              case _               => throw Exception(s"Expected 2 arguments, but found ${io._1.size} arguments")
          }
        }
      isFunction2 = function2s.nonEmpty
      isFunction3 <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            stringArrayType,
            stringArrayType,
            stringArrayType
          ).map(_.toList)
        ).map(_.nonEmpty)

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(fn.name, args = "['foo'], ['foo']")
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(
        fn.name,
        args = "['foo'], ['foo']",
        fuzzOverWindow = false
      )
    yield
      if !isFunction2 || !isFunction3 then
        logger.error(s"Unexpected result for hardcoded function ${fn.name}. Skipping it.")
        fn
      else
        val modes =
          if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
          else Set(CHFunction.Mode.NoOverWindow)

        fn.copy(
          modes = fn.modes ++ modes,
          settings = settings,
          function1Ns = function2s
            .map(_._1)
            .distinct
            .map(arg1 =>
              Function1N(
                arg1,
                CHSpecialType.Array(CHAggregatedType.Any),
                CHSpecialType.Array(CHSpecialType.TupleN(CHAggregatedType.Any))
              )
            )
        )

  private def fuzzPointInEllipses(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val float64Type: Seq[(CHFuzzableType, Seq[String])] = Seq(
      (CHFuzzableType.Float64, CHFuzzableType.Float64.fuzzingValues)
    )
    for
      function2Ns <-
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

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(fn.name, args = "1., 1., 1., 1., 1., 1.")
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(
        fn.name,
        args = "1., 1., 1., 1., 1., 1.",
        fuzzOverWindow = false
      )
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function2Ns = function2Ns
      )

  private def fuzzProportionsZTest(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val number = CHFuzzableAbstractType.Number.chFuzzableTypes.map(t => (t, Seq(t.fuzzingValues.head)))
    val usevarType = Seq((CHFuzzableType.Usevar, CHFuzzableType.Usevar.fuzzingValues))

    for
      function6s <-
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

      sampleIO = function6s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function6s = function6s
      )

  private def fuzzRankCorr(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val xyTypes = Seq(
      (CHFuzzableType.UInt8, Seq("number::UInt8")),
      (CHFuzzableType.UInt16, Seq("number::UInt16")),
      (CHFuzzableType.UInt32, Seq("number::UInt32")),
      (CHFuzzableType.UInt64, Seq("number::UInt64")),
      (CHFuzzableType.UInt128, Seq("number::UInt128")),
      (CHFuzzableType.UInt256, Seq("number::UInt256")),
      (CHFuzzableType.Int8, Seq("number::Int8")),
      (CHFuzzableType.Int16, Seq("number::Int16")),
      (CHFuzzableType.Int32, Seq("number::Int32")),
      (CHFuzzableType.Int64, Seq("number::Int64")),
      (CHFuzzableType.Int128, Seq("number::Int128")),
      (CHFuzzableType.Int256, Seq("number::Int256")),
      (CHFuzzableType.Decimal32, Seq("number::Decimal32(1)")),
      (CHFuzzableType.Decimal64, Seq("number::Decimal64(1)")),
      (CHFuzzableType.Decimal128, Seq("number::Decimal128(1)")),
      (CHFuzzableType.Decimal256, Seq("number::Decimal256(1)"))
    )

    for
      function2s <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            xyTypes,
            xyTypes
          ).map(_.toList),
          sourceTable = Some("numbers(2)")
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(arg1, arg2) => Function2(arg1, arg2, io._2)
              case _               => throw Exception(s"Expected 2 arguments, but found ${io._1.size} arguments")
          }
        }

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(
        fn.name,
        args = "number, number",
        sourceTable = Some("numbers(2)")
      )
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(
        fn.name,
        args = "number, number",
        fuzzOverWindow = false,
        sourceTable = Some("numbers(2)")
      )
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function2s = function2s
      )

  private def fuzzReinterpret(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    Future.successful(
      fn.copy(
        modes = fn.modes + CHFunction.Mode.NoOverWindow,
        function2s = Seq(
          Function2(CHAggregatedType.Any, CHFuzzableType.ClickHouseType, CHAggregatedType.Any)
        )
      )
    )

  private def fuzzS2CapUnionLike(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    for
      function4s <-
        FuzzerNonParametricFunctions
          .fuzzAbstractInputCombinations(
            fn.name,
            Seq(
              Seq(
                CHFuzzableAbstractType.Number,
                CHFuzzableAbstractType.Number,
                CHFuzzableAbstractType.Number,
                CHFuzzableAbstractType.Number
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

      sampleIO = function4s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function4s = function4s
      )

  private def fuzzSequenceCountLike(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val patternType = Seq(
      (CHFuzzableType.SpecialFixedString, Seq("'.'::FixedString(1)")),
      (CHFuzzableType.SpecialString, Seq("'.'::String"))
    )

    val timestampType = CHFuzzableAbstractType.nonCustomFuzzableTypes
      .map(t => (t, t.fuzzingValues))

    val conditionType = Seq(
      (CHFuzzableType.BooleanType, CHFuzzableType.BooleanType.fuzzingValues)
    )

    for
      parametric1Function2Ns <-
        fuzzParametricSignatures(
          fn.name,
          patternType.map(Seq(_)),
          crossJoin(
            timestampType,
            conditionType,
            conditionType
          ).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            (io._1, io._2) match
              case (Seq(param1), Seq(arg1, arg2, arg3)) => Parametric1Function2N(param1, arg1, arg2, arg3, io._3)
              case _ =>
                throw Exception(
                  s"Expected 1 parameters and 3 arguments, but found ${io._1.size} parameters and ${io._2.size} arguments"
                )
          }
        }

      isRepeatedConfirmation <-
        fuzzParametricSignatures(
          fn.name,
          patternType.map(Seq(_)),
          crossJoin(
            timestampType,
            conditionType,
            conditionType,
            conditionType
          ).map(_.toList)
        ).map(_.nonEmpty)

      sampleIO = parametric1Function2Ns.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      if !isRepeatedConfirmation then
        logger.error(s"Unexpected result for hardcoded function ${fn.name}. Skipping it.")
        fn
      else
        val modes =
          if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
          else Set(CHFunction.Mode.NoOverWindow)

        fn.copy(
          modes = fn.modes ++ modes,
          settings = settings,
          parametric1Function2Ns = parametric1Function2Ns
        )

  private def fuzzSequenceNextNode(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    // TODO Test it similarly to "encrypt"
    // This method has many parameters and arguments. It's not worth to fuzz it.
    import CHFuzzableType.*
    import CHSpecialType.*

    // format: off
    val timestampTypes =
      Seq(
        BooleanType, UInt8, UInt16, UInt32, UInt64, Date, DateTime, LowCardinalityBoolean,
        LowCardinalityUInt8, LowCardinalityUInt16, LowCardinalityUInt32, LowCardinalityUInt64,
        LowCardinalityDate, LowCardinalityDateTime, LowCardinalityNullableBoolean, LowCardinalityNullableUInt8,
        LowCardinalityNullableUInt16, LowCardinalityNullableUInt32, LowCardinalityNullableUInt64,
        LowCardinalityNullableDate, LowCardinalityNullableDateTime, NullableBoolean, NullableUInt8,
        NullableUInt16, NullableUInt32, NullableUInt64, NullableDate, NullableDateTime
      ).filter(CHFuzzableAbstractType.nonCustomFuzzableTypes.contains) // Removes Nullable etc. based on Configuration values

    Future.successful(
      fn.copy(
        modes = fn.modes ++ Set(CHFunction.Mode.NoOverWindow),
        settings = Set(CHSetting.AllowExperimentalFunnelFunctions(true)),
        specialParametric2Function2Ns =
          for
            (direction, base) <- 
              Seq(
                (SequenceDirectionForward, SequenceBaseHead),
                (SequenceDirectionBackward, SequenceBaseTail)
              )
            timestamp <- timestampTypes
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
            timestamp <- timestampTypes
          yield {
            CHFunctionIO.Parametric2Function3N(direction, base, timestamp, StringType, BooleanType, BooleanType, CHSpecialType.Nullable(CHFuzzableType.StringType))
          }
      )
    )
    // format: on

  private def fuzzSeriesOutliersDetectTukey(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    for
      function1s <-
        FuzzerNonParametricFunctions
          .fuzzAbstractInputCombinations(
            fn.name,
            Seq(
              Seq(CHFuzzableAbstractType.ArrayNumber)
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
                CHFuzzableAbstractType.ArrayNumber,
                CHFuzzableAbstractType.Number,
                CHFuzzableAbstractType.Number,
                CHFuzzableAbstractType.Number
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

      sampleIO = function1s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function1s = function1s,
        function4s = function4s
      )

  private def fuzzToModifiedJulianDay(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    for
      function1s <-
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

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(fn.name, args = "'2024-01-01'")
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(fn.name, args = "'2024-01-01'", fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function1s = function1s
      )

  private def fuzzTimeSlots(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val dateTimeTypes = CHFuzzableAbstractType.DateTime.chFuzzableTypes.map(t => (t, t.fuzzingValues))

    val allNumberCHTypes = CHFuzzableAbstractType.Number.chFuzzableTypes
    val nonExpectedNumberTypes =
      allNumberCHTypes.filterNot(t =>
        t.name == "Bool" || t.name.startsWith("UInt") || t.name.startsWith("Int") || t.name.startsWith(
          "Float"
        ) || t.name.startsWith("Decimal")
      )

    if nonExpectedNumberTypes.nonEmpty then
      throw Exception(
        s"Expected to handle only signed, unsigned, floating numbers, as well as Decimal. Found: ${nonExpectedNumberTypes.map(_.name).mkString(", ")}"
      )

    val numberTypes =
      allNumberCHTypes
        .filter(t => t.name.startsWith("UInt") || t.name.startsWith("Int"))
        .map(t => (t, Seq(s"1::${t.name}"))) ++
        allNumberCHTypes
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

      sampleIO = function2s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function2s = function2s,
        function3s = function3s
      )

  private def fuzzTupleElement(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val indexOrNameTypes = CHFuzzableAbstractType.nonCustomFuzzableTypes
      .map(t => (t, t.fuzzingValues))

    for
      function2s <-
        fuzzSignatures(
          fn.name,
          crossJoin(
            Seq((CHFuzzableType.ArrayTuple1UUID, CHFuzzableType.ArrayTuple1UUID.fuzzingValues)),
            indexOrNameTypes
          ).map(_.toList)
        ).map { signatures =>
          signatures.map { io =>
            io._1 match
              case Seq(arg1, arg2) => Function2(arg1, arg2, io._2)
              case _               => throw Exception(s"Expected 2 arguments, but found ${io._1.size} arguments")
          }
        }

      validIndexOrNameTypes = function2s.map(_.arg2)

      isValidSignature <-
        client
          .executeNoResult(
            s"SELECT toTypeName(${fn.name}(${CHFuzzableType.ArrayTuple1UUID.fuzzingValues.head}, 1))"
          )
          .flatMap(_ =>
            client.executeNoResult(
              s"SELECT toTypeName(${fn.name}(${CHFuzzableType.Tuple1Date.fuzzingValues.head}, 1))"
            )
          )
          .flatMap(_ =>
            client.executeNoResult(
              s"SELECT toTypeName(${fn.name}(${CHFuzzableType.ArrayTuple1UUID.fuzzingValues.head}, 1, 1))"
            )
          )
          .flatMap(_ =>
            client.executeNoResult(
              s"SELECT toTypeName(${fn.name}(${CHFuzzableType.Tuple1Date.fuzzingValues.head}, 1, 1))"
            )
          )
          .map(_ => true)
          .recover(_ => false)

      sampleIO = function2s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      if !isValidSignature then
        logger.error(
          s"Unexpected result for hardcoded function ${fn.name}, it may not exists anymore. Skipping it."
        )
        fn
      else
        val modes =
          if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
          else Set(CHFunction.Mode.NoOverWindow)

        fn.copy(
          modes = fn.modes ++ modes,
          settings = settings,
          function2s = validIndexOrNameTypes.flatMap(indexOrNameType =>
            Seq(
              CHFunctionIO.Function2(
                CHSpecialType.Array(CHSpecialType.TupleN(CHAggregatedType.Any)),
                indexOrNameType,
                CHAggregatedType.Any
              ),
              CHFunctionIO
                .Function2(CHSpecialType.TupleN(CHAggregatedType.Any), indexOrNameType, CHAggregatedType.Any)
            )
          ),
          function3s = validIndexOrNameTypes.flatMap(indexOrNameType =>
            Seq(
              CHFunctionIO.Function3(
                CHSpecialType.Array(CHSpecialType.TupleN(CHAggregatedType.Any)),
                indexOrNameType,
                CHAggregatedType.Any,
                CHAggregatedType.Any
              ),
              CHFunctionIO.Function3(
                CHSpecialType.TupleN(CHAggregatedType.Any),
                indexOrNameType,
                CHAggregatedType.Any,
                CHAggregatedType.Any
              )
            )
          )
        )

  private def fuzzVariantElement(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val variantColumn = Seq((CHFuzzableType.Variant, Seq("123456::Variant(UInt64, String, Array(UInt64))")))
    val typeColumn = Seq(
      (CHFuzzableType.StringType, Seq("'UInt32'::String", "'UInt64'::String")),
      (CHFuzzableType.FixedString, Seq("'UInt32'::FixedString(6)", "'UInt64'::FixedString(6)"))
    )

    for
      function2s <-
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

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(
        fn.name,
        args = "123456::Variant(UInt64, String, Array(UInt64)), 'UInt64'::String"
      )
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(
        fn.name,
        args = "123456::Variant(UInt64, String, Array(UInt64)), 'UInt64'::String",
        fuzzOverWindow = false
      )
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)
      val function3s = function2s.map { case Function2(arg1, arg2, _) =>
        Function3(arg1, arg2, CHAggregatedType.Any, CHAggregatedType.Any)
      }

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function2s = function2s,
        function3s = function3s
      )

  private def fuzzVariantType(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val variantColumn = Seq((CHFuzzableType.Variant, Seq("123456::Variant(UInt64, String, Array(UInt64))")))

    for
      function1s <-
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

      supportOverWindow <- Fuzzer.testSampleInputWithOverWindow(
        fn.name,
        args = "123456::Variant(UInt64, String, Array(UInt64))"
      )
      settings <- Fuzzer.detectMandatorySettingsFromSampleInput(
        fn.name,
        args = "123456::Variant(UInt64, String, Array(UInt64))",
        fuzzOverWindow = false
      )
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function1s = function1s
      )

  private def fuzzWidthBucket(
      fn: CHFunctionFuzzResult
  )(using client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    for
      function4s <-
        FuzzerNonParametricFunctions
          .fuzzAbstractInputCombinations(
            fn.name,
            Seq(
              Seq(
                CHFuzzableAbstractType.Number,
                CHFuzzableAbstractType.Number,
                CHFuzzableAbstractType.Number,
                CHFuzzableAbstractType.Number
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

      sampleIO = function4s.head
      supportOverWindow <- Fuzzer.testSampleFunctionWithOverWindow(fn.name, sampleIO)
      settings <- Fuzzer.detectMandatorySettingsFromSampleFunction(fn.name, sampleIO, fuzzOverWindow = false)
    yield
      val modes =
        if supportOverWindow then Set(CHFunction.Mode.NoOverWindow, CHFunction.Mode.OverWindow)
        else Set(CHFunction.Mode.NoOverWindow)

      fn.copy(
        modes = fn.modes ++ modes,
        settings = settings,
        function4s = function4s
      )
}
