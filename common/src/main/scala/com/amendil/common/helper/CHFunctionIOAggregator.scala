package com.amendil.common.helper

import com.amendil.common.entities.*
import com.amendil.common.entities.`type`.*
import com.amendil.common.entities.function.CHFunctionIO
import com.typesafe.scalalogging.StrictLogging

object CHFunctionIOAggregator extends StrictLogging:
  def aggregate[T <: CHFunctionIO](functions: Seq[T], supportJson: Boolean = true): Seq[CHFunctionIO] =
    require(
      functions.map(_.kind).distinct.size <= 1,
      s"Cannot aggregate different kind of functions, but asked to aggregate '${functions.map(_.kind).distinct.sorted.mkString("', '")}' together."
    )

    given SupportJson = supportJson

    var aggregatedSignatures: Seq[CHFunctionIO] = functions

    aggregatedSignatures = deduplicateArguments(aggregatedSignatures)
    aggregatedSignatures = deduplicateParameters(aggregatedSignatures)

    aggregatedSignatures = aggregateSimilarIOs(aggregatedSignatures)
    aggregatedSignatures = aggregateArrayArgumentsWithMapOutput(aggregatedSignatures)
    aggregatedSignatures = aggregateArrayArgumentsWithTupleArrayNullableOutput(aggregatedSignatures)
    aggregatedSignatures = aggregateArrayArgumentsWithTupleArrayOutput(aggregatedSignatures)

    if aggregatedSignatures.size == functions.size then functions
    else aggregate(aggregatedSignatures)
  

  private def equalOrUnknown(t1: CHType, t2: CHType): Boolean =
    (t1, t2) match
      case (CHSpecialType.Array(innerT1), CHSpecialType.Array(innerT2)) =>
        equalOrUnknown(innerT1, innerT2)
      case (CHSpecialType.LowCardinality(innerT1), CHSpecialType.LowCardinality(innerT2)) =>
        equalOrUnknown(innerT1, innerT2)
      case (CHSpecialType.Map(k1, v1), CHSpecialType.Map(k2, v2)) =>
        equalOrUnknown(k1, k2) && equalOrUnknown(v1, v2)
      case (CHSpecialType.Nullable(innerT1), CHSpecialType.Nullable(innerT2)) =>
        equalOrUnknown(innerT1, innerT2)
      case (CHSpecialType.Tuple(innerTypes1), CHSpecialType.Tuple(innerTypes2)) =>
        innerTypes1.size == innerTypes2.size && innerTypes1.zip(innerTypes2).forall(equalOrUnknown)
      case (CHSpecialType.TupleN(innerT1), CHSpecialType.TupleN(innerT2)) =>
        equalOrUnknown(innerT1, innerT2)
      case (CHSpecialType.UnknownType, _) => true
      case (_, CHSpecialType.UnknownType) => true
      case _                              => t1.name == t2.name

  private def aggregateSimilarIOs(signatures: Seq[CHFunctionIO])(using supportJson: SupportJson): Seq[CHFunctionIO] =
    var aggregatedSignatureIOs: Seq[Seq[CHType]] =
      signatures.map(s => (s.parameters ++ s.arguments :+ s.output).map(CHType.normalize))
    val paramCount = signatures.head.parameters.size
    val argCount = signatures.head.arguments.size

    // -1 to avoid handling the output, we want to overview types that we are fully in control of (e.g. Scalar, Array, Tuple, Map)
    // The output type can have multiple level of type nesting (e.g. Array(Array(Array(UInt8)))), and our aggregation rules are not handling that
    Range.apply(0, aggregatedSignatureIOs.head.size - 1).foreach { idx =>

      // When there is no "Scalar", we know we cannot merge Array, Bitmap and Map
      // So let's aggregate them separately
      val groupedAggregatedSignatureIOs =
        if aggregatedSignatureIOs
            .map(_(idx))
            .distinct
            .forall(t =>
              t.name.startsWith("Array(") ||
                t.name.startsWith("Bitmap(") ||
                t.name.startsWith("Map(") ||
                t.name.startsWith("Tuple(")
            )
        then
          Seq(
            aggregatedSignatureIOs.filter(_(idx).name.startsWith("Array(")),
            aggregatedSignatureIOs.filter(_(idx).name.startsWith("Bitmap(")),
            aggregatedSignatureIOs.filter(_(idx).name.startsWith("Map(")),
            aggregatedSignatureIOs.filter(_(idx).name.startsWith("Tuple("))
          ).filter(_.nonEmpty)
        else Seq(aggregatedSignatureIOs)

      aggregatedSignatureIOs = groupedAggregatedSignatureIOs.flatMap { aggregatedSignatureIOsGroup =>
        val wantedTypes = aggregatedSignatureIOsGroup.map(_(idx)).distinct
        val wantedTypeConverter: CHTypeConverter = CHTypeConverter.getConverter(wantedTypes)

        val aggregatedSignatureIOsGroupWithSimilarities: Seq[(Seq[CHType], Map[Int, CHTypeConverter])] =
          aggregatedSignatureIOsGroup.map { types =>
            val wantedType = wantedTypeConverter.extract(types(idx))
            val typesWithConverter =
              types.zipWithIndex.collect {
                case (t, idx) if equalOrUnknown(t, wantedType) =>
                  (idx, CHTypeConverter.ScalarTypeConverter)

                case (t, idx) if equalOrUnknown(t, CHSpecialType.Array(CHSpecialType.Array(wantedType))) =>
                  (idx, CHTypeConverter.ArrayArrayTypeConverter)

                case (t, idx)
                    if equalOrUnknown(
                      t,
                      CHSpecialType.Array(CHSpecialType.Tuple(Seq(CHFuzzableType.StringType, wantedType)))
                    ) || equalOrUnknown(
                      t,
                      CHSpecialType.Array(
                        CHSpecialType.Tuple(Seq(CHFuzzableType.StringType, wantedType, CHSpecialType.UnknownType))
                      )
                    ) || equalOrUnknown(
                      t,
                      CHSpecialType.Array(
                        CHSpecialType.Tuple(
                          Seq(
                            CHFuzzableType.StringType,
                            wantedType,
                            CHSpecialType.UnknownType,
                            CHSpecialType.UnknownType
                          )
                        )
                      )
                    ) || equalOrUnknown(
                      t,
                      CHSpecialType.Array(
                        CHSpecialType.Tuple(
                          Seq(
                            CHFuzzableType.StringType,
                            wantedType,
                            CHSpecialType.UnknownType,
                            CHSpecialType.UnknownType,
                            CHSpecialType.UnknownType
                          )
                        )
                      )
                    ) =>
                  (
                    idx,
                    CHTypeConverter.ArrayTupleStringTypeConverter(tail =
                      t.asInstanceOf[CHSpecialType.Array]
                        .innerType
                        .asInstanceOf[CHSpecialType.Tuple]
                        .innerTypes
                        .tail
                        .tail
                    )
                  ) // .tail.tail to remove String + wanted type

                case (t, idx)
                    if equalOrUnknown(t, CHSpecialType.Array(CHSpecialType.Tuple(Seq(wantedType)))) || equalOrUnknown(
                      t,
                      CHSpecialType.Array(CHSpecialType.Tuple(Seq(wantedType, CHSpecialType.UnknownType)))
                    ) || equalOrUnknown(
                      t,
                      CHSpecialType.Array(
                        CHSpecialType.Tuple(Seq(wantedType, CHSpecialType.UnknownType, CHSpecialType.UnknownType))
                      )
                    ) || equalOrUnknown(
                      t,
                      CHSpecialType.Array(
                        CHSpecialType.Tuple(
                          Seq(
                            wantedType,
                            CHSpecialType.UnknownType,
                            CHSpecialType.UnknownType,
                            CHSpecialType.UnknownType
                          )
                        )
                      )
                    ) =>
                  (
                    idx,
                    CHTypeConverter.ArrayTupleTypeConverter(tail =
                      t.asInstanceOf[CHSpecialType.Array].innerType.asInstanceOf[CHSpecialType.Tuple].innerTypes.tail
                    )
                  )

                case (t, idx) if equalOrUnknown(t, CHSpecialType.Array(wantedType)) =>
                  (idx, CHTypeConverter.ArrayTypeConverter)

                case (t, idx) if equalOrUnknown(t, CHSpecialType.Bitmap(wantedType)) =>
                  (idx, CHTypeConverter.BitmapTypeConverter)

                case (t, idx) if equalOrUnknown(t, CHSpecialType.LowCardinality(CHSpecialType.Nullable(wantedType))) =>
                  (idx, CHTypeConverter.LowCardinalityNullableTypeConverter)

                case (t, idx) if equalOrUnknown(t, CHSpecialType.LowCardinality(wantedType)) =>
                  (idx, CHTypeConverter.LowCardinalityTypeConverter)

                case (t, idx) if equalOrUnknown(t, CHSpecialType.Map(wantedType, CHSpecialType.UnknownType)) =>
                  (idx, CHTypeConverter.MapTypeConverter(t.asInstanceOf[CHSpecialType.Map].valueType))

                case (t, idx) if equalOrUnknown(t, CHSpecialType.Nullable(wantedType)) =>
                  (idx, CHTypeConverter.NullableTypeConverter)

                case (t, idx)
                    if wantedTypeConverter == CHTypeConverter.MapTypeConverter(
                      CHSpecialType.UnknownType
                    ) && equalOrUnknown(t, CHSpecialType.Tuple(Seq(wantedType, CHSpecialType.UnknownType))) =>
                  (idx, CHTypeConverter.TupleTypeConverter(tail = Seq(CHSpecialType.UnknownType)))

                case (t, idx)
                    if equalOrUnknown(t, CHSpecialType.Tuple(Seq(wantedType))) || equalOrUnknown(
                      t,
                      CHSpecialType.Tuple(Seq(wantedType, CHSpecialType.UnknownType))
                    ) || equalOrUnknown(
                      t,
                      CHSpecialType.Tuple(Seq(wantedType, CHSpecialType.UnknownType, CHSpecialType.UnknownType))
                    ) || equalOrUnknown(
                      t,
                      CHSpecialType.Tuple(
                        Seq(wantedType, CHSpecialType.UnknownType, CHSpecialType.UnknownType, CHSpecialType.UnknownType)
                      )
                    ) =>
                  (idx, CHTypeConverter.TupleTypeConverter(tail = t.asInstanceOf[CHSpecialType.Tuple].innerTypes.tail))

                case (t, idx)
                    if equalOrUnknown(
                      t,
                      CHSpecialType.Tuple(Seq(CHSpecialType.Array(CHSpecialType.Nullable(wantedType))))
                    ) =>
                  (
                    idx,
                    CHTypeConverter
                      .TupleArrayNullableTypeConverter(tail = t.asInstanceOf[CHSpecialType.Tuple].innerTypes.tail)
                  )

                case (t, idx) if equalOrUnknown(t, CHSpecialType.Tuple(Seq(CHSpecialType.Array(wantedType)))) =>
                  (
                    idx,
                    CHTypeConverter.TupleArrayTypeConverter(tail = t.asInstanceOf[CHSpecialType.Tuple].innerTypes.tail)
                  )
              }
            (types, typesWithConverter.toMap)
          }

        // Currently this threshold must be strictly over 50%, so that for one index we select at most one converter.
        // Should we get the best converter per index and then have a threshold that might go lower?
        val threshold = 0.66 * aggregatedSignatureIOsGroup.size
        val similarInputMetadata: Seq[InputMetadata] =
          Range.apply(0, aggregatedSignatureIOs.head.size).flatMap { idx =>
            aggregatedSignatureIOsGroupWithSimilarities
              .flatMap { case (_, similarities) =>
                similarities.get(idx)
              }
              .groupBy(identity)
              .collectFirst {
                case (converter, duplicates) if duplicates.size > threshold => (idx, converter)
              }
          }

        val (aggregatableSignaturesWithSimilarities, nonAggregatableSignaturesWithSimilarities) =
          aggregatedSignatureIOsGroupWithSimilarities.partition((_, similarities) =>
            similarInputMetadata.forall(inputMetadata =>
              similarities.get(inputMetadata.idx).exists(_.compatibleConverters().contains(inputMetadata.converter))
            )
          )
        val aggregatableSignatures = aggregatableSignaturesWithSimilarities.map(_._1)
        val nonAggregatableSignatures = nonAggregatableSignaturesWithSimilarities.map(_._1)

        logger.trace(
          s"aggregateSimilarIOs - ability to aggregate arguments at indexes ${similarInputMetadata.map(_.idx).sorted.mkString(", ")}"
        )
        aggregatableSignatures
          .groupBy(
            _.zipWithIndex.collect { case (t, idx) if !similarInputMetadata.exists(_.idx == idx) => t }
          )
          .map { case (groupingKey, groupedSignatureIOs) =>
            // Different types might lead to different results because the fuzzing we are doing is not 100% exhaustive for Array
            // and maybe other similar types
            // Merging Arrays can lead to `Array(Any)`, while in that situation merging inner types of Arrays will not lead to `Any`.
            similarInputMetadata
              .groupBy(_.converter)
              .map((_, groupedInputMetadata) => groupedInputMetadata.head)
              .map { inputMetadata =>
                val types = groupedSignatureIOs.map(_(inputMetadata.idx)).toSet
                val aggregatedTypes = CHType
                  .mergeInputTypes(types, supportJson = supportJson)
                  .map(t => inputMetadata.converter.extract(CHType.normalize(t)))

                if groupedSignatureIOs.size == aggregatedTypes.size then groupedSignatureIOs
                else
                  aggregatedTypes.map(aggregatedType =>
                    val genericType =
                      aggregatedType match
                        case t if types.contains(t) => t // Type was not aggregated
                        case t if similarInputMetadata.size == 1 =>
                          t // Type was aggregated, but we were working on a single column
                        case t => // Type was aggregated and we are working on multiple columns
                          createGenericType(
                            t,
                            genericTypeIdx = CHType.getGenericTypes(groupedSignatureIOs.head).size + 1
                          )

                    groupedSignatureIOs.head.zipWithIndex.map { case (t, idx) =>
                      similarInputMetadata
                        .find(_.idx == idx)
                        .map(_.converter.wrap(genericType))
                        .getOrElse(t)
                    }
                  )
              }
              .minBy(_.size)
          }
          .flatten
          .toSeq ++ nonAggregatableSignatures
      }
    }

    if aggregatedSignatureIOs.size == signatures.size then signatures
    else
      aggregatedSignatureIOs.map { signatureIO =>
        val aggregatedParameters = signatureIO.zipWithIndex.collect {
          case (t, idx) if idx < paramCount => t
        }

        val aggregatedArguments = signatureIO.zipWithIndex.collect {
          case (t, idx) if idx >= paramCount && idx < paramCount + argCount => t
        }

        val aggregatedOutput = signatureIO.zipWithIndex.collect {
          case (t, idx) if idx == paramCount + argCount => t
        }.head

        new CHFunctionIO:
          override def kind: String = signatures.head.kind
          override def parameters: Seq[CHType] = aggregatedParameters
          override def arguments: Seq[CHType] = aggregatedArguments
          override def repeatedParameterIdxOpt: Option[Int] = signatures.head.repeatedParameterIdxOpt
          override def repeatedArgumentIdxOpt: Option[Int] = signatures.head.repeatedArgumentIdxOpt
          override def isParametric: Boolean = signatures.head.isParametric
          override def output: CHType = aggregatedOutput
      }

  private def deduplicateArguments(signature: Seq[CHFunctionIO]): Seq[CHFunctionIO] =
    var aggregatedSignatures = signature
    Range.apply(0, signature.head.arguments.size).foreach { argumentIdx =>
      val newAggregatedSignatures = aggregatedSignatures
        .groupBy(sig =>
          (
            sig.parameters,
            sig.arguments.zipWithIndex.collect { case (arg, idx) if idx < argumentIdx => arg },
            sig.arguments.zipWithIndex.collect { case (arg, idx) if idx > argumentIdx => arg },
            sig.output
          )
        )
        .map { case (groupingKey, groupedSignatures) =>
          val types = groupedSignatures.map(_.arguments(argumentIdx)).toSet
          val deduplicatedTypes = CHType.deduplicateSupertypes(types)

          if types.size == deduplicatedTypes.size then groupedSignatures
          else
            deduplicatedTypes.map(aggregatedType =>
              new CHFunctionIO:
                override def kind: String = groupedSignatures.head.kind
                override def parameters: Seq[CHType] = groupedSignatures.head.parameters
                override def arguments: Seq[CHType] = (groupingKey._2 :+ aggregatedType) ++ groupingKey._3
                override def repeatedParameterIdxOpt: Option[Int] = groupedSignatures.head.repeatedParameterIdxOpt
                override def repeatedArgumentIdxOpt: Option[Int] = groupedSignatures.head.repeatedArgumentIdxOpt
                override def isParametric: Boolean = groupedSignatures.head.isParametric
                override def output: CHType = groupedSignatures.head.output
            )
        }
        .flatten
        .toSeq

      if newAggregatedSignatures.size < aggregatedSignatures.size then
        logger.trace(s"deduplicateArguments - deduplicating argument $argumentIdx")
        aggregatedSignatures = newAggregatedSignatures

      // Some functions working with both Date and DateTime arguments are accepting an optional argument being a timezone
      // In such case it checks the timezone argument is a String, and validates it as a TimeZone only for DateTime.
      //
      // In such circumstances we remove the signatures that specify a String instead of a TimeZone.
      if aggregatedSignatures.exists(_.arguments(argumentIdx) == CHFuzzableType.TimeZone)
      then aggregatedSignatures = aggregatedSignatures.filterNot(_.arguments(argumentIdx) == CHFuzzableType.StringType)
    }
    aggregatedSignatures

  private def deduplicateParameters(signature: Seq[CHFunctionIO]): Seq[CHFunctionIO] =
    var aggregatedSignatures = signature
    Range.apply(0, signature.head.parameters.size).foreach { parameterIdx =>
      val newAggregatedSignatures = aggregatedSignatures
        .groupBy(sig =>
          (
            sig.parameters.zipWithIndex.collect { case (arg, idx) if idx < parameterIdx => arg },
            sig.parameters.zipWithIndex.collect { case (arg, idx) if idx > parameterIdx => arg },
            sig.arguments,
            sig.output
          )
        )
        .map { case (groupingKey, groupedSignatures) =>
          val types = groupedSignatures.map(_.parameters(parameterIdx)).toSet
          val deduplicatedTypes = CHType.deduplicateSupertypes(types)

          if types.size == deduplicatedTypes.size then groupedSignatures
          else
            deduplicatedTypes.map(aggregatedType =>
              new CHFunctionIO:
                override def kind: String = groupedSignatures.head.kind
                override def parameters: Seq[CHType] = (groupingKey._1 :+ aggregatedType) ++ groupingKey._2
                override def arguments: Seq[CHType] = groupedSignatures.head.arguments
                override def repeatedParameterIdxOpt: Option[Int] = groupedSignatures.head.repeatedParameterIdxOpt
                override def repeatedArgumentIdxOpt: Option[Int] = groupedSignatures.head.repeatedArgumentIdxOpt
                override def isParametric: Boolean = groupedSignatures.head.isParametric
                override def output: CHType = groupedSignatures.head.output
            )
        }
        .flatten
        .toSeq

      if newAggregatedSignatures.size < aggregatedSignatures.size then
        logger.trace(s"deduplicateParameter - deduplicated parameter $parameterIdx")
        aggregatedSignatures = newAggregatedSignatures
    }
    aggregatedSignatures

  private def aggregateArrayArgumentsWithMapOutput(
      functions: Seq[CHFunctionIO]
  )(using supportJson: SupportJson): Seq[CHFunctionIO] =
    var aggregatedSignatures: Seq[CHFunctionIO] = functions
    Range.apply(0, functions.head.arguments.size).foreach { argumentIdx =>
      val argAndOutput = aggregatedSignatures.map(fn =>
        (
          CHType.normalize(fn.arguments(argumentIdx)),
          fn.output
        )
      )

      if argAndOutput.forall((i, o) => i.name.startsWith("Array(") && o.isInstanceOf[CHSpecialType.Map]) then
        logger
          .trace(s"aggregateArrayArgumentsWithMapOutput - we may have the ability to aggregate argument $argumentIdx")
        // I'm sorry if you stumble on a bug to investigate in this part of the codebase.
        // Here are kittens to comfort you: ≡<^_^>≡
        //   ∧,,,,,,∧
        //  (  ̳• · • ̳)
        //  /       づ♡

        val argAndOutputTyped = argAndOutput.map { case (i, o) =>
          (
            CHType.normalize(i).asInstanceOf[CHSpecialType.Array],
            o.asInstanceOf[CHSpecialType.Map]
          )
        }

        // Match on `argument's inner type` == `output's key type`
        // and Map value types are all the same.
        if argAndOutputTyped.forall { case (iArr, oMap) => equalOrUnknown(iArr.innerType, oMap.keyType) } &&
          argAndOutputTyped
            .collect {
              case (
                    CHSpecialType.Array(CHSpecialType.Map(_, CHSpecialType.UnknownType)),
                    CHSpecialType.Map(CHSpecialType.Map(_, v), _)
                  ) =>
                v
            }
            .distinct
            .size <= 1
        then
          val outputMapValueTypes = argAndOutput.map((_, o) => o.asInstanceOf[CHSpecialType.Map].valueType)
          if outputMapValueTypes.distinct.size == 1 then
            logger.trace(
              s"aggregateArrayArgumentsWithMapOutput - ability to aggregate argument $argumentIdx with a single generic type"
            )
            // Value type is unique, great, we can aggregate those maps.
            aggregatedSignatures = aggregateWithGenericType(
              aggregatedSignatures,
              argumentIdx = argumentIdx,
              typeSelector = fn => fn.arguments(argumentIdx).asInstanceOf[CHSpecialType.Array],
              aggregatedTypeGenerator = t => CHType.normalize(t).asInstanceOf[CHSpecialType.Array].innerType,
              inputTypeGenerator = t => CHSpecialType.Array(t),
              outputTypeGenerator = t => CHSpecialType.Map(t, outputMapValueTypes.head)
            )
          else
            logger.trace(
              s"aggregateArrayArgumentsWithMapOutput - we may have the ability to aggregate argument $argumentIdx, looking for another column to help"
            )
            // Value type is not unique, but maybe it's defined by another column?
            Range.apply(0, functions.head.arguments.size).filter(_ != argumentIdx).foreach { argumentIdx2 =>
              val arg2AndOutput = aggregatedSignatures.map(fn =>
                (
                  CHType.normalize(fn.arguments(argumentIdx2)),
                  fn.output.asInstanceOf[CHSpecialType.Map]
                )
              )

              // Match on `argument2's inner type` == `output's value type`
              // and `argument2's KV types` == `output's tuple's types`,
              // and Map value types are all the same.
              if arg2AndOutput.forall { case (j, CHSpecialType.Map(_, outputValueType)) =>
                  j match
                    case CHSpecialType.Array(innerType) => equalOrUnknown(innerType, outputValueType)
                    case CHSpecialType.Map(keyType, valueType) =>
                      outputValueType match
                        case CHSpecialType.Tuple(Seq(t1, t2)) =>
                          equalOrUnknown(keyType, t1) && equalOrUnknown(valueType, t2)
                        case _ => false
                    case _ => false
                } && arg2AndOutput
                  .collect {
                    case (
                          CHSpecialType.Map(_, CHSpecialType.UnknownType),
                          CHSpecialType.Map(_, CHSpecialType.Tuple(Seq(_, t2)))
                        ) =>
                      Seq(t2)
                    case (
                          CHSpecialType.Map(_, t1),
                          CHSpecialType.Map(_, CHSpecialType.Tuple(Seq(_, t2)))
                        ) =>
                      Seq(t1, t2)
                  }
                  .flatten
                  .distinct
                  .size <= 1
              then
                logger.trace(
                  s"aggregateArrayArgumentsWithMapOutput - ability to aggregate arguments $argumentIdx and $argumentIdx2"
                )

                // Determine the unique map value
                val mapValueTypes = arg2AndOutput.collect {
                  case (CHSpecialType.Map(_, t1), CHSpecialType.Map(_, CHSpecialType.Tuple(Seq(_, t2)))) => Seq(t1, t2)
                }.flatten
                val mapValueFinalType =
                  if mapValueTypes.isEmpty then
                    CHSpecialType.UnknownType // This variable is only used when there is a map
                  else mapValueTypes.find(_ == CHSpecialType.UnknownType).getOrElse(mapValueTypes.head)

                // Aggregate
                aggregatedSignatures = aggregateWithGenericTypes(
                  aggregatedSignatures.filter(_.arguments(argumentIdx2).name.startsWith("Array(")),
                  argumentIdx1 = argumentIdx,
                  argumentIdx2 = argumentIdx2,
                  typeSelector1 = fn => fn.arguments(argumentIdx),
                  typeSelector2 = fn => fn.arguments(argumentIdx2),
                  aggregatedTypeGenerator1 = t => CHType.normalize(t).asInstanceOf[CHSpecialType.Array].innerType,
                  aggregatedTypeGenerator2 = t => CHType.normalize(t).asInstanceOf[CHSpecialType.Array].innerType,
                  inputTypeGenerator1 = t1 => CHSpecialType.Array(t1),
                  inputTypeGenerator2 = t2 => CHSpecialType.Array(t2),
                  outputTypeGenerator = (t1, t2) => CHSpecialType.Map(t1, t2)
                ) ++
                  aggregateWithGenericTypes(
                    aggregatedSignatures.filter(_.arguments(argumentIdx2).name.startsWith("Map(")),
                    argumentIdx1 = argumentIdx,
                    argumentIdx2 = argumentIdx2,
                    typeSelector1 = fn => fn.arguments(argumentIdx),
                    typeSelector2 = fn => fn.arguments(argumentIdx2),
                    aggregatedTypeGenerator1 = t => CHType.normalize(t).asInstanceOf[CHSpecialType.Array].innerType,
                    aggregatedTypeGenerator2 = t => CHType.normalize(t).asInstanceOf[CHSpecialType.Map].keyType,
                    inputTypeGenerator1 = t1 => CHSpecialType.Array(t1),
                    inputTypeGenerator2 = t2 => CHSpecialType.Map(t2, mapValueFinalType),
                    outputTypeGenerator =
                      (t1, t2) => CHSpecialType.Map(t1, CHSpecialType.Tuple(Seq(t2, mapValueFinalType)))
                  )
            }
    }
    aggregatedSignatures

  private def aggregateArrayArgumentsWithTupleArrayNullableOutput(
      functions: Seq[CHFunctionIO]
  )(using supportJson: SupportJson): Seq[CHFunctionIO] =
    val twoArgumentsFn =
      functions.filter(fn =>
        fn.output match
          case CHSpecialType.Tuple(innerTypes) => innerTypes.size == 2 && fn.arguments.size >= 2
          case _                               => false
      )

    val threeArgumentsFn =
      functions.filter(fn =>
        fn.output match
          case CHSpecialType.Tuple(innerTypes) => innerTypes.size == 3 && fn.arguments.size >= 3
          case _                               => false
      )

    val unhandledFn =
      functions.filterNot(f => twoArgumentsFn.contains(f) || threeArgumentsFn.contains(f))

    var aggregatedSignatures: Seq[CHFunctionIO] = unhandledFn

    // Testing two arguments
    var twoArgumentsFnAggregated: Seq[CHFunctionIO] = Nil
    var cancelAggregationTwoArgumentsFn = false
    Range.apply(0, twoArgumentsFn.headOption.map(_.arguments.size).getOrElse(0) - 1).foreach { argumentIdx1 =>
      // We assume arguments are provided in order
      val argumentIdx2 = argumentIdx1 + 1

      val (aggregatableSignatures, nonAggregatableSignatures) =
        twoArgumentsFn.partition(sig =>
          (
            CHType.normalize(sig.arguments(argumentIdx1)),
            CHType.normalize(sig.arguments(argumentIdx2)),
            sig.output
          ) match
            case (
                  CHSpecialType.Array(t1),
                  CHSpecialType.Array(t2),
                  CHSpecialType.Tuple(
                    Seq(
                      CHSpecialType.Array(CHSpecialType.Nullable(outputT1)),
                      CHSpecialType.Array(CHSpecialType.Nullable(outputT2))
                    )
                  )
                ) =>
              equalOrUnknown(t1, outputT1) && equalOrUnknown(t2, outputT2)
            case _ =>
              false
        )

      val argsAndOutput = aggregatableSignatures
        .map(fn =>
          (
            CHType.normalize(fn.arguments(argumentIdx1)),
            CHType.normalize(fn.arguments(argumentIdx2)),
            fn.output
          )
        )
        .distinct

      if aggregatableSignatures.size > 0.66 * twoArgumentsFn.size && argsAndOutput
          .map(_._1)
          .distinct
          .size * argsAndOutput.map(_._2).distinct.size == argsAndOutput.size
      then
        logger.trace(
          s"aggregateArrayArgumentsWithTupleArrayNullableOutput - we may have the ability to aggregate arguments $argumentIdx1 and $argumentIdx2"
        )

        if twoArgumentsFnAggregated.nonEmpty then
          logger.warn(
            "aggregateArrayArgumentsWithTupleArrayNullableOutput - Multiple combination of 2 arguments can be used for aggregating signatures, avoiding aggreggating those signatures"
          )
          cancelAggregationTwoArgumentsFn = true

        twoArgumentsFnAggregated = aggregateWithGenericTypes(
          aggregatableSignatures,
          argumentIdx1,
          argumentIdx2,
          typeSelector1 = fn => fn.arguments(argumentIdx1),
          typeSelector2 = fn => fn.arguments(argumentIdx2),
          aggregatedTypeGenerator1 = t => CHType.normalize(t).asInstanceOf[CHSpecialType.Array].innerType,
          aggregatedTypeGenerator2 = t => CHType.normalize(t).asInstanceOf[CHSpecialType.Array].innerType,
          inputTypeGenerator1 = t => CHSpecialType.Array(t),
          inputTypeGenerator2 = t => CHSpecialType.Array(t),
          outputTypeGenerator = (t1, t2) =>
            CHSpecialType.Tuple(
              Seq(CHSpecialType.Array(CHSpecialType.Nullable(t1)), CHSpecialType.Array(CHSpecialType.Nullable(t2)))
            )
        ) ++ nonAggregatableSignatures
    }
    if twoArgumentsFnAggregated.nonEmpty && !cancelAggregationTwoArgumentsFn then
      aggregatedSignatures ++= twoArgumentsFnAggregated
    else aggregatedSignatures ++= twoArgumentsFn

    // Testing three arguments
    var threeArgumentsFnAggregated: Seq[CHFunctionIO] = Nil
    var cancelAggregationThreeArgumentsFn = false
    Range.apply(0, threeArgumentsFn.headOption.map(_.arguments.size).getOrElse(0) - 2).foreach { argumentIdx1 =>
      // We assume arguments are provided in order
      val argumentIdx2 = argumentIdx1 + 1
      val argumentIdx3 = argumentIdx1 + 2

      val (aggregatableSignatures, nonAggregatableSignatures) =
        threeArgumentsFn.partition(sig =>
          (
            CHType.normalize(sig.arguments(argumentIdx1)),
            CHType.normalize(sig.arguments(argumentIdx2)),
            CHType.normalize(sig.arguments(argumentIdx3)),
            sig.output
          ) match
            case (
                  CHSpecialType.Array(t1),
                  CHSpecialType.Array(t2),
                  CHSpecialType.Array(t3),
                  CHSpecialType.Tuple(
                    Seq(
                      CHSpecialType.Array(CHSpecialType.Nullable(outputT1)),
                      CHSpecialType.Array(CHSpecialType.Nullable(outputT2)),
                      CHSpecialType.Array(CHSpecialType.Nullable(outputT3))
                    )
                  )
                ) =>
              equalOrUnknown(t1, outputT1) && equalOrUnknown(t2, outputT2) && equalOrUnknown(t3, outputT3)
            case _ =>
              false
        )

      val argsAndOutput = aggregatableSignatures
        .map(fn =>
          (
            CHType.normalize(fn.arguments(argumentIdx1)),
            CHType.normalize(fn.arguments(argumentIdx2)),
            CHType.normalize(fn.arguments(argumentIdx3)),
            fn.output
          )
        )
        .distinct

      if aggregatableSignatures.size > 0.66 * threeArgumentsFn.size && argsAndOutput
          .map(_._1)
          .distinct
          .size * argsAndOutput.map(_._2).distinct.size * argsAndOutput.map(_._3).distinct.size == argsAndOutput.size
      then
        logger.trace(
          s"aggregateArrayArgumentsWithTupleArrayNullableOutput - we may have the ability to aggregate arguments $argumentIdx1, $argumentIdx2 and $argumentIdx3"
        )

        if threeArgumentsFnAggregated.nonEmpty then
          logger.warn(
            "aggregateArrayArgumentsWithTupleArrayNullableOutput - Multiple combination of 2 arguments can be used for aggregating signatures, avoiding aggreggating those signatures"
          )
          cancelAggregationThreeArgumentsFn = true

        threeArgumentsFnAggregated = aggregateWithGenericTypes(
          aggregatableSignatures,
          argumentIdx1,
          argumentIdx2,
          argumentIdx3,
          typeSelector1 = fn => fn.arguments(argumentIdx1),
          typeSelector2 = fn => fn.arguments(argumentIdx2),
          typeSelector3 = fn => fn.arguments(argumentIdx3),
          aggregatedTypeGenerator1 = t => CHType.normalize(t).asInstanceOf[CHSpecialType.Array].innerType,
          aggregatedTypeGenerator2 = t => CHType.normalize(t).asInstanceOf[CHSpecialType.Array].innerType,
          aggregatedTypeGenerator3 = t => CHType.normalize(t).asInstanceOf[CHSpecialType.Array].innerType,
          inputTypeGenerator1 = t => CHSpecialType.Array(t),
          inputTypeGenerator2 = t => CHSpecialType.Array(t),
          inputTypeGenerator3 = t => CHSpecialType.Array(t),
          outputTypeGenerator = (t1, t2, t3) =>
            CHSpecialType.Tuple(
              Seq(
                CHSpecialType.Array(CHSpecialType.Nullable(t1)),
                CHSpecialType.Array(CHSpecialType.Nullable(t2)),
                CHSpecialType.Array(CHSpecialType.Nullable(t3))
              )
            )
        ) ++ nonAggregatableSignatures
    }
    if threeArgumentsFnAggregated.nonEmpty && !cancelAggregationThreeArgumentsFn then
      aggregatedSignatures ++= threeArgumentsFnAggregated
    else aggregatedSignatures ++= threeArgumentsFn

    aggregatedSignatures

  private def aggregateArrayArgumentsWithTupleArrayOutput(
      functions: Seq[CHFunctionIO]
  )(using supportJson: SupportJson): Seq[CHFunctionIO] =
    val twoArgumentsFn =
      functions.filter(fn =>
        fn.output match
          case CHSpecialType.Tuple(innerTypes) => innerTypes.size == 2 && fn.arguments.size >= 2
          case _                               => false
      )

    val threeArgumentsFn =
      functions.filter(fn =>
        fn.output match
          case CHSpecialType.Tuple(innerTypes) => innerTypes.size == 3 && fn.arguments.size >= 3
          case _                               => false
      )

    val unhandledFn =
      functions.filterNot(f => twoArgumentsFn.contains(f) || threeArgumentsFn.contains(f))

    var aggregatedSignatures: Seq[CHFunctionIO] = unhandledFn

    // Testing two arguments
    var twoArgumentsFnAggregated: Seq[CHFunctionIO] = Nil
    var cancelAggregationTwoArgumentsFn = false
    Range.apply(0, twoArgumentsFn.headOption.map(_.arguments.size).getOrElse(0) - 1).foreach { argumentIdx1 =>
      // We assume arguments are provided in order
      val argumentIdx2 = argumentIdx1 + 1

      val (aggregatableSignatures, nonAggregatableSignatures) =
        twoArgumentsFn.partition(sig =>
          (
            CHType.normalize(sig.arguments(argumentIdx1)),
            CHType.normalize(sig.arguments(argumentIdx2)),
            sig.output
          ) match
            case (
                  CHSpecialType.Array(t1),
                  CHSpecialType.Array(t2),
                  CHSpecialType.Tuple(Seq(CHSpecialType.Array(outputT1), CHSpecialType.Array(outputT2)))
                ) =>
              equalOrUnknown(t1, outputT1) && equalOrUnknown(t2, outputT2)
            case _ =>
              false
        )

      val argsAndOutput = aggregatableSignatures
        .map(fn =>
          (
            CHType.normalize(fn.arguments(argumentIdx1)),
            CHType.normalize(fn.arguments(argumentIdx2)),
            fn.output
          )
        )
        .distinct

      if aggregatableSignatures.size > 0.66 * twoArgumentsFn.size && argsAndOutput
          .map(_._1)
          .distinct
          .size * argsAndOutput.map(_._2).distinct.size == argsAndOutput.size
      then
        logger.trace(
          s"aggregateArrayArgumentsWithTupleArrayOutput - we may have the ability to aggregate arguments $argumentIdx1 and $argumentIdx2"
        )

        if twoArgumentsFnAggregated.nonEmpty then
          logger.warn(
            "aggregateArrayArgumentsWithTupleArrayOutput - Multiple combination of 2 arguments can be used for aggregating signatures, avoiding aggreggating those signatures"
          )
          cancelAggregationTwoArgumentsFn = true

        twoArgumentsFnAggregated = aggregateWithGenericTypes(
          aggregatableSignatures,
          argumentIdx1,
          argumentIdx2,
          typeSelector1 = fn => fn.arguments(argumentIdx1),
          typeSelector2 = fn => fn.arguments(argumentIdx2),
          aggregatedTypeGenerator1 = t => CHType.normalize(t).asInstanceOf[CHSpecialType.Array].innerType,
          aggregatedTypeGenerator2 = t => CHType.normalize(t).asInstanceOf[CHSpecialType.Array].innerType,
          inputTypeGenerator1 = t => CHSpecialType.Array(t),
          inputTypeGenerator2 = t => CHSpecialType.Array(t),
          outputTypeGenerator = (t1, t2) => CHSpecialType.Tuple(Seq(CHSpecialType.Array(t1), CHSpecialType.Array(t2)))
        ) ++ nonAggregatableSignatures
    }
    if twoArgumentsFnAggregated.nonEmpty && !cancelAggregationTwoArgumentsFn then
      aggregatedSignatures ++= twoArgumentsFnAggregated
    else aggregatedSignatures ++= twoArgumentsFn

    // Testing three arguments
    var threeArgumentsFnAggregated: Seq[CHFunctionIO] = Nil
    var cancelAggregationThreeArgumentsFn = false
    Range.apply(0, threeArgumentsFn.headOption.map(_.arguments.size).getOrElse(0) - 2).foreach { argumentIdx1 =>
      // We assume arguments are provided in order
      val argumentIdx2 = argumentIdx1 + 1
      val argumentIdx3 = argumentIdx1 + 2

      val (aggregatableSignatures, nonAggregatableSignatures) =
        threeArgumentsFn.partition(sig =>
          (
            CHType.normalize(sig.arguments(argumentIdx1)),
            CHType.normalize(sig.arguments(argumentIdx2)),
            CHType.normalize(sig.arguments(argumentIdx3)),
            sig.output
          ) match
            case (
                  CHSpecialType.Array(t1),
                  CHSpecialType.Array(t2),
                  CHSpecialType.Array(t3),
                  CHSpecialType.Tuple(
                    Seq(CHSpecialType.Array(outputT1), CHSpecialType.Array(outputT2), CHSpecialType.Array(outputT3))
                  )
                ) =>
              equalOrUnknown(t1, outputT1) && equalOrUnknown(t2, outputT2) && equalOrUnknown(t3, outputT3)
            case _ =>
              false
        )

      val argsAndOutput = aggregatableSignatures
        .map(fn =>
          (
            CHType.normalize(fn.arguments(argumentIdx1)),
            CHType.normalize(fn.arguments(argumentIdx2)),
            CHType.normalize(fn.arguments(argumentIdx3)),
            fn.output
          )
        )
        .distinct

      if aggregatableSignatures.size > 0.66 * threeArgumentsFn.size && argsAndOutput
          .map(_._1)
          .distinct
          .size * argsAndOutput.map(_._2).distinct.size * argsAndOutput.map(_._3).distinct.size == argsAndOutput.size
      then
        logger.trace(
          s"aggregateArrayArgumentsWithTupleArrayNullableOutput - we may have the ability to aggregate arguments $argumentIdx1, $argumentIdx2 and $argumentIdx3"
        )

        if threeArgumentsFnAggregated.nonEmpty then
          logger.warn(
            "aggregateArrayArgumentsWithTupleArrayOutput - Multiple combination of 2 arguments can be used for aggregating signatures, avoiding aggreggating those signatures"
          )
          cancelAggregationThreeArgumentsFn = true

        threeArgumentsFnAggregated = aggregateWithGenericTypes(
          aggregatableSignatures,
          argumentIdx1,
          argumentIdx2,
          argumentIdx3,
          typeSelector1 = fn => fn.arguments(argumentIdx1),
          typeSelector2 = fn => fn.arguments(argumentIdx2),
          typeSelector3 = fn => fn.arguments(argumentIdx3),
          aggregatedTypeGenerator1 = t => CHType.normalize(t).asInstanceOf[CHSpecialType.Array].innerType,
          aggregatedTypeGenerator2 = t => CHType.normalize(t).asInstanceOf[CHSpecialType.Array].innerType,
          aggregatedTypeGenerator3 = t => CHType.normalize(t).asInstanceOf[CHSpecialType.Array].innerType,
          inputTypeGenerator1 = t => CHSpecialType.Array(t),
          inputTypeGenerator2 = t => CHSpecialType.Array(t),
          inputTypeGenerator3 = t => CHSpecialType.Array(t),
          outputTypeGenerator = (t1, t2, t3) =>
            CHSpecialType.Tuple(Seq(CHSpecialType.Array(t1), CHSpecialType.Array(t2), CHSpecialType.Array(t3)))
        ) ++ nonAggregatableSignatures
    }
    if threeArgumentsFnAggregated.nonEmpty && !cancelAggregationThreeArgumentsFn then
      aggregatedSignatures ++= threeArgumentsFnAggregated
    else aggregatedSignatures ++= threeArgumentsFn

    aggregatedSignatures

  private def aggregateWithGenericType(
      functions: Seq[CHFunctionIO],
      argumentIdx: Int,
      typeSelector: CHFunctionIO => CHType,
      aggregatedTypeGenerator: CHType => CHType,
      inputTypeGenerator: CHType => CHType,
      outputTypeGenerator: CHType => CHType
  )(using supportJson: SupportJson): Seq[CHFunctionIO] =
    if functions.map(typeSelector).distinct.size <= 1
    then functions
    else
      functions
        .groupBy(sig =>
          (
            sig.parameters,
            sig.arguments.zipWithIndex.collect { case (arg, idx) if idx < argumentIdx => arg },
            sig.arguments.zipWithIndex.collect { case (arg, idx) if idx > argumentIdx => arg }
          )
        )
        .toSeq
        .flatMap { case (groupingKey, groupedSignatures) =>
          val types = groupedSignatures.map(typeSelector).toSet
          val aggregatedTypes = CHType.mergeInputTypes(types, supportJson = supportJson).toSeq

          if aggregatedTypes.size < types.size then
            aggregatedTypes.map(aggregatedType =>
              val genericType =
                aggregatedTypeGenerator(aggregatedType) match
                  case t: CHAggregatedType =>
                    CHSpecialType.GenericType(s"T${functions.head.getGenericTypes().size + 1}", t)
                  case t => t

              new CHFunctionIO:
                override def kind: String = groupedSignatures.head.kind
                override def parameters: Seq[CHType] = groupedSignatures.head.parameters
                override def arguments: Seq[CHType] =
                  (groupingKey._2 :+ inputTypeGenerator(genericType)) ++ groupingKey._3
                override def repeatedParameterIdxOpt: Option[Int] = groupedSignatures.head.repeatedParameterIdxOpt
                override def repeatedArgumentIdxOpt: Option[Int] = groupedSignatures.head.repeatedArgumentIdxOpt
                override def isParametric: Boolean = groupedSignatures.head.isParametric
                override def output: CHType = outputTypeGenerator(genericType)
            )
          else groupedSignatures
        }

  private def aggregateWithGenericTypes(
      functions: Seq[CHFunctionIO],
      argumentIdx1: Int,
      argumentIdx2: Int,
      typeSelector1: CHFunctionIO => CHType,
      typeSelector2: CHFunctionIO => CHType,
      aggregatedTypeGenerator1: CHType => CHType,
      aggregatedTypeGenerator2: CHType => CHType,
      inputTypeGenerator1: CHType => CHType,
      inputTypeGenerator2: CHType => CHType,
      outputTypeGenerator: (CHType, CHType) => CHType
  )(using supportJson: SupportJson): Seq[CHFunctionIO] =
    if functions.map(f => (typeSelector1(f), typeSelector2(f))).distinct.size <= 1
    then functions
    else
      val argsByIdx = Seq(
        (argumentIdx1, typeSelector1, aggregatedTypeGenerator1, inputTypeGenerator1),
        (argumentIdx2, typeSelector2, aggregatedTypeGenerator2, inputTypeGenerator2)
      )
      val indexOrder = Seq(argumentIdx1, argumentIdx2).zipWithIndex.sortBy(_._1).map((_, idx) => idx)

      val argumentIdxT1 = argsByIdx(indexOrder(0))._1
      val argumentIdxT2 = argsByIdx(indexOrder(1))._1
      val typeSelectorT1 = argsByIdx(indexOrder(0))._2
      val typeSelectorT2 = argsByIdx(indexOrder(1))._2
      val aggregatedTypeGeneratorT1 = argsByIdx(indexOrder(0))._3
      val aggregatedTypeGeneratorT2 = argsByIdx(indexOrder(1))._3
      val inputTypeGeneratorT1 = argsByIdx(indexOrder(0))._4
      val inputTypeGeneratorT2 = argsByIdx(indexOrder(1))._4

      functions
        .groupBy(sig =>
          (
            sig.parameters,
            sig.arguments.zipWithIndex.collect { case (arg, idx) if idx < argumentIdxT1 => arg },
            sig.arguments.zipWithIndex.collect { case (arg, idx) if idx > argumentIdxT1 && idx < argumentIdxT2 => arg },
            sig.arguments.zipWithIndex.collect { case (arg, idx) if idx > argumentIdxT2 => arg }
          )
        )
        .toSeq
        .flatMap { case (groupingKey, groupedSignatures) =>
          // We will handle two difference cases
          // 1. Generic types T1 and T2 are the same.
          // 2. For all type part of T1, T2 is exactly the same Set of types

          if groupedSignatures.forall(f => typeSelectorT1(f) == typeSelectorT2(f)) then
            val types = groupedSignatures.map(typeSelectorT1).toSet
            val aggregatedTypes = CHType.mergeInputTypes(types, supportJson = supportJson).toSeq

            if aggregatedTypes.size < types.size then
              aggregatedTypes.map(aggregatedType =>
                val genericType =
                  aggregatedTypeGeneratorT1(aggregatedType) match
                    case t: CHAggregatedType =>
                      CHSpecialType.GenericType(s"T${functions.head.getGenericTypes().size + 1}", t)
                    case t => t

                new CHFunctionIO:
                  override def kind: String = groupedSignatures.head.kind
                  override def parameters: Seq[CHType] = groupedSignatures.head.parameters
                  override def arguments: Seq[CHType] =
                    (groupingKey._2 :+ inputTypeGeneratorT1(genericType)) ++
                      (groupingKey._3 :+ inputTypeGeneratorT2(genericType)) ++
                      groupingKey._4
                  override def repeatedParameterIdxOpt: Option[Int] = groupedSignatures.head.repeatedParameterIdxOpt
                  override def repeatedArgumentIdxOpt: Option[Int] = groupedSignatures.head.repeatedArgumentIdxOpt
                  override def isParametric: Boolean = groupedSignatures.head.isParametric
                  override def output: CHType = outputTypeGenerator(genericType, genericType)
              )
            else groupedSignatures
          else if groupedSignatures
              .map(typeSelectorT1)
              .distinct
              .size * groupedSignatures.map(typeSelectorT2).distinct.size == groupedSignatures.size
          then
            val types1 = groupedSignatures.map(typeSelectorT1).toSet
            val types2 = groupedSignatures.map(typeSelectorT2).toSet
            val aggregatedTypes1 = CHType.mergeInputTypes(types1, supportJson = supportJson).toSeq
            val aggregatedTypes2 = CHType.mergeInputTypes(types2, supportJson = supportJson).toSeq

            if aggregatedTypes1.size < types1.size || aggregatedTypes2.size < types2.size then
              for
                aggregatedType1 <- aggregatedTypes1
                aggregatedType2 <- aggregatedTypes2
              yield
                val (genericType1, genericType2) =
                  (aggregatedTypeGeneratorT1(aggregatedType1), aggregatedTypeGeneratorT2(aggregatedType2)) match
                    case Tuple2(t1, t2) if t1.isInstanceOf[CHAggregatedType] && t2.isInstanceOf[CHAggregatedType] =>
                      (
                        CHSpecialType.GenericType(s"T${functions.head.getGenericTypes().size + 1}", t1),
                        CHSpecialType.GenericType(s"T${functions.head.getGenericTypes().size + 2}", t2)
                      )
                    case Tuple2(t1, t2) if t1.isInstanceOf[CHAggregatedType] =>
                      (
                        CHSpecialType.GenericType(s"T${functions.head.getGenericTypes().size + 1}", t1),
                        t2
                      )
                    case Tuple2(t1, t2) if t2.isInstanceOf[CHAggregatedType] =>
                      (
                        t1,
                        CHSpecialType.GenericType(s"T${functions.head.getGenericTypes().size + 1}", t2)
                      )
                    case Tuple2(t1, t2) => (t1, t2)

                new CHFunctionIO:
                  override def kind: String = groupedSignatures.head.kind
                  override def parameters: Seq[CHType] = groupedSignatures.head.parameters
                  override def arguments: Seq[CHType] =
                    (groupingKey._2 :+ inputTypeGeneratorT1(genericType1)) ++
                      (groupingKey._3 :+ inputTypeGeneratorT2(genericType2)) ++
                      groupingKey._4
                  override def repeatedParameterIdxOpt: Option[Int] = groupedSignatures.head.repeatedParameterIdxOpt
                  override def repeatedArgumentIdxOpt: Option[Int] = groupedSignatures.head.repeatedArgumentIdxOpt
                  override def isParametric: Boolean = groupedSignatures.head.isParametric
                  override def output: CHType = outputTypeGenerator(genericType1, genericType2)
            else groupedSignatures
          else groupedSignatures
        }

  private def aggregateWithGenericTypes(
      functions: Seq[CHFunctionIO],
      argumentIdx1: Int,
      argumentIdx2: Int,
      argumentIdx3: Int,
      typeSelector1: CHFunctionIO => CHType,
      typeSelector2: CHFunctionIO => CHType,
      typeSelector3: CHFunctionIO => CHType,
      aggregatedTypeGenerator1: CHType => CHType,
      aggregatedTypeGenerator2: CHType => CHType,
      aggregatedTypeGenerator3: CHType => CHType,
      inputTypeGenerator1: CHType => CHType,
      inputTypeGenerator2: CHType => CHType,
      inputTypeGenerator3: CHType => CHType,
      outputTypeGenerator: (CHType, CHType, CHType) => CHType
  )(using supportJson: SupportJson): Seq[CHFunctionIO] =
    if functions.map(f => (typeSelector1(f), typeSelector2(f))).distinct.size <= 1
    then functions
    else
      val argsByIdx = Seq(
        (argumentIdx1, typeSelector1, aggregatedTypeGenerator1, inputTypeGenerator1),
        (argumentIdx2, typeSelector2, aggregatedTypeGenerator2, inputTypeGenerator2),
        (argumentIdx3, typeSelector3, aggregatedTypeGenerator3, inputTypeGenerator3)
      )
      val indexOrder = Seq(argumentIdx1, argumentIdx2, argumentIdx3).zipWithIndex.sortBy(_._1).map((_, idx) => idx)

      val argumentIdxT1 = argsByIdx(indexOrder(0))._1
      val argumentIdxT2 = argsByIdx(indexOrder(1))._1
      val argumentIdxT3 = argsByIdx(indexOrder(2))._1
      val typeSelectorT1 = argsByIdx(indexOrder(0))._2
      val typeSelectorT2 = argsByIdx(indexOrder(1))._2
      val typeSelectorT3 = argsByIdx(indexOrder(2))._2
      val aggregatedTypeGeneratorT1 = argsByIdx(indexOrder(0))._3
      val aggregatedTypeGeneratorT2 = argsByIdx(indexOrder(1))._3
      val aggregatedTypeGeneratorT3 = argsByIdx(indexOrder(2))._3
      val inputTypeGeneratorT1 = argsByIdx(indexOrder(0))._4
      val inputTypeGeneratorT2 = argsByIdx(indexOrder(1))._4
      val inputTypeGeneratorT3 = argsByIdx(indexOrder(2))._4

      functions
        .groupBy(sig =>
          (
            sig.parameters,
            sig.arguments.zipWithIndex.collect { case (arg, idx) if idx < argumentIdxT1 => arg },
            sig.arguments.zipWithIndex.collect { case (arg, idx) if idx > argumentIdxT1 && idx < argumentIdxT2 => arg },
            sig.arguments.zipWithIndex.collect { case (arg, idx) if idx > argumentIdxT2 && idx < argumentIdxT3 => arg },
            sig.arguments.zipWithIndex.collect { case (arg, idx) if idx > argumentIdxT3 => arg }
          )
        )
        .toSeq
        .flatMap { case (groupingKey, groupedSignatures) =>
          // We will handle two difference cases
          // 1. Generic types T1 T2 and T3 are the same.
          // 2. For all type part of T1, T2 and T3 are both exactly the same Set of types

          if groupedSignatures
              .forall(f => typeSelectorT1(f) == typeSelectorT2(f) && typeSelectorT1(f) == typeSelectorT3(f))
          then
            val types = groupedSignatures.map(typeSelectorT1).toSet
            val aggregatedTypes = CHType.mergeInputTypes(types, supportJson = supportJson).toSeq

            if aggregatedTypes.size < types.size then
              aggregatedTypes.map(aggregatedType =>
                val genericType =
                  aggregatedTypeGeneratorT1(aggregatedType) match
                    case t: CHAggregatedType =>
                      CHSpecialType.GenericType(s"T${functions.head.getGenericTypes().size + 1}", t)
                    case t => t

                new CHFunctionIO:
                  override def kind: String = groupedSignatures.head.kind
                  override def parameters: Seq[CHType] = groupedSignatures.head.parameters
                  override def arguments: Seq[CHType] =
                    (groupingKey._2 :+ inputTypeGeneratorT1(genericType)) ++
                      (groupingKey._3 :+ inputTypeGeneratorT2(genericType)) ++
                      (groupingKey._4 :+ inputTypeGeneratorT3(genericType)) ++
                      groupingKey._5
                  override def repeatedParameterIdxOpt: Option[Int] = groupedSignatures.head.repeatedParameterIdxOpt
                  override def repeatedArgumentIdxOpt: Option[Int] = groupedSignatures.head.repeatedArgumentIdxOpt
                  override def isParametric: Boolean = groupedSignatures.head.isParametric
                  override def output: CHType = outputTypeGenerator(genericType, genericType, genericType)
              )
            else groupedSignatures
          else if groupedSignatures.map(typeSelectorT1).distinct.size *
              groupedSignatures.map(typeSelectorT2).distinct.size *
              groupedSignatures.map(typeSelectorT3).distinct.size == groupedSignatures.size
          then
            val types1 = groupedSignatures.map(typeSelectorT1).toSet
            val types2 = groupedSignatures.map(typeSelectorT2).toSet
            val types3 = groupedSignatures.map(typeSelectorT3).toSet
            val aggregatedTypes1 = CHType.mergeInputTypes(types1, supportJson = supportJson).toSeq
            val aggregatedTypes2 = CHType.mergeInputTypes(types2, supportJson = supportJson).toSeq
            val aggregatedTypes3 = CHType.mergeInputTypes(types3, supportJson = supportJson).toSeq

            if aggregatedTypes1.size < types1.size || aggregatedTypes2.size < types2.size || aggregatedTypes3.size < types3.size
            then
              for
                aggregatedType1 <- aggregatedTypes1
                aggregatedType2 <- aggregatedTypes2
                aggregatedType3 <- aggregatedTypes3
              yield
                var genericTypeIdx = groupedSignatures.head.getGenericTypes().size + 1
                val genericType1 =
                  aggregatedType1 match
                    case t if types1.contains(t) => aggregatedTypeGeneratorT1(t) // Type was not aggregated
                    case t => // Type was aggregated
                      val genericType = createGenericType(aggregatedTypeGeneratorT1(t), genericTypeIdx)
                      genericTypeIdx += 1
                      genericType

                val genericType2 =
                  aggregatedType2 match
                    case t if types2.contains(t) => aggregatedTypeGeneratorT2(t) // Type was not aggregated
                    case t => // Type was aggregated
                      val genericType = createGenericType(aggregatedTypeGeneratorT2(t), genericTypeIdx)
                      genericTypeIdx += 1
                      genericType

                val genericType3 =
                  aggregatedType3 match
                    case t if types3.contains(t) => aggregatedTypeGeneratorT3(t) // Type was not aggregated
                    case t => // Type was aggregated
                      val genericType = createGenericType(aggregatedTypeGeneratorT3(t), genericTypeIdx)
                      genericTypeIdx += 1
                      genericType

                new CHFunctionIO:
                  override def kind: String = groupedSignatures.head.kind
                  override def parameters: Seq[CHType] = groupedSignatures.head.parameters
                  override def arguments: Seq[CHType] =
                    (groupingKey._2 :+ inputTypeGeneratorT1(genericType1)) ++
                      (groupingKey._3 :+ inputTypeGeneratorT2(genericType2)) ++
                      (groupingKey._4 :+ inputTypeGeneratorT3(genericType3)) ++
                      groupingKey._5
                  override def repeatedParameterIdxOpt: Option[Int] = groupedSignatures.head.repeatedParameterIdxOpt
                  override def repeatedArgumentIdxOpt: Option[Int] = groupedSignatures.head.repeatedArgumentIdxOpt
                  override def isParametric: Boolean = groupedSignatures.head.isParametric
                  override def output: CHType = outputTypeGenerator(genericType1, genericType2, genericType3)
            else groupedSignatures
          else groupedSignatures
        }

  private def createGenericType(aggregatedType: CHType, genericTypeIdx: Int): CHType =
    // We want the generic type to be a Scalar super type (e.g. Any, StringLike, Number)
    // if we have aggregated a special type, we need to dig into it to find the Scalar.
    // That's the case for Array, Bitmap, Map, Tuple, ...

    // Create a generic based on the Scalar type
    val converter = CHTypeConverter.getConverter(Seq(aggregatedType))
    val genericScalarType =
      converter.extract(aggregatedType) match
        case t: CHAggregatedType => CHSpecialType.GenericType(s"T$genericTypeIdx", t)
        case t                   => t

    // Re-create our aggregated type, using the generic type
    converter.wrap(genericScalarType)

  sealed private[CHFunctionIOAggregator] trait CHTypeConverter:
    def extract(t: CHType): CHType
    def wrap(t: CHType): CHType

    def compatibleConverters(): Seq[CHTypeConverter] = Seq(this)

  object CHTypeConverter:
    private[CHFunctionIOAggregator] def getConverter(types: Seq[CHType]) =
      if types.forall(_.name.startsWith("Array(Array(")) then CHTypeConverter.ArrayArrayTypeConverter
      else if types.forall(t =>
          t.name.startsWith("Array(Tuple(") &&
            t.asInstanceOf[CHSpecialType.Array].innerType.asInstanceOf[CHSpecialType.Tuple].innerTypes.size == 2 &&
            t.asInstanceOf[CHSpecialType.Array]
              .innerType
              .asInstanceOf[CHSpecialType.Tuple]
              .innerTypes
              .head == CHFuzzableType.StringType
        )
      then CHTypeConverter.ArrayTupleStringTypeConverter(Nil)
      else if types.forall(t =>
          t.name.startsWith("Array(Tuple(") &&
            t.asInstanceOf[CHSpecialType.Array].innerType.asInstanceOf[CHSpecialType.Tuple].innerTypes.size == 1
        )
      then CHTypeConverter.ArrayTupleTypeConverter(Nil)
      else if types.forall(_.name.startsWith("Array(")) then CHTypeConverter.ArrayTypeConverter
      else if types.forall(_.name.startsWith("Bitmap(")) then CHTypeConverter.BitmapTypeConverter
      else if types.forall(_.name.startsWith("LowCardinality(Nullable(")) then
        CHTypeConverter.LowCardinalityNullableTypeConverter
      else if types.forall(_.name.startsWith("LowCardinality(")) then CHTypeConverter.LowCardinalityTypeConverter
      else if types.forall(_.name.startsWith("Map(")) then CHTypeConverter.MapTypeConverter(CHSpecialType.UnknownType)
      else if types.forall(_.name.startsWith("Nullable(")) then CHTypeConverter.NullableTypeConverter
      else if types.forall(t =>
          t.name.startsWith("Tuple(") &&
            t.asInstanceOf[CHSpecialType.Tuple].innerTypes.size == 1
        )
      then CHTypeConverter.TupleTypeConverter(Nil)
      else CHTypeConverter.ScalarTypeConverter

    private[CHFunctionIOAggregator] case object ArrayArrayTypeConverter extends CHTypeConverter:
      override def extract(t: CHType): CHType =
        t.asInstanceOf[CHSpecialType.Array].innerType.asInstanceOf[CHSpecialType.Array].innerType
      override def wrap(t: CHType): CHType = CHSpecialType.Array(CHSpecialType.Array(t))

      override def compatibleConverters(): Seq[CHTypeConverter] = ArrayTypeConverter.compatibleConverters() :+ this

    private[CHFunctionIOAggregator] case class ArrayTupleStringTypeConverter(tail: Seq[CHType]) extends CHTypeConverter:
      override def extract(t: CHType): CHType =
        t.asInstanceOf[CHSpecialType.Array].innerType.asInstanceOf[CHSpecialType.Tuple].innerTypes(1)
      override def wrap(t: CHType): CHType =
        CHSpecialType.Array(CHSpecialType.Tuple(CHFuzzableType.StringType +: t +: tail))

      override def compatibleConverters(): Seq[CHTypeConverter] = ArrayTypeConverter.compatibleConverters() :+ this

    private[CHFunctionIOAggregator] case class ArrayTupleTypeConverter(tail: Seq[CHType]) extends CHTypeConverter:
      override def extract(t: CHType): CHType =
        t.asInstanceOf[CHSpecialType.Array].innerType.asInstanceOf[CHSpecialType.Tuple].innerTypes.head
      override def wrap(t: CHType): CHType = CHSpecialType.Array(CHSpecialType.Tuple(t +: tail))

      override def compatibleConverters(): Seq[CHTypeConverter] = ArrayTypeConverter.compatibleConverters() :+ this

    private[CHFunctionIOAggregator] case object ArrayTypeConverter extends CHTypeConverter:
      override def extract(t: CHType): CHType = t.asInstanceOf[CHSpecialType.Array].innerType
      override def wrap(t: CHType): CHType = CHSpecialType.Array(t)

    private[CHFunctionIOAggregator] case object BitmapTypeConverter extends CHTypeConverter:
      override def extract(t: CHType): CHType = t.asInstanceOf[CHSpecialType.Bitmap].innerType
      override def wrap(t: CHType): CHType = CHSpecialType.Bitmap(t)

    private[CHFunctionIOAggregator] case object LowCardinalityNullableTypeConverter extends CHTypeConverter:
      override def extract(t: CHType): CHType =
        t.asInstanceOf[CHSpecialType.LowCardinality].innerType.asInstanceOf[CHSpecialType.Nullable].innerType
      override def wrap(t: CHType): CHType = CHSpecialType.LowCardinality(CHSpecialType.Nullable(t))

      override def compatibleConverters(): Seq[CHTypeConverter] =
        LowCardinalityTypeConverter.compatibleConverters() :+ this

    private[CHFunctionIOAggregator] case object LowCardinalityTypeConverter extends CHTypeConverter:
      override def extract(t: CHType): CHType = t.asInstanceOf[CHSpecialType.LowCardinality].innerType
      override def wrap(t: CHType): CHType = CHSpecialType.LowCardinality(t)

    private[CHFunctionIOAggregator] case class MapTypeConverter(valueType: CHType) extends CHTypeConverter:
      override def extract(t: CHType): CHType = t.asInstanceOf[CHSpecialType.Map].keyType
      override def wrap(t: CHType): CHType = CHSpecialType.Map(t, valueType)

    private[CHFunctionIOAggregator] case object NullableTypeConverter extends CHTypeConverter:
      override def extract(t: CHType): CHType = t.asInstanceOf[CHSpecialType.Nullable].innerType
      override def wrap(t: CHType): CHType = CHSpecialType.Nullable(t)

    private[CHFunctionIOAggregator] case object ScalarTypeConverter extends CHTypeConverter:
      override def extract(t: CHType): CHType = t
      override def wrap(t: CHType): CHType = t

    private[CHFunctionIOAggregator] case class TupleArrayNullableTypeConverter(tail: Seq[CHType]) extends CHTypeConverter:
      override def extract(t: CHType): CHType = t
        .asInstanceOf[CHSpecialType.Tuple]
        .innerTypes
        .head
        .asInstanceOf[CHSpecialType.Array]
        .innerType
        .asInstanceOf[CHSpecialType.Nullable]
        .innerType
      override def wrap(t: CHType): CHType = CHSpecialType.Tuple(CHSpecialType.Array(CHSpecialType.Nullable(t)) +: tail)

      override def compatibleConverters(): Seq[CHTypeConverter] =
        TupleArrayTypeConverter(tail).compatibleConverters() :+ this

    private[CHFunctionIOAggregator] case class TupleArrayTypeConverter(tail: Seq[CHType]) extends CHTypeConverter:
      override def extract(t: CHType): CHType =
        t.asInstanceOf[CHSpecialType.Tuple].innerTypes.head.asInstanceOf[CHSpecialType.Array].innerType
      override def wrap(t: CHType): CHType = CHSpecialType.Tuple(CHSpecialType.Array(t) +: tail)

      override def compatibleConverters(): Seq[CHTypeConverter] =
        TupleTypeConverter(tail).compatibleConverters() :+ this

    private[CHFunctionIOAggregator] case class TupleTypeConverter(tail: Seq[CHType]) extends CHTypeConverter:
      override def extract(t: CHType): CHType = t.asInstanceOf[CHSpecialType.Tuple].innerTypes.head
      override def wrap(t: CHType): CHType = CHSpecialType.Tuple(t +: tail)

  opaque type InputMetadata = (Int, CHTypeConverter)
  extension (in: InputMetadata)
    def idx: Int = in._1
    def converter: CHTypeConverter = in._2

  opaque type SupportJson = Boolean