package com.amendil.signature.entities

import com.amendil.common.entities.*
import com.amendil.common.entities.`type`.*
import com.amendil.signature.Settings

sealed trait CHFunctionIO:
  def kind: String
  def parameters: Seq[CHType] = Nil
  def arguments: Seq[CHType] = Nil
  def repeatedParameterIdxOpt: Option[Int] = None
  def repeatedArgumentIdxOpt: Option[Int] = None
  def isParametric: Boolean = false
  def output: CHType

  def asString(): String =
    val genericTypes = getGenericTypes()
    val genericTypesStr =
      if genericTypes.isEmpty then ""
      else genericTypes.sortBy(_.typeName).map(t => s"${t.typeName} <: ${t.superType.name}").mkString("[", ", ", "] ")

    val parametersStr =
      if !isParametric then ""
      else
        s"(${parameters.zipWithIndex
            .map((param, idx) => param.name + (if repeatedParameterIdxOpt.exists(_ == idx) then "..." else ""))
            .mkString(", ")})"

    val argumentsStr =
      s"(${arguments.zipWithIndex
          .map((param, idx) => param.name + (if repeatedArgumentIdxOpt.exists(_ == idx) then "..." else ""))
          .mkString(", ")})"

    s"$genericTypesStr$parametersStr$argumentsStr => ${output.name}".stripMargin

  protected def getGenericTypes(): Seq[CHSpecialType.GenericType] =
    val genericTypes = getGenericTypes(output).distinct
    require(genericTypes.distinctBy(_.typeName).size == genericTypes.size)
    genericTypes

  private def getGenericTypes(t: CHType): Seq[CHSpecialType.GenericType] =
    t match
      case t: CHSpecialType.GenericType    => Seq(t)
      case arr: CHSpecialType.Array        => getGenericTypes(arr.innerType)
      case l: CHSpecialType.LowCardinality => getGenericTypes(l.innerType)
      case m: CHSpecialType.Map            => getGenericTypes(m.keyType) ++ getGenericTypes(m.valueType)
      case n: CHSpecialType.Nullable       => getGenericTypes(n.innerType)
      case t: CHSpecialType.Tuple          => t.innerTypes.map(getGenericTypes).flatten
      case t: CHSpecialType.TupleN         => getGenericTypes(t.innerType)
      case _                               => Nil

sealed trait CHFunctionNIO extends CHFunctionIO:
  override def repeatedArgumentIdxOpt: Option[Int] = Some(this.arguments.size - 1)

sealed trait CHParametricFunction extends CHFunctionIO:
  override def isParametric: Boolean = true

sealed trait CHParametricNFunctionNIO extends CHParametricFunction:
  override def repeatedParameterIdxOpt: Option[Int] = Some(this.parameters.size - 1)
  override def repeatedArgumentIdxOpt: Option[Int] = Some(this.arguments.size - 1)

sealed trait CHParametricNFunctionIO extends CHParametricFunction:
  override def repeatedParameterIdxOpt: Option[Int] = Some(this.parameters.size - 1)

sealed trait CHParametricFunctionNIO extends CHParametricFunction with CHFunctionNIO

object CHFunctionIO:

  def aggregate[T <: CHFunctionIO](functions: Seq[T]): Seq[CHFunctionIO] =
    require(
      functions.map(_.kind).distinct.size <= 1,
      s"Cannot aggregate different kind of functions, but asked to aggregate '${functions.map(_.kind).distinct.sorted.mkString("', '")}' together."
    )

    var deduplicatedSignatures: Seq[CHFunctionIO] = functions
    Range.apply(0, functions.head.arguments.size).foreach { i =>
      deduplicatedSignatures = deduplicatedSignatures
        .groupBy(sig =>
          (
            sig.parameters,
            sig.arguments.zipWithIndex.collect { case (arg, idx) if idx < i => arg },
            sig.arguments.zipWithIndex.collect { case (arg, idx) if idx > i => arg },
            sig.output
          )
        )
        .map { case (groupingKey, groupedSignatures) =>
          val types = groupedSignatures.map(_.arguments(i)).toSet

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
    }

    if deduplicatedSignatures.size < 2 || !Settings.Fuzzer.aggregateSignature then deduplicatedSignatures
    else
      var aggregatedSignatures: Seq[CHFunctionIO] = deduplicatedSignatures
      // First aggregate one input at a time
      Range.apply(0, functions.head.arguments.size).foreach { i =>
        aggregatedSignatures = aggregatedSignatures
          .groupBy(sig =>
            (
              sig.parameters,
              sig.arguments.zipWithIndex.collect { case (arg, idx) if idx < i => arg },
              sig.arguments.zipWithIndex.collect { case (arg, idx) if idx > i => arg },
              sig.output
            )
          )
          .map { case (groupingKey, groupedSignatures) =>
            val types = groupedSignatures.map(_.arguments(i)).toSet

            val aggregatedTypes = CHType.mergeInputTypes(types, supportJson = Settings.Fuzzer.supportJson)

            if types.size == aggregatedTypes.size then groupedSignatures
            else
              aggregatedTypes.map(aggregatedType =>
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
      }

      Range.apply(0, functions.head.parameters.size).foreach { i =>
        aggregatedSignatures = aggregatedSignatures
          .groupBy(sig =>
            (
              sig.parameters.zipWithIndex.collect { case (arg, idx) if idx < i => arg },
              sig.parameters.zipWithIndex.collect { case (arg, idx) if idx > i => arg },
              sig.arguments,
              sig.output
            )
          )
          .map { case (groupingKey, groupedSignatures) =>
            val types = groupedSignatures.map(_.parameters(i)).toSet
            val aggregatedTypes = CHType.mergeInputTypes(types, supportJson = Settings.Fuzzer.supportJson)

            if types.size == aggregatedTypes.size then groupedSignatures
            else
              aggregatedTypes
                .map(aggregatedType =>
                  new CHFunctionIO:
                    override def kind: String = groupedSignatures.head.kind
                    override def parameters: Seq[CHType] = (groupingKey._1 :+ aggregatedType) ++ groupingKey._2
                    override def arguments: Seq[CHType] = groupedSignatures.head.arguments
                    override def repeatedParameterIdxOpt: Option[Int] = groupedSignatures.head.repeatedParameterIdxOpt
                    override def repeatedArgumentIdxOpt: Option[Int] = groupedSignatures.head.repeatedArgumentIdxOpt
                    override def isParametric: Boolean = groupedSignatures.head.isParametric
                    override def output: CHType = groupedSignatures.head.output
                )
                .toSeq
          }
          .flatten
          .toSeq
      }

      // Second aggregate input with same type as output
      Range.apply(0, functions.head.arguments.size).foreach { argumentIdx =>
        val argAndOutput = aggregatedSignatures.map(fn =>
          (
            CHType.getByName(fn.arguments(argumentIdx).name),
            fn.output
          )
        )

        if argAndOutput.forall((i, o) => equalOrUnknown(i, o)) then
          if argAndOutput.forall((i, o) => i.name.startsWith("Array(")) then
            aggregatedSignatures = aggregateWithGenericType(
              aggregatedSignatures,
              argumentIdx = argumentIdx,
              typeSelector = fn => fn.arguments(argumentIdx),
              aggregatedTypeGenerator = t => CHType.getByName(t.name).asInstanceOf[CHSpecialType.Array].innerType,
              inputTypeGenerator = t => CHSpecialType.Array(t),
              outputTypeGenerator = t => CHSpecialType.Array(t)
            )
          else if argAndOutput.forall((i, o) => i.isInstanceOf[CHSpecialType.Map]) then
            val mapValueTypes = argAndOutput.map((_, o) => o.asInstanceOf[CHSpecialType.Map].valueType).distinct
            assume(
              mapValueTypes.size == 1,
              "The value type of each map is supposed to be unique, but found different types."
            )
            val valueType = mapValueTypes.head

            aggregatedSignatures = aggregateWithGenericType(
              aggregatedSignatures,
              argumentIdx = argumentIdx,
              typeSelector = fn => fn.output.asInstanceOf[CHSpecialType.Map],
              aggregatedTypeGenerator = t => CHType.getByName(t.name).asInstanceOf[CHSpecialType.Map].keyType,
              inputTypeGenerator = t => CHSpecialType.Map(t, valueType),
              outputTypeGenerator = t => CHSpecialType.Map(t, valueType)
            )
          else
            aggregatedSignatures = aggregateWithGenericType(
              aggregatedSignatures,
              argumentIdx = argumentIdx,
              typeSelector = fn => fn.arguments(argumentIdx),
              aggregatedTypeGenerator = identity,
              inputTypeGenerator = identity,
              outputTypeGenerator = identity
            )
        else if argAndOutput.forall((i, o) => i.name == CHSpecialType.Array(o).name) then
          val aggregatedSignatures1 =
            aggregateWithGenericType(
              aggregatedSignatures,
              argumentIdx = argumentIdx,
              typeSelector = fn => fn.output,
              aggregatedTypeGenerator = identity,
              inputTypeGenerator = t => CHSpecialType.Array(t),
              outputTypeGenerator = identity
            )
          val aggregatedSignatures2 =
            aggregateWithGenericType(
              aggregatedSignatures,
              argumentIdx = argumentIdx,
              typeSelector = fn => fn.arguments(argumentIdx),
              aggregatedTypeGenerator = t => CHType.getByName(t.name).asInstanceOf[CHSpecialType.Array].innerType,
              inputTypeGenerator = t => CHSpecialType.Array(t),
              outputTypeGenerator = identity
            )
          aggregatedSignatures =
            if aggregatedSignatures1.size < aggregatedSignatures2.size
            then aggregatedSignatures1
            else aggregatedSignatures2
        else if argAndOutput.forall((i, o) => CHSpecialType.Array(i).name == o.name) then
          val aggregatedSignatures1 =
            aggregateWithGenericType(
              aggregatedSignatures,
              argumentIdx = argumentIdx,
              typeSelector = fn => fn.output,
              aggregatedTypeGenerator = t => CHType.getByName(t.name).asInstanceOf[CHSpecialType.Array].innerType,
              inputTypeGenerator = identity,
              outputTypeGenerator = t => CHSpecialType.Array(t)
            )
          val aggregatedSignatures2 =
            aggregateWithGenericType(
              aggregatedSignatures,
              argumentIdx = argumentIdx,
              typeSelector = fn => fn.arguments(argumentIdx),
              aggregatedTypeGenerator = identity,
              inputTypeGenerator = identity,
              outputTypeGenerator = t => CHSpecialType.Array(t)
            )
          aggregatedSignatures =
            if aggregatedSignatures1.size < aggregatedSignatures2.size
            then aggregatedSignatures1
            else aggregatedSignatures2
        else if argAndOutput.forall((i, o) => i.name == CHSpecialType.Map(o, CHSpecialType.UnknownType).name) then
          aggregatedSignatures = aggregateWithGenericType(
            aggregatedSignatures,
            argumentIdx = argumentIdx,
            typeSelector = fn => fn.arguments(argumentIdx),
            aggregatedTypeGenerator = t => CHType.getByName(t.name).asInstanceOf[CHSpecialType.Map].keyType,
            inputTypeGenerator = t => CHSpecialType.Map(t, CHSpecialType.UnknownType),
            outputTypeGenerator = identity
          )
        else if argAndOutput.forall((i, o) =>
            o.isInstanceOf[CHSpecialType.Map] &&
              i.name == o.asInstanceOf[CHSpecialType.Map].keyType.name
          ) && argAndOutput.distinctBy(_._2.asInstanceOf[CHSpecialType.Map].valueType).size == 1
        then
          val mapOutputType = argAndOutput.head.asInstanceOf[CHSpecialType.Map].valueType

          aggregatedSignatures = aggregateWithGenericType(
            aggregatedSignatures,
            argumentIdx = argumentIdx,
            typeSelector = fn => fn.arguments(argumentIdx),
            aggregatedTypeGenerator = identity,
            inputTypeGenerator = identity,
            outputTypeGenerator = t => CHSpecialType.Map(t, mapOutputType)
          )
        else if argAndOutput.forall((i, o) =>
            equalOrUnknown(
              CHSpecialType.Array(CHSpecialType.Tuple(Seq(i, CHSpecialType.UnknownType, CHSpecialType.UnknownType))),
              o
            )
          ) && argAndOutput
            .distinctBy(
              _._2.asInstanceOf[CHSpecialType.Array].innerType.asInstanceOf[CHSpecialType.Tuple].innerTypes.tail
            )
            .size == 1
        then
          val tupleTailTypes = argAndOutput.head._2
            .asInstanceOf[CHSpecialType.Array]
            .innerType
            .asInstanceOf[CHSpecialType.Tuple]
            .innerTypes
            .tail

          aggregatedSignatures = aggregateWithGenericType(
            aggregatedSignatures,
            argumentIdx = argumentIdx,
            typeSelector = fn => fn.arguments(argumentIdx),
            aggregatedTypeGenerator = identity,
            inputTypeGenerator = identity,
            outputTypeGenerator = t => CHSpecialType.Array(CHSpecialType.Tuple(t +: tupleTailTypes))
          )
        else if argAndOutput.forall((i, o) => i.name.startsWith("Array(") && o.isInstanceOf[CHSpecialType.Map]) then
          // I'm sorry if you stumble on a bug to investigate in this part of the codebase.
          // Here are kittens to comfort you: ≡<^_^>≡
          //   ∧,,,,,,∧
          //  (  ̳• · • ̳)
          //  /       づ♡

          val argAndOutputTyped = argAndOutput.map { case (i, o) =>
            (
              CHType.getByName(i.name).asInstanceOf[CHSpecialType.Array],
              o.asInstanceOf[CHSpecialType.Map]
            )
          }

          if argAndOutputTyped.forall { case (iArr, oMap) =>
              (iArr, oMap) match
                case (
                      CHSpecialType.Array(CHSpecialType.Map(k1, CHSpecialType.UnknownType)),
                      CHSpecialType.Map(CHSpecialType.Map(k2, _), _)
                    ) =>
                  k1.name == k2.name
                case _ => iArr.innerType.name == oMap.keyType.name
            } && argAndOutputTyped
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
              // Value type is unique, great, we can aggregate those maps.
              aggregatedSignatures = aggregateWithGenericType(
                aggregatedSignatures,
                argumentIdx = argumentIdx,
                typeSelector = fn => fn.arguments.asInstanceOf[CHSpecialType.Array],
                aggregatedTypeGenerator = t => CHType.getByName(t.name).asInstanceOf[CHSpecialType.Array].innerType,
                inputTypeGenerator = t => CHSpecialType.Array(t),
                outputTypeGenerator = t => CHSpecialType.Map(t, outputMapValueTypes.head)
              )
            else
              // Value type is not unique, but maybe it's defined by another column?
              Range.apply(0, functions.head.arguments.size).filter(_ != argumentIdx).foreach { argumentIdx2 =>
                val argsAndOutput = aggregatedSignatures.map(fn =>
                  (
                    CHType.getByName(fn.arguments(argumentIdx).name),
                    CHType.getByName(fn.arguments(argumentIdx2).name),
                    fn.output.asInstanceOf[CHSpecialType.Map]
                  )
                )

                if argsAndOutput.forall { case (_, j, CHSpecialType.Map(_, outputValueType)) =>
                    j match
                      case CHSpecialType.Array(innerType) => equalOrUnknown(innerType, outputValueType)
                      case CHSpecialType.Map(keyType, valueType) =>
                        outputValueType match
                          case CHSpecialType.Tuple(Seq(t1, t2)) =>
                            equalOrUnknown(keyType, t1) && equalOrUnknown(valueType, t2)
                          case _ => false
                      case _ => false
                  } && argsAndOutput
                    .collect {
                      case (
                            _,
                            CHSpecialType.Map(_, CHSpecialType.UnknownType),
                            CHSpecialType.Map(_, CHSpecialType.Tuple(Seq(_, t2)))
                          ) =>
                        Seq(t2)
                      case (_, CHSpecialType.Map(_, t1), CHSpecialType.Map(_, CHSpecialType.Tuple(Seq(_, t2)))) =>
                        Seq(t1, t2)
                    }
                    .flatten
                    .distinct
                    .size <= 1
                then
                  val mapValueTypes = argsAndOutput.collect {
                    case (_, CHSpecialType.Map(_, t1), CHSpecialType.Map(_, CHSpecialType.Tuple(Seq(_, t2)))) =>
                      Seq(t1, t2)
                  }.flatten
                  val mapValueFinalType =
                    mapValueTypes.find(_ == CHSpecialType.UnknownType).getOrElse(mapValueTypes.head)

                  aggregatedSignatures = aggregateWithGenericTypes(
                    aggregatedSignatures.filter(_.arguments(argumentIdx2).name.startsWith("Array(")),
                    argumentIdx1 = argumentIdx,
                    argumentIdx2 = argumentIdx2,
                    typeSelector1 = fn => fn.arguments(argumentIdx),
                    typeSelector2 = fn => fn.arguments(argumentIdx2),
                    aggregatedTypeGenerator1 =
                      t => CHType.getByName(t.name).asInstanceOf[CHSpecialType.Array].innerType,
                    aggregatedTypeGenerator2 =
                      t => CHType.getByName(t.name).asInstanceOf[CHSpecialType.Array].innerType,
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
                      aggregatedTypeGenerator1 =
                        t => CHType.getByName(t.name).asInstanceOf[CHSpecialType.Array].innerType,
                      aggregatedTypeGenerator2 = t => CHType.getByName(t.name).asInstanceOf[CHSpecialType.Map].keyType,
                      inputTypeGenerator1 = t1 => CHSpecialType.Array(t1),
                      inputTypeGenerator2 = t2 => CHSpecialType.Map(t2, mapValueFinalType),
                      outputTypeGenerator =
                        (t1, t2) => CHSpecialType.Map(t1, CHSpecialType.Tuple(Seq(t2, mapValueFinalType)))
                    )
              }
        else if argAndOutput.forall { (i, o) =>
            i match
              case CHSpecialType.Array(innerType) => equalOrUnknown(innerType, o)
              case CHSpecialType.Map(keyType, valueType) =>
                o match
                  case CHSpecialType.Tuple(Seq(t1, t2)) => equalOrUnknown(keyType, t1) && equalOrUnknown(valueType, t2)
                  case _                                => false
              case _ => false
          } && argAndOutput
            .collect {
              case (
                    CHSpecialType.Map(_, CHSpecialType.UnknownType),
                    CHSpecialType.Tuple(Seq(_, t2))
                  ) =>
                Seq(t2)
              case (CHSpecialType.Map(_, t1), CHSpecialType.Tuple(Seq(_, t2))) => Seq(t1, t2)
            }
            .flatten
            .distinct
            .size <= 1
        then
          val mapValueTypes = argAndOutput.collect { case (CHSpecialType.Map(_, t1), CHSpecialType.Tuple(Seq(_, t2))) =>
            Seq(t1, t2)
          }.flatten
          val mapValueFinalType =
            mapValueTypes.find(_ == CHSpecialType.UnknownType).getOrElse(mapValueTypes.head)

          aggregatedSignatures = aggregateWithGenericType(
            aggregatedSignatures.filter(_.arguments(argumentIdx).name.startsWith("Array(")),
            argumentIdx = argumentIdx,
            typeSelector = fn => fn.arguments(argumentIdx),
            aggregatedTypeGenerator = t => CHType.getByName(t.name).asInstanceOf[CHSpecialType.Array].innerType,
            inputTypeGenerator = t => CHSpecialType.Array(t),
            outputTypeGenerator = t => t
          ) ++
            aggregateWithGenericType(
              aggregatedSignatures.filter(_.arguments(argumentIdx).name.startsWith("Map(")),
              argumentIdx = argumentIdx,
              typeSelector = fn => fn.arguments(argumentIdx),
              aggregatedTypeGenerator = t => CHType.getByName(t.name).asInstanceOf[CHSpecialType.Map].keyType,
              inputTypeGenerator = t => CHSpecialType.Map(t, mapValueFinalType),
              outputTypeGenerator = t => CHSpecialType.Tuple(Seq(t, mapValueFinalType))
            )
      }

      if aggregatedSignatures.size > Settings.Fuzzer.aggregateSignatureThreshold then deduplicatedSignatures
      else aggregatedSignatures

  private def equalOrUnknown(t1: CHType, t2: CHType): Boolean =
    (t1, t2) match
      case (CHSpecialType.Array(innerT1), CHSpecialType.Array(innerT2)) =>
        equalOrUnknown(innerT1, innerT2)
      case (CHSpecialType.Map(k1, v1), CHSpecialType.Map(k2, v2)) =>
        equalOrUnknown(k1, k2) && equalOrUnknown(v1, v2)
      case (CHSpecialType.Tuple(innerTypes1), CHSpecialType.Tuple(innerTypes2)) =>
        innerTypes1.size == innerTypes2.size && innerTypes1.zip(innerTypes2).forall(equalOrUnknown)
      case (CHSpecialType.TupleN(innerT1), CHSpecialType.TupleN(innerT2)) =>
        equalOrUnknown(innerT1, innerT2)
      case (CHSpecialType.UnknownType, _) => true
      case (_, CHSpecialType.UnknownType) => true
      case _                              => t1.name == t2.name

  private def aggregateWithGenericType(
      functions: Seq[CHFunctionIO],
      argumentIdx: Int,
      typeSelector: CHFunctionIO => CHType,
      aggregatedTypeGenerator: CHType => CHType,
      inputTypeGenerator: CHType => CHType,
      outputTypeGenerator: CHType => CHType
  ): Seq[CHFunctionIO] =
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
          val aggregatedTypes = CHType.mergeInputTypes(types, supportJson = Settings.Fuzzer.supportJson).toSeq

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
  ): Seq[CHFunctionIO] =
    if functions.map(f => (typeSelector1(f), typeSelector2(f))).distinct.size <= 1
    then functions
    else
      val (
        argumentIdxT1,
        argumentIdxT2,
        typeSelectorT1,
        typeSelectorT2,
        aggregatedTypeGeneratorT1,
        aggregatedTypeGeneratorT2,
        inputTypeGeneratorT1,
        inputTypeGeneratorT2
      ) =
        if argumentIdx1 < argumentIdx2
        then
          (
            argumentIdx1,
            argumentIdx2,
            typeSelector1,
            typeSelector2,
            aggregatedTypeGenerator1,
            aggregatedTypeGenerator2,
            inputTypeGenerator1,
            inputTypeGenerator2
          )
        else
          (
            argumentIdx2,
            argumentIdx1,
            typeSelector2,
            typeSelector1,
            aggregatedTypeGenerator2,
            aggregatedTypeGenerator1,
            inputTypeGenerator2,
            inputTypeGenerator1
          )

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
            val aggregatedTypes = CHType.mergeInputTypes(types, supportJson = Settings.Fuzzer.supportJson).toSeq

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
            val aggregatedTypes1 = CHType.mergeInputTypes(types1, supportJson = Settings.Fuzzer.supportJson).toSeq
            val aggregatedTypes2 = CHType.mergeInputTypes(types2, supportJson = Settings.Fuzzer.supportJson).toSeq

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

  case class Function0N(argN: CHType, output: CHType) extends CHFunctionNIO:
    val kind = "function0N"
    override val arguments = Seq(argN)

  case class Function0N1(argN: CHType, arg1: CHType, output: CHType) extends CHFunctionIO:
    val kind = "Function0N1"
    override val arguments = Seq(argN, arg1)
    override val repeatedArgumentIdxOpt = Some(0)

  case class Function1N(arg1: CHType, argN: CHType, output: CHType) extends CHFunctionNIO:
    val kind = "function1N"
    override val arguments = Seq(arg1, argN)

  case class Function1N1(arg1: CHType, argN: CHType, arg2: CHType, output: CHType) extends CHFunctionIO:
    val kind = "Function1N1"
    override val arguments = Seq(arg1, argN, arg2)
    override val repeatedArgumentIdxOpt = Some(1)

  case class Function2N(arg1: CHType, arg2: CHType, argN: CHType, output: CHType) extends CHFunctionNIO:
    val kind = "function2N"
    override val arguments = Seq(arg1, arg2, argN)

  case class Function3N(arg1: CHType, arg2: CHType, arg3: CHType, argN: CHType, output: CHType) extends CHFunctionNIO:
    val kind = "function3N"
    override val arguments = Seq(arg1, arg2, arg3, argN)

  case class Function0(output: CHType) extends CHFunctionIO:
    val kind = "function0"

  case class Function1(arg1: CHType, output: CHType) extends CHFunctionIO:
    val kind = "function1"
    override val arguments = Seq(arg1)

  case class Function2(arg1: CHType, arg2: CHType, output: CHType) extends CHFunctionIO:
    val kind = "function2"
    override val arguments = Seq(arg1, arg2)

  case class Function3(arg1: CHType, arg2: CHType, arg3: CHType, output: CHType) extends CHFunctionIO:
    val kind = "function3"
    override val arguments = Seq(arg1, arg2, arg3)

  case class Function4(arg1: CHType, arg2: CHType, arg3: CHType, arg4: CHType, output: CHType) extends CHFunctionIO:
    val kind = "function4"
    override val arguments = Seq(arg1, arg2, arg3, arg4)

  case class Function5(arg1: CHType, arg2: CHType, arg3: CHType, arg4: CHType, arg5: CHType, output: CHType)
      extends CHFunctionIO:
    val kind = "function5"
    override val arguments = Seq(arg1, arg2, arg3, arg4, arg5)

  case class Function6(
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      arg4: CHType,
      arg5: CHType,
      arg6: CHType,
      output: CHType
  ) extends CHFunctionIO:
    val kind = "function6"
    override val arguments = Seq(arg1, arg2, arg3, arg4, arg5, arg6)

  case class Function7(
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      arg4: CHType,
      arg5: CHType,
      arg6: CHType,
      arg7: CHType,
      output: CHType
  ) extends CHFunctionIO:
    val kind = "function7"
    override val arguments = Seq(arg1, arg2, arg3, arg4, arg5, arg6, arg7)

  case class Function8(
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      arg4: CHType,
      arg5: CHType,
      arg6: CHType,
      arg7: CHType,
      arg8: CHType,
      output: CHType
  ) extends CHFunctionIO:
    val kind = "function8"
    override val arguments = Seq(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)

  case class Function9(
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      arg4: CHType,
      arg5: CHType,
      arg6: CHType,
      arg7: CHType,
      arg8: CHType,
      arg9: CHType,
      output: CHType
  ) extends CHFunctionIO:
    val kind = "function9"
    override val arguments = Seq(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)

  case class LambdaArrayFunction0N(lambdaArg: CHSpecialType.LambdaType, argN: CHSpecialType.Array, output: CHType)
      extends CHFunctionNIO:
    val kind = "lambdaArrayFunction0N"
    override val arguments = Seq(lambdaArg, argN)

  case class LambdaArrayFunction1N(
      lambdaArg: CHSpecialType.LambdaType,
      arg1: CHSpecialType.Array,
      argN: CHSpecialType.Array,
      output: CHType
  ) extends CHFunctionNIO:
    val kind = "lambdaArrayFunction1N"
    override val arguments = Seq(lambdaArg, arg1, argN)

  case class LambdaArrayFunction1N1(
      lambdaArg: CHSpecialType.LambdaType,
      arg1: CHSpecialType.Array,
      argN: CHSpecialType.Array,
      arg2: CHType,
      output: CHType
  ) extends CHFunctionIO:
    val kind = "LambdaArrayFunction1N1"
    override val arguments = Seq(lambdaArg, arg1, argN, arg2)
    override val repeatedArgumentIdxOpt = Some(2)

  case class LambdaMapFunction1(lambdaArg: CHSpecialType.LambdaType, arg1: CHSpecialType.Map, output: CHType)
      extends CHFunctionIO:
    val kind = "LambdaMapFunction1"
    override val arguments = Seq(lambdaArg, arg1)

  case class Parametric0NFunction0N(paramArgN: CHType, argN: CHType, output: CHType) extends CHParametricNFunctionNIO:
    val kind = "parametric0NFunction0N"
    override val parameters = Seq(paramArgN)
    override val arguments = Seq(argN)

  case class Parametric0NFunction1N(paramArgN: CHType, arg1: CHType, argN: CHType, output: CHType)
      extends CHParametricNFunctionNIO:
    val kind = "parametric0NFunction1N"
    override val parameters = Seq(paramArgN)
    override val arguments = Seq(arg1, argN)

  case class Parametric0NFunction2N(paramArgN: CHType, arg1: CHType, arg2: CHType, argN: CHType, output: CHType)
      extends CHParametricNFunctionNIO:
    val kind = "parametric0NFunction2N"
    override val parameters = Seq(paramArgN)
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric0NFunction1(paramArgN: CHType, arg1: CHType, output: CHType) extends CHParametricNFunctionIO:
    val kind = "parametric0NFunction1"
    override val parameters = Seq(paramArgN)
    override val arguments = Seq(arg1)

  case class Parametric0NFunction2(paramArgN: CHType, arg1: CHType, arg2: CHType, output: CHType)
      extends CHParametricNFunctionIO:
    val kind = "parametric0NFunction2"
    override val parameters = Seq(paramArgN)
    override val arguments = Seq(arg1, arg2)

  case class Parametric0NFunction3(paramArgN: CHType, arg1: CHType, arg2: CHType, arg3: CHType, output: CHType)
      extends CHParametricNFunctionIO:
    val kind = "parametric0NFunction3"
    override val parameters = Seq(paramArgN)
    override val arguments = Seq(arg1, arg2, arg3)

  case class Parametric1NFunction0N(paramArg1: CHType, paramArgN: CHType, argN: CHType, output: CHType)
      extends CHParametricNFunctionNIO:
    val kind = "parametric1NFunction0N"
    override val parameters = Seq(paramArg1, paramArgN)
    override val arguments = Seq(argN)

  case class Parametric1NFunction1N(
      paramArg1: CHType,
      paramArgN: CHType,
      arg1: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric1NFunction1N"
    override val parameters = Seq(paramArg1, paramArgN)
    override val arguments = Seq(arg1, argN)

  case class Parametric1NFunction2N(
      paramArg1: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric1NFunction2N"
    override val parameters = Seq(paramArg1, paramArgN)
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric1NFunction3N(
      paramArg1: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric1NFunction3N"
    override val parameters = Seq(paramArg1, paramArgN)
    override val arguments = Seq(arg1, arg2, arg3, argN)

  case class Parametric1NFunction1(paramArg1: CHType, paramArgN: CHType, arg1: CHType, output: CHType)
      extends CHParametricNFunctionIO:
    val kind = "parametric1NFunction1"
    override val parameters = Seq(paramArg1, paramArgN)
    override val arguments = Seq(arg1)

  case class Parametric1NFunction2(
      paramArg1: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      output: CHType
  ) extends CHParametricNFunctionIO:
    val kind = "parametric1NFunction2"
    override val parameters = Seq(paramArg1, paramArgN)
    override val arguments = Seq(arg1, arg2)

  case class Parametric1NFunction3(
      paramArg1: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      output: CHType
  ) extends CHParametricNFunctionIO:
    val kind = "parametric1NFunction3"
    override val parameters = Seq(paramArg1, paramArgN)
    override val arguments = Seq(arg1, arg2, arg3)

  case class Parametric2NFunction0N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArgN: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric2NFunction0N"
    override val parameters = Seq(paramArg1, paramArg2, paramArgN)
    override val arguments = Seq(argN)

  case class Parametric2NFunction1N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArgN: CHType,
      arg1: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric2NFunction1N"
    override val parameters = Seq(paramArg1, paramArg2, paramArgN)
    override val arguments = Seq(arg1, argN)

  case class Parametric2NFunction2N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric2NFunction2N"
    override val parameters = Seq(paramArg1, paramArg2, paramArgN)
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric2NFunction1(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArgN: CHType,
      arg1: CHType,
      output: CHType
  ) extends CHParametricNFunctionIO:
    val kind = "parametric2NFunction1"
    override val parameters = Seq(paramArg1, paramArg2, paramArgN)
    override val arguments = Seq(arg1)

  case class Parametric2NFunction2(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      output: CHType
  ) extends CHParametricNFunctionIO:
    val kind = "parametric2NFunction2"
    override val parameters = Seq(paramArg1, paramArg2, paramArgN)
    override val arguments = Seq(arg1, arg2)

  case class Parametric2NFunction3(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      output: CHType
  ) extends CHParametricNFunctionIO:
    val kind = "parametric2NFunction3"
    override val parameters = Seq(paramArg1, paramArg2, paramArgN)
    override val arguments = Seq(arg1, arg2, arg3)

  case class Parametric3NFunction0N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArgN: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric3NFunction0N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArgN)
    override val arguments = Seq(argN)

  case class Parametric3NFunction1N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArgN: CHType,
      arg1: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric3NFunction1N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArgN)
    override val arguments = Seq(arg1, argN)

  case class Parametric3NFunction2N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricNFunctionNIO:
    val kind = "parametric3NFunction2N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArgN)
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric3NFunction1(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArgN: CHType,
      arg1: CHType,
      output: CHType
  ) extends CHParametricNFunctionIO:
    val kind = "parametric3NFunction1"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArgN)
    override val arguments = Seq(arg1)

  case class Parametric3NFunction2(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      output: CHType
  ) extends CHParametricNFunctionIO:
    val kind = "parametric3NFunction2"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArgN)
    override val arguments = Seq(arg1, arg2)

  case class Parametric3NFunction3(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArgN: CHType,
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      output: CHType
  ) extends CHParametricNFunctionIO:
    val kind = "parametric3NFunction3"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArgN)
    override val arguments = Seq(arg1, arg2, arg3)

  case class Parametric0Function0N(argN: CHType, output: CHType) extends CHParametricFunctionNIO:
    val kind = "parametric0Function0N"
    override val parameters = Seq()
    override val arguments = Seq(argN)

  case class Parametric0Function1N(arg1: CHType, argN: CHType, output: CHType) extends CHParametricFunctionNIO:
    val kind = "parametric0Function1N"
    override val parameters = Seq()
    override val arguments = Seq(arg1, argN)

  case class Parametric0Function2N(arg1: CHType, arg2: CHType, argN: CHType, output: CHType)
      extends CHParametricFunctionNIO:
    val kind = "parametric0Function2N"
    override val parameters = Seq()
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric0Function1(arg1: CHType, output: CHType) extends CHParametricFunction:
    val kind = "parametric0Function1"
    override val parameters = Seq()
    override val arguments = Seq(arg1)

  case class Parametric0Function2(arg1: CHType, arg2: CHType, output: CHType) extends CHParametricFunction:
    val kind = "parametric0Function2"
    override val parameters = Seq()
    override val arguments = Seq(arg1, arg2)

  case class Parametric0Function3(arg1: CHType, arg2: CHType, arg3: CHType, output: CHType)
      extends CHParametricFunction:
    val kind = "parametric0Function3"
    override val parameters = Seq()
    override val arguments = Seq(arg1, arg2, arg3)

  case class Parametric1Function0N(paramArg1: CHType, argN: CHType, output: CHType) extends CHParametricFunctionNIO:
    val kind = "parametric1Function0N"
    override val parameters = Seq(paramArg1)
    override val arguments = Seq(argN)

  case class Parametric1Function1N(paramArg1: CHType, arg1: CHType, argN: CHType, output: CHType)
      extends CHParametricFunctionNIO:
    val kind = "parametric1Function1N"
    override val parameters = Seq(paramArg1)
    override val arguments = Seq(arg1, argN)

  case class Parametric1Function2N(paramArg1: CHType, arg1: CHType, arg2: CHType, argN: CHType, output: CHType)
      extends CHParametricFunctionNIO:
    val kind = "parametric1Function2N"
    override val parameters = Seq(paramArg1)
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric1Function1(paramArg1: CHType, arg1: CHType, output: CHType) extends CHParametricFunction:
    val kind = "parametric1Function1"
    override val parameters = Seq(paramArg1)
    override val arguments = Seq(arg1)

  case class Parametric1Function2(paramArg1: CHType, arg1: CHType, arg2: CHType, output: CHType)
      extends CHParametricFunction:
    val kind = "parametric1Function2"
    override val parameters = Seq(paramArg1)
    override val arguments = Seq(arg1, arg2)

  case class Parametric1Function3(paramArg1: CHType, arg1: CHType, arg2: CHType, arg3: CHType, output: CHType)
      extends CHParametricFunction:
    val kind = "parametric1Function3"
    override val parameters = Seq(paramArg1)
    override val arguments = Seq(arg1, arg2, arg3)

  case class Parametric2Function0N(paramArg1: CHType, paramArg2: CHType, argN: CHType, output: CHType)
      extends CHParametricFunctionNIO:
    val kind = "parametric2Function0N"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(argN)

  case class Parametric2Function1N(
      paramArg1: CHType,
      paramArg2: CHType,
      arg1: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric2Function1N"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(arg1, argN)

  case class Parametric2Function2N(
      paramArg1: CHType,
      paramArg2: CHType,
      arg1: CHType,
      arg2: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric2Function2N"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric2Function3N(
      paramArg1: CHType,
      paramArg2: CHType,
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric2Function3N"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(arg1, arg2, arg3, argN)

  case class Parametric2Function1(paramArg1: CHType, paramArg2: CHType, arg1: CHType, output: CHType)
      extends CHParametricFunction:
    val kind = "parametric2Function1"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(arg1)

  case class Parametric2Function2(
      paramArg1: CHType,
      paramArg2: CHType,
      arg1: CHType,
      arg2: CHType,
      output: CHType
  ) extends CHParametricFunction:
    val kind = "parametric2Function2"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(arg1, arg2)

  case class Parametric2Function3(
      paramArg1: CHType,
      paramArg2: CHType,
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      output: CHType
  ) extends CHParametricFunction:
    val kind = "parametric2Function3"
    override val parameters = Seq(paramArg1, paramArg2)
    override val arguments = Seq(arg1, arg2, arg3)

  case class Parametric3Function0N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric3Function0N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3)
    override val arguments = Seq(argN)

  case class Parametric3Function1N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      arg1: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric3Function1N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3)
    override val arguments = Seq(arg1, argN)

  case class Parametric3Function2N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      arg1: CHType,
      arg2: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric3Function2N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3)
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric3Function1(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      arg1: CHType,
      output: CHType
  ) extends CHParametricFunction:
    val kind = "parametric3Function1"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3)
    override val arguments = Seq(arg1)

  case class Parametric3Function2(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      arg1: CHType,
      arg2: CHType,
      output: CHType
  ) extends CHParametricFunction:
    val kind = "parametric3Function2"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3)
    override val arguments = Seq(arg1, arg2)

  case class Parametric3Function3(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      output: CHType
  ) extends CHParametricFunction:
    val kind = "parametric3Function3"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3)
    override val arguments = Seq(arg1, arg2, arg3)

  case class Parametric4Function0N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric4Function0N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4)
    override val arguments = Seq(argN)

  case class Parametric4Function1N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      arg1: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric4Function1N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4)
    override val arguments = Seq(arg1, argN)

  case class Parametric4Function2N(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      arg1: CHType,
      arg2: CHType,
      argN: CHType,
      output: CHType
  ) extends CHParametricFunctionNIO:
    val kind = "parametric4Function2N"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4)
    override val arguments = Seq(arg1, arg2, argN)

  case class Parametric4Function1(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      arg1: CHType,
      output: CHType
  ) extends CHParametricFunction:
    val kind = "parametric4Function1"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4)
    override val arguments = Seq(arg1)

  case class Parametric4Function2(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      arg1: CHType,
      arg2: CHType,
      output: CHType
  ) extends CHParametricFunction:
    val kind = "parametric4Function2"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4)
    override val arguments = Seq(arg1, arg2)

  case class Parametric4Function3(
      paramArg1: CHType,
      paramArg2: CHType,
      paramArg3: CHType,
      paramArg4: CHType,
      arg1: CHType,
      arg2: CHType,
      arg3: CHType,
      output: CHType
  ) extends CHParametricFunction:
    val kind = "parametric4Function3"
    override val parameters = Seq(paramArg1, paramArg2, paramArg3, paramArg4)
    override val arguments = Seq(arg1, arg2, arg3)
