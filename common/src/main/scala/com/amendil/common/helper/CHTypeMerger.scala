package com.amendil.common.helper

import com.amendil.common.entities.`type`.*

import scala.collection.immutable.SortedMap

object CHTypeMerger {

  lazy val allNumberTypes: Set[CHType] =
    getSubtypes(CHAggregatedType.Number) + CHAggregatedType.Number

  lazy val allLowCardinalityNumberTypes: Set[CHType] =
    getSubtypes(CHSpecialType.LowCardinality(CHAggregatedType.NonDecimal)) + CHSpecialType.LowCardinality(
      CHAggregatedType.NonDecimal
    )

  lazy val allLowCardinalityNullableNumberTypes: Set[CHType] =
    getSubtypes(CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.NonDecimal))) + CHSpecialType
      .LowCardinality(CHSpecialType.Nullable(CHAggregatedType.NonDecimal))

  lazy val allNullableNumberTypes: Set[CHType] =
    getSubtypes(CHSpecialType.Nullable(CHAggregatedType.Number)) + CHSpecialType.Nullable(CHAggregatedType.Number)

  lazy val allArrayNumberTypes: Set[CHType] =
    getSubtypes(CHSpecialType.Array(CHAggregatedType.Number)) + CHSpecialType.Array(CHAggregatedType.Number)

  lazy val allBitmapNumberTypes: Set[CHType] =
    getSubtypes(CHSpecialType.Bitmap(CHAggregatedType.NonDecimalNorFloatMax64Bits)) + CHSpecialType.Bitmap(
      CHAggregatedType.NonDecimalNorFloatMax64Bits
    )

  lazy val allMapNumberTypes: Set[CHType] =
    getSubtypes(CHSpecialType.Map(CHAggregatedType.NonDecimalNorFloat, CHSpecialType.UnknownType)) + CHSpecialType.Map(
      CHAggregatedType.NonDecimalNorFloat,
      CHSpecialType.UnknownType
    )

  lazy val allTuple1NumberTypes: Set[CHType] =
    getSubtypes(CHSpecialType.Tuple(Seq(CHAggregatedType.Number))) + CHSpecialType.Tuple(Seq(CHAggregatedType.Number))

  def deduplicateSupertypes(types: Set[CHType]): Set[CHType] =
    filterCHTypes(
      cleanTypes(types),
      supertypeDeduplicationRules
    )

  private lazy val filteredSubstitutionRulesNoJson = typeSubstitutionRules.collect {
    case (rule, mergedType) if !mergedType.name.toLowerCase.contains("json") =>
      (rule.filter(t => !t.name.toLowerCase.contains("json")), Seq(mergedType))
  }
  private lazy val allFilteredSubstitutionRules = typeSubstitutionRules.map { case (rule, mergedType) =>
    (rule, Seq(mergedType))
  }
  def mergeInputTypes(types: Set[CHType], supportJson: Boolean = true): Set[CHType] =
    val filteredSubstitutionRules =
      if supportJson then allFilteredSubstitutionRules else filteredSubstitutionRulesNoJson

    filterCHTypes(
      cleanTypes(deduplicateSupertypes(types)),
      filteredSubstitutionRules
    )

  def mergeOutputType(type1: CHType, type2: CHType): CHType =
    import CHAggregatedType.*
    import CHFuzzableType.*

    if type1 == type2 then type1 // Expects both type to be identical, should be the most obvious use case
    else if deduplicateSupertypes(Set(type1, type2)).size == 1 then
      CHTypeParser.getByName(deduplicateSupertypes(Set(type1, type2)).head.name)
    else
      val mergedType: CHType =
        if type1 == type2 then type1
        else if type1.isInstanceOf[CHSpecialType.Array] && type2.isInstanceOf[CHSpecialType.Array] then
          CHSpecialType.Array(
            mergeOutputType(
              type1.asInstanceOf[CHSpecialType.Array].innerType,
              type2.asInstanceOf[CHSpecialType.Array].innerType
            )
          )
        else if type1.isInstanceOf[CHSpecialType.Map] && type2.isInstanceOf[CHSpecialType.Map] then
          val mapType1 = type1.asInstanceOf[CHSpecialType.Map]
          val mapType2 = type2.asInstanceOf[CHSpecialType.Map]

          CHSpecialType.Map(
            mergeOutputType(mapType1.keyType, mapType2.keyType),
            mergeOutputType(mapType1.valueType, mapType2.valueType)
          )
        else if type1.isInstanceOf[CHSpecialType.Nullable] && type2.isInstanceOf[CHSpecialType.Nullable] then
          CHSpecialType.Nullable(
            mergeOutputType(
              type1.asInstanceOf[CHSpecialType.Nullable].innerType,
              type2.asInstanceOf[CHSpecialType.Nullable].innerType
            )
          )
        else if type1.isInstanceOf[CHSpecialType.Tuple] && type2.isInstanceOf[CHSpecialType.Tuple] then
          val tupleType1 = type1.asInstanceOf[CHSpecialType.Tuple]
          val tupleType2 = type2.asInstanceOf[CHSpecialType.Tuple]

          if tupleType1.innerTypes.size != tupleType2.innerTypes.size then CHSpecialType.TupleN(Any)
          else
            CHSpecialType.Tuple(
              tupleType1.innerTypes.zip(tupleType2.innerTypes).map(mergeOutputType)
            )
        else if type1.isInstanceOf[CHSpecialType.TupleN] then
          type2 match
            case _: CHSpecialType.TupleN | _: CHSpecialType.Tuple => CHSpecialType.TupleN(Any)
            case _                                                => Any
        else if type2.isInstanceOf[CHSpecialType.TupleN] then
          type1 match
            case _: CHSpecialType.TupleN | _: CHSpecialType.Tuple => CHSpecialType.TupleN(Any)
            case _                                                => Any
        else if type1 == BooleanType then
          type2 match
            case UInt8 | UInt16 | UInt32 | UInt64 | UInt128 | UInt256 | Int16 | Int32 | Int64 | Int128 | Int256 =>
              type2
            case _ => Any
        else if type2 == BooleanType then
          type1 match
            case UInt8 | UInt16 | UInt32 | UInt64 | UInt128 | UInt256 | Int16 | Int32 | Int64 | Int128 | Int256 =>
              type1
            case _ => Any
        else if type1 == Int8 then
          type2 match
            case UInt8                                   => Int16
            case UInt16                                  => Int32
            case UInt32                                  => Int64
            case UInt64                                  => Int128
            case UInt128 | UInt256                       => Int256
            case Int16 | Int32 | Int64 | Int128 | Int256 => type2
            case Enum16                                  => Int16
            case _                                       => Any
        else if type2 == Int8 then
          type1 match
            case UInt8                                   => Int16
            case UInt16                                  => Int32
            case UInt32                                  => Int64
            case UInt64                                  => Int128
            case UInt128 | UInt256                       => Int256
            case Int16 | Int32 | Int64 | Int128 | Int256 => type1
            case Enum16                                  => Int16
            case _                                       => Any
        else if type1 == Int16 then
          type2 match
            case UInt8                           => Int16
            case UInt16                          => Int32
            case UInt32                          => Int64
            case UInt64                          => Int128
            case UInt128 | UInt256               => Int256
            case Int32 | Int64 | Int128 | Int256 => type2
            case Enum8                           => Int16
            case _                               => Any
        else if type2 == Int16 then
          type1 match
            case UInt8                           => Int16
            case UInt16                          => Int32
            case UInt32                          => Int64
            case UInt64                          => Int128
            case UInt128 | UInt256               => Int256
            case Int32 | Int64 | Int128 | Int256 => type1
            case Enum8                           => Int16
            case _                               => Any
        else if type1 == Int32 then
          type2 match
            case UInt8 | UInt16          => Int32
            case UInt32                  => Int64
            case UInt64                  => Int128
            case UInt128 | UInt256       => Int256
            case Int64 | Int128 | Int256 => type2
            case _                       => Any
        else if type2 == Int32 then
          type1 match
            case UInt8 | UInt16          => Int32
            case UInt32                  => Int64
            case UInt64                  => Int128
            case UInt128 | UInt256       => Int256
            case Int64 | Int128 | Int256 => type1
            case _                       => Any
        else if type1 == Int64 then
          type2 match
            case UInt8 | UInt16 | UInt32 => Int64
            case UInt64                  => Int128
            case UInt128 | UInt256       => Int256
            case Int128 | Int256         => type2
            case _                       => Any
        else if type2 == Int64 then
          type1 match
            case UInt8 | UInt16 | UInt32 => Int64
            case UInt64                  => Int128
            case UInt128 | UInt256       => Int256
            case Int128 | Int256         => type1
            case _                       => Any
        else if type1 == Int128 then
          type2 match
            case UInt8 | UInt16 | UInt32 | UInt64 => Int128
            case UInt128 | UInt256                => Int256
            case Int256                           => Int256
            case _                                => Any
        else if type2 == Int128 then
          type1 match
            case UInt8 | UInt16 | UInt32 | UInt64 => Int128
            case UInt128 | UInt256                => Int256
            case Int256                           => Int256
            case _                                => Any
        else if type1 == Int256 then Int256
        else if type2 == Int256 then Int256
        // From now on, neither type1 nor type2 can be a signed integer
        else if type1 == UInt8 then
          type2 match
            case UInt16 | UInt32 | UInt64 | UInt128 | UInt256 => type2
            case _                                            => Any
        else if type2 == UInt8 then
          type1 match
            case UInt16 | UInt32 | UInt64 | UInt128 | UInt256 => type1
            case _                                            => Any
        else if type1 == UInt16 then
          type2 match
            case UInt32 | UInt64 | UInt128 | UInt256 => type2
            case _                                   => Any
        else if type2 == UInt16 then
          type1 match
            case UInt32 | UInt64 | UInt128 | UInt256 => type1
            case _                                   => Any
        else if type1 == UInt32 then
          type2 match
            case UInt64 | UInt128 | UInt256 => type2
            case _                          => Any
        else if type2 == UInt32 then
          type1 match
            case UInt64 | UInt128 | UInt256 => type1
            case _                          => Any
        else if type1 == UInt64 then
          type2 match
            case UInt128 | UInt256 => type2
            case _                 => Any
        else if type2 == UInt64 then
          type1 match
            case UInt128 | UInt256 => type1
            case _                 => Any
        else if type1 == UInt128 then
          type2 match
            case UInt256 => type2
            case _       => Any
        else if type2 == UInt128 then
          type1 match
            case UInt256 => type1
            case _       => Any
        // From now on, neither type1 nor type2 can be a signed or unsigned integer
        else if type1 == Float32 then
          type2 match
            case Float64 => Float64
            case _       => Any
        else if type2 == Float32 then
          type1 match
            case Float64 => Float64
            case _       => Any
        else if type1 == Decimal32 then
          type2 match
            case Decimal64 | Decimal128 | Decimal256 => type2
            case _                                   => Any
        else if type2 == Decimal32 then
          type1 match
            case Decimal64 | Decimal128 | Decimal256 => type1
            case _                                   => Any
        else if type1 == Decimal64 then
          type2 match
            case Decimal128 | Decimal256 => type2
            case _                       => Any
        else if type2 == Decimal64 then
          type1 match
            case Decimal128 | Decimal256 => type1
            case _                       => Any
        else if type1 == Decimal128 then
          type2 match
            case Decimal256 => type2
            case _          => Any
        else if type2 == Decimal128 then
          type1 match
            case Decimal256 => type1
            case _          => Any
        // From now on, neither type1 nor type2 can be a number
        else if type1 == Date then
          type2 match
            case Date32 | DateTime | DateTime64 => type2
            case _                              => Any
        else if type2 == Date then
          type1 match
            case Date32 | DateTime | DateTime64 => type1
            case _                              => Any
        else if type1 == DateTime then
          type2 match
            case DateTime64 => DateTime64
            case _          => Any
        else if type2 == Date then
          type1 match
            case DateTime64 => DateTime64
            case _          => Any
        // From now on, neither type1 nor type2 can be a number nor a date/datetime
        else if type1 == Enum8 || type1 == Enum16 || type1 == Enum then
          type2 match
            case Enum8 | Enum16 | Enum => Enum
            case _                     => Any
        else Any

      mergedType

  // Utils
  private def cleanTypes(types: Set[CHType]): Set[CHType] =
    types.map { t =>
      CHFuzzableType.fuzzableTypeByName.getOrElse(t.name, t)
    }

  private def filterCHTypes(types: Set[CHType], filteringRules: Map[Set[CHType], Seq[CHType]]): Set[CHType] =
    var mergedTypes = types
    var toMerge = true
    while toMerge do
      val newMergedTypes = filteringRules.foldLeft(mergedTypes) { case (currentTypes, (subTypes, aggregatedTypes)) =>
        if subTypes.forall(currentTypes) then currentTypes.removedAll(subTypes) ++ aggregatedTypes
        else currentTypes
      }

      toMerge = mergedTypes.size != newMergedTypes.size
      mergedTypes = newMergedTypes

    mergedTypes

  def getSubtypes(t: CHType): Set[CHType] =
    var types: Set[CHType] = Set(t)
    val m: Map[CHType, Set[CHType]] =
      typeSubstitutionRules.groupMap(_._2)(_._1).view.mapValues(_.toSet.flatten).toMap
    while
      val tmp = types ++ types.flatMap(t => m.getOrElse(t, Set.empty))
      if tmp.size != types.size then
        types = tmp
        true
      else false
    do ()

    types - t

  // format: off
  given Ordering[Set[CHType]] = ((k1: Set[CHType], k2: Set[CHType]) =>
    k1.toSeq.sortBy(_.name).map(_.name).mkString(", ").compareTo(
      k2.toSeq.sortBy(_.name).map(_.name).mkString(", ")
  ))

  private val typeSubstitutionRules = SortedMap(
    (Set(
      CHAggregatedType.DateLikeOrDateTime, CHAggregatedType.Interval, CHAggregatedType.NonDecimalNorFloatMax64Bits
    ), CHAggregatedType.Integer64Like),

    (Set(
      CHAggregatedType.Number, CHAggregatedType.DateLikeOrDateTime, CHAggregatedType.Interval
    ), CHAggregatedType.NumberOrDateLikeOrDateTimeOrInterval),

    (Set(
      CHAggregatedType.Integer64Like, CHAggregatedType.DecimalLike, CHAggregatedType.Float, CHFuzzableType.Int128,
      CHFuzzableType.Int256, CHFuzzableType.UInt128, CHFuzzableType.UInt256
    ), CHAggregatedType.NumberOrDateLikeOrDateTimeOrInterval),

    (Set(
      CHAggregatedType.Integer64Like, CHFuzzableType.Int128, CHFuzzableType.UInt128,
      CHFuzzableType.Int256, CHFuzzableType.UInt256, CHAggregatedType.StringLike, CHFuzzableType.UUID
    ), CHAggregatedType.MapKey),
    
    (Set(
      CHAggregatedType.NonDecimalNorFloat, CHAggregatedType.DateLikeOrDateTime, CHAggregatedType.Interval, CHAggregatedType.StringLike,
      CHFuzzableType.UUID
    ), CHAggregatedType.MapKey),

    (Set(
      CHAggregatedType.NonDecimal, CHAggregatedType.DateLikeOrDateTime, CHAggregatedType.Interval, CHAggregatedType.StringLike,
      CHFuzzableType.UUID
    ), CHAggregatedType.AnyLowCardinalityInnerType),

    (Set(
      CHAggregatedType.MapKey, CHAggregatedType.DecimalLike, CHAggregatedType.Float, CHFuzzableType.DateTime64
    ), CHAggregatedType.AnyNullableInnerType),

    (Set(
      CHAggregatedType.NumberOrDateLikeOrDateTimeOrInterval, CHFuzzableType.DateTime64, CHAggregatedType.StringLike,
      CHFuzzableType.UUID
    ), CHAggregatedType.AnyNullableInnerType),

    (Set(
      CHAggregatedType.AnyLowCardinalityInnerType, CHAggregatedType.DecimalLike, CHFuzzableType.DateTime64
    ), CHAggregatedType.AnyNullableInnerType),

    (Set(
      CHAggregatedType.DateLikeOrDateTimeLike, CHAggregatedType.Interval, CHAggregatedType.Number, CHAggregatedType.StringLike,
      CHFuzzableType.UUID
    ), CHAggregatedType.AnyNullableInnerType),

    (Set(
      CHAggregatedType.MapKey, CHSpecialType.Bitmap(CHAggregatedType.NonDecimalNorFloatMax64Bits), CHFuzzableType.DateTime64,
      CHAggregatedType.DecimalLike, CHAggregatedType.Float, CHFuzzableType.Json, CHSpecialType.Array(CHAggregatedType.Any),
      CHSpecialType.Tuple(Seq(CHAggregatedType.Any))
    ), CHAggregatedType.AnyNonMapNonNullableNonLowCardinality),

    (Set(
      CHAggregatedType.AnyNullableInnerType, CHSpecialType.Bitmap(CHAggregatedType.NonDecimalNorFloatMax64Bits),
      CHFuzzableType.Json, CHSpecialType.Array(CHAggregatedType.Any), CHSpecialType.Tuple(Seq(CHAggregatedType.Any))
    ), CHAggregatedType.AnyNonMapNonNullableNonLowCardinality),

    (Set(
      CHAggregatedType.AnyNullableInnerType, CHFuzzableType.Json, CHSpecialType.Array(CHAggregatedType.Any),
      CHSpecialType.Map(CHAggregatedType.MapKey, CHSpecialType.UnknownType), CHSpecialType.Tuple(Seq(CHAggregatedType.Any))
    ), CHAggregatedType.AnyNonBitmapNonNullableNonLowCardinality),

    (Set(
      CHAggregatedType.AnyNonBitmapNonNullableNonLowCardinality, CHSpecialType.Bitmap(CHAggregatedType.NonDecimalNorFloatMax64Bits)
    ), CHAggregatedType.AnyNonNullableNonLowCardinality),

    (Set(
      CHAggregatedType.AnyNonMapNonNullableNonLowCardinality, CHSpecialType.Map(CHAggregatedType.MapKey, CHSpecialType.UnknownType)
    ), CHAggregatedType.AnyNonNullableNonLowCardinality),

    (Set(
      CHAggregatedType.AnyNonNullableNonLowCardinality, CHSpecialType.Nullable(CHAggregatedType.AnyNullableInnerType), CHSpecialType.LowCardinality(CHAggregatedType.AnyLowCardinalityInnerType),
      CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.AnyLowCardinalityInnerType))
    ), CHAggregatedType.Any),

    (Set(CHFuzzableType.DateUnit, CHFuzzableType.TimeUnit), CHAggregatedType.DateTimeUnit),
    (Set(CHFuzzableType.TimeUnit, CHFuzzableType.Time64Unit), CHAggregatedType.TimeOrTime64Unit),
    (Set(CHAggregatedType.DateTimeUnit, CHFuzzableType.Time64Unit), CHAggregatedType.DateTimeUnit),
    (Set(CHAggregatedType.TimeOrTime64Unit, CHFuzzableType.DateUnit), CHAggregatedType.DateTimeUnit),

    (Set(CHFuzzableType.Int8, CHFuzzableType.Int16), CHAggregatedType.IntMax16Bits),
    (Set(CHAggregatedType.IntMax16Bits, CHFuzzableType.Int32), CHAggregatedType.IntMax32Bits),
    (Set(CHAggregatedType.IntMax32Bits, CHFuzzableType.Int64), CHAggregatedType.IntMax64Bits),
    (Set(CHAggregatedType.IntMax64Bits, CHFuzzableType.Int128), CHAggregatedType.IntMax128Bits),
    (Set(CHAggregatedType.IntMax128Bits, CHFuzzableType.Int256), CHAggregatedType.Int),
    
    (Set(CHFuzzableType.UInt8, CHFuzzableType.UInt16), CHAggregatedType.UIntMax16Bits),
    (Set(CHAggregatedType.UIntMax16Bits, CHFuzzableType.UInt32), CHAggregatedType.UIntMax32Bits),
    (Set(CHAggregatedType.UIntMax32Bits, CHFuzzableType.UInt64), CHAggregatedType.UIntMax64Bits),
    (Set(CHAggregatedType.UIntMax64Bits, CHFuzzableType.UInt128), CHAggregatedType.UIntMax128Bits),
    (Set(CHAggregatedType.UIntMax128Bits, CHFuzzableType.UInt256), CHAggregatedType.UInt),

    (Set(CHFuzzableType.Float32, CHFuzzableType.Float64), CHAggregatedType.Float),
    (Set(CHFuzzableType.Decimal32, CHFuzzableType.Decimal64, CHFuzzableType.Decimal128, CHFuzzableType.Decimal256), CHAggregatedType.DecimalLike),

    (Set(CHFuzzableType.Int8, CHFuzzableType.UInt8), CHAggregatedType.NonDecimalNorFloatMax8Bits),
    (Set(CHAggregatedType.IntMax16Bits, CHAggregatedType.UIntMax16Bits), CHAggregatedType.NonDecimalNorFloatMax16Bits),
    (Set(CHAggregatedType.NonDecimalNorFloatMax8Bits, CHFuzzableType.Int16, CHFuzzableType.UInt16), CHAggregatedType.NonDecimalNorFloatMax16Bits),
    (Set(CHAggregatedType.IntMax32Bits, CHAggregatedType.UIntMax32Bits), CHAggregatedType.NonDecimalNorFloatMax32Bits),
    (Set(CHAggregatedType.NonDecimalNorFloatMax16Bits, CHFuzzableType.Int32, CHFuzzableType.UInt32), CHAggregatedType.NonDecimalNorFloatMax32Bits),
    (Set(CHAggregatedType.IntMax64Bits, CHAggregatedType.UIntMax64Bits), CHAggregatedType.NonDecimalNorFloatMax64Bits),
    (Set(CHAggregatedType.NonDecimalNorFloatMax32Bits, CHFuzzableType.Int64, CHFuzzableType.UInt64), CHAggregatedType.NonDecimalNorFloatMax64Bits),
    (Set(CHAggregatedType.Int, CHAggregatedType.UInt), CHAggregatedType.NonDecimalNorFloat),
    (Set(CHAggregatedType.NonDecimalNorFloatMax64Bits, CHFuzzableType.Int128, CHFuzzableType.UInt128, CHFuzzableType.Int256, CHFuzzableType.UInt256), CHAggregatedType.NonDecimalNorFloat),

    (Set(CHAggregatedType.NonDecimalNorFloatMax32Bits, CHFuzzableType.Float32), CHAggregatedType.NonDecimalMax32Bits),
    (Set(CHAggregatedType.NonDecimalNorFloatMax64Bits, CHAggregatedType.Float), CHAggregatedType.NonDecimalMax64Bits),
    (Set(CHAggregatedType.NonFloat32NorDecimalMax64Bits, CHFuzzableType.Float32), CHAggregatedType.NonDecimalMax64Bits),
    (Set(CHAggregatedType.NonFloat64NorDecimalMax64Bits, CHFuzzableType.Float64), CHAggregatedType.NonDecimalMax64Bits),
    (Set(CHAggregatedType.NonDecimalMax32Bits, CHFuzzableType.Int64, CHFuzzableType.UInt64, CHFuzzableType.Float64), CHAggregatedType.NonDecimalMax64Bits),
    (Set(CHAggregatedType.NonDecimalNorFloat, CHAggregatedType.Float), CHAggregatedType.NonDecimal),
    (Set(CHAggregatedType.NonDecimalMax64Bits, CHFuzzableType.Int128, CHFuzzableType.UInt128, CHFuzzableType.Int256, CHFuzzableType.UInt256), CHAggregatedType.NonDecimal),

    (Set(CHAggregatedType.NonDecimalNorFloatMax64Bits, CHFuzzableType.Float64), CHAggregatedType.NonFloat32NorDecimalMax64Bits),
    (Set(CHAggregatedType.DecimalLike, CHAggregatedType.NonFloat32NorDecimalMax64Bits, CHFuzzableType.Int128, CHFuzzableType.Int256, CHFuzzableType.UInt128, CHFuzzableType.UInt256), CHAggregatedType.NonFloat32Number),
    (Set(CHAggregatedType.DecimalLike, CHAggregatedType.NonDecimalNorFloat, CHFuzzableType.Float64), CHAggregatedType.NonFloat32Number),

    (Set(CHAggregatedType.NonDecimalNorFloatMax64Bits, CHFuzzableType.Float32), CHAggregatedType.NonFloat64NorDecimalMax64Bits),
    (Set(CHAggregatedType.DecimalLike, CHAggregatedType.NonFloat64NorDecimalMax64Bits, CHFuzzableType.Int128, CHFuzzableType.Int256, CHFuzzableType.UInt128, CHFuzzableType.UInt256), CHAggregatedType.NonFloat64Number),
    (Set(CHAggregatedType.DecimalLike, CHAggregatedType.NonDecimalNorFloat, CHFuzzableType.Float32), CHAggregatedType.NonFloat64Number),

    (Set(CHFuzzableType.Decimal32, CHFuzzableType.Decimal64, CHAggregatedType.NonDecimalMax64Bits), CHAggregatedType.NumberMax64Bits),
    (Set(CHAggregatedType.NumberMax64Bits, CHFuzzableType.Decimal128, CHFuzzableType.Decimal256, CHFuzzableType.Int128, CHFuzzableType.UInt128, CHFuzzableType.Int256, CHFuzzableType.UInt256), CHAggregatedType.Number),
    (Set(CHAggregatedType.DecimalLike, CHAggregatedType.NonDecimal), CHAggregatedType.Number),
    (Set(CHAggregatedType.NonFloat32Number, CHFuzzableType.Float32), CHAggregatedType.Number),
    (Set(CHAggregatedType.NonFloat64Number, CHFuzzableType.Float64), CHAggregatedType.Number),

    (Set(
      CHFuzzableType.BitmapInt8, CHFuzzableType.BitmapInt16, CHFuzzableType.BitmapInt32, CHFuzzableType.BitmapInt64, 
      CHFuzzableType.BitmapUInt8, CHFuzzableType.BitmapUInt16, CHFuzzableType.BitmapUInt32, CHFuzzableType.BitmapUInt64
    ), CHSpecialType.Bitmap(CHAggregatedType.NonDecimalNorFloatMax64Bits)),

    (Set(CHFuzzableType.Date, CHFuzzableType.DateTime), CHAggregatedType.DateOrDateTime),
    (Set(CHFuzzableType.Date, CHFuzzableType.Date32), CHAggregatedType.DateLike),
    (Set(CHFuzzableType.DateTime, CHFuzzableType.DateTime64), CHAggregatedType.DateTimeLike),
    (Set(CHFuzzableType.Date32, CHFuzzableType.DateTime64), CHAggregatedType.Date32OrDateTime64),
    (Set(CHAggregatedType.DateLike, CHFuzzableType.DateTime), CHAggregatedType.DateLikeOrDateTime),
    (Set(CHAggregatedType.DateOrDateTime, CHFuzzableType.Date32), CHAggregatedType.DateLikeOrDateTime),
    (Set(CHAggregatedType.DateLikeOrDateTime, CHFuzzableType.DateTime64), CHAggregatedType.DateLikeOrDateTimeLike),
    (Set(CHAggregatedType.DateLike, CHAggregatedType.DateTimeLike), CHAggregatedType.DateLikeOrDateTimeLike),
    (Set(CHAggregatedType.Date32OrDateTime64, CHAggregatedType.DateOrDateTime), CHAggregatedType.DateLikeOrDateTimeLike),

    (Set(CHFuzzableType.InputFormat, CHFuzzableType.OutputFormat), CHAggregatedType.Format),

    (Set(CHFuzzableType.Point, CHFuzzableType.Ring, CHFuzzableType.Polygon, CHFuzzableType.MultiPolygon), CHAggregatedType.Geo),

    (Set(CHFuzzableType.IntervalNanosecond, CHFuzzableType.IntervalMicrosecond, CHFuzzableType.IntervalMillisecond), CHAggregatedType.IntervalTime64),
    (Set(CHFuzzableType.IntervalSecond, CHFuzzableType.IntervalMinute, CHFuzzableType.IntervalHour), CHAggregatedType.IntervalTime),
    (Set(CHFuzzableType.IntervalDay, CHFuzzableType.IntervalWeek, CHFuzzableType.IntervalMonth, CHFuzzableType.IntervalQuarter, CHFuzzableType.IntervalYear), CHAggregatedType.IntervalDate),
    (Set(CHAggregatedType.IntervalTime64, CHAggregatedType.IntervalTime), CHAggregatedType.IntervalTimeOrTime64),
    (Set(CHAggregatedType.IntervalTimeOrTime64, CHAggregatedType.IntervalDate), CHAggregatedType.Interval),

    (Set(CHFuzzableType.IPv4, CHFuzzableType.IPv6), CHAggregatedType.IP),

    (Set(CHFuzzableType.StringType, CHFuzzableType.FixedString), CHAggregatedType.StringLike),

    (Set(CHFuzzableType.ArrayInt8, CHFuzzableType.ArrayInt16), CHSpecialType.Array(CHAggregatedType.IntMax16Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.IntMax16Bits), CHFuzzableType.ArrayInt32), CHSpecialType.Array(CHAggregatedType.IntMax32Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.IntMax32Bits), CHFuzzableType.ArrayInt64), CHSpecialType.Array(CHAggregatedType.IntMax64Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.IntMax64Bits), CHFuzzableType.ArrayInt128), CHSpecialType.Array(CHAggregatedType.IntMax128Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.IntMax128Bits), CHFuzzableType.ArrayInt256), CHSpecialType.Array(CHAggregatedType.Int)),

    (Set(CHFuzzableType.ArrayUInt8, CHFuzzableType.ArrayUInt16), CHSpecialType.Array(CHAggregatedType.UIntMax16Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.UIntMax16Bits), CHFuzzableType.ArrayUInt32), CHSpecialType.Array(CHAggregatedType.UIntMax32Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.UIntMax32Bits), CHFuzzableType.ArrayUInt64), CHSpecialType.Array(CHAggregatedType.UIntMax64Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.UIntMax64Bits), CHFuzzableType.ArrayUInt128), CHSpecialType.Array(CHAggregatedType.UIntMax128Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.UIntMax128Bits), CHFuzzableType.ArrayUInt256), CHSpecialType.Array(CHAggregatedType.UInt)),

    (Set(CHFuzzableType.ArrayFloat32, CHFuzzableType.ArrayFloat64), CHSpecialType.Array(CHAggregatedType.Float)),
    (Set(CHFuzzableType.ArrayDecimal32, CHFuzzableType.ArrayDecimal64, CHFuzzableType.ArrayDecimal128, CHFuzzableType.ArrayDecimal256), CHSpecialType.Array(CHAggregatedType.DecimalLike)),

    (Set(CHFuzzableType.ArrayInt8, CHFuzzableType.ArrayUInt8), CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax8Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.IntMax16Bits), CHSpecialType.Array(CHAggregatedType.UIntMax16Bits)), CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax16Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax8Bits), CHFuzzableType.ArrayInt16, CHFuzzableType.ArrayUInt16), CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax16Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.IntMax32Bits), CHSpecialType.Array(CHAggregatedType.UIntMax32Bits)), CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax32Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax16Bits), CHFuzzableType.ArrayInt32, CHFuzzableType.ArrayUInt32), CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax32Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.IntMax64Bits), CHSpecialType.Array(CHAggregatedType.UIntMax64Bits)), CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax64Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax32Bits), CHFuzzableType.ArrayInt64, CHFuzzableType.ArrayUInt64), CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax64Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.Int), CHSpecialType.Array(CHAggregatedType.UInt)), CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloat)),
    (Set(CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax64Bits), CHFuzzableType.ArrayInt128, CHFuzzableType.ArrayUInt128, CHFuzzableType.ArrayInt256, CHFuzzableType.ArrayUInt256), CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloat)),

    (Set(CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax32Bits), CHFuzzableType.ArrayFloat32), CHSpecialType.Array(CHAggregatedType.NonDecimalMax32Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax64Bits), CHSpecialType.Array(CHAggregatedType.Float)), CHSpecialType.Array(CHAggregatedType.NonDecimalMax64Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.NonFloat32NorDecimalMax64Bits), CHFuzzableType.ArrayFloat32), CHSpecialType.Array(CHAggregatedType.NonDecimalMax64Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.NonDecimalMax32Bits), CHFuzzableType.ArrayInt64, CHFuzzableType.ArrayUInt64, CHFuzzableType.ArrayFloat64), CHSpecialType.Array(CHAggregatedType.NonDecimalMax64Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloat), CHSpecialType.Array(CHAggregatedType.Float)), CHSpecialType.Array(CHAggregatedType.NonDecimal)),
    (Set(CHSpecialType.Array(CHAggregatedType.NonDecimalMax64Bits), CHFuzzableType.ArrayInt128, CHFuzzableType.ArrayUInt128, CHFuzzableType.ArrayInt256, CHFuzzableType.ArrayUInt256), CHSpecialType.Array(CHAggregatedType.NonDecimal)),

    (Set(CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax64Bits), CHFuzzableType.ArrayFloat64), CHSpecialType.Array(CHAggregatedType.NonFloat32NorDecimalMax64Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.DecimalLike), CHSpecialType.Array(CHAggregatedType.NonFloat32NorDecimalMax64Bits), CHFuzzableType.ArrayInt128, CHFuzzableType.ArrayInt256, CHFuzzableType.ArrayUInt128, CHFuzzableType.ArrayUInt256), CHSpecialType.Array(CHAggregatedType.NonFloat32Number)),
    (Set(CHSpecialType.Array(CHAggregatedType.DecimalLike), CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloat), CHFuzzableType.ArrayFloat64), CHSpecialType.Array(CHAggregatedType.NonFloat32Number)),

    (Set(CHFuzzableType.ArrayDecimal32, CHFuzzableType.ArrayDecimal64, CHSpecialType.Array(CHAggregatedType.NonDecimalMax64Bits)), CHSpecialType.Array(CHAggregatedType.NumberMax64Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.NumberMax64Bits), CHFuzzableType.ArrayDecimal128, CHFuzzableType.ArrayDecimal256, CHFuzzableType.ArrayInt128, CHFuzzableType.ArrayUInt128, CHFuzzableType.ArrayInt256, CHFuzzableType.ArrayUInt256), CHSpecialType.Array(CHAggregatedType.Number)),
    (Set(CHSpecialType.Array(CHAggregatedType.DecimalLike), CHSpecialType.Array(CHAggregatedType.NonDecimal)), CHSpecialType.Array(CHAggregatedType.Number)),
    (Set(CHSpecialType.Array(CHAggregatedType.NonFloat32Number), CHFuzzableType.ArrayFloat32), CHSpecialType.Array(CHAggregatedType.Number)),

    (Set(CHFuzzableType.ArrayDate, CHFuzzableType.ArrayDateTime), CHSpecialType.Array(CHAggregatedType.DateOrDateTime)),
    (Set(CHFuzzableType.ArrayDate, CHFuzzableType.ArrayDate32), CHSpecialType.Array(CHAggregatedType.DateLike)),
    (Set(CHFuzzableType.ArrayDateTime, CHFuzzableType.ArrayDateTime64), CHSpecialType.Array(CHAggregatedType.DateTimeLike)),
    (Set(CHFuzzableType.ArrayDate32, CHFuzzableType.ArrayDateTime64), CHSpecialType.Array(CHAggregatedType.Date32OrDateTime64)),
    (Set(CHSpecialType.Array(CHAggregatedType.DateOrDateTime), CHFuzzableType.ArrayDate32, CHFuzzableType.ArrayDateTime64), CHSpecialType.Array(CHAggregatedType.DateLikeOrDateTimeLike)),
    (Set(CHSpecialType.Array(CHAggregatedType.DateLike), CHSpecialType.Array(CHAggregatedType.DateTimeLike)), CHSpecialType.Array(CHAggregatedType.DateLikeOrDateTimeLike)),
    (Set(CHSpecialType.Array(CHAggregatedType.Date32OrDateTime64), CHSpecialType.Array(CHAggregatedType.DateOrDateTime)), CHSpecialType.Array(CHAggregatedType.DateLikeOrDateTimeLike)),

    (Set(CHFuzzableType.ArrayEnum8, CHFuzzableType.ArrayEnum16), CHFuzzableType.ArrayEnum),

    (Set(CHFuzzableType.ArrayIntervalNanosecond, CHFuzzableType.ArrayIntervalMicrosecond, CHFuzzableType.ArrayIntervalMillisecond), CHSpecialType.Array(CHAggregatedType.IntervalTime64)),
    (Set(CHFuzzableType.ArrayIntervalSecond, CHFuzzableType.ArrayIntervalMinute, CHFuzzableType.ArrayIntervalHour), CHSpecialType.Array(CHAggregatedType.IntervalTime)),
    (Set(CHFuzzableType.ArrayIntervalDay, CHFuzzableType.ArrayIntervalWeek, CHFuzzableType.ArrayIntervalMonth, CHFuzzableType.ArrayIntervalQuarter, CHFuzzableType.ArrayIntervalYear), CHSpecialType.Array(CHAggregatedType.IntervalDate)),
    (Set(CHSpecialType.Array(CHAggregatedType.IntervalTime64), CHSpecialType.Array(CHAggregatedType.IntervalTime)), CHSpecialType.Array(CHAggregatedType.IntervalTimeOrTime64)),
    (Set(CHSpecialType.Array(CHAggregatedType.IntervalTimeOrTime64), CHSpecialType.Array(CHAggregatedType.IntervalDate)), CHSpecialType.Array(CHAggregatedType.Interval)),

    (Set(CHFuzzableType.ArrayIPv4, CHFuzzableType.ArrayIPv6), CHSpecialType.Array(CHAggregatedType.IP)),

    (Set(CHFuzzableType.ArrayString, CHFuzzableType.ArrayFixedString), CHSpecialType.Array(CHAggregatedType.StringLike)),

    (Set(
      CHSpecialType.Array(CHAggregatedType.DateLikeOrDateTimeLike), CHSpecialType.Array(CHAggregatedType.Interval),
      CHSpecialType.Array(CHAggregatedType.Number), CHSpecialType.Array(CHAggregatedType.StringLike),
      CHFuzzableType.ArrayUUID
    ), CHSpecialType.Array(CHAggregatedType.AnyNullableInnerType)),

    (Set(
      CHSpecialType.Array(CHAggregatedType.AnyNullableInnerType), CHFuzzableType.ArrayJson,
      CHFuzzableType.ArrayArrayUUID, CHFuzzableType.ArrayMapUUIDInt, CHFuzzableType.ArrayTuple1UUID
    ), CHSpecialType.Array(CHAggregatedType.Any)),

    (Set(CHFuzzableType.MapInt8Int, CHFuzzableType.MapInt16Int, CHFuzzableType.MapInt32Int, CHFuzzableType.MapInt64Int), CHSpecialType.Map(CHAggregatedType.IntMax64Bits, CHSpecialType.UnknownType)),
    (Set(CHFuzzableType.MapUInt8Int, CHFuzzableType.MapUInt16Int, CHFuzzableType.MapUInt32Int, CHFuzzableType.MapUInt64Int), CHSpecialType.Map(CHAggregatedType.UIntMax64Bits, CHSpecialType.UnknownType)),
    (Set(CHSpecialType.Map(CHAggregatedType.IntMax64Bits, CHSpecialType.UnknownType), CHFuzzableType.MapInt128Int, CHFuzzableType.MapInt256Int), CHSpecialType.Map(CHAggregatedType.Int, CHSpecialType.UnknownType)),
    (Set(CHSpecialType.Map(CHAggregatedType.UIntMax64Bits, CHSpecialType.UnknownType), CHFuzzableType.MapUInt128Int, CHFuzzableType.MapUInt256Int), CHSpecialType.Map(CHAggregatedType.UInt, CHSpecialType.UnknownType)),
    (Set(CHSpecialType.Map(CHAggregatedType.IntMax64Bits, CHSpecialType.UnknownType), CHSpecialType.Map(CHAggregatedType.UIntMax64Bits, CHSpecialType.UnknownType)), CHSpecialType.Map(CHAggregatedType.NonDecimalNorFloatMax64Bits, CHSpecialType.UnknownType)),
    (Set(CHSpecialType.Map(CHAggregatedType.Int, CHSpecialType.UnknownType), CHSpecialType.Map(CHAggregatedType.UInt, CHSpecialType.UnknownType)), CHSpecialType.Map(CHAggregatedType.NonDecimalNorFloat, CHSpecialType.UnknownType)),
    (Set(CHSpecialType.Map(CHAggregatedType.NonDecimalNorFloatMax64Bits, CHSpecialType.UnknownType), CHFuzzableType.MapInt128Int, CHFuzzableType.MapInt256Int, CHFuzzableType.MapUInt128Int, CHFuzzableType.MapUInt256Int), CHSpecialType.Map(CHAggregatedType.NonDecimalNorFloat, CHSpecialType.UnknownType)),

    (Set(CHFuzzableType.MapDateInt, CHFuzzableType.MapDateTimeInt), CHSpecialType.Map(CHAggregatedType.DateOrDateTime, CHSpecialType.UnknownType)),
    (Set(CHFuzzableType.MapDateInt, CHFuzzableType.MapDate32Int), CHSpecialType.Map(CHAggregatedType.DateLike, CHSpecialType.UnknownType)),
    (Set(CHSpecialType.Map(CHAggregatedType.DateOrDateTime, CHSpecialType.UnknownType), CHFuzzableType.MapDate32Int), CHSpecialType.Map(CHAggregatedType.DateLikeOrDateTime, CHSpecialType.UnknownType)),
    (Set(CHSpecialType.Map(CHAggregatedType.DateLike, CHSpecialType.UnknownType), CHFuzzableType.MapDateTimeInt), CHSpecialType.Map(CHAggregatedType.DateLikeOrDateTime, CHSpecialType.UnknownType)),

    (Set(CHFuzzableType.MapEnum8Int, CHFuzzableType.MapEnum16Int), CHFuzzableType.MapEnumInt),

    (Set(CHFuzzableType.MapIntervalNanosecondInt, CHFuzzableType.MapIntervalMicrosecondInt, CHFuzzableType.MapIntervalMillisecondInt), CHSpecialType.Map(CHAggregatedType.IntervalTime64, CHSpecialType.UnknownType)),
    (Set(CHFuzzableType.MapIntervalSecondInt, CHFuzzableType.MapIntervalMinuteInt, CHFuzzableType.MapIntervalHourInt), CHSpecialType.Map(CHAggregatedType.IntervalTime, CHSpecialType.UnknownType)),
    (Set(CHFuzzableType.MapIntervalDayInt, CHFuzzableType.MapIntervalWeekInt, CHFuzzableType.MapIntervalMonthInt, CHFuzzableType.MapIntervalQuarterInt, CHFuzzableType.MapIntervalYearInt), CHSpecialType.Map(CHAggregatedType.IntervalDate, CHSpecialType.UnknownType)),
    (Set(CHSpecialType.Map(CHAggregatedType.IntervalTime64, CHSpecialType.UnknownType), CHSpecialType.Map(CHAggregatedType.IntervalTime, CHSpecialType.UnknownType)), CHSpecialType.Map(CHAggregatedType.IntervalTimeOrTime64, CHSpecialType.UnknownType)),
    (Set(CHSpecialType.Map(CHAggregatedType.IntervalTimeOrTime64, CHSpecialType.UnknownType), CHSpecialType.Map(CHAggregatedType.IntervalDate, CHSpecialType.UnknownType)), CHSpecialType.Map(CHAggregatedType.Interval, CHSpecialType.UnknownType)),

    (Set(CHFuzzableType.MapIPv4Int, CHFuzzableType.MapIPv6Int), CHSpecialType.Map(CHAggregatedType.IP, CHSpecialType.UnknownType)),

    (Set(CHFuzzableType.MapStringInt, CHFuzzableType.MapFixedStringInt), CHSpecialType.Map(CHAggregatedType.StringLike, CHSpecialType.UnknownType)),

    (Set(
      CHSpecialType.Map(CHAggregatedType.NonDecimalNorFloat, CHSpecialType.UnknownType),
      CHSpecialType.Map(CHAggregatedType.DateLikeOrDateTime, CHSpecialType.UnknownType),
      CHSpecialType.Map(CHAggregatedType.Interval, CHSpecialType.UnknownType),
      CHSpecialType.Map(CHAggregatedType.StringLike, CHSpecialType.UnknownType), CHFuzzableType.MapUUIDInt
    ), CHSpecialType.Map(CHAggregatedType.MapKey, CHSpecialType.UnknownType)),

    (Set(CHFuzzableType.Tuple1Int8, CHFuzzableType.Tuple1Int16), CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax16Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax16Bits)), CHFuzzableType.Tuple1Int32), CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax32Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax32Bits)), CHFuzzableType.Tuple1Int64), CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax64Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax64Bits)), CHFuzzableType.Tuple1Int128), CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax128Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax128Bits)), CHFuzzableType.Tuple1Int256), CHSpecialType.Tuple(Seq(CHAggregatedType.Int))),

    (Set(CHFuzzableType.Tuple1UInt8, CHFuzzableType.Tuple1UInt16), CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax16Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax16Bits)), CHFuzzableType.Tuple1UInt32), CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax32Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax32Bits)), CHFuzzableType.Tuple1UInt64), CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax64Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax64Bits)), CHFuzzableType.Tuple1UInt128), CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax128Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax128Bits)), CHFuzzableType.Tuple1UInt256), CHSpecialType.Tuple(Seq(CHAggregatedType.UInt))),

    (Set(CHFuzzableType.Tuple1Float32, CHFuzzableType.Tuple1Float64), CHSpecialType.Tuple(Seq(CHAggregatedType.Float))),
    (Set(CHFuzzableType.Tuple1Decimal32, CHFuzzableType.Tuple1Decimal64, CHFuzzableType.Tuple1Decimal128, CHFuzzableType.Tuple1Decimal256), CHSpecialType.Tuple(Seq(CHAggregatedType.DecimalLike))),

    (Set(CHFuzzableType.Tuple1Int8, CHFuzzableType.Tuple1UInt8), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax8Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax16Bits)), CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax16Bits))), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax16Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax8Bits)), CHFuzzableType.Tuple1Int16, CHFuzzableType.Tuple1UInt16), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax16Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax32Bits)), CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax32Bits))), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax32Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax16Bits)), CHFuzzableType.Tuple1Int32, CHFuzzableType.Tuple1UInt32), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax32Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax64Bits)), CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax64Bits))), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax64Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax32Bits)), CHFuzzableType.Tuple1Int64, CHFuzzableType.Tuple1UInt64), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax64Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.Int)), CHSpecialType.Tuple(Seq(CHAggregatedType.UInt))), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloat))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax64Bits)), CHFuzzableType.Tuple1Int128, CHFuzzableType.Tuple1UInt128, CHFuzzableType.Tuple1Int256, CHFuzzableType.Tuple1UInt256), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloat))),

    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax32Bits)), CHFuzzableType.Tuple1Float32), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalMax32Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax64Bits)), CHSpecialType.Tuple(Seq(CHAggregatedType.Float))), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalMax64Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonFloat32NorDecimalMax64Bits)), CHFuzzableType.Tuple1Float32), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalMax64Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalMax32Bits)), CHFuzzableType.Tuple1Int64, CHFuzzableType.Tuple1UInt64, CHFuzzableType.Tuple1Float64), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalMax64Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloat)), CHSpecialType.Tuple(Seq(CHAggregatedType.Float))), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimal))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalMax64Bits)), CHFuzzableType.Tuple1Int128, CHFuzzableType.Tuple1UInt128, CHFuzzableType.Tuple1Int256, CHFuzzableType.Tuple1UInt256), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimal))),

    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax64Bits)), CHFuzzableType.Tuple1Float64), CHSpecialType.Tuple(Seq(CHAggregatedType.NonFloat32NorDecimalMax64Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.DecimalLike)), CHSpecialType.Tuple(Seq(CHAggregatedType.NonFloat32NorDecimalMax64Bits)), CHFuzzableType.Tuple1Int128, CHFuzzableType.Tuple1Int256, CHFuzzableType.Tuple1UInt128, CHFuzzableType.Tuple1UInt256), CHSpecialType.Tuple(Seq(CHAggregatedType.NonFloat32Number))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.DecimalLike)), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloat)), CHFuzzableType.Tuple1Float64), CHSpecialType.Tuple(Seq(CHAggregatedType.NonFloat32Number))),

    (Set(CHFuzzableType.Tuple1Decimal32, CHFuzzableType.Tuple1Decimal64, CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalMax64Bits))), CHSpecialType.Tuple(Seq(CHAggregatedType.NumberMax64Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NumberMax64Bits)), CHFuzzableType.Tuple1Decimal128, CHFuzzableType.Tuple1Decimal256, CHFuzzableType.Tuple1Int128, CHFuzzableType.Tuple1UInt128, CHFuzzableType.Tuple1Int256, CHFuzzableType.Tuple1UInt256), CHSpecialType.Tuple(Seq(CHAggregatedType.Number))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.DecimalLike)), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimal))), CHSpecialType.Tuple(Seq(CHAggregatedType.Number))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonFloat32Number)), CHFuzzableType.Tuple1Float32), CHSpecialType.Tuple(Seq(CHAggregatedType.Number))),

    (Set(CHFuzzableType.Tuple1Date, CHFuzzableType.Tuple1DateTime), CHSpecialType.Tuple(Seq(CHAggregatedType.DateOrDateTime))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.DateOrDateTime)), CHFuzzableType.Tuple1Date32, CHFuzzableType.Tuple1DateTime64), CHSpecialType.Tuple(Seq(CHAggregatedType.DateLikeOrDateTimeLike))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.DateLike)), CHSpecialType.Tuple(Seq(CHAggregatedType.DateTimeLike))), CHSpecialType.Tuple(Seq(CHAggregatedType.DateLikeOrDateTimeLike))),
    (Set(CHFuzzableType.Tuple1Date, CHFuzzableType.Tuple1Date32), CHSpecialType.Tuple(Seq(CHAggregatedType.DateLike))),
    (Set(CHFuzzableType.Tuple1DateTime, CHFuzzableType.Tuple1DateTime64), CHSpecialType.Tuple(Seq(CHAggregatedType.DateTimeLike))),

    (Set(CHFuzzableType.Tuple1Enum8, CHFuzzableType.Tuple1Enum16), CHFuzzableType.Tuple1Enum),

    (Set(CHFuzzableType.Tuple1Point, CHFuzzableType.Tuple1Ring, CHFuzzableType.Tuple1Polygon, CHFuzzableType.Tuple1MultiPolygon), CHSpecialType.Tuple(Seq(CHAggregatedType.Geo))),

    (Set(CHFuzzableType.Tuple1IntervalNanosecond, CHFuzzableType.Tuple1IntervalMicrosecond, CHFuzzableType.Tuple1IntervalMillisecond), CHSpecialType.Tuple(Seq(CHAggregatedType.IntervalTime64))),
    (Set(CHFuzzableType.Tuple1IntervalSecond, CHFuzzableType.Tuple1IntervalMinute, CHFuzzableType.Tuple1IntervalHour), CHSpecialType.Tuple(Seq(CHAggregatedType.IntervalTime))),
    (Set(CHFuzzableType.Tuple1IntervalDay, CHFuzzableType.Tuple1IntervalWeek, CHFuzzableType.Tuple1IntervalMonth, CHFuzzableType.Tuple1IntervalQuarter, CHFuzzableType.Tuple1IntervalYear), CHSpecialType.Tuple(Seq(CHAggregatedType.IntervalDate))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.IntervalTime64)), CHSpecialType.Tuple(Seq(CHAggregatedType.IntervalTime))), CHSpecialType.Tuple(Seq(CHAggregatedType.IntervalTimeOrTime64))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.IntervalTimeOrTime64)), CHSpecialType.Tuple(Seq(CHAggregatedType.IntervalDate))), CHSpecialType.Tuple(Seq(CHAggregatedType.Interval))),

    (Set(CHFuzzableType.Tuple1IPv4, CHFuzzableType.Tuple1IPv6), CHSpecialType.Tuple(Seq(CHAggregatedType.IP))),

    (Set(CHFuzzableType.Tuple1String, CHFuzzableType.Tuple1FixedString), CHSpecialType.Tuple(Seq(CHAggregatedType.StringLike))),

    (Set(
      CHSpecialType.Tuple(Seq(CHAggregatedType.Number)), CHSpecialType.Tuple(Seq(CHAggregatedType.DateLikeOrDateTimeLike)),
      CHSpecialType.Tuple(Seq(CHAggregatedType.Interval)), CHSpecialType.Tuple(Seq(CHAggregatedType.StringLike)), CHFuzzableType.Tuple1Json,
      CHFuzzableType.Tuple1UUID, CHFuzzableType.Tuple1ArrayUUID, CHFuzzableType.Tuple1MapUUIDInt
    ), CHSpecialType.Tuple(Seq(CHAggregatedType.Any))),

    (Set(
      CHAggregatedType.NonDecimal, CHAggregatedType.DateLikeOrDateTime, CHAggregatedType.Interval, CHAggregatedType.StringLike,
      CHFuzzableType.UUID
    ), CHAggregatedType.AnyLowCardinalityInnerType),

    (Set(
      CHSpecialType.LowCardinality(CHAggregatedType.NonDecimal), CHFuzzableType.LowCardinalityDate, CHFuzzableType.LowCardinalityDate32,
      CHFuzzableType.LowCardinalityDateTime, CHFuzzableType.LowCardinalityIntervalNanosecond,
      CHFuzzableType.LowCardinalityIntervalMicrosecond, CHFuzzableType.LowCardinalityIntervalMillisecond,
      CHFuzzableType.LowCardinalityIntervalSecond, CHFuzzableType.LowCardinalityIntervalMinute,
      CHFuzzableType.LowCardinalityIntervalHour, CHFuzzableType.LowCardinalityIntervalDay, CHFuzzableType.LowCardinalityIntervalWeek,
      CHFuzzableType.LowCardinalityIntervalMonth, CHFuzzableType.LowCardinalityIntervalQuarter, CHFuzzableType.LowCardinalityIntervalYear,
      CHFuzzableType.LowCardinalityFixedString, CHFuzzableType.LowCardinalityString, CHFuzzableType.LowCardinalityUUID
    ), CHSpecialType.LowCardinality(CHAggregatedType.AnyLowCardinalityInnerType)),

    (Set(CHFuzzableType.LowCardinalityFloat32, CHFuzzableType.LowCardinalityFloat64), CHSpecialType.LowCardinality(CHAggregatedType.Float)),
    (Set(CHSpecialType.LowCardinality(CHAggregatedType.IntMax64Bits), CHFuzzableType.LowCardinalityInt128, CHFuzzableType.LowCardinalityInt256), CHSpecialType.LowCardinality(CHAggregatedType.Int)),
    (Set(CHFuzzableType.LowCardinalityInt8, CHFuzzableType.LowCardinalityInt16, CHFuzzableType.LowCardinalityInt32, CHFuzzableType.LowCardinalityInt64), CHSpecialType.LowCardinality(CHAggregatedType.IntMax64Bits)),
    (Set(CHSpecialType.LowCardinality(CHAggregatedType.Float), CHSpecialType.LowCardinality(CHAggregatedType.Int), CHSpecialType.LowCardinality(CHAggregatedType.UInt)), CHSpecialType.LowCardinality(CHAggregatedType.NonDecimal)),
    (Set(CHSpecialType.LowCardinality(CHAggregatedType.NonDecimalMax64Bits), CHFuzzableType.LowCardinalityInt128, CHFuzzableType.LowCardinalityInt256, CHFuzzableType.LowCardinalityUInt128, CHFuzzableType.LowCardinalityUInt256), CHSpecialType.LowCardinality(CHAggregatedType.NonDecimal)),
    (Set(CHSpecialType.LowCardinality(CHAggregatedType.NonDecimalNorFloatMax64Bits), CHSpecialType.LowCardinality(CHAggregatedType.Float)), CHSpecialType.LowCardinality(CHAggregatedType.NonDecimalMax64Bits)),
    (Set(CHSpecialType.LowCardinality(CHAggregatedType.IntMax64Bits), CHSpecialType.LowCardinality(CHAggregatedType.UIntMax64Bits)), CHSpecialType.LowCardinality(CHAggregatedType.NonDecimalNorFloatMax64Bits)),
    (Set(CHSpecialType.LowCardinality(CHAggregatedType.UIntMax64Bits), CHFuzzableType.LowCardinalityUInt128, CHFuzzableType.LowCardinalityUInt256), CHSpecialType.LowCardinality(CHAggregatedType.UInt)),
    (Set(CHFuzzableType.LowCardinalityUInt8, CHFuzzableType.LowCardinalityUInt16, CHFuzzableType.LowCardinalityUInt32, CHFuzzableType.LowCardinalityUInt64), CHSpecialType.LowCardinality(CHAggregatedType.UIntMax64Bits)),

    (Set(
      CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.NonDecimal)), CHFuzzableType.LowCardinalityNullableDate,
      CHFuzzableType.LowCardinalityNullableDate32, CHFuzzableType.LowCardinalityNullableDateTime,
      CHFuzzableType.LowCardinalityNullableIntervalNanosecond, CHFuzzableType.LowCardinalityNullableIntervalMicrosecond,
      CHFuzzableType.LowCardinalityNullableIntervalMillisecond, CHFuzzableType.LowCardinalityNullableIntervalSecond,
      CHFuzzableType.LowCardinalityNullableIntervalMinute, CHFuzzableType.LowCardinalityNullableIntervalHour,
      CHFuzzableType.LowCardinalityNullableIntervalDay, CHFuzzableType.LowCardinalityNullableIntervalWeek,
      CHFuzzableType.LowCardinalityNullableIntervalMonth, CHFuzzableType.LowCardinalityNullableIntervalQuarter,
      CHFuzzableType.LowCardinalityNullableIntervalYear, CHFuzzableType.LowCardinalityNullableFixedString,
      CHFuzzableType.LowCardinalityNullableString, CHFuzzableType.LowCardinalityNullableUUID
    ), CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.AnyLowCardinalityInnerType))),

    (Set(CHFuzzableType.LowCardinalityNullableFloat32, CHFuzzableType.LowCardinalityNullableFloat64), CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.Float))),
    (Set(CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.IntMax64Bits)), CHFuzzableType.LowCardinalityNullableInt128, CHFuzzableType.LowCardinalityNullableInt256), CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.Int))),
    (Set(CHFuzzableType.LowCardinalityNullableInt8, CHFuzzableType.LowCardinalityNullableInt16, CHFuzzableType.LowCardinalityNullableInt32, CHFuzzableType.LowCardinalityNullableInt64), CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.IntMax64Bits))),
    (Set(CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.Float)), CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.Int)), CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.UInt))), CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.NonDecimal))),
    (Set(CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.NonDecimalMax64Bits)), CHFuzzableType.LowCardinalityNullableInt128, CHFuzzableType.LowCardinalityNullableInt256, CHFuzzableType.LowCardinalityNullableUInt128, CHFuzzableType.LowCardinalityNullableUInt256), CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.NonDecimal))),
    (Set(CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.NonDecimalNorFloatMax64Bits)), CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.Float))), CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.NonDecimalMax64Bits))),
    (Set(CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.IntMax64Bits)), CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.UIntMax64Bits))), CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.NonDecimalNorFloatMax64Bits))),
    (Set(CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.UIntMax64Bits)), CHFuzzableType.LowCardinalityNullableUInt128, CHFuzzableType.LowCardinalityNullableUInt256), CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.UInt))),
    (Set(CHFuzzableType.LowCardinalityNullableUInt8, CHFuzzableType.LowCardinalityNullableUInt16, CHFuzzableType.LowCardinalityNullableUInt32, CHFuzzableType.LowCardinalityNullableUInt64), CHSpecialType.LowCardinality(CHSpecialType.Nullable(CHAggregatedType.UIntMax64Bits))),

    (Set(
      CHSpecialType.Nullable(CHAggregatedType.Number), CHFuzzableType.NullableDate, CHFuzzableType.NullableDate32,
      CHFuzzableType.NullableDateTime, CHFuzzableType.NullableDateTime64, CHFuzzableType.NullableIntervalNanosecond,
      CHFuzzableType.NullableIntervalMicrosecond, CHFuzzableType.NullableIntervalMillisecond,
      CHFuzzableType.NullableIntervalSecond, CHFuzzableType.NullableIntervalMinute, CHFuzzableType.NullableIntervalHour,
      CHFuzzableType.NullableIntervalDay, CHFuzzableType.NullableIntervalWeek, CHFuzzableType.NullableIntervalMonth,
      CHFuzzableType.NullableIntervalQuarter, CHFuzzableType.NullableIntervalYear, CHFuzzableType.NullableFixedString,
      CHFuzzableType.NullableString, CHFuzzableType.NullableUUID
    ), CHSpecialType.Nullable(CHAggregatedType.AnyNullableInnerType)),

    (Set(CHFuzzableType.NullableDecimal32, CHFuzzableType.NullableDecimal64, CHFuzzableType.NullableDecimal128, CHFuzzableType.NullableDecimal256), CHSpecialType.Nullable(CHAggregatedType.DecimalLike)),
    (Set(CHFuzzableType.NullableFloat32, CHFuzzableType.NullableFloat64), CHSpecialType.Nullable(CHAggregatedType.Float)),
    (Set(CHSpecialType.Nullable(CHAggregatedType.IntMax64Bits), CHFuzzableType.NullableInt128, CHFuzzableType.NullableInt256), CHSpecialType.Nullable(CHAggregatedType.Int)),
    (Set(CHFuzzableType.NullableInt8, CHFuzzableType.NullableInt16, CHFuzzableType.NullableInt32, CHFuzzableType.NullableInt64), CHSpecialType.Nullable(CHAggregatedType.IntMax64Bits)),
    (Set(CHSpecialType.Nullable(CHAggregatedType.Float), CHSpecialType.Nullable(CHAggregatedType.Int), CHSpecialType.Nullable(CHAggregatedType.UInt)), CHSpecialType.Nullable(CHAggregatedType.NonDecimal)),
    (Set(CHSpecialType.Nullable(CHAggregatedType.NonDecimalMax64Bits), CHFuzzableType.NullableInt128, CHFuzzableType.NullableInt256, CHFuzzableType.NullableUInt128, CHFuzzableType.NullableUInt256), CHSpecialType.Nullable(CHAggregatedType.NonDecimal)),
    (Set(CHSpecialType.Nullable(CHAggregatedType.NonDecimalNorFloatMax64Bits), CHSpecialType.Nullable(CHAggregatedType.Float)), CHSpecialType.Nullable(CHAggregatedType.NonDecimalMax64Bits)),
    (Set(CHSpecialType.Nullable(CHAggregatedType.IntMax64Bits), CHSpecialType.Nullable(CHAggregatedType.UIntMax64Bits)), CHSpecialType.Nullable(CHAggregatedType.NonDecimalNorFloatMax64Bits)),
    (Set(CHSpecialType.Nullable(CHAggregatedType.DecimalLike), CHSpecialType.Nullable(CHAggregatedType.NonDecimal)), CHSpecialType.Nullable(CHAggregatedType.Number)),
    (Set(CHSpecialType.Nullable(CHAggregatedType.UIntMax64Bits), CHFuzzableType.NullableUInt128, CHFuzzableType.NullableUInt256), CHSpecialType.Nullable(CHAggregatedType.UInt)),
    (Set(CHFuzzableType.NullableUInt8, CHFuzzableType.NullableUInt16, CHFuzzableType.NullableUInt32, CHFuzzableType.NullableUInt64), CHSpecialType.Nullable(CHAggregatedType.UIntMax64Bits))
  )

  private val supertypeDeduplicationRules: Map[Set[CHType], Seq[CHType]] = Map(
    // Array based
    (Set(CHFuzzableType.ArrayTuple1UUID, CHFuzzableType.Point, CHFuzzableType.Ring, CHFuzzableType.Polygon, CHFuzzableType.MultiPolygon), Seq(CHFuzzableType.ArrayTuple1UUID)),
    (Set(CHFuzzableType.Tuple1ArrayUUID, CHFuzzableType.Tuple1Point, CHFuzzableType.Tuple1Ring, CHFuzzableType.Tuple1Polygon, CHFuzzableType.Tuple1MultiPolygon), Seq(CHFuzzableType.Tuple1ArrayUUID)),

    // Number based - cf method `getNumericType` in https://github.com/ClickHouse/ClickHouse/blob/master/src/DataTypes/getLeastSupertype.cpp
    // Number based - Enum
    (Set(CHFuzzableType.Int8, CHFuzzableType.Enum8), Seq(CHFuzzableType.Int8)),
    (Set(CHFuzzableType.Int16, CHFuzzableType.Enum16), Seq(CHFuzzableType.Int16)),
    (Set(CHFuzzableType.Int8, CHFuzzableType.Int16, CHFuzzableType.Enum), Seq(CHFuzzableType.Int8, CHFuzzableType.Int16)),
    (Set(CHFuzzableType.ArrayInt8, CHFuzzableType.ArrayEnum8), Seq(CHFuzzableType.ArrayInt8)),
    (Set(CHFuzzableType.ArrayInt16, CHFuzzableType.ArrayEnum16), Seq(CHFuzzableType.ArrayInt16)),
    (Set(CHFuzzableType.ArrayInt8, CHFuzzableType.ArrayInt16, CHFuzzableType.ArrayEnum), Seq(CHFuzzableType.ArrayInt8, CHFuzzableType.ArrayInt16)),
    (Set(CHFuzzableType.MapInt8Int, CHFuzzableType.MapEnum8Int), Seq(CHFuzzableType.MapInt8Int)),
    (Set(CHFuzzableType.MapInt16Int, CHFuzzableType.MapEnum16Int), Seq(CHFuzzableType.MapInt16Int)),
    (Set(CHFuzzableType.MapInt8Int, CHFuzzableType.MapInt16Int, CHFuzzableType.MapEnumInt), Seq(CHFuzzableType.MapInt8Int, CHFuzzableType.MapInt16Int)),
    (Set(CHFuzzableType.Tuple1Int8, CHFuzzableType.Tuple1Enum8), Seq(CHFuzzableType.Tuple1Int8)),
    (Set(CHFuzzableType.Tuple1Int16, CHFuzzableType.Tuple1Enum16), Seq(CHFuzzableType.Tuple1Int16)),
    (Set(CHFuzzableType.Tuple1Int8, CHFuzzableType.Tuple1Int16, CHFuzzableType.Tuple1Enum), Seq(CHFuzzableType.Tuple1Int8, CHFuzzableType.Tuple1Int16)),
    (Set(CHFuzzableType.NullableInt8, CHFuzzableType.NullableEnum8), Seq(CHFuzzableType.NullableInt8)),
    (Set(CHFuzzableType.NullableInt16, CHFuzzableType.NullableEnum16), Seq(CHFuzzableType.NullableInt16)),
    (Set(CHFuzzableType.NullableInt8, CHFuzzableType.NullableInt16, CHFuzzableType.NullableEnum), Seq(CHFuzzableType.NullableInt8, CHFuzzableType.NullableInt16)),

    // Number based - IP
    (Set(CHFuzzableType.SpecialUInt64), Seq(CHFuzzableType.UInt64)),
    (Set(CHFuzzableType.UInt32, CHFuzzableType.IPv4), Seq(CHFuzzableType.UInt32)),
    (Set(CHFuzzableType.UInt128, CHFuzzableType.IPv6), Seq(CHFuzzableType.UInt128)),
    (Set(CHFuzzableType.ArrayUInt32, CHFuzzableType.ArrayIPv4), Seq(CHFuzzableType.ArrayUInt32)),
    (Set(CHFuzzableType.ArrayUInt128, CHFuzzableType.ArrayIPv6), Seq(CHFuzzableType.ArrayUInt128)),
    (Set(CHFuzzableType.MapUInt32Int, CHFuzzableType.MapIPv4Int), Seq(CHFuzzableType.MapUInt32Int)),
    (Set(CHFuzzableType.MapUInt128Int, CHFuzzableType.MapIPv6Int), Seq(CHFuzzableType.MapUInt128Int)),
    (Set(CHFuzzableType.Tuple1UInt32, CHFuzzableType.Tuple1IPv4), Seq(CHFuzzableType.Tuple1UInt32)),
    (Set(CHFuzzableType.Tuple1UInt128, CHFuzzableType.Tuple1IPv6), Seq(CHFuzzableType.Tuple1UInt128)),
    (Set(CHFuzzableType.NullableUInt32, CHFuzzableType.NullableIPv4), Seq(CHFuzzableType.NullableUInt32)),
    (Set(CHFuzzableType.NullableUInt128, CHFuzzableType.NullableIPv6), Seq(CHFuzzableType.NullableUInt128)),

    // String based
    (Set(CHFuzzableType.SpecialArrayFixedString), Seq(CHFuzzableType.ArrayFixedString)),
    (Set(CHFuzzableType.SpecialArrayString), Seq(CHFuzzableType.ArrayString)),
    (Set(CHFuzzableType.SpecialFixedString), Seq(CHFuzzableType.FixedString)),
    (Set(CHFuzzableType.SpecialLowCardinalityNullableUInt64), Seq(CHFuzzableType.LowCardinalityNullableUInt64)),
    (Set(CHFuzzableType.SpecialLowCardinalityUInt64), Seq(CHFuzzableType.LowCardinalityUInt64)),
    (Set(CHFuzzableType.SpecialNullableUInt64), Seq(CHFuzzableType.NullableUInt64)),
    (Set(CHFuzzableType.SpecialString), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.Charset), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.ClickHouseType), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.DateUnit), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.DictionaryName), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.EncryptionMode), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.InputFormat), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.OutputFormat), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.PValueComputationMethod), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.SequencePattern), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.ServerPortName), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.SynonymExtensionName), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.TestAlternative), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.Time64Unit), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.TimeUnit), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.TimeZone), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.TopKOption), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.Usevar), Seq(CHFuzzableType.StringType)),
    (Set(CHFuzzableType.StringType, CHFuzzableType.WindowFunctionMode), Seq(CHFuzzableType.StringType))
  ) ++
    allNumberTypes.map(t => (Set(t, CHFuzzableType.BooleanType), Seq(t))).toMap ++
    allLowCardinalityNumberTypes.map(t => (Set(t, CHFuzzableType.LowCardinalityBoolean), Seq(t))).toMap ++
    allLowCardinalityNullableNumberTypes.map(t => (Set(t, CHFuzzableType.LowCardinalityNullableBoolean), Seq(t))).toMap ++
    allNullableNumberTypes.map(t => (Set(t, CHFuzzableType.NullableBoolean), Seq(t))).toMap ++
    allArrayNumberTypes.map(t => (Set(t, CHFuzzableType.ArrayBoolean), Seq(t))).toMap ++
    allBitmapNumberTypes.map(t => (Set(t, CHFuzzableType.BitmapBoolean), Seq(t))).toMap ++
    allMapNumberTypes.map(t => (Set(t, CHFuzzableType.MapBooleanInt), Seq(t))).toMap ++
    allTuple1NumberTypes.map(t => (Set(t, CHFuzzableType.Tuple1Boolean), Seq(t))).toMap ++
    CHFuzzableType.values.toSet.map(t => (Set(t, CHFuzzableType.Dynamic), Seq(t))).toMap
  // format: on

}
