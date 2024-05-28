package com.amendil.common.entities.`type`

import com.typesafe.scalalogging.StrictLogging
import fastparse.*
import fastparse.NoWhitespace.*
import fastparse.Parsed.{Failure, Success}

import scala.annotation.nowarn

trait CHType:
  def name: String

object CHType extends StrictLogging:

  def mergeInputTypes(types: Set[CHType]): Set[CHType] =
    var mergedTypes = types

    // Handle Special types
    var toMerge = true
    while toMerge do
      val newMergedTypes = specialTypesSubstitutionRules.foldLeft(mergedTypes) {
        case (currentTypes, (subTypes, aggregatedType)) =>
          if subTypes.forall(currentTypes.contains) then currentTypes.removedAll(subTypes) + aggregatedType
          else currentTypes
      }

      toMerge = mergedTypes.size != newMergedTypes.size
      mergedTypes = newMergedTypes

    // Remove boolean when there is another kind of number being supported
    if mergedTypes.contains(CHFuzzableType.BooleanType) && mergedTypes.exists(allNumberTypes.contains) then
      mergedTypes -= CHFuzzableType.BooleanType

    if mergedTypes.contains(CHFuzzableType.LowCardinalityBoolean) && mergedTypes.exists(
        allLowCardinalityNumberTypes.contains
      )
    then mergedTypes -= CHFuzzableType.LowCardinalityBoolean

    if mergedTypes.contains(CHFuzzableType.LowCardinalityNullableBoolean) && mergedTypes.exists(
        allLowCardinalityNullableNumberTypes.contains
      )
    then mergedTypes -= CHFuzzableType.LowCardinalityNullableBoolean

    if mergedTypes.contains(CHFuzzableType.NullableBoolean) && mergedTypes.exists(allNullableNumberTypes.contains) then
      mergedTypes -= CHFuzzableType.NullableBoolean

    if mergedTypes.contains(CHFuzzableType.ArrayBoolean) && mergedTypes.exists(allArrayNumberTypes.contains) then
      mergedTypes -= CHFuzzableType.ArrayBoolean

    if mergedTypes.contains(CHFuzzableType.MapBooleanInt) && mergedTypes.exists(allMapNumberIntTypes.contains) then
      mergedTypes -= CHFuzzableType.MapBooleanInt

    if mergedTypes.contains(CHFuzzableType.Tuple1Boolean) && mergedTypes.exists(allTuple1NumberTypes.contains) then
      mergedTypes -= CHFuzzableType.Tuple1Boolean

    // Handle other rules
    toMerge = true
    while toMerge do
      val newMergedTypes = nonSpecialTypesSubstitutionRules.foldLeft(mergedTypes) {
        case (currentTypes, (subTypes, aggregatedType)) =>
          if subTypes.forall(currentTypes.contains) then currentTypes.removedAll(subTypes) + aggregatedType
          else currentTypes
      }

      toMerge = mergedTypes.size != newMergedTypes.size
      mergedTypes = newMergedTypes

    mergedTypes

  def mergeOutputType(type1: CHType, type2: CHType): CHType =
    import CHAggregatedType.*
    import CHFuzzableType.*

    if type1 == type2 then type1 // Expects both type to be identical, should be the most obvious use case
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
            case _                                       => Any
        else if type2 == Int8 then
          type1 match
            case UInt8                                   => Int16
            case UInt16                                  => Int32
            case UInt32                                  => Int64
            case UInt64                                  => Int128
            case UInt128 | UInt256                       => Int256
            case Int16 | Int32 | Int64 | Int128 | Int256 => type1
            case _                                       => Any
        else if type1 == Int16 then
          type2 match
            case UInt8                           => Int16
            case UInt16                          => Int32
            case UInt32                          => Int64
            case UInt64                          => Int128
            case UInt128 | UInt256               => Int256
            case Int32 | Int64 | Int128 | Int256 => type2
            case _                               => Any
        else if type2 == Int16 then
          type1 match
            case UInt8                           => Int16
            case UInt16                          => Int32
            case UInt32                          => Int64
            case UInt64                          => Int128
            case UInt128 | UInt256               => Int256
            case Int32 | Int64 | Int128 | Int256 => type1
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
        // From now on, neither type1 nor type2 can be an unsigned integer
        else if type1 == Float32 then
          type2 match
            case Float64 => Float64
            case _       => Any
        else if type2 == Float32 then
          type1 match
            case Float64 => Float64
            case _       => Any
        // From now on, neither type1 nor type2 can be a float number
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
        else Any

      mergedType

  def getByName(name: String): CHType =
    // nowarn reason: https://github.com/scala/scala3/issues/19872
    parse(name.replaceAll("\n\\s+", ""), root: @nowarn) match
      case Success(value, _) => value
      case Failure(label, idx, extra) =>
        logger.warn(s"Failed to parse type $name: label: $label, idx: $idx")
        throw IllegalArgumentException(s"Failed to parse type $name: label: $label, idx: $idx")

  // Utils
  // format: off
  private val nonSpecialTypesSubstitutionRules = Map(
    (Set(
      CHAggregatedType.Bitmap, CHAggregatedType.DateLikeOrDateTimeLike, CHFuzzableType.Enum, CHAggregatedType.Interval,
      CHAggregatedType.IP, CHAggregatedType.Geo, CHFuzzableType.Json, CHAggregatedType.Number, CHAggregatedType.StringLike,
      CHFuzzableType.UUID, CHSpecialType.Array(CHAggregatedType.Any), CHSpecialType.Map(CHAggregatedType.MapKey, CHAggregatedType.Int),
      CHSpecialType.Tuple(Seq(CHAggregatedType.Any))
    ), CHAggregatedType.AnyNonNullableNonLowCardinality),

    (Set(
      CHAggregatedType.AnyNonNullableNonLowCardinality, CHAggregatedType.Nullable, CHAggregatedType.LowCardinality,
      CHAggregatedType.LowCardinalityNullable
    ), CHAggregatedType.Any),

    (Set(CHFuzzableType.Decimal32, CHFuzzableType.Decimal64, CHFuzzableType.Decimal128, CHFuzzableType.Decimal256), CHAggregatedType.DecimalLike),
    (Set(CHFuzzableType.Float32, CHFuzzableType.Float64), CHAggregatedType.Float),
    (Set(CHAggregatedType.IntMax64Bits, CHFuzzableType.Int128, CHFuzzableType.Int256), CHAggregatedType.Int),
    (Set(CHFuzzableType.Int8, CHFuzzableType.Int16, CHFuzzableType.Int32, CHFuzzableType.Int64), CHAggregatedType.IntMax64Bits),
    (Set(CHAggregatedType.Float, CHAggregatedType.Int, CHAggregatedType.UInt), CHAggregatedType.NonDecimal),
    (Set(CHAggregatedType.NonDecimalMax64Bits, CHFuzzableType.Int128, CHFuzzableType.Int256, CHFuzzableType.UInt128, CHFuzzableType.UInt256), CHAggregatedType.NonDecimal),
    (Set(CHAggregatedType.NonDecimalNorFloatMax64Bits, CHAggregatedType.Float), CHAggregatedType.NonDecimalMax64Bits),
    (Set(CHAggregatedType.IntMax64Bits, CHAggregatedType.UIntMax64Bits), CHAggregatedType.NonDecimalNorFloatMax64Bits),
    (Set(CHAggregatedType.DecimalLike, CHAggregatedType.NonDecimal), CHAggregatedType.Number),
    (Set(CHAggregatedType.UIntMax64Bits, CHFuzzableType.UInt128, CHFuzzableType.UInt256), CHAggregatedType.UInt),
    (Set(CHFuzzableType.UInt8, CHFuzzableType.UInt16, CHFuzzableType.UInt32, CHFuzzableType.UInt64), CHAggregatedType.UIntMax64Bits),

    (Set(
      CHFuzzableType.BitmapInt8, CHFuzzableType.BitmapInt16, CHFuzzableType.BitmapInt32, CHFuzzableType.BitmapInt64, 
      CHFuzzableType.BitmapUInt8, CHFuzzableType.BitmapUInt16, CHFuzzableType.BitmapUInt32, CHFuzzableType.BitmapUInt64
    ), CHAggregatedType.Bitmap),

    (Set(CHFuzzableType.Date, CHFuzzableType.DateTime), CHAggregatedType.DateOrDateTime),
    (Set(CHAggregatedType.DateOrDateTime, CHFuzzableType.Date32, CHFuzzableType.DateTime64), CHAggregatedType.DateLikeOrDateTimeLike),
    (Set(CHAggregatedType.DateLike, CHAggregatedType.DateTimeLike), CHAggregatedType.DateLikeOrDateTimeLike),
    (Set(CHFuzzableType.Date, CHFuzzableType.Date32), CHAggregatedType.DateLike),
    (Set(CHFuzzableType.DateTime, CHFuzzableType.DateTime64), CHAggregatedType.DateTimeLike),

    (Set(CHFuzzableType.Enum8, CHFuzzableType.Enum16), CHFuzzableType.Enum),

    (Set(CHFuzzableType.IntervalNanosecond, CHFuzzableType.IntervalMicrosecond, CHFuzzableType.IntervalMillisecond, CHAggregatedType.IntervalTime), CHAggregatedType.IntervalTime64),
    (Set(CHFuzzableType.IntervalSecond, CHFuzzableType.IntervalMinute, CHFuzzableType.IntervalHour), CHAggregatedType.IntervalTime),
    (Set(CHFuzzableType.IntervalDay, CHFuzzableType.IntervalWeek, CHFuzzableType.IntervalMonth, CHFuzzableType.IntervalQuarter, CHFuzzableType.IntervalYear), CHAggregatedType.IntervalDate),
    (Set(CHAggregatedType.IntervalTime64, CHAggregatedType.IntervalDate), CHAggregatedType.Interval),

    (Set(CHFuzzableType.IPv4, CHFuzzableType.IPv6), CHAggregatedType.IP),

    (Set(CHFuzzableType.Point, CHFuzzableType.Ring, CHFuzzableType.Polygon, CHFuzzableType.MultiPolygon), CHAggregatedType.Geo),

    (Set(CHFuzzableType.StringType, CHFuzzableType.FixedString), CHAggregatedType.StringLike),

    (Set(CHFuzzableType.ArrayDecimal32, CHFuzzableType.ArrayDecimal64, CHFuzzableType.ArrayDecimal128, CHFuzzableType.ArrayDecimal256), CHSpecialType.Array(CHAggregatedType.DecimalLike)),
    (Set(CHFuzzableType.ArrayFloat32, CHFuzzableType.ArrayFloat64), CHSpecialType.Array(CHAggregatedType.Float)),
    (Set(CHSpecialType.Array(CHAggregatedType.IntMax64Bits), CHFuzzableType.ArrayInt128, CHFuzzableType.ArrayInt256), CHSpecialType.Array(CHAggregatedType.Int)),
    (Set(CHFuzzableType.ArrayInt8, CHFuzzableType.ArrayInt16, CHFuzzableType.ArrayInt32, CHFuzzableType.ArrayInt64), CHSpecialType.Array(CHAggregatedType.IntMax64Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.Float), CHSpecialType.Array(CHAggregatedType.Int), CHSpecialType.Array(CHAggregatedType.UInt)), CHSpecialType.Array(CHAggregatedType.NonDecimal)),
    (Set(CHSpecialType.Array(CHAggregatedType.NonDecimalMax64Bits), CHFuzzableType.ArrayInt128, CHFuzzableType.ArrayInt256, CHFuzzableType.ArrayUInt128, CHFuzzableType.ArrayUInt256), CHSpecialType.Array(CHAggregatedType.NonDecimal)),
    (Set(CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax64Bits), CHSpecialType.Array(CHAggregatedType.Float)), CHSpecialType.Array(CHAggregatedType.NonDecimalMax64Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.IntMax64Bits), CHSpecialType.Array(CHAggregatedType.UIntMax64Bits)), CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax64Bits)),
    (Set(CHSpecialType.Array(CHAggregatedType.DecimalLike), CHSpecialType.Array(CHAggregatedType.NonDecimal)), CHSpecialType.Array(CHAggregatedType.Number)),
    (Set(CHSpecialType.Array(CHAggregatedType.UIntMax64Bits), CHFuzzableType.ArrayUInt128, CHFuzzableType.ArrayUInt256), CHSpecialType.Array(CHAggregatedType.UInt)),
    (Set(CHFuzzableType.ArrayUInt8, CHFuzzableType.ArrayUInt16, CHFuzzableType.ArrayUInt32, CHFuzzableType.ArrayUInt64), CHSpecialType.Array(CHAggregatedType.UIntMax64Bits)),

    (Set(CHFuzzableType.ArrayDate, CHFuzzableType.ArrayDateTime), CHSpecialType.Array(CHAggregatedType.DateOrDateTime)),
    (Set(CHSpecialType.Array(CHAggregatedType.DateOrDateTime), CHFuzzableType.ArrayDate32, CHFuzzableType.ArrayDateTime64), CHSpecialType.Array(CHAggregatedType.DateLikeOrDateTimeLike)),
    (Set(CHSpecialType.Array(CHAggregatedType.DateLike), CHSpecialType.Array(CHAggregatedType.DateTimeLike)), CHSpecialType.Array(CHAggregatedType.DateLikeOrDateTimeLike)),
    (Set(CHFuzzableType.ArrayDate, CHFuzzableType.ArrayDate32), CHSpecialType.Array(CHAggregatedType.DateLike)),
    (Set(CHFuzzableType.ArrayDateTime, CHFuzzableType.ArrayDateTime64), CHSpecialType.Array(CHAggregatedType.DateTimeLike)),

    (Set(CHFuzzableType.ArrayEnum8, CHFuzzableType.ArrayEnum16), CHFuzzableType.ArrayEnum),

    (Set(CHFuzzableType.ArrayIntervalNanosecond, CHFuzzableType.ArrayIntervalMicrosecond, CHFuzzableType.ArrayIntervalMillisecond, CHSpecialType.Array(CHAggregatedType.IntervalTime)), CHSpecialType.Array(CHAggregatedType.IntervalTime64)),
    (Set(CHFuzzableType.ArrayIntervalSecond, CHFuzzableType.ArrayIntervalMinute, CHFuzzableType.ArrayIntervalHour), CHSpecialType.Array(CHAggregatedType.IntervalTime)),
    (Set(CHFuzzableType.ArrayIntervalDay, CHFuzzableType.ArrayIntervalWeek, CHFuzzableType.ArrayIntervalMonth, CHFuzzableType.ArrayIntervalQuarter, CHFuzzableType.ArrayIntervalYear), CHSpecialType.Array(CHAggregatedType.IntervalDate)),
    (Set(CHSpecialType.Array(CHAggregatedType.IntervalTime64), CHSpecialType.Array(CHAggregatedType.IntervalDate)), CHSpecialType.Array(CHAggregatedType.Interval)),

    (Set(CHFuzzableType.ArrayIPv4, CHFuzzableType.ArrayIPv6), CHSpecialType.Array(CHAggregatedType.IP)),

    (Set(CHFuzzableType.ArrayPoint, CHFuzzableType.ArrayRing, CHFuzzableType.ArrayPolygon, CHFuzzableType.ArrayMultiPolygon), CHSpecialType.Array(CHAggregatedType.Geo)),

    (Set(CHFuzzableType.ArrayString, CHFuzzableType.ArrayFixedString), CHSpecialType.Array(CHAggregatedType.StringLike)),

    (Set(
      CHSpecialType.Array(CHAggregatedType.Number), CHSpecialType.Array(CHAggregatedType.DateLikeOrDateTimeLike),
      CHSpecialType.Array(CHAggregatedType.Interval), CHSpecialType.Array(CHAggregatedType.Geo),
      CHFuzzableType.ArrayEnum, CHSpecialType.Array(CHAggregatedType.StringLike), CHSpecialType.Array(CHAggregatedType.IP),
      CHFuzzableType.ArrayJson, CHFuzzableType.ArrayUUID, CHFuzzableType.ArrayMapStringInt, CHFuzzableType.ArrayTuple1UInt8
    ), CHSpecialType.Array(CHAggregatedType.Any)),

    (Set(CHFuzzableType.MapInt8Int, CHFuzzableType.MapInt16Int, CHFuzzableType.MapInt32Int, CHFuzzableType.MapInt64Int), CHSpecialType.Map(CHAggregatedType.IntMax64Bits, CHAggregatedType.Int)),
    (Set(CHFuzzableType.MapUInt8Int, CHFuzzableType.MapUInt16Int, CHFuzzableType.MapUInt32Int, CHFuzzableType.MapUInt64Int), CHSpecialType.Map(CHAggregatedType.UIntMax64Bits, CHAggregatedType.Int)),
    (Set(CHSpecialType.Map(CHAggregatedType.IntMax64Bits, CHAggregatedType.Int), CHFuzzableType.MapInt128Int, CHFuzzableType.MapInt256Int), CHSpecialType.Map(CHAggregatedType.Int, CHAggregatedType.Int)),
    (Set(CHSpecialType.Map(CHAggregatedType.UIntMax64Bits, CHAggregatedType.Int), CHFuzzableType.MapUInt128Int, CHFuzzableType.MapUInt256Int), CHSpecialType.Map(CHAggregatedType.UInt, CHAggregatedType.Int)),
    (Set(CHSpecialType.Map(CHAggregatedType.IntMax64Bits, CHAggregatedType.Int), CHSpecialType.Map(CHAggregatedType.UIntMax64Bits, CHAggregatedType.Int)), CHSpecialType.Map(CHAggregatedType.NonDecimalNorFloatMax64Bits, CHAggregatedType.Int)),
    (Set(CHSpecialType.Map(CHAggregatedType.Int, CHAggregatedType.Int), CHSpecialType.Map(CHAggregatedType.UInt, CHAggregatedType.Int)), CHSpecialType.Map(CHAggregatedType.NonDecimalNorFloat, CHAggregatedType.Int)),
    (Set(CHSpecialType.Map(CHAggregatedType.NonDecimalNorFloatMax64Bits, CHAggregatedType.Int), CHFuzzableType.MapInt128Int, CHFuzzableType.MapInt256Int, CHFuzzableType.MapUInt128Int, CHFuzzableType.MapUInt256Int), CHSpecialType.Map(CHAggregatedType.NonDecimalNorFloat, CHAggregatedType.Int)),

    (Set(CHFuzzableType.MapDateInt, CHFuzzableType.MapDateTimeInt), CHSpecialType.Map(CHAggregatedType.DateOrDateTime, CHAggregatedType.Int)),
    (Set(CHFuzzableType.MapDateInt, CHFuzzableType.MapDate32Int), CHSpecialType.Map(CHAggregatedType.DateLike, CHAggregatedType.Int)),
    (Set(CHSpecialType.Map(CHAggregatedType.DateOrDateTime, CHAggregatedType.Int), CHFuzzableType.MapDate32Int), CHSpecialType.Map(CHAggregatedType.DateLikeOrDateTime, CHAggregatedType.Int)),
    (Set(CHSpecialType.Map(CHAggregatedType.DateLike, CHAggregatedType.Int), CHFuzzableType.MapDateTimeInt), CHSpecialType.Map(CHAggregatedType.DateLikeOrDateTime, CHAggregatedType.Int)),

    (Set(CHFuzzableType.MapEnum8Int, CHFuzzableType.MapEnum16Int), CHFuzzableType.MapEnumInt),

    (Set(CHFuzzableType.MapIntervalNanosecondInt, CHFuzzableType.MapIntervalMicrosecondInt, CHFuzzableType.MapIntervalMillisecondInt, CHSpecialType.Map(CHAggregatedType.IntervalTime, CHAggregatedType.Int)), CHSpecialType.Map(CHAggregatedType.IntervalTime64, CHAggregatedType.Int)),
    (Set(CHFuzzableType.MapIntervalSecondInt, CHFuzzableType.MapIntervalMinuteInt, CHFuzzableType.MapIntervalHourInt), CHSpecialType.Map(CHAggregatedType.IntervalTime, CHAggregatedType.Int)),
    (Set(CHFuzzableType.MapIntervalDayInt, CHFuzzableType.MapIntervalWeekInt, CHFuzzableType.MapIntervalMonthInt, CHFuzzableType.MapIntervalQuarterInt, CHFuzzableType.MapIntervalYearInt), CHSpecialType.Map(CHAggregatedType.IntervalDate, CHAggregatedType.Int)),
    (Set(CHSpecialType.Map(CHAggregatedType.IntervalTime64, CHAggregatedType.Int), CHSpecialType.Map(CHAggregatedType.IntervalDate, CHAggregatedType.Int)), CHSpecialType.Map(CHAggregatedType.Interval, CHAggregatedType.Int)),

    (Set(CHFuzzableType.MapIPv4Int, CHFuzzableType.MapIPv6Int), CHSpecialType.Map(CHAggregatedType.IP, CHAggregatedType.Int)),

    (Set(CHFuzzableType.MapStringInt, CHFuzzableType.MapFixedStringInt), CHSpecialType.Map(CHAggregatedType.StringLike, CHAggregatedType.Int)),

    (Set(
      CHSpecialType.Map(CHAggregatedType.NonDecimalNorFloat, CHAggregatedType.Int), CHSpecialType.Map(CHAggregatedType.DateLikeOrDateTime, CHAggregatedType.Int),
      CHSpecialType.Map(CHAggregatedType.Interval, CHAggregatedType.Int), CHFuzzableType.MapEnumInt, CHSpecialType.Map(CHAggregatedType.StringLike, CHAggregatedType.Int),
      CHSpecialType.Map(CHAggregatedType.IP, CHAggregatedType.Int), CHFuzzableType.MapUUIDInt
    ), CHSpecialType.Map(CHAggregatedType.MapKey, CHAggregatedType.Int)),

    (Set(CHFuzzableType.Tuple1Decimal32, CHFuzzableType.Tuple1Decimal64, CHFuzzableType.Tuple1Decimal128, CHFuzzableType.Tuple1Decimal256), CHSpecialType.Tuple(Seq(CHAggregatedType.DecimalLike))),
    (Set(CHFuzzableType.Tuple1Float32, CHFuzzableType.Tuple1Float64), CHSpecialType.Tuple(Seq(CHAggregatedType.Float))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax64Bits)), CHFuzzableType.Tuple1Int128, CHFuzzableType.Tuple1Int256), CHSpecialType.Tuple(Seq(CHAggregatedType.Int))),
    (Set(CHFuzzableType.Tuple1Int8, CHFuzzableType.Tuple1Int16, CHFuzzableType.Tuple1Int32, CHFuzzableType.Tuple1Int64), CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax64Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.Float)), CHSpecialType.Tuple(Seq(CHAggregatedType.Int)), CHSpecialType.Tuple(Seq(CHAggregatedType.UInt))), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimal))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalMax64Bits)), CHFuzzableType.Tuple1Int128, CHFuzzableType.Tuple1Int256, CHFuzzableType.Tuple1UInt128, CHFuzzableType.Tuple1UInt256), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimal))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax64Bits)), CHSpecialType.Tuple(Seq(CHAggregatedType.Float))), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalMax64Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax64Bits)), CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax64Bits))), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax64Bits))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.DecimalLike)), CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimal))), CHSpecialType.Tuple(Seq(CHAggregatedType.Number))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax64Bits)), CHFuzzableType.Tuple1UInt128, CHFuzzableType.Tuple1UInt256), CHSpecialType.Tuple(Seq(CHAggregatedType.UInt))),
    (Set(CHFuzzableType.Tuple1UInt8, CHFuzzableType.Tuple1UInt16, CHFuzzableType.Tuple1UInt32, CHFuzzableType.Tuple1UInt64), CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax64Bits))),

    (Set(CHFuzzableType.Tuple1Date, CHFuzzableType.Tuple1DateTime), CHSpecialType.Tuple(Seq(CHAggregatedType.DateOrDateTime))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.DateOrDateTime)), CHFuzzableType.Tuple1Date32, CHFuzzableType.Tuple1DateTime64), CHSpecialType.Tuple(Seq(CHAggregatedType.DateLikeOrDateTimeLike))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.DateLike)), CHSpecialType.Tuple(Seq(CHAggregatedType.DateTimeLike))), CHSpecialType.Tuple(Seq(CHAggregatedType.DateLikeOrDateTimeLike))),
    (Set(CHFuzzableType.Tuple1Date, CHFuzzableType.Tuple1Date32), CHSpecialType.Tuple(Seq(CHAggregatedType.DateLike))),
    (Set(CHFuzzableType.Tuple1DateTime, CHFuzzableType.Tuple1DateTime64), CHSpecialType.Tuple(Seq(CHAggregatedType.DateTimeLike))),

    (Set(CHFuzzableType.Tuple1Enum8, CHFuzzableType.Tuple1Enum16), CHFuzzableType.Tuple1Enum),

    (Set(CHFuzzableType.Tuple1IntervalNanosecond, CHFuzzableType.Tuple1IntervalMicrosecond, CHFuzzableType.Tuple1IntervalMillisecond, CHSpecialType.Tuple(Seq(CHAggregatedType.IntervalTime))), CHSpecialType.Tuple(Seq(CHAggregatedType.IntervalTime64))),
    (Set(CHFuzzableType.Tuple1IntervalSecond, CHFuzzableType.Tuple1IntervalMinute, CHFuzzableType.Tuple1IntervalHour), CHSpecialType.Tuple(Seq(CHAggregatedType.IntervalTime))),
    (Set(CHFuzzableType.Tuple1IntervalDay, CHFuzzableType.Tuple1IntervalWeek, CHFuzzableType.Tuple1IntervalMonth, CHFuzzableType.Tuple1IntervalQuarter, CHFuzzableType.Tuple1IntervalYear), CHSpecialType.Tuple(Seq(CHAggregatedType.IntervalDate))),
    (Set(CHSpecialType.Tuple(Seq(CHAggregatedType.IntervalTime64)), CHSpecialType.Tuple(Seq(CHAggregatedType.IntervalDate))), CHSpecialType.Tuple(Seq(CHAggregatedType.Interval))),

    (Set(CHFuzzableType.Tuple1IPv4, CHFuzzableType.Tuple1IPv6), CHSpecialType.Tuple(Seq(CHAggregatedType.IP))),

    (Set(CHFuzzableType.Tuple1Point, CHFuzzableType.Tuple1Ring, CHFuzzableType.Tuple1Polygon, CHFuzzableType.Tuple1MultiPolygon), CHSpecialType.Tuple(Seq(CHAggregatedType.Geo))),

    (Set(CHFuzzableType.Tuple1String, CHFuzzableType.Tuple1FixedString), CHSpecialType.Tuple(Seq(CHAggregatedType.StringLike))),

    (Set(
      CHSpecialType.Tuple(Seq(CHAggregatedType.Number)), CHSpecialType.Tuple(Seq(CHAggregatedType.DateLikeOrDateTimeLike)),
      CHSpecialType.Tuple(Seq(CHAggregatedType.Interval)), CHSpecialType.Tuple(Seq(CHAggregatedType.Geo)),
      CHFuzzableType.Tuple1Enum, CHSpecialType.Tuple(Seq(CHAggregatedType.StringLike)),
      CHSpecialType.Tuple(Seq(CHAggregatedType.IP)), CHFuzzableType.Tuple1Json, CHFuzzableType.Tuple1UUID,
      CHFuzzableType.Tuple1ArrayUInt8, CHFuzzableType.Tuple1MapStringInt
    ), CHSpecialType.Tuple(Seq(CHAggregatedType.Any))),

    (Set(
      CHAggregatedType.LowCardinalityNonDecimal, CHFuzzableType.LowCardinalityDate, CHFuzzableType.LowCardinalityDate32, CHFuzzableType.LowCardinalityDateTime, CHFuzzableType.LowCardinalityFixedString, CHFuzzableType.LowCardinalityString
    ), CHAggregatedType.LowCardinality),

    (Set(CHFuzzableType.LowCardinalityFloat32, CHFuzzableType.LowCardinalityFloat64), CHAggregatedType.LowCardinalityFloat),
    (Set(CHAggregatedType.LowCardinalityIntMax64Bits, CHFuzzableType.LowCardinalityInt128, CHFuzzableType.LowCardinalityInt256), CHAggregatedType.LowCardinalityInt),
    (Set(CHFuzzableType.LowCardinalityInt8, CHFuzzableType.LowCardinalityInt16, CHFuzzableType.LowCardinalityInt32, CHFuzzableType.LowCardinalityInt64), CHAggregatedType.LowCardinalityIntMax64Bits),
    (Set(CHAggregatedType.LowCardinalityFloat, CHAggregatedType.LowCardinalityInt, CHAggregatedType.LowCardinalityUInt), CHAggregatedType.LowCardinalityNonDecimal),
    (Set(CHAggregatedType.LowCardinalityNonDecimalMax64Bits, CHFuzzableType.LowCardinalityInt128, CHFuzzableType.LowCardinalityInt256, CHFuzzableType.LowCardinalityUInt128, CHFuzzableType.LowCardinalityUInt256), CHAggregatedType.LowCardinalityNonDecimal),
    (Set(CHAggregatedType.LowCardinalityNonDecimalNorFloatMax64Bits, CHAggregatedType.LowCardinalityFloat), CHAggregatedType.LowCardinalityNonDecimalMax64Bits),
    (Set(CHAggregatedType.LowCardinalityIntMax64Bits, CHAggregatedType.LowCardinalityUIntMax64Bits), CHAggregatedType.LowCardinalityNonDecimalNorFloatMax64Bits),
    (Set(CHAggregatedType.LowCardinalityUIntMax64Bits, CHFuzzableType.LowCardinalityUInt128, CHFuzzableType.LowCardinalityUInt256), CHAggregatedType.LowCardinalityUInt),
    (Set(CHFuzzableType.LowCardinalityUInt8, CHFuzzableType.LowCardinalityUInt16, CHFuzzableType.LowCardinalityUInt32, CHFuzzableType.LowCardinalityUInt64), CHAggregatedType.LowCardinalityUIntMax64Bits),

    (Set(
      CHAggregatedType.LowCardinalityNullableNonDecimal, CHFuzzableType.LowCardinalityNullableDate, CHFuzzableType.LowCardinalityNullableDate32, CHFuzzableType.LowCardinalityNullableDateTime, CHFuzzableType.LowCardinalityNullableFixedString, CHFuzzableType.LowCardinalityNullableString
    ), CHAggregatedType.LowCardinalityNullable),

    (Set(CHFuzzableType.LowCardinalityNullableFloat32, CHFuzzableType.LowCardinalityNullableFloat64), CHAggregatedType.LowCardinalityNullableFloat),
    (Set(CHAggregatedType.LowCardinalityNullableIntMax64Bits, CHFuzzableType.LowCardinalityNullableInt128, CHFuzzableType.LowCardinalityNullableInt256), CHAggregatedType.LowCardinalityNullableInt),
    (Set(CHFuzzableType.LowCardinalityNullableInt8, CHFuzzableType.LowCardinalityNullableInt16, CHFuzzableType.LowCardinalityNullableInt32, CHFuzzableType.LowCardinalityNullableInt64), CHAggregatedType.LowCardinalityNullableIntMax64Bits),
    (Set(CHAggregatedType.LowCardinalityNullableFloat, CHAggregatedType.LowCardinalityNullableInt, CHAggregatedType.LowCardinalityNullableUInt), CHAggregatedType.LowCardinalityNullableNonDecimal),
    (Set(CHAggregatedType.LowCardinalityNullableNonDecimalMax64Bits, CHFuzzableType.LowCardinalityNullableInt128, CHFuzzableType.LowCardinalityNullableInt256, CHFuzzableType.LowCardinalityNullableUInt128, CHFuzzableType.LowCardinalityNullableUInt256), CHAggregatedType.LowCardinalityNullableNonDecimal),
    (Set(CHAggregatedType.LowCardinalityNullableNonDecimalNorFloatMax64Bits, CHAggregatedType.LowCardinalityNullableFloat), CHAggregatedType.LowCardinalityNullableNonDecimalMax64Bits),
    (Set(CHAggregatedType.LowCardinalityNullableIntMax64Bits, CHAggregatedType.LowCardinalityNullableUIntMax64Bits), CHAggregatedType.LowCardinalityNullableNonDecimalNorFloatMax64Bits),
    (Set(CHAggregatedType.LowCardinalityNullableUIntMax64Bits, CHFuzzableType.LowCardinalityNullableUInt128, CHFuzzableType.LowCardinalityNullableUInt256), CHAggregatedType.LowCardinalityNullableUInt),
    (Set(CHFuzzableType.LowCardinalityNullableUInt8, CHFuzzableType.LowCardinalityNullableUInt16, CHFuzzableType.LowCardinalityNullableUInt32, CHFuzzableType.LowCardinalityNullableUInt64), CHAggregatedType.LowCardinalityNullableUIntMax64Bits),

    (Set(
      CHAggregatedType.NullableNumber, CHFuzzableType.NullableDate, CHFuzzableType.NullableDate32,
      CHFuzzableType.NullableDateTime, CHFuzzableType.NullableDateTime64, CHFuzzableType.NullableIntervalNanosecond,
      CHFuzzableType.NullableIntervalMicrosecond, CHFuzzableType.NullableIntervalMillisecond,
      CHFuzzableType.NullableIntervalSecond, CHFuzzableType.NullableIntervalMinute, CHFuzzableType.NullableIntervalHour,
      CHFuzzableType.NullableIntervalDay, CHFuzzableType.NullableIntervalWeek, CHFuzzableType.NullableIntervalMonth,
      CHFuzzableType.NullableIntervalQuarter, CHFuzzableType.NullableIntervalYear, CHFuzzableType.NullableEnum,
      CHFuzzableType.NullableEnum8, CHFuzzableType.NullableEnum16, CHFuzzableType.NullableFixedString, CHFuzzableType.NullableIPv4,
      CHFuzzableType.NullableIPv6, CHFuzzableType.NullableString, CHFuzzableType.NullableUUID
    ), CHAggregatedType.Nullable),

    (Set(CHFuzzableType.NullableDecimal32, CHFuzzableType.NullableDecimal64, CHFuzzableType.NullableDecimal128, CHFuzzableType.NullableDecimal256), CHAggregatedType.NullableDecimalLike),
    (Set(CHFuzzableType.NullableFloat32, CHFuzzableType.NullableFloat64), CHAggregatedType.NullableFloat),
    (Set(CHAggregatedType.NullableIntMax64Bits, CHFuzzableType.NullableInt128, CHFuzzableType.NullableInt256), CHAggregatedType.NullableInt),
    (Set(CHFuzzableType.NullableInt8, CHFuzzableType.NullableInt16, CHFuzzableType.NullableInt32, CHFuzzableType.NullableInt64), CHAggregatedType.NullableIntMax64Bits),
    (Set(CHAggregatedType.NullableFloat, CHAggregatedType.NullableInt, CHAggregatedType.NullableUInt), CHAggregatedType.NullableNonDecimal),
    (Set(CHAggregatedType.NullableNonDecimalMax64Bits, CHFuzzableType.NullableInt128, CHFuzzableType.NullableInt256, CHFuzzableType.NullableUInt128, CHFuzzableType.NullableUInt256), CHAggregatedType.NullableNonDecimal),
    (Set(CHAggregatedType.NullableNonDecimalNorFloatMax64Bits, CHAggregatedType.NullableFloat), CHAggregatedType.NullableNonDecimalMax64Bits),
    (Set(CHAggregatedType.NullableIntMax64Bits, CHAggregatedType.NullableUIntMax64Bits), CHAggregatedType.NullableNonDecimalNorFloatMax64Bits),
    (Set(CHAggregatedType.NullableDecimalLike, CHAggregatedType.NullableNonDecimal), CHAggregatedType.NullableNumber),
    (Set(CHAggregatedType.NullableUIntMax64Bits, CHFuzzableType.NullableUInt128, CHFuzzableType.NullableUInt256), CHAggregatedType.NullableUInt),
    (Set(CHFuzzableType.NullableUInt8, CHFuzzableType.NullableUInt16, CHFuzzableType.NullableUInt32, CHFuzzableType.NullableUInt64), CHAggregatedType.NullableUIntMax64Bits)
  )

  private val specialTypesSubstitutionRules = Map(
    (Set(CHFuzzableType.ArrayFixedString, CHFuzzableType.SpecialArrayFixedString), CHFuzzableType.ArrayFixedString),
    (Set(CHFuzzableType.ArrayString, CHFuzzableType.SpecialArrayString), CHFuzzableType.ArrayString),
    (Set(CHFuzzableType.FixedString, CHFuzzableType.SpecialFixedString), CHFuzzableType.FixedString),
    (Set(CHFuzzableType.LowCardinalityNullableUInt64, CHFuzzableType.SpecialLowCardinalityNullableUInt64), CHFuzzableType.LowCardinalityNullableUInt64),
    (Set(CHFuzzableType.LowCardinalityUInt64, CHFuzzableType.SpecialLowCardinalityUInt64), CHFuzzableType.LowCardinalityUInt64),
    (Set(CHFuzzableType.NullableUInt64, CHFuzzableType.SpecialNullableUInt64), CHFuzzableType.NullableUInt64),
    (Set(CHFuzzableType.StringType, CHFuzzableType.Charset), CHFuzzableType.StringType),
    (Set(CHFuzzableType.StringType, CHFuzzableType.ClickHouseType), CHFuzzableType.StringType),
    (Set(CHFuzzableType.StringType, CHFuzzableType.DateUnit), CHFuzzableType.StringType),
    (Set(CHFuzzableType.StringType, CHFuzzableType.DictionaryName), CHFuzzableType.StringType),
    (Set(CHFuzzableType.StringType, CHFuzzableType.EncryptionMode), CHFuzzableType.StringType),
    (Set(CHFuzzableType.StringType, CHFuzzableType.PValueComputationMethod), CHFuzzableType.StringType),
    (Set(CHFuzzableType.StringType, CHFuzzableType.SequencePattern), CHFuzzableType.StringType),
    (Set(CHFuzzableType.StringType, CHFuzzableType.ServerPortName), CHFuzzableType.StringType),
    (Set(CHFuzzableType.StringType, CHFuzzableType.SpecialString), CHFuzzableType.StringType),
    (Set(CHFuzzableType.StringType, CHFuzzableType.SynonymExtensionName), CHFuzzableType.StringType),
    (Set(CHFuzzableType.StringType, CHFuzzableType.TestAlternative), CHFuzzableType.StringType),
    (Set(CHFuzzableType.StringType, CHFuzzableType.Time64Unit), CHFuzzableType.StringType),
    (Set(CHFuzzableType.StringType, CHFuzzableType.TimeUnit), CHFuzzableType.StringType),
    (Set(CHFuzzableType.StringType, CHFuzzableType.TimeZone), CHFuzzableType.StringType),
    (Set(CHFuzzableType.StringType, CHFuzzableType.Usevar), CHFuzzableType.StringType),
    (Set(CHFuzzableType.StringType, CHFuzzableType.WindowFunctionMode), CHFuzzableType.StringType),
    (Set(CHFuzzableType.UInt64, CHFuzzableType.SpecialUInt64), CHFuzzableType.UInt64)
  )
  // format: on

  private val allNumberTypes: Set[CHType] =
    getSubtypes(CHAggregatedType.Number) + CHAggregatedType.Number

  private val allLowCardinalityNumberTypes: Set[CHType] =
    getSubtypes(CHAggregatedType.LowCardinalityNonDecimal) + CHAggregatedType.LowCardinalityNonDecimal

  private val allLowCardinalityNullableNumberTypes: Set[CHType] =
    getSubtypes(CHAggregatedType.LowCardinalityNullableNonDecimal) + CHAggregatedType.LowCardinalityNullableNonDecimal

  private val allNullableNumberTypes: Set[CHType] =
    getSubtypes(CHAggregatedType.NullableNumber) + CHAggregatedType.NullableNumber

  private val allArrayNumberTypes: Set[CHType] =
    getSubtypes(CHSpecialType.Array(CHAggregatedType.Number)) + CHSpecialType.Array(CHAggregatedType.Number)

  private val allMapNumberIntTypes: Set[CHType] =
    getSubtypes(CHSpecialType.Map(CHAggregatedType.NonDecimalNorFloat, CHAggregatedType.Int)) + CHSpecialType.Map(
      CHAggregatedType.NonDecimalNorFloat,
      CHAggregatedType.Int
    )

  private val allTuple1NumberTypes: Set[CHType] =
    getSubtypes(CHSpecialType.Tuple(Seq(CHAggregatedType.Number))) + CHSpecialType.Tuple(Seq(CHAggregatedType.Number))

  private def getSubtypes(t: CHType): Set[CHType] =
    var types: Set[CHType] = Set(t)
    val m: Map[CHType, Set[CHType]] =
      nonSpecialTypesSubstitutionRules.groupMap(_._2)(_._1).view.mapValues(_.toSet.flatten).toMap
    while
      val tmp = types ++ types.flatMap(t => m.getOrElse(t, Set.empty))
      if tmp.size != types.size then
        types = tmp
        true
      else false
    do ()

    types - t

  private def root[$: P]: P[CHType] = P(Start ~ any ~ End)
  private def digits[$: P]: P[Int] = P(CharsWhileIn("0-9\\-").!).map(_.toInt)
  private def enumElement[$: P]: P[Unit] = (plainString ~ " = " ~ digits).map(_ => (): Unit)
  private def escapedString[$: P]: P[String] =
    "`" ~ CharsWhile(_ != '`').! ~ "`" // TODO an escaped String could have backticks in it
  private def plainString[$: P]: P[String] =
    "'" ~ CharsWhile(_ != '\'').! ~ "'" // TODO a plain String could have single quotes in it
  private def tupleElement[$: P]: P[CHType] =
    (escapedString ~ " " ~ any).map((_, chType) => chType) |
      (CharsWhileIn("A-Za-z0-9_") ~ " " ~ any) |
      any

  // ClickHouse types
  // Types starting with '_' are only here to parse type names internal to this project (e.g. DateTime64, without parameters)
  private def any[$: P]: P[CHType] = P(
    aggregateFunction | array | bool | date | date32 | datetime | _datetime64 | datetime64 | datetimeTZ | datetime64TZ | decimal | decimal32 | decimal64 | decimal128 | decimal256 | _enum | _enum16 | _enum8 | enum16 | enum8 | _fixedstring | fixedstring | float32 | float64 | int128 | int16 | int256 | int32 | int64 | int8 | intervalday | intervalhour | intervalmicrosecond | intervalmillisecond | intervalminute | intervalmonth | intervalnanosecond | intervalquarter | intervalsecond | intervalweek | intervalyear | ipv4 | ipv6 | json | lowcardinality | map | multipolygon | nothing | nullable | point | polygon | ring | string | tuple | uint128 | uint16 | uint256 | uint32 | uint64 | uint8 | uuid
  )
  private def aggregateFunction[$: P]: P[CHType] =
    P("AggregateFunction(" ~/ CharsWhile(_ != ',').! ~ ", " ~ any ~ ")").map { (fnName, innerType) =>
      fnName match
        case "groupBitmap" =>
          innerType match
            case CHFuzzableType.Int8   => CHFuzzableType.BitmapInt8
            case CHFuzzableType.Int16  => CHFuzzableType.BitmapInt16
            case CHFuzzableType.Int32  => CHFuzzableType.BitmapInt32
            case CHFuzzableType.Int64  => CHFuzzableType.BitmapInt64
            case CHFuzzableType.UInt8  => CHFuzzableType.BitmapUInt8
            case CHFuzzableType.UInt16 => CHFuzzableType.BitmapUInt16
            case CHFuzzableType.UInt32 => CHFuzzableType.BitmapUInt32
            case CHFuzzableType.UInt64 => CHFuzzableType.BitmapUInt64
            case _                     => CHSpecialType.AggregateFunction(fnName, innerType)
        case _ => CHSpecialType.AggregateFunction(fnName, innerType)
    }
  private def array[$: P]: P[CHType] = P("Array(" ~/ any ~ ")").map(CHSpecialType.Array(_))
  private def bool[$: P]: P[CHType] = P("Bool").map(_ => CHFuzzableType.BooleanType)
  private def date[$: P]: P[CHType] = P("Date" ~ !("Time" | "32")).map(_ => CHFuzzableType.Date)
  private def date32[$: P]: P[CHType] = P("Date32").map(_ => CHFuzzableType.Date32)
  private def datetime[$: P]: P[CHType] = P("DateTime" ~ !("(" | "64")).map(_ => CHFuzzableType.DateTime)
  private def _datetime64[$: P]: P[CHType] = P("DateTime64" ~ !"(").map(_ => CHFuzzableType.DateTime64)
  private def datetime64[$: P]: P[CHType] = P("DateTime64(" ~ digits ~ ")").map(_ => CHFuzzableType.DateTime64)
  private def datetimeTZ[$: P]: P[CHType] = P("DateTime(" ~/ plainString ~ ")").map(_ => CHFuzzableType.DateTime)
  private def datetime64TZ[$: P]: P[CHType] =
    P("DateTime64(" ~ digits ~ ", " ~ plainString ~ ")").map(_ => CHFuzzableType.DateTime64)
  private def decimal[$: P]: P[CHType] = P("Decimal(" ~/ digits ~ ", " ~/ digits ~ ")").map { (d1, _) =>
    if d1 <= 9 then CHFuzzableType.Decimal32
    if d1 <= 18 then CHFuzzableType.Decimal64
    if d1 <= 38 then CHFuzzableType.Decimal128
    if d1 <= 76 then CHFuzzableType.Decimal256
    else throw new IllegalArgumentException(s"Unknown precision for Decimal: $d1, expected a precision up to 76.")
  }
  private def decimal32[$: P]: P[CHType] = P("Decimal32").map(_ => CHFuzzableType.Decimal32)
  private def decimal64[$: P]: P[CHType] = P("Decimal64").map(_ => CHFuzzableType.Decimal64)
  private def decimal128[$: P]: P[CHType] = P("Decimal128").map(_ => CHFuzzableType.Decimal128)
  private def decimal256[$: P]: P[CHType] = P("Decimal256").map(_ => CHFuzzableType.Decimal256)
  private def _enum[$: P]: P[CHType] = P("Enum" ~ !("(" | "16" | "8")).map(_ => CHFuzzableType.Enum)
  private def _enum16[$: P]: P[CHType] = P("Enum16" ~ !"(").map(_ => CHFuzzableType.Enum16)
  private def _enum8[$: P]: P[CHType] = P("Enum8" ~ !"(").map(_ => CHFuzzableType.Enum8)
  private def enum16[$: P]: P[CHType] =
    P("Enum16(" ~/ enumElement ~ ("," ~ " ".? ~ enumElement).rep ~ ")").map(_ => CHFuzzableType.Enum16)
  private def enum8[$: P]: P[CHType] =
    P("Enum8(" ~/ enumElement ~ ("," ~ " ".? ~ enumElement).rep ~ ")").map(_ => CHFuzzableType.Enum8)
  private def _fixedstring[$: P]: P[CHType] = P("FixedString" ~ !"(").map(_ => CHFuzzableType.FixedString)
  private def fixedstring[$: P]: P[CHType] = P("FixedString(" ~/ digits ~ ")").map(_ => CHFuzzableType.FixedString)
  private def float32[$: P]: P[CHType] = P("Float32").map(_ => CHFuzzableType.Float32)
  private def float64[$: P]: P[CHType] = P("Float64").map(_ => CHFuzzableType.Float64)
  private def int128[$: P]: P[CHType] = P("Int128").map(_ => CHFuzzableType.Int128)
  private def int16[$: P]: P[CHType] = P("Int16").map(_ => CHFuzzableType.Int16)
  private def int256[$: P]: P[CHType] = P("Int256").map(_ => CHFuzzableType.Int256)
  private def int32[$: P]: P[CHType] = P("Int32").map(_ => CHFuzzableType.Int32)
  private def int64[$: P]: P[CHType] = P("Int64").map(_ => CHFuzzableType.Int64)
  private def int8[$: P]: P[CHType] = P("Int8").map(_ => CHFuzzableType.Int8)
  private def intervalday[$: P]: P[CHType] = P("IntervalDay").map(_ => CHFuzzableType.IntervalDay)
  private def intervalhour[$: P]: P[CHType] = P("IntervalHour").map(_ => CHFuzzableType.IntervalHour)
  private def intervalmicrosecond[$: P]: P[CHType] =
    P("IntervalMicrosecond").map(_ => CHFuzzableType.IntervalMicrosecond)
  private def intervalmillisecond[$: P]: P[CHType] =
    P("IntervalMillisecond").map(_ => CHFuzzableType.IntervalMillisecond)
  private def intervalminute[$: P]: P[CHType] = P("IntervalMinute").map(_ => CHFuzzableType.IntervalMinute)
  private def intervalmonth[$: P]: P[CHType] = P("IntervalMonth").map(_ => CHFuzzableType.IntervalMonth)
  private def intervalnanosecond[$: P]: P[CHType] = P("IntervalNanosecond").map(_ => CHFuzzableType.IntervalNanosecond)
  private def intervalquarter[$: P]: P[CHType] = P("IntervalQuarter").map(_ => CHFuzzableType.IntervalQuarter)
  private def intervalsecond[$: P]: P[CHType] = P("IntervalSecond").map(_ => CHFuzzableType.IntervalSecond)
  private def intervalweek[$: P]: P[CHType] = P("IntervalWeek").map(_ => CHFuzzableType.IntervalWeek)
  private def intervalyear[$: P]: P[CHType] = P("IntervalYear").map(_ => CHFuzzableType.IntervalYear)
  private def ipv4[$: P]: P[CHType] = P("IPv4").map(_ => CHFuzzableType.IPv4)
  private def ipv6[$: P]: P[CHType] = P("IPv6").map(_ => CHFuzzableType.IPv6)
  private def json[$: P]: P[CHType] = P("Object('json')").map(_ => CHFuzzableType.Json)
  private def lowcardinality[$: P]: P[CHType] = P("LowCardinality(" ~/ any ~ ")").map(CHSpecialType.LowCardinality(_))
  private def map[$: P]: P[CHType] = P("Map(" ~ any ~ "," ~ " ".? ~ any ~ ")").map(CHSpecialType.Map(_, _))
  private def multipolygon[$: P]: P[CHType] = P("MultiPolygon").map(_ => CHFuzzableType.MultiPolygon)
  private def nothing[$: P]: P[CHType] = P("Nothing").map(_ => CHSpecialType.Nothing)
  private def nullable[$: P]: P[CHType] = P("Nullable(" ~/ any ~ ")").map(CHSpecialType.Nullable(_))
  private def point[$: P]: P[CHType] = P("Point").map(_ => CHFuzzableType.Point)
  private def polygon[$: P]: P[CHType] = P("Polygon").map(_ => CHFuzzableType.Polygon)
  private def ring[$: P]: P[CHType] = P("Ring").map(_ => CHFuzzableType.Ring)
  private def string[$: P]: P[CHType] = P("String").map(_ => CHFuzzableType.StringType)
  private def tuple[$: P]: P[CHType] =
    P("Tuple(" ~/ tupleElement ~ ("," ~ " ".? ~ tupleElement).rep ~ ")").map((head, tail) =>
      CHSpecialType.Tuple(head +: tail)
    )
  private def uint128[$: P]: P[CHType] = P("UInt128").map(_ => CHFuzzableType.UInt128)
  private def uint16[$: P]: P[CHType] = P("UInt16").map(_ => CHFuzzableType.UInt16)
  private def uint256[$: P]: P[CHType] = P("UInt256").map(_ => CHFuzzableType.UInt256)
  private def uint32[$: P]: P[CHType] = P("UInt32").map(_ => CHFuzzableType.UInt32)
  private def uint64[$: P]: P[CHType] = P("UInt64").map(_ => CHFuzzableType.UInt64)
  private def uint8[$: P]: P[CHType] = P("UInt8").map(_ => CHFuzzableType.UInt8)
  private def uuid[$: P]: P[CHType] = P("UUID").map(_ => CHFuzzableType.UUID)
