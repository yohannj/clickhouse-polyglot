package com.amendil.common.entities

import com.amendil.common.Settings
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

enum CHSpecialType(val name: String) extends CHType:
  case AggregateFunction(fnName: String, innerType: CHType)
      extends CHSpecialType(s"AggregateFunction($fnName, ${innerType.name})")
  case Array(innerType: CHType) extends CHSpecialType(s"Array(${innerType.name})")
  case GenericType(typeName: String) extends CHSpecialType(typeName)
  case LambdaType(outputType: CHType) extends CHSpecialType(s"Lambda(${outputType.name})")
  case LowCardinality(innerType: CHType) extends CHSpecialType(s"LowCardinality(${innerType.name})")
  case Map(keyType: CHType, valueType: CHType) extends CHSpecialType(s"Map(${keyType.name}, ${valueType.name})")
  case Nullable(innerType: CHType) extends CHSpecialType(s"Nullable(${innerType.name})")
  case Tuple(innerTypes: Seq[CHType]) extends CHSpecialType(s"Tuple(${innerTypes.map(_.name).mkString(", ")})")
  case TupleN(innerType: CHType) extends CHSpecialType(s"TupleN(${innerType.name})")

  case CatboostParameter
      extends CHSpecialType("CatboostParameter") // UIntX, IntX, Float32, Float64, Date, Date32, DateTime

  case Nothing extends CHSpecialType("Nothing")

  case SequenceBaseFirstMatch extends CHSpecialType("SequenceBaseFirstMatch") // "'first_match'"
  case SequenceBaseHead extends CHSpecialType("SequenceBaseHead") // "'head'"
  case SequenceBaseLastMatch extends CHSpecialType("SequenceBaseLastMatch") // "'last_match'"
  case SequenceBaseTail extends CHSpecialType("SequenceBaseTail") // "'tail'"
  case SequenceDirectionForward extends CHSpecialType("SequenceDirectionForward") // "'forward'"
  case SequenceDirectionBackward extends CHSpecialType("SequenceDirectionBackward") // "'backward'"

enum CHAggregatedType(val name: String) extends CHType:
  case Any extends CHAggregatedType("Any")
  case AnyNonNullableNonLowCardinality extends CHAggregatedType("AnyNonNullableNonLowCardinality")
  case Bitmap extends CHAggregatedType("AnyDecimal")
  case DateOrDateTime extends CHAggregatedType("DateOrDateTime")
  case DateLikeOrDateTime extends CHAggregatedType("DateLikeOrDateTime")
  case DateLikeOrDateTimeLike extends CHAggregatedType("DateLikeOrDateTimeLike")
  case DateLike extends CHAggregatedType("DateLike")
  case DateTimeLike extends CHAggregatedType("DateTimeLike")
  case Geo extends CHAggregatedType("Geo")
  case Interval extends CHAggregatedType("Interval")
  case IntervalDate extends CHAggregatedType("IntervalDate")
  case IntervalTime extends CHAggregatedType("IntervalTime")
  case IntervalTime64 extends CHAggregatedType("IntervalTimed64")
  case IP extends CHAggregatedType("IP")
  case LowCardinality extends CHAggregatedType("LowCardinality")
  case LowCardinalityNullable extends CHAggregatedType("LowCardinalityNullable")
  case MapKey extends CHAggregatedType("MapKey")
  case Nullable extends CHAggregatedType("Nullable")
  case StringLike extends CHAggregatedType("StringLike")

  // Numbers
  case DecimalLike extends CHAggregatedType("AnyDecimal")
  case Float extends CHAggregatedType("Float")
  case Int extends CHAggregatedType("Int")
  case IntMax64Bits extends CHAggregatedType("IntMax64Bits")
  case NonDecimal extends CHAggregatedType("NonDecimal")
  case NonDecimalMax64Bits extends CHAggregatedType("NonDecimalMax64Bits")
  case NonDecimalNorFloat extends CHAggregatedType("NonDecimalNorFloat")
  case NonDecimalNorFloatMax64Bits extends CHAggregatedType("NonDecimalNorFloatMax64Bits")
  case Number extends CHAggregatedType("Numbers")
  case UInt extends CHAggregatedType("UInt")
  case UIntMax64Bits extends CHAggregatedType("UIntMax64Bits")

  // LowCardinality(Numbers)
  case LowCardinalityFloat extends CHAggregatedType("LowCardinalityFloat")
  case LowCardinalityInt extends CHAggregatedType("LowCardinalityInt")
  case LowCardinalityIntMax64Bits extends CHAggregatedType("LowCardinalityIntMax64Bits")
  case LowCardinalityNonDecimal extends CHAggregatedType("LowCardinalityNonDecimal")
  case LowCardinalityNonDecimalMax64Bits extends CHAggregatedType("LowCardinalityNonDecimalMax64Bits")
  case LowCardinalityNonDecimalNorFloatMax64Bits extends CHAggregatedType("LowCardinalityNonDecimalNorFloatMax64Bits")
  case LowCardinalityUInt extends CHAggregatedType("LowCardinalityUInt")
  case LowCardinalityUIntMax64Bits extends CHAggregatedType("LowCardinalityUIntMax64Bits")

  // LowCardinality(Nullable(Numbers))
  case LowCardinalityNullableFloat extends CHAggregatedType("LowCardinalityNullableFloat")
  case LowCardinalityNullableInt extends CHAggregatedType("LowCardinalityNullableInt")
  case LowCardinalityNullableIntMax64Bits extends CHAggregatedType("LowCardinalityNullableIntMax64Bits")
  case LowCardinalityNullableNonDecimal extends CHAggregatedType("LowCardinalityNullableNonDecimal")
  case LowCardinalityNullableNonDecimalMax64Bits extends CHAggregatedType("LowCardinalityNullableNonDecimalMax64Bits")
  case LowCardinalityNullableNonDecimalNorFloatMax64Bits
      extends CHAggregatedType("LowCardinalityNullableNonDecimalNorFloatMax64Bits")
  case LowCardinalityNullableUInt extends CHAggregatedType("LowCardinalityNullableUInt")
  case LowCardinalityNullableUIntMax64Bits extends CHAggregatedType("LowCardinalityNullableUIntMax64Bits")

  // Nullable(Numbers)
  case NullableDecimalLike extends CHAggregatedType("AnyNullableDecimal")
  case NullableFloat extends CHAggregatedType("NullableFloat")
  case NullableInt extends CHAggregatedType("NullableInt")
  case NullableIntMax64Bits extends CHAggregatedType("NullableIntMax64Bits")
  case NullableNonDecimal extends CHAggregatedType("NullableNonDecimal")
  case NullableNonDecimalMax64Bits extends CHAggregatedType("NullableNonDecimalMax64Bits")
  case NullableNonDecimalNorFloatMax64Bits extends CHAggregatedType("NullableNonDecimalNorFloatMax64Bits")
  case NullableNumber extends CHAggregatedType("NullableNumber")
  case NullableUInt extends CHAggregatedType("NullableUInt")
  case NullableUIntMax64Bits extends CHAggregatedType("NullableUIntMax64Bits")

enum CHFuzzableType(
    val name: String,
    val fuzzingValues: Seq[String]
) extends CHType:

  // Numbers
  case BooleanType
      extends CHFuzzableType(
        "Bool",
        Seq("false::Bool", "true::Bool")
      )
  case Int8
      extends CHFuzzableType(
        "Int8",
        Seq("-128::Int8", "127::Int8", "0::Int8", "1::Int8", "8::Int8")
      )
  case Int16
      extends CHFuzzableType(
        "Int16",
        Seq("-32768::Int16", "32767::Int16", "0::Int16", "1::Int16", "8::Int16")
      )
  case Int32
      extends CHFuzzableType(
        "Int32",
        Seq("-2147483648::Int32", "2147483647::Int32", "0::Int32", "1::Int32", "8::Int32")
      )
  case Int64
      extends CHFuzzableType(
        "Int64",
        Seq("-9223372036854775808::Int64", "9223372036854775807::Int64", "0::Int64", "1::Int64", "8::Int64")
      )
  case Int128
      extends CHFuzzableType(
        "Int128",
        Seq(
          "-170141183460469231731687303715884105728::Int128",
          "170141183460469231731687303715884105727::Int128",
          "0::Int128",
          "1::Int128",
          "8::Int128"
        )
      )
  case Int256
      extends CHFuzzableType(
        "Int256",
        Seq(
          "-57896044618658097711785492504343953926634992332820282019728792003956564819968::Int256",
          "57896044618658097711785492504343953926634992332820282019728792003956564819967::Int256",
          "0::Int256",
          "1::Int256",
          "8::Int256"
        )
      )
  case UInt8
      extends CHFuzzableType(
        "UInt8",
        Seq("0::UInt8", "1::UInt8", "8::UInt8", "255::UInt8")
      )
  case UInt16
      extends CHFuzzableType(
        "UInt16",
        Seq("0::UInt16", "1::UInt16", "8::UInt16", "65535::UInt16")
      )
  case UInt32
      extends CHFuzzableType(
        "UInt32",
        Seq("0::UInt32", "1::UInt32", "8::UInt32", "4294967295::UInt32")
      )
  case UInt64
      extends CHFuzzableType(
        "UInt64",
        Seq("0::UInt64", "1::UInt64", "8::UInt64", "18446744073709551615::UInt64")
      )
  case UInt128
      extends CHFuzzableType(
        "UInt128",
        Seq("0::UInt128", "1::UInt128", "8::UInt128", "340282366920938463463374607431768211455::UInt128")
      )
  case UInt256
      extends CHFuzzableType(
        "UInt256",
        Seq(
          "0::UInt256",
          "1::UInt256",
          "8::UInt256",
          "115792089237316195423570985008687907853269984665640564039457584007913129639935::UInt256"
        )
      )

  case Float32
      extends CHFuzzableType(
        "Float32",
        Seq("-inf::Float32", "nan::Float32", "0.5::Float32", "0::Float32", "8::Float32")
      )
  case Float64
      extends CHFuzzableType(
        "Float64",
        Seq("-inf::Float64", "nan::Float64", "0.5::Float64", "0::Float64", "8::Float64")
      )
  case Decimal32
      extends CHFuzzableType(
        "Decimal32",
        Seq(
          "-999999999::Decimal32(0)",
          "1::Decimal32(0)",
          "8::Decimal32(0)",
          "999999999::Decimal32(0)",
          "-0.999999999::Decimal32(9)",
          "0.999999999::Decimal32(9)"
        )
      )
  case Decimal64
      extends CHFuzzableType(
        "Decimal64",
        Seq(
          "-999999999999999999::Decimal64(0)",
          "1::Decimal64(0)",
          "8::Decimal64(0)",
          "999999999999999999::Decimal64(0)",
          "-0.999999999999999999::Decimal64(18)",
          "0.999999999999999999::Decimal64(18)",
          "-999999999999999999::Decimal",
          "-9999999999::Decimal",
          "9999999999::Decimal",
          "999999999999999999::Decimal"
        )
      )
  case Decimal128
      extends CHFuzzableType(
        "Decimal128",
        Seq(
          "-999999999999999999999999999999999999::Decimal128(0)",
          "1::Decimal128(0)",
          "8::Decimal128(0)",
          "999999999999999999999999999999999999::Decimal128(0)",
          "-0.99999999999999999999999999999999999999::Decimal128(38)",
          "0.99999999999999999999999999999999999999::Decimal128(38)"
        )
      )
  case Decimal256
      extends CHFuzzableType(
        "Decimal256",
        Seq(
          "-999999999999999999999999999999999999999999999999999999999999999999999999::Decimal256(0)",
          "1::Decimal256(0)",
          "8::Decimal256(0)",
          "999999999999999999999999999999999999999999999999999999999999999999999999::Decimal256(0)",
          "-0.9999999999999999999999999999999999999999999999999999999999999999999999999999::Decimal256(76)",
          "0.9999999999999999999999999999999999999999999999999999999999999999999999999999::Decimal256(76)",
          "-99999999999999999999999999999999999999::Decimal(39, 38)",
          "-9.99999999999999999999999999999999999999::Decimal(39, 38)",
          "9.99999999999999999999999999999999999999::Decimal(39, 38)",
          "99999999999999999999999999999999999999::Decimal(39, 38)",
          "-9999999999999999999999999999999999999999999999999999999999999999999999999999::Decimal(40)",
          "-9999999999999999999999999999999999999999::Decimal(40)",
          "9999999999999999999999999999999999999999::Decimal(40)",
          "9999999999999999999999999999999999999999999999999999999999999999999999999999::Decimal(40)"
        )
      )

  // Bitmap
  case BitmapInt8
      extends CHFuzzableType(
        "Bitmap(Int8)",
        Seq("bitmapBuild([1::Int8])")
      )
  case BitmapInt16
      extends CHFuzzableType(
        "Bitmap(Int16)",
        Seq("bitmapBuild([1::Int16])")
      )
  case BitmapInt32
      extends CHFuzzableType(
        "Bitmap(Int32)",
        Seq("bitmapBuild([1::Int32])")
      )
  case BitmapInt64
      extends CHFuzzableType(
        "Bitmap(Int64)",
        Seq("bitmapBuild([1::Int64])")
      )
  case BitmapUInt8
      extends CHFuzzableType(
        "Bitmap(UInt8)",
        Seq("bitmapBuild([1::UInt8])")
      )
  case BitmapUInt16
      extends CHFuzzableType(
        "Bitmap(UInt16)",
        Seq("bitmapBuild([1::UInt16])")
      )
  case BitmapUInt32
      extends CHFuzzableType(
        "Bitmap(UInt32)",
        Seq("bitmapBuild([1::UInt32])")
      )
  case BitmapUInt64
      extends CHFuzzableType(
        "Bitmap(UInt64)",
        Seq("bitmapBuild([1::UInt64])")
      )

  // Date
  case Date
      extends CHFuzzableType(
        "Date",
        Seq("'1970-01-01'::Date", "'1970-01-02'::Date", "'2149-06-06'::Date")
      )
  case Date32
      extends CHFuzzableType(
        "Date32",
        Seq("'1900-01-01'::Date32", "'2299-12-31'::Date32")
      )
  case DateTime
      extends CHFuzzableType(
        "DateTime",
        Seq(
          "'1970-01-01 00:00:00'::DateTime('Asia/Istanbul')",
          "'2106-02-07 06:28:15'::DateTime('Asia/Istanbul')",
          "'1970-01-01 00:00:00'::DateTime",
          "'2106-02-07 06:28:15'::DateTime"
        )
      )
  case DateTime64
      extends CHFuzzableType(
        "DateTime64",
        Seq(
          "'1900-01-01 00:00:00'::DateTime64(0, 'Asia/Istanbul')",
          "'1900-01-01 00:00:00'::DateTime64(9, 'Asia/Istanbul')",
          "'2299-12-31 23:59:59.99999999'::DateTime64(0, 'Asia/Istanbul')",
          "'2299-12-31 23:59:59.99999999'::DateTime64(8, 'Asia/Istanbul')",
          "'2262-04-12 02:47:16.854775807'::DateTime64(9, 'Asia/Istanbul')",
          "'1900-01-01 00:00:00'::DateTime64(0)",
          "'1900-01-01 00:00:00'::DateTime64(9)",
          "'2299-12-31 23:59:59.99999999'::DateTime64(0)",
          "'2299-12-31 23:59:59.99999999'::DateTime64(8)",
          "'2262-04-11 23:47:16.854775807'::DateTime64(9)"
        )
      )
  case IntervalNanosecond
      extends CHFuzzableType(
        "IntervalNanosecond",
        Seq("INTERVAL 1 Nanosecond::IntervalNanosecond")
      )
  case IntervalMicrosecond
      extends CHFuzzableType(
        "IntervalMicrosecond",
        Seq("INTERVAL 1 Microsecond::IntervalMicrosecond")
      )
  case IntervalMillisecond
      extends CHFuzzableType(
        "IntervalMillisecond",
        Seq("INTERVAL 1 Millisecond::IntervalMillisecond")
      )
  case IntervalSecond
      extends CHFuzzableType(
        "IntervalSecond",
        Seq("INTERVAL 1 Second::IntervalSecond")
      )
  case IntervalMinute
      extends CHFuzzableType(
        "IntervalMinute",
        Seq("INTERVAL 1 Minute::IntervalMinute")
      )
  case IntervalHour
      extends CHFuzzableType(
        "IntervalHour",
        Seq("INTERVAL 1 Hour::IntervalHour")
      )
  case IntervalDay
      extends CHFuzzableType(
        "IntervalDay",
        Seq("INTERVAL 1 Day::IntervalDay")
      )
  case IntervalWeek
      extends CHFuzzableType(
        "IntervalWeek",
        Seq("INTERVAL 1 Week::IntervalWeek")
      )
  case IntervalMonth
      extends CHFuzzableType(
        "IntervalMonth",
        Seq("INTERVAL 1 Month::IntervalMonth")
      )
  case IntervalQuarter
      extends CHFuzzableType(
        "IntervalQuarter",
        Seq("INTERVAL 1 Quarter::IntervalQuarter")
      )
  case IntervalYear
      extends CHFuzzableType(
        "IntervalYear",
        Seq("INTERVAL 1 Year::IntervalYear")
      )

  // Geo
  case Point extends CHFuzzableType("Point", Seq("(0, 0)::Point"))
  case Ring extends CHFuzzableType("Ring", Seq("[(0, 0), (10, 0), (10, 10), (0, 10)]::Ring"))
  case Polygon
      extends CHFuzzableType(
        "Polygon",
        Seq("[[(20, 20), (50, 20), (50, 50), (20, 50)], [(30, 30), (50, 50), (50, 30)]]::Polygon")
      )
  case MultiPolygon
      extends CHFuzzableType(
        "MultiPolygon",
        Seq(
          "[[[(0,0),(10,0),(10,10),(0,10)]],[[(20,20),(50,20),(50,50),(20,50)],[(30,30),(50,50),(50,30)]]]::MultiPolygon"
        )
      )

  // Misc
  case Enum
      extends CHFuzzableType(
        "Enum",
        Seq(
          "'hello'::Enum('hello' = 1, 'world' = 2)",
          "'hello'::Enum('hello', 'world')"
        )
      )
  case Enum8
      extends CHFuzzableType(
        "Enum8",
        Seq(
          "'hello'::Enum8('hello' = -128, 'world' = 2)",
          "'hello'::Enum8('hello' = 127, 'world' = 2)",
          "'hello'::Enum8('hello', 'world')"
        )
      )
  case Enum16
      extends CHFuzzableType(
        "Enum16",
        Seq(
          "'hello'::Enum16('hello' = -32768, 'world' = 2)",
          "'hello'::Enum16('hello' = 32767, 'world' = 2)",
          "'hello'::Enum16('hello', 'world')"
        )
      )
  case FixedString
      extends CHFuzzableType(
        "FixedString",
        Seq(
          "'a'::FixedString(1)",
          "'azertyuiop'::FixedString(10)"
        )
      )
  case IPv4 extends CHFuzzableType("IPv4", Seq("'116.106.34.242'::IPv4"))
  case IPv6
      extends CHFuzzableType(
        "IPv6",
        Seq(
          "'2001:44c8:129:2632:33:0:252:2'::IPv6",
          "'2a02:e980:1e::1'::IPv6",
          "'116.106.34.242'::IPv6"
        )
      )
  case Json
      extends CHFuzzableType(
        "Object('json')",
        Seq("""'{"a": 1, "b": { "c": "foo", "d": [1, 2, 3] }, "c": null}'::Object('json')""")
      )
  case StringType
      extends CHFuzzableType(
        "String",
        Seq(
          "'a'::String"
        )
      )
  case UUID
      extends CHFuzzableType(
        "UUID",
        Seq("'00000000-0000-0000-0000-000000000000'::UUID", "'61f0c404-5cb3-11e7-907b-a6006ad3dba0'::UUID")
      )

  // LowCardinality
  case LowCardinalityBoolean
      extends CHFuzzableType(
        "LowCardinality(Bool)",
        CHFuzzableType.lowCardinalityFuzzingValues(BooleanType.fuzzingValues)
      )
  case LowCardinalityInt8
      extends CHFuzzableType(
        "LowCardinality(Int8)",
        CHFuzzableType.lowCardinalityFuzzingValues(Int8.fuzzingValues)
      )
  case LowCardinalityInt16
      extends CHFuzzableType(
        "LowCardinality(Int16)",
        CHFuzzableType.lowCardinalityFuzzingValues(Int16.fuzzingValues)
      )
  case LowCardinalityInt32
      extends CHFuzzableType(
        "LowCardinality(Int32)",
        CHFuzzableType.lowCardinalityFuzzingValues(Int32.fuzzingValues)
      )
  case LowCardinalityInt64
      extends CHFuzzableType(
        "LowCardinality(Int64)",
        CHFuzzableType.lowCardinalityFuzzingValues(Int64.fuzzingValues)
      )
  case LowCardinalityInt128
      extends CHFuzzableType(
        "LowCardinality(Int128)",
        CHFuzzableType.lowCardinalityFuzzingValues(Int128.fuzzingValues)
      )
  case LowCardinalityInt256
      extends CHFuzzableType(
        "LowCardinality(Int256)",
        CHFuzzableType.lowCardinalityFuzzingValues(Int256.fuzzingValues)
      )
  case LowCardinalityUInt8
      extends CHFuzzableType(
        "LowCardinality(UInt8)",
        CHFuzzableType.lowCardinalityFuzzingValues(UInt8.fuzzingValues)
      )
  case LowCardinalityUInt16
      extends CHFuzzableType(
        "LowCardinality(UInt16)",
        CHFuzzableType.lowCardinalityFuzzingValues(UInt16.fuzzingValues)
      )
  case LowCardinalityUInt32
      extends CHFuzzableType(
        "LowCardinality(UInt32)",
        CHFuzzableType.lowCardinalityFuzzingValues(UInt32.fuzzingValues)
      )
  case LowCardinalityUInt64
      extends CHFuzzableType(
        "LowCardinality(UInt64)",
        CHFuzzableType.lowCardinalityFuzzingValues(UInt64.fuzzingValues)
      )
  case LowCardinalityUInt128
      extends CHFuzzableType(
        "LowCardinality(UInt128)",
        CHFuzzableType.lowCardinalityFuzzingValues(UInt128.fuzzingValues)
      )
  case LowCardinalityUInt256
      extends CHFuzzableType(
        "LowCardinality(UInt256)",
        CHFuzzableType.lowCardinalityFuzzingValues(UInt256.fuzzingValues)
      )

  case LowCardinalityFloat32
      extends CHFuzzableType(
        "LowCardinality(Float32)",
        CHFuzzableType.lowCardinalityFuzzingValues(Float32.fuzzingValues)
      )
  case LowCardinalityFloat64
      extends CHFuzzableType(
        "LowCardinality(Float64)",
        CHFuzzableType.lowCardinalityFuzzingValues(Float64.fuzzingValues)
      )

  case LowCardinalityDate
      extends CHFuzzableType(
        "LowCardinality(Date)",
        CHFuzzableType.lowCardinalityFuzzingValues(Date.fuzzingValues)
      )
  case LowCardinalityDate32
      extends CHFuzzableType(
        "LowCardinality(Date32)",
        CHFuzzableType.lowCardinalityFuzzingValues(Date32.fuzzingValues)
      )
  case LowCardinalityDateTime
      extends CHFuzzableType(
        "LowCardinality(DateTime)",
        CHFuzzableType.lowCardinalityFuzzingValues(DateTime.fuzzingValues)
      )
  case LowCardinalityFixedString
      extends CHFuzzableType(
        "LowCardinality(FixedString)",
        CHFuzzableType.lowCardinalityFuzzingValues(FixedString.fuzzingValues)
      )
  case LowCardinalityString
      extends CHFuzzableType(
        "LowCardinality(String)",
        CHFuzzableType.lowCardinalityFuzzingValues(StringType.fuzzingValues)
      )

  // LowCardinality(Nullable)
  case LowCardinalityNullableBoolean
      extends CHFuzzableType(
        "LowCardinality(Nullable(Bool))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(BooleanType.fuzzingValues))
      )
  case LowCardinalityNullableInt8
      extends CHFuzzableType(
        "LowCardinality(Nullable(Int8))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(Int8.fuzzingValues))
      )
  case LowCardinalityNullableInt16
      extends CHFuzzableType(
        "LowCardinality(Nullable(Int16))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(Int16.fuzzingValues))
      )
  case LowCardinalityNullableInt32
      extends CHFuzzableType(
        "LowCardinality(Nullable(Int32))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(Int32.fuzzingValues))
      )
  case LowCardinalityNullableInt64
      extends CHFuzzableType(
        "LowCardinality(Nullable(Int64))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(Int64.fuzzingValues))
      )
  case LowCardinalityNullableInt128
      extends CHFuzzableType(
        "LowCardinality(Nullable(Int128))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(Int128.fuzzingValues))
      )
  case LowCardinalityNullableInt256
      extends CHFuzzableType(
        "LowCardinality(Nullable(Int256))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(Int256.fuzzingValues))
      )
  case LowCardinalityNullableUInt8
      extends CHFuzzableType(
        "LowCardinality(Nullable(UInt8))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(UInt8.fuzzingValues))
      )
  case LowCardinalityNullableUInt16
      extends CHFuzzableType(
        "LowCardinality(Nullable(UInt16))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(UInt16.fuzzingValues))
      )
  case LowCardinalityNullableUInt32
      extends CHFuzzableType(
        "LowCardinality(Nullable(UInt32))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(UInt32.fuzzingValues))
      )
  case LowCardinalityNullableUInt64
      extends CHFuzzableType(
        "LowCardinality(Nullable(UInt64))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(UInt64.fuzzingValues))
      )
  case LowCardinalityNullableUInt128
      extends CHFuzzableType(
        "LowCardinality(Nullable(UInt128))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(UInt128.fuzzingValues))
      )
  case LowCardinalityNullableUInt256
      extends CHFuzzableType(
        "LowCardinality(Nullable(UInt256))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(UInt256.fuzzingValues))
      )

  case LowCardinalityNullableFloat32
      extends CHFuzzableType(
        "LowCardinality(Nullable(Float32))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(Float32.fuzzingValues))
      )
  case LowCardinalityNullableFloat64
      extends CHFuzzableType(
        "LowCardinality(Nullable(Float64))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(Float64.fuzzingValues))
      )

  case LowCardinalityNullableDate
      extends CHFuzzableType(
        "LowCardinality(Nullable(Date))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(Date.fuzzingValues))
      )
  case LowCardinalityNullableDate32
      extends CHFuzzableType(
        "LowCardinality(Nullable(Date32))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(Date32.fuzzingValues))
      )
  case LowCardinalityNullableDateTime
      extends CHFuzzableType(
        "LowCardinality(Nullable(DateTime))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(DateTime.fuzzingValues))
      )
  case LowCardinalityNullableFixedString
      extends CHFuzzableType(
        "LowCardinality(Nullable(FixedString))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(FixedString.fuzzingValues))
      )
  case LowCardinalityNullableString
      extends CHFuzzableType(
        "LowCardinality(Nullable(String))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(StringType.fuzzingValues))
      )

  // Nullable
  case NullableBoolean
      extends CHFuzzableType(
        "Nullable(Bool)",
        CHFuzzableType.nullableFuzzingValues(BooleanType.fuzzingValues)
      )
  case NullableInt8
      extends CHFuzzableType(
        "Nullable(Int8)",
        CHFuzzableType.nullableFuzzingValues(Int8.fuzzingValues)
      )
  case NullableInt16
      extends CHFuzzableType(
        "Nullable(Int16)",
        CHFuzzableType.nullableFuzzingValues(Int16.fuzzingValues)
      )
  case NullableInt32
      extends CHFuzzableType(
        "Nullable(Int32)",
        CHFuzzableType.nullableFuzzingValues(Int32.fuzzingValues)
      )
  case NullableInt64
      extends CHFuzzableType(
        "Nullable(Int64)",
        CHFuzzableType.nullableFuzzingValues(Int64.fuzzingValues)
      )
  case NullableInt128
      extends CHFuzzableType(
        "Nullable(Int128)",
        CHFuzzableType.nullableFuzzingValues(Int128.fuzzingValues)
      )
  case NullableInt256
      extends CHFuzzableType(
        "Nullable(Int256)",
        CHFuzzableType.nullableFuzzingValues(Int256.fuzzingValues)
      )
  case NullableUInt8
      extends CHFuzzableType(
        "Nullable(UInt8)",
        CHFuzzableType.nullableFuzzingValues(UInt8.fuzzingValues)
      )
  case NullableUInt16
      extends CHFuzzableType(
        "Nullable(UInt16)",
        CHFuzzableType.nullableFuzzingValues(UInt16.fuzzingValues)
      )
  case NullableUInt32
      extends CHFuzzableType(
        "Nullable(UInt32)",
        CHFuzzableType.nullableFuzzingValues(UInt32.fuzzingValues)
      )
  case NullableUInt64
      extends CHFuzzableType(
        "Nullable(UInt64)",
        CHFuzzableType.nullableFuzzingValues(UInt64.fuzzingValues)
      )
  case NullableUInt128
      extends CHFuzzableType(
        "Nullable(UInt128)",
        CHFuzzableType.nullableFuzzingValues(UInt128.fuzzingValues)
      )
  case NullableUInt256
      extends CHFuzzableType(
        "Nullable(UInt256)",
        CHFuzzableType.nullableFuzzingValues(UInt256.fuzzingValues)
      )

  case NullableFloat32
      extends CHFuzzableType(
        "Nullable(Float32)",
        CHFuzzableType.nullableFuzzingValues(Float32.fuzzingValues)
      )
  case NullableFloat64
      extends CHFuzzableType(
        "Nullable(Float64)",
        CHFuzzableType.nullableFuzzingValues(Float64.fuzzingValues)
      )
  case NullableDecimal32
      extends CHFuzzableType(
        "Nullable(Decimal32)",
        CHFuzzableType.nullableFuzzingValues(Decimal32.fuzzingValues)
      )
  case NullableDecimal64
      extends CHFuzzableType(
        "Nullable(Decimal64)",
        CHFuzzableType.nullableFuzzingValues(Decimal64.fuzzingValues)
      )
  case NullableDecimal128
      extends CHFuzzableType(
        "Nullable(Decimal128)",
        CHFuzzableType.nullableFuzzingValues(Decimal128.fuzzingValues)
      )
  case NullableDecimal256
      extends CHFuzzableType(
        "Nullable(Decimal256)",
        CHFuzzableType.nullableFuzzingValues(Decimal256.fuzzingValues)
      )

  // Date
  case NullableDate
      extends CHFuzzableType(
        "Nullable(Date)",
        CHFuzzableType.nullableFuzzingValues(Date.fuzzingValues)
      )
  case NullableDate32
      extends CHFuzzableType(
        "Nullable(Date32)",
        CHFuzzableType.nullableFuzzingValues(Date32.fuzzingValues)
      )
  case NullableDateTime
      extends CHFuzzableType(
        "Nullable(DateTime)",
        CHFuzzableType.nullableFuzzingValues(DateTime.fuzzingValues)
      )
  case NullableDateTime64
      extends CHFuzzableType(
        "Nullable(DateTime64)",
        CHFuzzableType.nullableFuzzingValues(DateTime64.fuzzingValues)
      )
  case NullableIntervalNanosecond
      extends CHFuzzableType(
        "Nullable(IntervalNanosecond)",
        CHFuzzableType.nullableFuzzingValues(IntervalNanosecond.fuzzingValues)
      )
  case NullableIntervalMicrosecond
      extends CHFuzzableType(
        "Nullable(IntervalMicrosecond)",
        CHFuzzableType.nullableFuzzingValues(IntervalMicrosecond.fuzzingValues)
      )
  case NullableIntervalMillisecond
      extends CHFuzzableType(
        "Nullable(IntervalMillisecond)",
        CHFuzzableType.nullableFuzzingValues(IntervalMillisecond.fuzzingValues)
      )
  case NullableIntervalSecond
      extends CHFuzzableType(
        "Nullable(IntervalSecond)",
        CHFuzzableType.nullableFuzzingValues(IntervalSecond.fuzzingValues)
      )
  case NullableIntervalMinute
      extends CHFuzzableType(
        "Nullable(IntervalMinute)",
        CHFuzzableType.nullableFuzzingValues(IntervalMinute.fuzzingValues)
      )
  case NullableIntervalHour
      extends CHFuzzableType(
        "Nullable(IntervalHour)",
        CHFuzzableType.nullableFuzzingValues(IntervalHour.fuzzingValues)
      )
  case NullableIntervalDay
      extends CHFuzzableType(
        "Nullable(IntervalDay)",
        CHFuzzableType.nullableFuzzingValues(IntervalDay.fuzzingValues)
      )
  case NullableIntervalWeek
      extends CHFuzzableType(
        "Nullable(IntervalWeek)",
        CHFuzzableType.nullableFuzzingValues(IntervalWeek.fuzzingValues)
      )
  case NullableIntervalMonth
      extends CHFuzzableType(
        "Nullable(IntervalMonth)",
        CHFuzzableType.nullableFuzzingValues(IntervalMonth.fuzzingValues)
      )
  case NullableIntervalQuarter
      extends CHFuzzableType(
        "Nullable(IntervalQuarter)",
        CHFuzzableType.nullableFuzzingValues(IntervalQuarter.fuzzingValues)
      )
  case NullableIntervalYear
      extends CHFuzzableType(
        "Nullable(IntervalYear)",
        CHFuzzableType.nullableFuzzingValues(IntervalYear.fuzzingValues)
      )
  case NullableEnum
      extends CHFuzzableType(
        "Nullable(Enum)",
        CHFuzzableType.nullableFuzzingValues(Enum.fuzzingValues)
      )
  case NullableEnum8
      extends CHFuzzableType(
        "Nullable(Enum8)",
        CHFuzzableType.nullableFuzzingValues(Enum8.fuzzingValues)
      )
  case NullableEnum16
      extends CHFuzzableType(
        "Nullable(Enum16)",
        CHFuzzableType.nullableFuzzingValues(Enum16.fuzzingValues)
      )
  case NullableFixedString
      extends CHFuzzableType(
        "Nullable(FixedString)",
        CHFuzzableType.nullableFuzzingValues(FixedString.fuzzingValues)
      )
  case NullableIPv4
      extends CHFuzzableType(
        "Nullable(IPv4)",
        CHFuzzableType.nullableFuzzingValues(IPv4.fuzzingValues)
      )
  case NullableIPv6
      extends CHFuzzableType(
        "Nullable(IPv6)",
        CHFuzzableType.nullableFuzzingValues(IPv6.fuzzingValues)
      )
  // case NullableNothing extends CHFuzzableType("Nullable(Nothing)", Seq("null::Nullable(Nothing)"))
  case NullableString
      extends CHFuzzableType(
        "Nullable(String)",
        CHFuzzableType.nullableFuzzingValues(StringType.fuzzingValues)
      )
  case NullableUUID
      extends CHFuzzableType(
        "Nullable(UUID)",
        CHFuzzableType.nullableFuzzingValues(UUID.fuzzingValues)
      )

  // Array
  case ArrayBoolean
      extends CHFuzzableType(
        "Array(Bool)",
        Seq(
          s"[${BooleanType.fuzzingValues.mkString(", ")}]::Array(Bool)",
          s"[${BooleanType.fuzzingValues.head}]::Array(Bool)"
        )
      )
  case ArrayInt8
      extends CHFuzzableType(
        "Array(Int8)",
        Seq(
          s"[${Int8.fuzzingValues.mkString(", ")}]::Array(Int8)",
          s"[${Int8.fuzzingValues.head}]::Array(Int8)"
        )
      )
  case ArrayInt16
      extends CHFuzzableType(
        "Array(Int16)",
        Seq(
          s"[${Int16.fuzzingValues.mkString(", ")}]::Array(Int16)",
          s"[${Int16.fuzzingValues.head}]::Array(Int16)"
        )
      )
  case ArrayInt32
      extends CHFuzzableType(
        "Array(Int32)",
        Seq(
          s"[${Int32.fuzzingValues.mkString(", ")}]::Array(Int32)",
          s"[${Int32.fuzzingValues.head}]::Array(Int32)"
        )
      )
  case ArrayInt64
      extends CHFuzzableType(
        "Array(Int64)",
        Seq(
          s"[${Int64.fuzzingValues.mkString(", ")}]::Array(Int64)",
          s"[${Int64.fuzzingValues.head}]::Array(Int64)"
        )
      )
  case ArrayInt128
      extends CHFuzzableType(
        "Array(Int128)",
        Seq(
          s"[${Int128.fuzzingValues.mkString(", ")}]::Array(Int128)",
          s"[${Int128.fuzzingValues.head}]::Array(Int128)"
        )
      )
  case ArrayInt256
      extends CHFuzzableType(
        "Array(Int256)",
        Seq(
          s"[${Int256.fuzzingValues.mkString(", ")}]::Array(Int256)",
          s"[${Int256.fuzzingValues.head}]::Array(Int256)"
        )
      )
  case ArrayUInt8
      extends CHFuzzableType(
        "Array(UInt8)",
        Seq(
          s"[${UInt8.fuzzingValues.mkString(", ")}]::Array(UInt8)",
          s"[${UInt8.fuzzingValues.head}]::Array(UInt8)"
        )
      )
  case ArrayUInt16
      extends CHFuzzableType(
        "Array(UInt16)",
        Seq(
          s"[${UInt16.fuzzingValues.mkString(", ")}]::Array(UInt16)",
          s"[${UInt16.fuzzingValues.head}]::Array(UInt16)"
        )
      )
  case ArrayUInt32
      extends CHFuzzableType(
        "Array(UInt32)",
        Seq(
          s"[${UInt32.fuzzingValues.mkString(", ")}]::Array(UInt32)",
          s"[${UInt32.fuzzingValues.head}]::Array(UInt32)"
        )
      )
  case ArrayUInt64
      extends CHFuzzableType(
        "Array(UInt64)",
        Seq(
          s"[${UInt64.fuzzingValues.mkString(", ")}]::Array(UInt64)",
          s"[${UInt64.fuzzingValues.head}]::Array(UInt64)"
        )
      )
  case ArrayUInt128
      extends CHFuzzableType(
        "Array(UInt128)",
        Seq(
          s"[${UInt128.fuzzingValues.mkString(", ")}]::Array(UInt128)",
          s"[${UInt128.fuzzingValues.head}]::Array(UInt128)"
        )
      )
  case ArrayUInt256
      extends CHFuzzableType(
        "Array(UInt256)",
        Seq(
          s"[${UInt256.fuzzingValues.mkString(", ")}]::Array(UInt256)",
          s"[${UInt256.fuzzingValues.head}]::Array(UInt256)"
        )
      )
  case ArrayFloat32
      extends CHFuzzableType(
        "Array(Float32)",
        Seq(
          s"[${Float32.fuzzingValues.mkString(", ")}]::Array(Float32)",
          s"[${Float32.fuzzingValues.head}]::Array(Float32)"
        )
      )
  case ArrayFloat64
      extends CHFuzzableType(
        "Array(Float64)",
        Seq(
          s"[${Float64.fuzzingValues.mkString(", ")}]::Array(Float64)",
          s"[${Float64.fuzzingValues.head}]::Array(Float64)"
        )
      )
  case ArrayDecimal32
      extends CHFuzzableType(
        "Array(Decimal32)",
        Seq(
          s"[-999999999::Decimal32(0)]::Array(Decimal32(0))"
        )
      )
  case ArrayDecimal64
      extends CHFuzzableType(
        "Array(Decimal64)",
        Seq(
          s"[-999999999999999999::Decimal64(0)]::Array(Decimal64(0))"
        )
      )
  case ArrayDecimal128
      extends CHFuzzableType(
        "Array(Decimal128)",
        Seq(
          s"[-999999999999999999999999999999999999::Decimal128(0)]::Array(Decimal128(0))"
        )
      )
  case ArrayDecimal256
      extends CHFuzzableType(
        "Array(Decimal256)",
        Seq(
          s"[-999999999999999999999999999999999999999999999999999999999999999999999999::Decimal256(0)]::Array(Decimal256(0))"
        )
      )
  case ArrayDate
      extends CHFuzzableType(
        "Array(Date)",
        Seq(
          s"[${Date.fuzzingValues.mkString(", ")}]::Array(Date)",
          s"[${Date.fuzzingValues.head}]::Array(Date)"
        )
      )
  case ArrayDate32
      extends CHFuzzableType(
        "Array(Date32)",
        Seq(
          s"[${Date32.fuzzingValues.mkString(", ")}]::Array(Date32)",
          s"[${Date32.fuzzingValues.head}]::Array(Date32)"
        )
      )
  case ArrayDateTime
      extends CHFuzzableType(
        "Array(DateTime)",
        Seq(
          s"['1970-01-01 00:00:00'::DateTime('Asia/Istanbul')]::Array(DateTime('Asia/Istanbul'))"
        )
      )
  case ArrayDateTime64
      extends CHFuzzableType(
        "Array(DateTime64)",
        Seq(
          s"['1900-01-01 00:00:00'::DateTime64(0, 'Asia/Istanbul')]::Array(DateTime64(0, 'Asia/Istanbul'))"
        )
      )
  case ArrayIntervalNanosecond
      extends CHFuzzableType("Array(IntervalNanosecond)", Seq("[INTERVAL 1 Nanosecond]::Array(IntervalNanosecond)"))
  case ArrayIntervalMicrosecond
      extends CHFuzzableType(
        "Array(IntervalMicrosecond)",
        Seq("[INTERVAL 1 Microsecond]::Array(IntervalMicrosecond)")
      )
  case ArrayIntervalMillisecond
      extends CHFuzzableType(
        "Array(IntervalMillisecond)",
        Seq("[INTERVAL 1 Millisecond]::Array(IntervalMillisecond)")
      )
  case ArrayIntervalSecond
      extends CHFuzzableType("Array(IntervalSecond)", Seq("[INTERVAL 1 Second]::Array(IntervalSecond)"))
  case ArrayIntervalMinute
      extends CHFuzzableType("Array(IntervalMinute)", Seq("[INTERVAL 1 Minute]::Array(IntervalMinute)"))
  case ArrayIntervalHour extends CHFuzzableType("Array(IntervalHour)", Seq("[INTERVAL 1 Hour]::Array(IntervalHour)"))
  case ArrayIntervalDay extends CHFuzzableType("Array(IntervalDay)", Seq("[INTERVAL 1 Day]::Array(IntervalDay)"))
  case ArrayIntervalWeek extends CHFuzzableType("Array(IntervalWeek)", Seq("[INTERVAL 1 Week]::Array(IntervalWeek)"))
  case ArrayIntervalMonth
      extends CHFuzzableType("Array(IntervalMonth)", Seq("[INTERVAL 1 Month]::Array(IntervalMonth)"))
  case ArrayIntervalQuarter
      extends CHFuzzableType("Array(IntervalQuarter)", Seq("[INTERVAL 1 Quarter]::Array(IntervalQuarter)"))
  case ArrayIntervalYear extends CHFuzzableType("Array(IntervalYear)", Seq("[INTERVAL 1 Year]::Array(IntervalYear)"))
  case ArrayPoint extends CHFuzzableType("Array(Point)", Seq("[(0, 0)]::Array(Point)"))
  case ArrayRing extends CHFuzzableType("Array(Ring)", Seq("[[(0, 0), (10, 0), (10, 10), (0, 10)]]::Array(Ring)"))
  case ArrayPolygon
      extends CHFuzzableType(
        "Array(Polygon)",
        Seq("[[[(20, 20), (50, 20), (50, 50), (20, 50)], [(30, 30), (50, 50), (50, 30)]]]::Array(Polygon)")
      )
  case ArrayMultiPolygon
      extends CHFuzzableType(
        "Array(MultiPolygon)",
        Seq(
          "[[[[(0,0),(10,0),(10,10),(0,10)]],[[(20,20),(50,20),(50,50),(20,50)],[(30,30),(50,50),(50,30)]]]]::Array(MultiPolygon)"
        )
      )
  case ArrayEnum
      extends CHFuzzableType(
        "Array(Enum)",
        Seq(
          s"['hello'::Enum('hello' = 1, 'world' = 2)]::Array(Enum('hello' = 1, 'world' = 2))"
        )
      )
  case ArrayEnum8
      extends CHFuzzableType(
        "Array(Enum8)",
        Seq(
          s"['hello'::Enum8('hello' = -128, 'world' = 2)]::Array(Enum8('hello' = -128, 'world' = 2))"
        )
      )
  case ArrayEnum16
      extends CHFuzzableType(
        "Array(Enum16)",
        Seq(
          s"['hello'::Enum16('hello' = -32768, 'world' = 2)]::Array(Enum16('hello' = -32768, 'world' = 2))"
        )
      )
  case ArrayFixedString
      extends CHFuzzableType(
        "Array(FixedString)",
        Seq(
          s"[${FixedString.fuzzingValues.head}]::Array(FixedString(32))",
          s"[${FixedString.fuzzingValues.mkString(", ")}]::Array(FixedString(96))"
        )
      )
  case ArrayIPv4
      extends CHFuzzableType(
        "Array(IPv4)",
        Seq(
          s"[${IPv4.fuzzingValues.mkString(", ")}]::Array(IPv4)",
          s"[${IPv4.fuzzingValues.head}]::Array(IPv4)"
        )
      )
  case ArrayIPv6
      extends CHFuzzableType(
        "Array(IPv6)",
        Seq(
          s"[${IPv6.fuzzingValues.mkString(", ")}]::Array(IPv6)",
          s"[${IPv6.fuzzingValues.head}]::Array(IPv6)"
        )
      )
  case ArrayJson
      extends CHFuzzableType(
        "Array(Object('json'))",
        Seq(
          s"[${Json.fuzzingValues.mkString(", ")}]::Array(Object('json'))",
          s"[${Json.fuzzingValues.head}]::Array(Object('json'))"
        )
      )
  case ArrayString
      extends CHFuzzableType(
        "Array(String)",
        Seq(
          s"[${StringType.fuzzingValues.head}]::Array(String)",
          s"[${StringType.fuzzingValues.mkString(", ")}]::Array(String)"
        )
      )
  case ArrayUUID
      extends CHFuzzableType(
        "Array(UUID)",
        Seq(
          s"[${UUID.fuzzingValues.mkString(", ")}]::Array(UUID)",
          s"[${UUID.fuzzingValues.head}]::Array(UUID)"
        )
      )

  // Map
  case MapBooleanInt
      extends CHFuzzableType(
        "Map(Bool, Int)",
        BooleanType.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapInt8Int
      extends CHFuzzableType(
        "Map(Int8, Int)",
        Int8.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapInt16Int
      extends CHFuzzableType(
        "Map(Int16, Int)",
        Int16.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapInt32Int
      extends CHFuzzableType(
        "Map(Int32, Int)",
        Int32.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapInt64Int
      extends CHFuzzableType(
        "Map(Int64, Int)",
        Int64.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapInt128Int
      extends CHFuzzableType(
        "Map(Int128, Int)",
        Int128.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapInt256Int
      extends CHFuzzableType(
        "Map(Int256, Int)",
        Int256.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapUInt8Int
      extends CHFuzzableType(
        "Map(UInt8, Int)",
        UInt8.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapUInt16Int
      extends CHFuzzableType(
        "Map(UInt16, Int)",
        UInt16.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapUInt32Int
      extends CHFuzzableType(
        "Map(UInt32, Int)",
        UInt32.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapUInt64Int
      extends CHFuzzableType(
        "Map(UInt64, Int)",
        UInt64.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapUInt128Int
      extends CHFuzzableType(
        "Map(UInt128, Int)",
        UInt128.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapUInt256Int
      extends CHFuzzableType(
        "Map(UInt256, Int)",
        UInt256.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapDateInt
      extends CHFuzzableType(
        "Map(Date, Int)",
        Date.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapDate32Int
      extends CHFuzzableType(
        "Map(Date32, Int)",
        Date32.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapDateTimeInt
      extends CHFuzzableType(
        "Map(DateTime, Int)",
        DateTime.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalNanosecondInt
      extends CHFuzzableType(
        "Map(IntervalNanosecond, Int)",
        DateTime.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalMicrosecondInt
      extends CHFuzzableType(
        "Map(IntervalMicrosecond, Int)",
        DateTime.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalMillisecondInt
      extends CHFuzzableType(
        "Map(IntervalMillisecond, Int)",
        IntervalMillisecond.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalSecondInt
      extends CHFuzzableType(
        "Map(IntervalSecond, Int)",
        IntervalSecond.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalMinuteInt
      extends CHFuzzableType(
        "Map(IntervalMinute, Int)",
        IntervalMinute.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalHourInt
      extends CHFuzzableType(
        "Map(IntervalHour, Int)",
        IntervalHour.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalDayInt
      extends CHFuzzableType(
        "Map(IntervalDay, Int)",
        IntervalDay.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalWeekInt
      extends CHFuzzableType(
        "Map(IntervalWeek, Int)",
        IntervalWeek.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalMonthInt
      extends CHFuzzableType(
        "Map(IntervalMonth, Int)",
        IntervalMonth.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalQuarterInt
      extends CHFuzzableType(
        "Map(IntervalQuarter, Int)",
        IntervalQuarter.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalYearInt
      extends CHFuzzableType(
        "Map(IntervalYear, Int)",
        IntervalYear.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapEnumInt
      extends CHFuzzableType(
        "Map(Enum, Int)",
        Seq(s"map('hello'::Enum8('hello' = -128, 'world' = 2), 1)")
      )
  case MapEnum8Int
      extends CHFuzzableType(
        "Map(Enum8, Int)",
        Seq(s"map('hello'::Enum8('hello' = -128, 'world' = 2), 1)")
      )
  case MapEnum16Int
      extends CHFuzzableType(
        "Map(Enum16, Int)",
        Seq(
          s"map('hello'::Enum16('hello' = -32768, 'world' = 2), 1)"
        )
      )
  case MapFixedStringInt
      extends CHFuzzableType(
        "Map(FixedString, Int)",
        FixedString.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIPv4Int
      extends CHFuzzableType(
        "Map(IPv4, Int)",
        IPv4.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIPv6Int
      extends CHFuzzableType(
        "Map(IPv6, Int)",
        IPv6.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapStringInt
      extends CHFuzzableType(
        "Map(String, Int)",
        StringType.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapUUIDInt
      extends CHFuzzableType(
        "Map(UUID, Int)",
        UUID.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )

  // Tuple1
  case Tuple1Boolean
      extends CHFuzzableType(
        "Tuple(Bool)",
        Seq(
          s"tuple(${BooleanType.fuzzingValues.head})::Tuple(Bool)",
          s"tuple(${BooleanType.fuzzingValues.head})::Tuple(a Bool)"
        )
      )
  case Tuple1Int8
      extends CHFuzzableType(
        "Tuple(Int8)",
        Seq(s"tuple(${Int8.fuzzingValues.head})::Tuple(Int8)", s"tuple(${Int8.fuzzingValues.head})::Tuple(a Int8)")
      )
  case Tuple1Int16
      extends CHFuzzableType(
        "Tuple(Int16)",
        Seq(s"tuple(${Int16.fuzzingValues.head})::Tuple(Int16)", s"tuple(${Int16.fuzzingValues.head})::Tuple(a Int16)")
      )
  case Tuple1Int32
      extends CHFuzzableType(
        "Tuple(Int32)",
        Seq(s"tuple(${Int32.fuzzingValues.head})::Tuple(Int32)", s"tuple(${Int32.fuzzingValues.head})::Tuple(a Int32)")
      )
  case Tuple1Int64
      extends CHFuzzableType(
        "Tuple(Int64)",
        Seq(s"tuple(${Int64.fuzzingValues.head})::Tuple(Int64)", s"tuple(${Int64.fuzzingValues.head})::Tuple(a Int64)")
      )
  case Tuple1Int128
      extends CHFuzzableType(
        "Tuple(Int128)",
        Seq(
          s"tuple(${Int128.fuzzingValues.head})::Tuple(Int128)",
          s"tuple(${Int128.fuzzingValues.head})::Tuple(a Int128)"
        )
      )
  case Tuple1Int256
      extends CHFuzzableType(
        "Tuple(Int256)",
        Seq(
          s"tuple(${Int256.fuzzingValues.head})::Tuple(Int256)",
          s"tuple(${Int256.fuzzingValues.head})::Tuple(a Int256)"
        )
      )
  case Tuple1UInt8
      extends CHFuzzableType(
        "Tuple(UInt8)",
        Seq(s"tuple(${UInt8.fuzzingValues.head})::Tuple(UInt8)", s"tuple(${UInt8.fuzzingValues.head})::Tuple(a UInt8)")
      )
  case Tuple1UInt16
      extends CHFuzzableType(
        "Tuple(UInt16)",
        Seq(
          s"tuple(${UInt16.fuzzingValues.head})::Tuple(UInt16)",
          s"tuple(${UInt16.fuzzingValues.head})::Tuple(a UInt16)"
        )
      )
  case Tuple1UInt32
      extends CHFuzzableType(
        "Tuple(UInt32)",
        Seq(
          s"tuple(${UInt32.fuzzingValues.head})::Tuple(UInt32)",
          s"tuple(${UInt32.fuzzingValues.head})::Tuple(a UInt32)"
        )
      )
  case Tuple1UInt64
      extends CHFuzzableType(
        "Tuple(UInt64)",
        Seq(
          s"tuple(${UInt64.fuzzingValues.head})::Tuple(UInt64)",
          s"tuple(${UInt64.fuzzingValues.head})::Tuple(a UInt64)"
        )
      )
  case Tuple1UInt128
      extends CHFuzzableType(
        "Tuple(UInt128)",
        Seq(
          s"tuple(${UInt128.fuzzingValues.head})::Tuple(UInt128)",
          s"tuple(${UInt128.fuzzingValues.head})::Tuple(a UInt128)"
        )
      )
  case Tuple1UInt256
      extends CHFuzzableType(
        "Tuple(UInt256)",
        Seq(
          s"tuple(${UInt256.fuzzingValues.head})::Tuple(UInt256)",
          s"tuple(${UInt256.fuzzingValues.head})::Tuple(a UInt256)"
        )
      )
  case Tuple1Float32
      extends CHFuzzableType(
        "Tuple(Float32)",
        Seq(
          s"tuple(${Float32.fuzzingValues.head})::Tuple(Float32)",
          s"tuple(${Float32.fuzzingValues.head})::Tuple(a Float32)"
        )
      )
  case Tuple1Float64
      extends CHFuzzableType(
        "Tuple(Float64)",
        Seq(
          s"tuple(${Float64.fuzzingValues.head})::Tuple(Float64)",
          s"tuple(${Float64.fuzzingValues.head})::Tuple(a Float64)"
        )
      )
  case Tuple1Decimal32
      extends CHFuzzableType(
        "Tuple(Decimal32)",
        Seq(
          s"tuple(${Decimal32.fuzzingValues.head})::Tuple(Decimal32(0))",
          s"tuple(${Decimal32.fuzzingValues.head})::Tuple(a Decimal32(0))"
        )
      )
  case Tuple1Decimal64
      extends CHFuzzableType(
        "Tuple(Decimal64)",
        Seq(
          s"tuple(${Decimal64.fuzzingValues.head})::Tuple(Decimal64(0))",
          s"tuple(${Decimal64.fuzzingValues.head})::Tuple(a Decimal64(0))"
        )
      )
  case Tuple1Decimal128
      extends CHFuzzableType(
        "Tuple(Decimal128)",
        Seq(
          s"tuple(${Decimal128.fuzzingValues.head})::Tuple(Decimal128(0))",
          s"tuple(${Decimal128.fuzzingValues.head})::Tuple(a Decimal128(0))"
        )
      )
  case Tuple1Decimal256
      extends CHFuzzableType(
        "Tuple(Decimal256)",
        Seq(
          s"tuple(${Decimal256.fuzzingValues.head})::Tuple(Decimal256(0))",
          s"tuple(${Decimal256.fuzzingValues.head})::Tuple(a Decimal256(0))"
        )
      )
  case Tuple1Date
      extends CHFuzzableType(
        "Tuple(Date)",
        Seq(s"tuple(${Date.fuzzingValues.head})::Tuple(Date)", s"tuple(${Date.fuzzingValues.head})::Tuple(a Date)")
      )
  case Tuple1Date32
      extends CHFuzzableType(
        "Tuple(Date32)",
        Seq(
          s"tuple(${Date32.fuzzingValues.head})::Tuple(Date32)",
          s"tuple(${Date32.fuzzingValues.head})::Tuple(a Date32)"
        )
      )
  case Tuple1DateTime
      extends CHFuzzableType(
        "Tuple(DateTime)",
        Seq(
          s"tuple(${DateTime.fuzzingValues.head})::Tuple(DateTime)",
          s"tuple(${DateTime.fuzzingValues.head})::Tuple(a DateTime)"
        )
      )
  case Tuple1DateTime64
      extends CHFuzzableType(
        "Tuple(DateTime64)",
        Seq(
          s"tuple(${DateTime64.fuzzingValues.head})::Tuple(DateTime64)",
          s"tuple(${DateTime64.fuzzingValues.head})::Tuple(a DateTime64)"
        )
      )
  case Tuple1IntervalNanosecond
      extends CHFuzzableType(
        "Tuple(IntervalNanosecond)",
        Seq(
          "tuple(INTERVAL 1 Nanosecond)::Tuple(IntervalNanosecond)",
          "tuple(INTERVAL 1 Nanosecond)::Tuple(a IntervalNanosecond)"
        )
      )
  case Tuple1IntervalMicrosecond
      extends CHFuzzableType(
        "Tuple(IntervalMicrosecond)",
        Seq(
          "tuple(INTERVAL 1 Microsecond)::Tuple(IntervalMicrosecond)",
          "tuple(INTERVAL 1 Microsecond)::Tuple(a IntervalMicrosecond)"
        )
      )
  case Tuple1IntervalMillisecond
      extends CHFuzzableType(
        "Tuple(IntervalMillisecond)",
        Seq(
          "tuple(INTERVAL 1 Millisecond)::Tuple(IntervalMillisecond)",
          "tuple(INTERVAL 1 Millisecond)::Tuple(a IntervalMillisecond)"
        )
      )
  case Tuple1IntervalSecond
      extends CHFuzzableType(
        "Tuple(IntervalSecond)",
        Seq("tuple(INTERVAL 1 Second)::Tuple(IntervalSecond)", "tuple(INTERVAL 1 Second)::Tuple(a IntervalSecond)")
      )
  case Tuple1IntervalMinute
      extends CHFuzzableType(
        "Tuple(IntervalMinute)",
        Seq("tuple(INTERVAL 1 Minute)::Tuple(IntervalMinute)", "tuple(INTERVAL 1 Minute)::Tuple(a IntervalMinute)")
      )
  case Tuple1IntervalHour
      extends CHFuzzableType(
        "Tuple(IntervalHour)",
        Seq("tuple(INTERVAL 1 Hour)::Tuple(IntervalHour)", "tuple(INTERVAL 1 Hour)::Tuple(a IntervalHour)")
      )
  case Tuple1IntervalDay
      extends CHFuzzableType(
        "Tuple(IntervalDay)",
        Seq("tuple(INTERVAL 1 Day)::Tuple(IntervalDay)", "tuple(INTERVAL 1 Day)::Tuple(a IntervalDay)")
      )
  case Tuple1IntervalWeek
      extends CHFuzzableType(
        "Tuple(IntervalWeek)",
        Seq("tuple(INTERVAL 1 Week)::Tuple(IntervalWeek)", "tuple(INTERVAL 1 Week)::Tuple(a IntervalWeek)")
      )
  case Tuple1IntervalMonth
      extends CHFuzzableType(
        "Tuple(IntervalMonth)",
        Seq("tuple(INTERVAL 1 Month)::Tuple(IntervalMonth)", "tuple(INTERVAL 1 Month)::Tuple(a IntervalMonth)")
      )
  case Tuple1IntervalQuarter
      extends CHFuzzableType(
        "Tuple(IntervalQuarter)",
        Seq("tuple(INTERVAL 1 Quarter)::Tuple(IntervalQuarter)", "tuple(INTERVAL 1 Quarter)::Tuple(a IntervalQuarter)")
      )
  case Tuple1IntervalYear
      extends CHFuzzableType(
        "Tuple(IntervalYear)",
        Seq("tuple(INTERVAL 1 Year)::Tuple(IntervalYear)", "tuple(INTERVAL 1 Year)::Tuple(a IntervalYear)")
      )
  case Tuple1Point
      extends CHFuzzableType(
        "Tuple(Point)",
        Seq("tuple((0, 0))::Tuple(Point)", "tuple((0, 0))::Tuple(a Point)")
      )
  case Tuple1Ring
      extends CHFuzzableType(
        "Tuple(Ring)",
        Seq(
          "tuple([(0, 0), (10, 0), (10, 10), (0, 10)])::Tuple(Ring)",
          "tuple([(0, 0), (10, 0), (10, 10), (0, 10)])::Tuple(a Ring)"
        )
      )
  case Tuple1Polygon
      extends CHFuzzableType(
        "Tuple(Polygon)",
        Seq(
          "tuple([[(20, 20), (50, 20), (50, 50), (20, 50)], [(30, 30), (50, 50), (50, 30)]])::Tuple(Polygon)",
          "tuple([[(20, 20), (50, 20), (50, 50), (20, 50)], [(30, 30), (50, 50), (50, 30)]])::Tuple(a Polygon)"
        )
      )
  case Tuple1MultiPolygon
      extends CHFuzzableType(
        "Tuple(MultiPolygon)",
        Seq(
          "tuple([[[(0,0),(10,0),(10,10),(0,10)]],[[(20,20),(50,20),(50,50),(20,50)],[(30,30),(50,50),(50,30)]]])::Tuple(MultiPolygon)",
          "tuple([[[(0,0),(10,0),(10,10),(0,10)]],[[(20,20),(50,20),(50,50),(20,50)],[(30,30),(50,50),(50,30)]]])::Tuple(a MultiPolygon)"
        )
      )
  case Tuple1Enum
      extends CHFuzzableType(
        "Tuple(Enum)",
        Seq(
          s"tuple('hello'::Enum('hello' = 1, 'world' = 2))::Tuple(Enum('hello' = 1, 'world' = 2))",
          s"tuple('hello'::Enum('hello' = 1, 'world' = 2))::Tuple(a Enum('hello' = 1, 'world' = 2))"
        )
      )
  case Tuple1Enum8
      extends CHFuzzableType(
        "Tuple(Enum8)",
        Seq(
          s"tuple('hello'::Enum8('hello' = -128, 'world' = 2))::Tuple(Enum8('hello' = -128, 'world' = 2))",
          s"tuple('hello'::Enum8('hello' = -128, 'world' = 2))::Tuple(a Enum8('hello' = -128, 'world' = 2))"
        )
      )
  case Tuple1Enum16
      extends CHFuzzableType(
        "Tuple(Enum16)",
        Seq(
          s"tuple('hello'::Enum16('hello' = -32768, 'world' = 2))::Tuple(Enum16('hello' = -32768, 'world' = 2))",
          s"tuple('hello'::Enum16('hello' = -32768, 'world' = 2))::Tuple(a Enum16('hello' = -32768, 'world' = 2))"
        )
      )
  case Tuple1FixedString
      extends CHFuzzableType(
        "Tuple(FixedString)",
        Seq(
          s"tuple('azertyuiop'::FixedString(10))::Tuple(FixedString(10))",
          s"tuple('azertyuiop'::FixedString(10))::Tuple(a FixedString(10))"
        )
      )
  case Tuple1IPv4
      extends CHFuzzableType(
        "Tuple(IPv4)",
        Seq(s"tuple(${IPv4.fuzzingValues.head})::Tuple(IPv4)", s"tuple(${IPv4.fuzzingValues.head})::Tuple(a IPv4)")
      )
  case Tuple1IPv6
      extends CHFuzzableType(
        "Tuple(IPv6)",
        Seq(s"tuple(${IPv6.fuzzingValues.head})::Tuple(IPv6)", s"tuple(${IPv6.fuzzingValues.head})::Tuple(a IPv6)")
      )
  case Tuple1Json
      extends CHFuzzableType(
        "Tuple(Object('json'))",
        Seq(
          s"tuple(${Json.fuzzingValues.head})::Tuple(Object('json'))",
          s"tuple(${Json.fuzzingValues.head})::Tuple(a Object('json'))"
        )
      )
  // case Tuple1Nothing
  //     extends CHFuzzableType(
  //       "Tuple(Nullable(Nothing))",
  //       Seq("tuple(null)::Tuple(Nullable(Nothing))", "tuple(null)::Tuple(a Nullable(Nothing))")
  //     )
  case Tuple1String
      extends CHFuzzableType(
        "Tuple(String)",
        Seq(
          s"tuple(${StringType.fuzzingValues.head})::Tuple(String)",
          s"tuple(${StringType.fuzzingValues.head})::Tuple(a String)"
        )
      )
  case Tuple1UUID
      extends CHFuzzableType(
        "Tuple(UUID)",
        Seq(s"tuple(${UUID.fuzzingValues.head})::Tuple(UUID)", s"tuple(${UUID.fuzzingValues.head})::Tuple(a UUID)")
      )

  // Nested
  case ArrayMapStringInt
      extends CHFuzzableType(
        "Array(Map(String, Int))",
        Seq(s"[map(${StringType.fuzzingValues.head}, 1)::Map(String, Int8)]::Array(Map(String, Int8))")
      )
  case ArrayTuple1UInt8
      extends CHFuzzableType(
        "Array(Tuple)",
        Seq(s"[${Tuple1UInt8.fuzzingValues.head}]::Array(Tuple(UInt8))")
      )
  case Tuple1ArrayUInt8
      extends CHFuzzableType(
        "Tuple(Array(UInt8))",
        Seq(
          s"tuple(${ArrayUInt8.fuzzingValues.head})::Tuple(Array(UInt8))",
          s"tuple(${ArrayUInt8.fuzzingValues.head})::Tuple(a Array(UInt8))"
        )
      )
  case Tuple1MapStringInt
      extends CHFuzzableType(
        "Tuple(Map(String, Int8))",
        Seq(
          s"tuple(map(${StringType.fuzzingValues.head}, 1)::Map(String, Int8))::Tuple(Map(String, Int8))",
          s"tuple(map(${StringType.fuzzingValues.head}, 1)::Map(String, Int8))::Tuple(a Map(String, Int8))"
        )
      )

  // Special
  case Charset
      extends CHFuzzableType(
        "Charset",
        Seq(s"'${CHCharset.UTF_8.name}'", s"'${CHCharset.US_ASCII.name}'")
      )
  case ClickHouseType
      extends CHFuzzableType(
        "ClickHouseType",
        CHBaseType.values.toSeq.map(baseType => s"'${baseType.name}'")
      )
  case DictionaryName
      extends CHFuzzableType(
        "DictionaryName",
        Settings.Type.dictionaryNames.map(dictName => s"'$dictName'")
      )
  case DateUnit
      extends CHFuzzableType(
        "DateUnit",
        CHDateUnit.values.toSeq.map(_.values.head).map(name => s"'$name'")
      )
  case EncryptionMode
      extends CHFuzzableType(
        "EncryptionMode",
        CHEncryptionMode.values.toSeq.map(mode => s"'${mode.name}'")
      )
  case TestAlternative
      extends CHFuzzableType(
        "TestAlternative",
        Seq("'two-sided'", "'greater'", "'less'")
      )
  case PValueComputationMethod
      extends CHFuzzableType(
        "PValueComputationMethod",
        Seq("'exact'", "'asymp'", "'asymptotic'", "'auto'")
      )
  case SequencePattern
      extends CHFuzzableType(
        "SequencePattern",
        Seq("'(?1)'")
      )
  case ServerPortName
      extends CHFuzzableType(
        "ServerPortName",
        Seq(
          "'grpc_port'",
          "'http_port'",
          "'https_port'",
          "'interserver_http_port'",
          "'interserver_https_port'",
          "'mysql_port'",
          "'postgresql_port'",
          "'prometheus.port'",
          "'tcp_port'",
          "'tcp_port_secure'"
        )
      )
  // Separate from their real types because those values are specific to some functions
  // That should save time overall to not have them in their real type
  // This will improve performances in many cases.
  case SpecialUInt64
      extends CHFuzzableType(
        "UInt64",
        Seq("599686042433355775::UInt64")
      )
  case SpecialLowCardinalityUInt64
      extends CHFuzzableType(
        "LowCardinality(UInt64)",
        CHFuzzableType.lowCardinalityFuzzingValues(SpecialUInt64.fuzzingValues)
      )
  case SpecialLowCardinalityNullableUInt64
      extends CHFuzzableType(
        "LowCardinality(Nullable(UInt64))",
        CHFuzzableType.lowCardinalityFuzzingValues(CHFuzzableType.nullableFuzzingValues(SpecialUInt64.fuzzingValues))
      )
  case SpecialNullableUInt64
      extends CHFuzzableType(
        "Nullable(UInt64)",
        CHFuzzableType.nullableFuzzingValues(SpecialUInt64.fuzzingValues)
      )
  case SpecialFixedString
      extends CHFuzzableType(
        "FixedString",
        Seq(
          "'01GNB2S2FGN2P93QPXDNB4EN2A'::FixedString(26)",
          "'public_suffix_list'::FixedString(18)",
          "'a/<@];!~p{jTj={)'::FixedString(16)",
          "'(.)'::FixedString(3)",
          "'SELECT 1'::FixedString(10)",
          "'log_queries'::FixedString(16)",
          "'127.0.0.1'::FixedString(9)",
          "'127.0.0.0/8'::FixedString(11)",
          "'en'::FixedString(2)",
          "'11s+22min'::FixedString(9)",
          "'MULTIPOLYGON(((2 0,10 0,10 10,0 10,2 0),(4 4,5 4,5 5,4 5,4 4)),((-10 -10,-10 -9,-9 10,-10 -10)))'::FixedString(96)",
          "'POLYGON((2 0,10 0,10 10,0 10,2 0))'::FixedString(34)",
          "'POINT (1.2 3.4)'::FixedString(15)",
          "'LINESTRING (1 1, 2 2, 3 3, 1 1)'::FixedString(31)",
          "'column1 String, column2 UInt32, column3 Array(String)'::FixedString(53)",
          "'id'::FixedString(53)",
          "'dateValue'::FixedString(53)",
          "'dateTimeValue'::FixedString(53)",
          "'float32Value'::FixedString(53)",
          "'float64Value'::FixedString(53)",
          "'int16Value'::FixedString(53)",
          "'int32Value'::FixedString(53)",
          "'int64Value'::FixedString(53)",
          "'int8Value'::FixedString(53)",
          "'iPv4Value'::FixedString(53)",
          "'iPv6Value'::FixedString(53)",
          "'stringValue'::FixedString(53)",
          "'uint16Value'::FixedString(53)",
          "'uint32Value'::FixedString(53)",
          "'uint64Value'::FixedString(53)",
          "'uint8Value'::FixedString(53)",
          "'uuidValue'::FixedString(53)"
        )
      )
  // Separate from String because those values are specific to some functions.
  // This will improve performances in many cases.
  case SpecialString
      extends CHFuzzableType(
        "String",
        Seq(
          s"'${Settings.Type.catboostPath}'::String",
          "'01GNB2S2FGN2P93QPXDNB4EN2A'::String",
          "'public_suffix_list'::String",
          "'a/<@];!~p{jTj={)'::String",
          "'(.)'::String",
          "'SELECT 1'::String",
          "'log_queries'::String",
          "'127.0.0.1'::String",
          "'127.0.0.0/8'::String",
          "'en'::String",
          "'11s+22min'::String",
          "'MULTIPOLYGON(((2 0,10 0,10 10,0 10,2 0),(4 4,5 4,5 5,4 5,4 4)),((-10 -10,-10 -9,-9 10,-10 -10)))'",
          "'POLYGON((2 0,10 0,10 10,0 10,2 0))'",
          "'POINT (1.2 3.4)'",
          "'LINESTRING (1 1, 2 2, 3 3, 1 1)'",
          "'column1 String, column2 UInt32, column3 Array(String)'"
        )
      )
  case SpecialArrayFixedString
      extends CHFuzzableType(
        "Array(FixedString)",
        Seq(
          s"[${SpecialFixedString.fuzzingValues.head}]::Array(FixedString(32))",
          s"[${SpecialFixedString.fuzzingValues.mkString(", ")}]::Array(FixedString(96))"
        )
      )
  case SpecialArrayString
      extends CHFuzzableType(
        "Array(String)",
        Seq(
          s"[${SpecialString.fuzzingValues.head}]::Array(String)",
          s"[${SpecialString.fuzzingValues.mkString(", ")}]::Array(String)"
        )
      )
  case SynonymExtensionName
      extends CHFuzzableType(
        "SynonymExtensionName",
        Seq(
          "'plain_en'",
          "'wordnet'"
        )
      )
  case Time64Unit
      extends CHFuzzableType(
        "Time64Unit",
        CHTime64Unit.values.toSeq.map(_.values.head).map(name => s"'$name'")
      )
  case TimeUnit
      extends CHFuzzableType(
        "TimeUnit",
        CHTimeUnit.values.toSeq.map(_.values.head).map(name => s"'$name'")
      )
  case TimeZone
      extends CHFuzzableType(
        "TimeZone",
        Seq(s"'${CHTimeZone.Asia_Istanbul.name}'") // Using the same timezone as when fuzzing DateTime
      )
  case Usevar
      extends CHFuzzableType(
        "Usevar",
        Seq("'pooled'", "'unpooled'")
      )
  // Variant could be the union of any kind of types
  // To avoid no-voluntary usage of this type, no values are provided
  case Variant
      extends CHFuzzableType(
        "Variant",
        Nil
      )
  case WindowFunctionMode
      extends CHFuzzableType(
        "WindowFunctionMode",
        Seq("'strict_deduplication'", "'strict_increase'", "'strict_order'")
      )

object CHFuzzableType:

  private def lowCardinalityFuzzingValues(baseFuzzingValues: Seq[String]) =
    // Build a sample fuzzingValue and fetch its type
    val firstFuzzingValue = baseFuzzingValues.head
    val typeIndex = firstFuzzingValue.lastIndexOf("::")
    val fuzzingValueWithoutType = firstFuzzingValue.substring(0, typeIndex)
    val fuzzingType = firstFuzzingValue.substring(typeIndex + "::".size)

    Seq(s"$fuzzingValueWithoutType::LowCardinality($fuzzingType)")

  private def nullableFuzzingValues(baseFuzzingValues: Seq[String]) =
    // Build a sample fuzzingValue and fetch its type
    val firstFuzzingValue = baseFuzzingValues.head
    val typeIndex = firstFuzzingValue.lastIndexOf("::")
    val fuzzingValueWithoutType = firstFuzzingValue.substring(0, typeIndex)
    val fuzzingType = firstFuzzingValue.substring(typeIndex + "::".size)

    Seq(
      s"$fuzzingValueWithoutType::Nullable($fuzzingType)",
      s"null::Nullable($fuzzingType)"
    )
