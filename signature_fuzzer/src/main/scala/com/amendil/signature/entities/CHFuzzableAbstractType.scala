package com.amendil.signature.entities

import com.amendil.common.entities.`type`.CHFuzzableType
import com.amendil.signature.Settings

sealed trait CustomAbstractType
sealed trait CustomArrayBasedAbstractType extends CustomAbstractType
sealed trait CustomStringBasedAbstractType extends CustomAbstractType

enum CHFuzzableAbstractType(_fuzzingValues: Seq[String], _chFuzzableTypes: Seq[CHFuzzableType]):
  val chFuzzableTypes: Seq[CHFuzzableType] = _chFuzzableTypes.filter { chType =>
    (Settings.Fuzzer.supportJson || !chType.name.toLowerCase().contains("json")) &&
    (Settings.Fuzzer.supportLowCardinality || !chType.name.toLowerCase().contains("lowcardinality")) &&
    (Settings.Fuzzer.supportNullable || !chType.name.toLowerCase().contains("nullable"))
  }

  val fuzzingValues: Seq[String] = if chFuzzableTypes.nonEmpty then _fuzzingValues else Nil

  // Numbers
  case Number
      extends CHFuzzableAbstractType(
        Seq("1", "0.5", "true", "-999999999::Decimal32(0)", "8", "1::UInt16", "1::UInt32", "1::Int64", "1::UInt64"),
        Seq(
          CHFuzzableType.BooleanType,
          CHFuzzableType.Int8,
          CHFuzzableType.Int16,
          CHFuzzableType.Int32,
          CHFuzzableType.Int64,
          CHFuzzableType.Int128,
          CHFuzzableType.Int256,
          CHFuzzableType.UInt8,
          CHFuzzableType.UInt16,
          CHFuzzableType.UInt32,
          CHFuzzableType.UInt64,
          CHFuzzableType.UInt128,
          CHFuzzableType.UInt256,
          CHFuzzableType.Float32,
          CHFuzzableType.Float64,
          CHFuzzableType.Decimal32,
          CHFuzzableType.Decimal64,
          CHFuzzableType.Decimal128,
          CHFuzzableType.Decimal256,
          CHFuzzableType.LowCardinalityBoolean,
          CHFuzzableType.LowCardinalityInt8,
          CHFuzzableType.LowCardinalityInt16,
          CHFuzzableType.LowCardinalityInt32,
          CHFuzzableType.LowCardinalityInt64,
          CHFuzzableType.LowCardinalityInt128,
          CHFuzzableType.LowCardinalityInt256,
          CHFuzzableType.LowCardinalityUInt8,
          CHFuzzableType.LowCardinalityUInt16,
          CHFuzzableType.LowCardinalityUInt32,
          CHFuzzableType.LowCardinalityUInt64,
          CHFuzzableType.LowCardinalityUInt128,
          CHFuzzableType.LowCardinalityUInt256,
          CHFuzzableType.LowCardinalityFloat32,
          CHFuzzableType.LowCardinalityFloat64,
          CHFuzzableType.LowCardinalityNullableBoolean,
          CHFuzzableType.LowCardinalityNullableInt8,
          CHFuzzableType.LowCardinalityNullableInt16,
          CHFuzzableType.LowCardinalityNullableInt32,
          CHFuzzableType.LowCardinalityNullableInt64,
          CHFuzzableType.LowCardinalityNullableInt128,
          CHFuzzableType.LowCardinalityNullableInt256,
          CHFuzzableType.LowCardinalityNullableUInt8,
          CHFuzzableType.LowCardinalityNullableUInt16,
          CHFuzzableType.LowCardinalityNullableUInt32,
          CHFuzzableType.LowCardinalityNullableUInt64,
          CHFuzzableType.LowCardinalityNullableUInt128,
          CHFuzzableType.LowCardinalityNullableUInt256,
          CHFuzzableType.LowCardinalityNullableFloat32,
          CHFuzzableType.LowCardinalityNullableFloat64,
          CHFuzzableType.NullableBoolean,
          CHFuzzableType.NullableInt8,
          CHFuzzableType.NullableInt16,
          CHFuzzableType.NullableInt32,
          CHFuzzableType.NullableInt64,
          CHFuzzableType.NullableInt128,
          CHFuzzableType.NullableInt256,
          CHFuzzableType.NullableUInt8,
          CHFuzzableType.NullableUInt16,
          CHFuzzableType.NullableUInt32,
          CHFuzzableType.NullableUInt64,
          CHFuzzableType.NullableUInt128,
          CHFuzzableType.NullableUInt256,
          CHFuzzableType.NullableFloat32,
          CHFuzzableType.NullableFloat64,
          CHFuzzableType.NullableDecimal32,
          CHFuzzableType.NullableDecimal64,
          CHFuzzableType.NullableDecimal128,
          CHFuzzableType.NullableDecimal256
        )
      )

  // Bitmap
  case Bitmap
      extends CHFuzzableAbstractType(
        Seq(
          CHFuzzableType.BitmapInt8,
          CHFuzzableType.BitmapInt16,
          CHFuzzableType.BitmapInt32,
          CHFuzzableType.BitmapInt64,
          CHFuzzableType.BitmapUInt8,
          CHFuzzableType.BitmapUInt16,
          CHFuzzableType.BitmapUInt32,
          CHFuzzableType.BitmapUInt64
        ).map(_.fuzzingValues.head),
        Seq(
          CHFuzzableType.BitmapInt8,
          CHFuzzableType.BitmapInt16,
          CHFuzzableType.BitmapInt32,
          CHFuzzableType.BitmapInt64,
          CHFuzzableType.BitmapUInt8,
          CHFuzzableType.BitmapUInt16,
          CHFuzzableType.BitmapUInt32,
          CHFuzzableType.BitmapUInt64
        )
      )

  // Date
  case Date
      extends CHFuzzableAbstractType(
        Seq("'1970-01-02'::Date", "'1900-01-01'::Date32"),
        Seq(
          CHFuzzableType.Date,
          CHFuzzableType.Date32,
          CHFuzzableType.LowCardinalityDate,
          CHFuzzableType.LowCardinalityDate32,
          CHFuzzableType.LowCardinalityNullableDate,
          CHFuzzableType.LowCardinalityNullableDate32,
          CHFuzzableType.NullableDate,
          CHFuzzableType.NullableDate32
        )
      )
  case DateTime
      extends CHFuzzableAbstractType(
        Seq("'1970-01-02 00:00:00'::DateTime", "'1900-01-01 00:00:00'::DateTime64(0, 'Asia/Istanbul')"),
        Seq(
          CHFuzzableType.DateTime,
          CHFuzzableType.DateTime64,
          CHFuzzableType.LowCardinalityDateTime,
          CHFuzzableType.LowCardinalityNullableDateTime,
          CHFuzzableType.NullableDateTime,
          CHFuzzableType.NullableDateTime64
        )
      )
  case IntervalDate
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.IntervalDay.fuzzingValues.head),
        Seq(
          CHFuzzableType.IntervalDay,
          CHFuzzableType.IntervalWeek,
          CHFuzzableType.IntervalMonth,
          CHFuzzableType.IntervalQuarter,
          CHFuzzableType.IntervalYear,
          CHFuzzableType.NullableIntervalDay,
          CHFuzzableType.NullableIntervalWeek,
          CHFuzzableType.NullableIntervalMonth,
          CHFuzzableType.NullableIntervalQuarter,
          CHFuzzableType.NullableIntervalYear
        )
      )
  case IntervalDateTime64
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.IntervalNanosecond.fuzzingValues.head),
        Seq(
          CHFuzzableType.IntervalNanosecond,
          CHFuzzableType.IntervalMicrosecond,
          CHFuzzableType.IntervalMillisecond,
          CHFuzzableType.NullableIntervalNanosecond,
          CHFuzzableType.NullableIntervalMicrosecond,
          CHFuzzableType.NullableIntervalMillisecond
        )
      )
  case IntervalDateTime
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.IntervalSecond.fuzzingValues.head),
        Seq(
          CHFuzzableType.IntervalSecond,
          CHFuzzableType.IntervalMinute,
          CHFuzzableType.IntervalHour,
          CHFuzzableType.NullableIntervalSecond,
          CHFuzzableType.NullableIntervalMinute,
          CHFuzzableType.NullableIntervalHour
        )
      )

  // LowCardinality and Nullable
  case LowCardinalityString
      extends CHFuzzableAbstractType(
        CHFuzzableType.LowCardinalityString.fuzzingValues :+ "'a/<@];!~p{jTj={)'::LowCardinality(FixedString(16))",
        Seq(
          CHFuzzableType.LowCardinalityFixedString,
          CHFuzzableType.LowCardinalityString
        )
      )

  case LowCardinalityNullableString
      extends CHFuzzableAbstractType(
        CHFuzzableType.LowCardinalityNullableString.fuzzingValues :+ "'a/<@];!~p{jTj={)'::LowCardinality(Nullable(FixedString(16)))",
        Seq(
          CHFuzzableType.LowCardinalityNullableFixedString,
          CHFuzzableType.LowCardinalityNullableString
        )
      )

  case NullableString
      extends CHFuzzableAbstractType(
        CHFuzzableType.NullableString.fuzzingValues :+ "'a/<@];!~p{jTj={)'::Nullable(FixedString(16))",
        Seq(
          CHFuzzableType.NullableFixedString,
          CHFuzzableType.NullableString
        )
      )

  // Misc
  case Enum
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.Enum.fuzzingValues.head),
        Seq(
          CHFuzzableType.Enum,
          CHFuzzableType.Enum8,
          CHFuzzableType.Enum16,
          CHFuzzableType.NullableEnum,
          CHFuzzableType.NullableEnum8,
          CHFuzzableType.NullableEnum16
        )
      )
  case IPv4
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.IPv4.fuzzingValues.head),
        Seq(CHFuzzableType.IPv4, CHFuzzableType.NullableIPv4)
      )
  case IPv6
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.IPv6.fuzzingValues.head),
        Seq(CHFuzzableType.IPv6, CHFuzzableType.NullableIPv6)
      )
  case Json extends CHFuzzableAbstractType(CHFuzzableType.Json.fuzzingValues.headOption.toSeq, Seq(CHFuzzableType.Json))
  // case Nothing extends CHFuzzableAbstractType(CHFuzzableType.NullableNothing.fuzzingValues, Seq(CHFuzzableType.NullableNothing))
  case String
      extends CHFuzzableAbstractType(
        CHFuzzableType.StringType.fuzzingValues :+ "'azertyuiop'::FixedString(10)",
        Seq(
          CHFuzzableType.StringType,
          CHFuzzableType.FixedString
        )
      )
  case UUID
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.UUID.fuzzingValues.head),
        Seq(CHFuzzableType.UUID, CHFuzzableType.NullableUUID)
      )

  // Array
  case ArrayNumber
      extends CHFuzzableAbstractType(
        Number.fuzzingValues.map(n => s"array($n)") :+ "array(1, 2, 3, 4, 5, 6, 7, 8)",
        Seq(
          CHFuzzableType.ArrayBoolean,
          CHFuzzableType.ArrayInt8,
          CHFuzzableType.ArrayInt16,
          CHFuzzableType.ArrayInt32,
          CHFuzzableType.ArrayInt64,
          CHFuzzableType.ArrayInt128,
          CHFuzzableType.ArrayInt256,
          CHFuzzableType.ArrayUInt8,
          CHFuzzableType.ArrayUInt16,
          CHFuzzableType.ArrayUInt32,
          CHFuzzableType.ArrayUInt64,
          CHFuzzableType.ArrayUInt128,
          CHFuzzableType.ArrayUInt256,
          CHFuzzableType.ArrayFloat32,
          CHFuzzableType.ArrayFloat64,
          CHFuzzableType.ArrayDecimal32,
          CHFuzzableType.ArrayDecimal64,
          CHFuzzableType.ArrayDecimal128,
          CHFuzzableType.ArrayDecimal256
        )
      )

  case ArrayDate
      extends CHFuzzableAbstractType(
        Seq(s"[${Date.fuzzingValues.head}]::Array(Date)"),
        Seq(CHFuzzableType.ArrayDate, CHFuzzableType.ArrayDate32)
      )
  case ArrayDateTime
      extends CHFuzzableAbstractType(
        Seq(s"[${DateTime.fuzzingValues.head}]::Array(DateTime)"),
        Seq(CHFuzzableType.ArrayDateTime, CHFuzzableType.ArrayDateTime64)
      )
  case ArrayIntervalDate
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.ArrayIntervalDay.fuzzingValues.head),
        Seq(
          CHFuzzableType.ArrayIntervalDay,
          CHFuzzableType.ArrayIntervalWeek,
          CHFuzzableType.ArrayIntervalMonth,
          CHFuzzableType.ArrayIntervalQuarter,
          CHFuzzableType.ArrayIntervalYear
        )
      )
  case ArrayIntervalDateTime
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.ArrayIntervalNanosecond.fuzzingValues.head),
        Seq(
          CHFuzzableType.ArrayIntervalNanosecond,
          CHFuzzableType.ArrayIntervalMicrosecond,
          CHFuzzableType.ArrayIntervalMillisecond,
          CHFuzzableType.ArrayIntervalSecond,
          CHFuzzableType.ArrayIntervalMinute,
          CHFuzzableType.ArrayIntervalHour
        )
      )
  case ArrayEnum
      extends CHFuzzableAbstractType(
        Seq(s"[${Enum.fuzzingValues.head}]"),
        Seq(CHFuzzableType.ArrayEnum, CHFuzzableType.ArrayEnum8, CHFuzzableType.ArrayEnum16)
      )
  case ArrayIPv4
      extends CHFuzzableAbstractType(Seq(s"[${IPv4.fuzzingValues.head}]::Array(IPv4)"), Seq(CHFuzzableType.ArrayIPv4))
  case ArrayIPv6
      extends CHFuzzableAbstractType(Seq(s"[${IPv6.fuzzingValues.head}]::Array(IPv6)"), Seq(CHFuzzableType.ArrayIPv6))
  case ArrayJson
      extends CHFuzzableAbstractType(
        CHFuzzableType.Json.fuzzingValues.headOption.map(v => s"[$v]::Array(JSON)").toSeq,
        Seq(CHFuzzableType.ArrayJson)
      )
  // case ArrayNothing extends CHFuzzableAbstractType(CHFuzzableType.ArrayNothing.fuzzingValues, Seq(CHFuzzableType.ArrayNothing))
  case ArrayString
      extends CHFuzzableAbstractType(
        Seq(s"[${String.fuzzingValues.head}]::Array(String)"),
        Seq(CHFuzzableType.ArrayFixedString, CHFuzzableType.ArrayString)
      )
  case ArrayUUID
      extends CHFuzzableAbstractType(Seq(s"[${UUID.fuzzingValues.head}]::Array(UUID)"), Seq(CHFuzzableType.ArrayUUID))

  // Map
  case MapNumberInt
      extends CHFuzzableAbstractType(
        Seq(s"map(${Number.fuzzingValues.head}, 1)"),
        Seq(
          CHFuzzableType.MapBooleanInt,
          CHFuzzableType.MapInt8Int,
          CHFuzzableType.MapInt16Int,
          CHFuzzableType.MapInt32Int,
          CHFuzzableType.MapInt64Int,
          CHFuzzableType.MapInt128Int,
          CHFuzzableType.MapInt256Int,
          CHFuzzableType.MapUInt8Int,
          CHFuzzableType.MapUInt16Int,
          CHFuzzableType.MapUInt32Int,
          CHFuzzableType.MapUInt64Int,
          CHFuzzableType.MapUInt128Int,
          CHFuzzableType.MapUInt256Int
        )
      )

  case MapDateInt
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.MapDateInt.fuzzingValues.head, CHFuzzableType.MapDate32Int.fuzzingValues.head),
        Seq(CHFuzzableType.MapDateInt, CHFuzzableType.MapDate32Int)
      )
  case MapDateTimeInt
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.MapDateTimeInt.fuzzingValues.head),
        Seq(CHFuzzableType.MapDateTimeInt)
      )
  case MapIntervalDateInt
      extends CHFuzzableAbstractType(
        Seq(
          CHFuzzableType.MapIntervalDayInt.fuzzingValues.head,
          CHFuzzableType.MapIntervalWeekInt.fuzzingValues.head,
          CHFuzzableType.MapIntervalMonthInt.fuzzingValues.head,
          CHFuzzableType.MapIntervalQuarterInt.fuzzingValues.head,
          CHFuzzableType.MapIntervalYearInt.fuzzingValues.head
        ),
        Seq(
          CHFuzzableType.MapIntervalDayInt,
          CHFuzzableType.MapIntervalWeekInt,
          CHFuzzableType.MapIntervalMonthInt,
          CHFuzzableType.MapIntervalQuarterInt,
          CHFuzzableType.MapIntervalYearInt
        )
      )
  case MapIntervalDateTimeInt
      extends CHFuzzableAbstractType(
        Seq(
          CHFuzzableType.MapIntervalNanosecondInt.fuzzingValues.head,
          CHFuzzableType.MapIntervalMicrosecondInt.fuzzingValues.head,
          CHFuzzableType.MapIntervalMillisecondInt.fuzzingValues.head,
          CHFuzzableType.MapIntervalSecondInt.fuzzingValues.head,
          CHFuzzableType.MapIntervalMinuteInt.fuzzingValues.head,
          CHFuzzableType.MapIntervalHourInt.fuzzingValues.head
        ),
        Seq(
          CHFuzzableType.MapIntervalNanosecondInt,
          CHFuzzableType.MapIntervalMicrosecondInt,
          CHFuzzableType.MapIntervalMillisecondInt,
          CHFuzzableType.MapIntervalSecondInt,
          CHFuzzableType.MapIntervalMinuteInt,
          CHFuzzableType.MapIntervalHourInt
        )
      )
  case MapEnumInt
      extends CHFuzzableAbstractType(
        CHFuzzableType.MapEnumInt.fuzzingValues ++ CHFuzzableType.MapEnum8Int.fuzzingValues ++ CHFuzzableType.MapEnum16Int.fuzzingValues,
        Seq(CHFuzzableType.MapEnumInt, CHFuzzableType.MapEnum8Int, CHFuzzableType.MapEnum16Int)
      )
  case MapIPv4Int
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.MapIPv4Int.fuzzingValues.head),
        Seq(CHFuzzableType.MapIPv4Int)
      )
  case MapIPv6Int
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.MapIPv6Int.fuzzingValues.head),
        Seq(CHFuzzableType.MapIPv6Int)
      )
  case MapStringInt
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.MapFixedStringInt.fuzzingValues.head, CHFuzzableType.MapStringInt.fuzzingValues.head),
        Seq(CHFuzzableType.MapFixedStringInt, CHFuzzableType.MapStringInt)
      )
  case MapUUIDInt
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.MapUUIDInt.fuzzingValues.head),
        Seq(CHFuzzableType.MapUUIDInt)
      )

  // Tuple1
  case Tuple1Number
      extends CHFuzzableAbstractType(
        CHFuzzableType.Tuple1UInt8.fuzzingValues,
        Seq(
          CHFuzzableType.Tuple1Int8,
          CHFuzzableType.Tuple1Int16,
          CHFuzzableType.Tuple1Int32,
          CHFuzzableType.Tuple1Int64,
          CHFuzzableType.Tuple1Int128,
          CHFuzzableType.Tuple1Int256,
          CHFuzzableType.Tuple1UInt8,
          CHFuzzableType.Tuple1UInt16,
          CHFuzzableType.Tuple1UInt32,
          CHFuzzableType.Tuple1UInt64,
          CHFuzzableType.Tuple1UInt128,
          CHFuzzableType.Tuple1UInt256,
          CHFuzzableType.Tuple1Float32,
          CHFuzzableType.Tuple1Float64,
          CHFuzzableType.Tuple1Decimal32,
          CHFuzzableType.Tuple1Decimal64,
          CHFuzzableType.Tuple1Decimal128,
          CHFuzzableType.Tuple1Decimal256
        )
      )
  case Tuple1Date
      extends CHFuzzableAbstractType(
        Seq(s"tuple(${Date.fuzzingValues.head})::Tuple(Date)", s"tuple(${Date.fuzzingValues.head})::Tuple(a Date)"),
        Seq(CHFuzzableType.Tuple1Date, CHFuzzableType.Tuple1Date32)
      )
  case Tuple1DateTime
      extends CHFuzzableAbstractType(
        Seq(
          s"tuple(${DateTime.fuzzingValues.head})::Tuple(DateTime)",
          s"tuple(${DateTime.fuzzingValues.head})::Tuple(a DateTime)"
        ),
        Seq(CHFuzzableType.Tuple1DateTime, CHFuzzableType.Tuple1DateTime64)
      )
  case Tuple1IntervalDate
      extends CHFuzzableAbstractType(
        CHFuzzableType.Tuple1IntervalDay.fuzzingValues,
        Seq(
          CHFuzzableType.Tuple1IntervalDay,
          CHFuzzableType.Tuple1IntervalWeek,
          CHFuzzableType.Tuple1IntervalMonth,
          CHFuzzableType.Tuple1IntervalQuarter,
          CHFuzzableType.Tuple1IntervalYear
        )
      )
  case Tuple1IntervalDateTime
      extends CHFuzzableAbstractType(
        CHFuzzableType.Tuple1IntervalNanosecond.fuzzingValues,
        Seq(
          CHFuzzableType.Tuple1IntervalNanosecond,
          CHFuzzableType.Tuple1IntervalMicrosecond,
          CHFuzzableType.Tuple1IntervalMillisecond,
          CHFuzzableType.Tuple1IntervalSecond,
          CHFuzzableType.Tuple1IntervalMinute,
          CHFuzzableType.Tuple1IntervalHour
        )
      )
  case Tuple1Enum
      extends CHFuzzableAbstractType(
        Seq(
          s"tuple(${Enum.fuzzingValues.head})::Tuple(Enum('hello' = 1, 'world' = 2))",
          s"tuple(${Enum.fuzzingValues.head})::Tuple(a Enum('hello' = 1, 'world' = 2))"
        ),
        Seq(CHFuzzableType.Tuple1Enum, CHFuzzableType.Tuple1Enum8, CHFuzzableType.Tuple1Enum16)
      )
  case Tuple1IPv4
      extends CHFuzzableAbstractType(
        Seq(s"tuple(${IPv4.fuzzingValues.head})::Tuple(IPv4)", s"tuple(${IPv4.fuzzingValues.head})::Tuple(a IPv4)"),
        Seq(CHFuzzableType.Tuple1IPv4)
      )
  case Tuple1IPv6
      extends CHFuzzableAbstractType(
        Seq(s"tuple(${IPv6.fuzzingValues.head})::Tuple(IPv6)", s"tuple(${IPv6.fuzzingValues.head})::Tuple(a IPv6)"),
        Seq(CHFuzzableType.Tuple1IPv6)
      )
  case Tuple1Json
      extends CHFuzzableAbstractType(
        CHFuzzableType.Json.fuzzingValues.headOption.toSeq.flatMap(v =>
          Seq(s"tuple($v)::Tuple(JSON)", s"tuple($v)::Tuple(a JSON)")
        ),
        Seq(CHFuzzableType.Tuple1Json)
      )
  // case Tuple1Nothing extends CHFuzzableAbstractType(CHFuzzableType.Tuple1Nothing.fuzzingValues, Seq(CHFuzzableType.Tuple1Nothing))
  case Tuple1String
      extends CHFuzzableAbstractType(
        Seq(
          s"tuple(${String.fuzzingValues.head})::Tuple(String)",
          s"tuple(${String.fuzzingValues.head})::Tuple(a String)"
        ),
        Seq(CHFuzzableType.Tuple1FixedString, CHFuzzableType.Tuple1String)
      )
  case Tuple1UUID
      extends CHFuzzableAbstractType(
        Seq(s"tuple(${UUID.fuzzingValues.head})::Tuple(UUID)", s"tuple(${UUID.fuzzingValues.head})::Tuple(a UUID)"),
        Seq(CHFuzzableType.Tuple1UUID)
      )

  // Nested
  case ArrayArrayString
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.ArrayArrayString.fuzzingValues.head),
        Seq(CHFuzzableType.ArrayArrayString)
      )
  case ArrayMapStringInt
      extends CHFuzzableAbstractType(
        Seq(s"[${MapStringInt.fuzzingValues.head}]::Array(Map(String, Int8))"),
        Seq(CHFuzzableType.ArrayMapStringInt)
      )
  case ArrayTuple1Number
      extends CHFuzzableAbstractType(
        Seq(s"[${Tuple1Number.fuzzingValues.head}]::Array(Tuple(UInt8))"),
        Seq(CHFuzzableType.ArrayTuple1UInt8)
      )
  case Tuple1ArrayNumber
      extends CHFuzzableAbstractType(
        Seq(
          s"tuple(${CHFuzzableType.ArrayUInt8.fuzzingValues.head})::Tuple(Array(UInt8))",
          s"tuple(${CHFuzzableType.ArrayUInt8.fuzzingValues.head})::Tuple(a Array(UInt8))"
        ),
        Seq(CHFuzzableType.Tuple1ArrayUInt8)
      )
  case Tuple1MapStringInt
      extends CHFuzzableAbstractType(
        Seq(
          s"tuple(${MapStringInt.fuzzingValues.head})::Tuple(Map(String, Int8))",
          s"tuple(${MapStringInt.fuzzingValues.head})::Tuple(a Map(String, Int8))"
        ),
        Seq(CHFuzzableType.Tuple1MapStringInt)
      )

  // Special
  // Special - Geo
  case Point
      extends CHFuzzableAbstractType(Seq(CHFuzzableType.Point.fuzzingValues.head), Seq(CHFuzzableType.Point))
      with CustomArrayBasedAbstractType
  case Ring
      extends CHFuzzableAbstractType(Seq(CHFuzzableType.Ring.fuzzingValues.head), Seq(CHFuzzableType.Ring))
      with CustomArrayBasedAbstractType
  case Polygon
      extends CHFuzzableAbstractType(Seq(CHFuzzableType.Polygon.fuzzingValues.head), Seq(CHFuzzableType.Polygon))
      with CustomArrayBasedAbstractType
  case MultiPolygon
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.MultiPolygon.fuzzingValues.head),
        Seq(CHFuzzableType.MultiPolygon)
      )
      with CustomArrayBasedAbstractType

  case Tuple1Point
      extends CHFuzzableAbstractType(
        CHFuzzableType.Tuple1Point.fuzzingValues,
        Seq(CHFuzzableType.Tuple1Point)
      )
      with CustomArrayBasedAbstractType
  case Tuple1Ring
      extends CHFuzzableAbstractType(CHFuzzableType.Tuple1Ring.fuzzingValues, Seq(CHFuzzableType.Tuple1Ring))
      with CustomArrayBasedAbstractType
  case Tuple1Polygon
      extends CHFuzzableAbstractType(
        CHFuzzableType.Tuple1Polygon.fuzzingValues,
        Seq(CHFuzzableType.Tuple1Polygon)
      )
      with CustomArrayBasedAbstractType
  case Tuple1MultiPolygon
      extends CHFuzzableAbstractType(
        CHFuzzableType.Tuple1MultiPolygon.fuzzingValues,
        Seq(CHFuzzableType.Tuple1MultiPolygon)
      )
      with CustomArrayBasedAbstractType

  // Special - Misc
  case Charset
      extends CHFuzzableAbstractType(
        CHFuzzableType.Charset.fuzzingValues,
        Seq(CHFuzzableType.Charset)
      )
      with CustomStringBasedAbstractType
  case ClickHouseType
      extends CHFuzzableAbstractType(
        CHFuzzableType.ClickHouseType.fuzzingValues,
        Seq(CHFuzzableType.ClickHouseType)
      )
      with CustomStringBasedAbstractType
  case DateTimeUnit
      extends CHFuzzableAbstractType(
        Seq(
          CHFuzzableType.DateUnit.fuzzingValues.head,
          CHFuzzableType.TimeUnit.fuzzingValues.head,
          CHFuzzableType.Time64Unit.fuzzingValues.head
        ),
        Seq(CHFuzzableType.DateUnit, CHFuzzableType.TimeUnit, CHFuzzableType.Time64Unit)
      )
      with CustomStringBasedAbstractType
  case DictionaryName
      extends CHFuzzableAbstractType(
        CHFuzzableType.DictionaryName.fuzzingValues,
        Seq(CHFuzzableType.DictionaryName)
      )
      with CustomStringBasedAbstractType
  case SequencePattern
      extends CHFuzzableAbstractType(
        CHFuzzableType.SequencePattern.fuzzingValues,
        Seq(CHFuzzableType.SequencePattern)
      )
      with CustomStringBasedAbstractType
  case ServerPortName
      extends CHFuzzableAbstractType(
        Seq("'tcp_port'"),
        Seq(CHFuzzableType.ServerPortName)
      )
      with CustomStringBasedAbstractType
  case SpecialUInt64
      extends CHFuzzableAbstractType(
        Seq("599686042433355775::UInt64"),
        Seq(
          CHFuzzableType.SpecialUInt64,
          CHFuzzableType.SpecialLowCardinalityUInt64,
          CHFuzzableType.SpecialLowCardinalityNullableUInt64,
          CHFuzzableType.SpecialNullableUInt64
        )
      )
      with CustomStringBasedAbstractType
  case SpecialString
      extends CHFuzzableAbstractType(
        CHFuzzableType.SpecialString.fuzzingValues :+ "'a/<@];!~p{jTj={)'::FixedString(16)",
        Seq(
          CHFuzzableType.SpecialString,
          CHFuzzableType.SpecialFixedString
        )
      )
      with CustomStringBasedAbstractType
  case SpecialArrayString
      extends CHFuzzableAbstractType(
        Seq(s"[${SpecialString.fuzzingValues.head}]::Array(String)"),
        Seq(CHFuzzableType.SpecialArrayFixedString, CHFuzzableType.SpecialArrayString)
      )
      with CustomStringBasedAbstractType
  case SynonymExtensionName
      extends CHFuzzableAbstractType(
        CHFuzzableType.SynonymExtensionName.fuzzingValues,
        Seq(CHFuzzableType.SynonymExtensionName)
      )
      with CustomStringBasedAbstractType
  case TimeZone
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.TimeZone.fuzzingValues.head),
        Seq(CHFuzzableType.TimeZone)
      )
      with CustomStringBasedAbstractType
  case TopKOption
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.TopKOption.fuzzingValues.head),
        Seq(CHFuzzableType.TopKOption)
      )
      with CustomStringBasedAbstractType
  case WindowFunctionMode
      extends CHFuzzableAbstractType(
        CHFuzzableType.WindowFunctionMode.fuzzingValues,
        Seq(CHFuzzableType.WindowFunctionMode)
      )
      with CustomStringBasedAbstractType
