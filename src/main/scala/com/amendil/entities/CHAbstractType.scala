package com.amendil.entities

enum CHFuzzableAbstractType(val fuzzingValues: Seq[String], val CHFuzzableTypes: Seq[CHFuzzableType]):
  // Numbers
  case Numbers
      extends CHFuzzableAbstractType(
        Seq("1"),
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

  // Date
  case Date
      extends CHFuzzableAbstractType(
        Seq("'1970-01-02'::Date"),
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
        Seq("'1970-01-02 00:00:00'::DateTime"),
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
  case IntervalDateTime
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.IntervalNanosecond.fuzzingValues.head),
        Seq(
          CHFuzzableType.IntervalNanosecond,
          CHFuzzableType.IntervalMicrosecond,
          CHFuzzableType.IntervalMillisecond,
          CHFuzzableType.IntervalSecond,
          CHFuzzableType.IntervalMinute,
          CHFuzzableType.IntervalHour,
          CHFuzzableType.NullableIntervalNanosecond,
          CHFuzzableType.NullableIntervalMicrosecond,
          CHFuzzableType.NullableIntervalMillisecond,
          CHFuzzableType.NullableIntervalSecond,
          CHFuzzableType.NullableIntervalMinute,
          CHFuzzableType.NullableIntervalHour
        )
      )

  // Geo
  case Point extends CHFuzzableAbstractType(Seq(CHFuzzableType.Point.fuzzingValues.head), Seq(CHFuzzableType.Point))
  case Ring extends CHFuzzableAbstractType(Seq(CHFuzzableType.Ring.fuzzingValues.head), Seq(CHFuzzableType.Ring))
  case Polygon
      extends CHFuzzableAbstractType(Seq(CHFuzzableType.Polygon.fuzzingValues.head), Seq(CHFuzzableType.Polygon))
  case MultiPolygon
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.MultiPolygon.fuzzingValues.head),
        Seq(CHFuzzableType.MultiPolygon)
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
  case Json extends CHFuzzableAbstractType(Seq(CHFuzzableType.Json.fuzzingValues.head), Seq(CHFuzzableType.Json))
  // case Nothing extends CHFuzzableAbstractType(CHFuzzableType.NullableNothing.fuzzingValues, Seq(CHFuzzableType.NullableNothing))
  case String
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.StringType.fuzzingValues.head),
        Seq(
          CHFuzzableType.StringType,
          CHFuzzableType.FixedString,
          CHFuzzableType.LowCardinalityFixedString,
          CHFuzzableType.LowCardinalityString,
          CHFuzzableType.LowCardinalityNullableFixedString,
          CHFuzzableType.LowCardinalityNullableString,
          CHFuzzableType.NullableFixedString,
          CHFuzzableType.NullableString
        )
      )
  case UUID
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.UUID.fuzzingValues.head),
        Seq(CHFuzzableType.UUID, CHFuzzableType.NullableUUID)
      )

  // Array
  case ArrayNumbers
      extends CHFuzzableAbstractType(
        Seq(s"array(${Numbers.fuzzingValues.head})"),
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
  case ArrayPoint
      extends CHFuzzableAbstractType(Seq(CHFuzzableType.ArrayPoint.fuzzingValues.head), Seq(CHFuzzableType.ArrayPoint))
  case ArrayRing
      extends CHFuzzableAbstractType(Seq(CHFuzzableType.ArrayRing.fuzzingValues.head), Seq(CHFuzzableType.ArrayRing))
  case ArrayPolygon
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.ArrayPolygon.fuzzingValues.head),
        Seq(CHFuzzableType.ArrayPolygon)
      )
  case ArrayMultiPolygon
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.ArrayMultiPolygon.fuzzingValues.head),
        Seq(CHFuzzableType.ArrayMultiPolygon)
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
      extends CHFuzzableAbstractType(Seq(s"[${Json.fuzzingValues.head}]::Array(JSON)"), Seq(CHFuzzableType.ArrayJson))
  // case ArrayNothing extends CHFuzzableAbstractType(CHFuzzableType.ArrayNothing.fuzzingValues, Seq(CHFuzzableType.ArrayNothing))
  case ArrayString
      extends CHFuzzableAbstractType(
        Seq(s"[${String.fuzzingValues.head}]::Array(String)"),
        Seq(CHFuzzableType.ArrayFixedString, CHFuzzableType.ArrayString)
      )
  case ArrayUUID
      extends CHFuzzableAbstractType(Seq(s"[${UUID.fuzzingValues.head}]::Array(UUID)"), Seq(CHFuzzableType.ArrayUUID))

  // Map
  case MapNumbersInt8
      extends CHFuzzableAbstractType(
        Seq(s"map(${Numbers.fuzzingValues.head}, 1)"),
        Seq(
          CHFuzzableType.MapBooleanInt8,
          CHFuzzableType.MapInt8Int8,
          CHFuzzableType.MapInt16Int8,
          CHFuzzableType.MapInt32Int8,
          CHFuzzableType.MapInt64Int8,
          CHFuzzableType.MapInt128Int8,
          CHFuzzableType.MapInt256Int8,
          CHFuzzableType.MapUInt8Int8,
          CHFuzzableType.MapUInt16Int8,
          CHFuzzableType.MapUInt32Int8,
          CHFuzzableType.MapUInt64Int8,
          CHFuzzableType.MapUInt128Int8,
          CHFuzzableType.MapUInt256Int8
        )
      )

  case MapDateInt8
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.MapDateInt8.fuzzingValues.head),
        Seq(CHFuzzableType.MapDateInt8, CHFuzzableType.MapDate32Int8)
      )
  case MapDateTimeInt8
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.MapDateTimeInt8.fuzzingValues.head),
        Seq(CHFuzzableType.MapDateTimeInt8)
      )
  case MapIntervalDateInt8
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.MapIntervalDayInt8.fuzzingValues.head),
        Seq(
          CHFuzzableType.MapIntervalDayInt8,
          CHFuzzableType.MapIntervalWeekInt8,
          CHFuzzableType.MapIntervalMonthInt8,
          CHFuzzableType.MapIntervalQuarterInt8,
          CHFuzzableType.MapIntervalYearInt8
        )
      )
  case MapIntervalDateTimeInt8
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.MapIntervalNanosecondInt8.fuzzingValues.head),
        Seq(
          CHFuzzableType.MapIntervalNanosecondInt8,
          CHFuzzableType.MapIntervalMicrosecondInt8,
          CHFuzzableType.MapIntervalMillisecondInt8,
          CHFuzzableType.MapIntervalSecondInt8,
          CHFuzzableType.MapIntervalMinuteInt8,
          CHFuzzableType.MapIntervalHourInt8
        )
      )
  case MapEnumInt8
      extends CHFuzzableAbstractType(
        Seq(s"map(${Enum.fuzzingValues.head}, 1)"),
        Seq(CHFuzzableType.MapEnumInt8, CHFuzzableType.MapEnum8Int8, CHFuzzableType.MapEnum16Int8)
      )
  case MapIPv4Int8
      extends CHFuzzableAbstractType(
        Seq(s"map(${IPv4.fuzzingValues.head}, 1)::Map(IPv4, Int8)"),
        Seq(CHFuzzableType.MapIPv4Int8)
      )
  case MapIPv6Int8
      extends CHFuzzableAbstractType(
        Seq(s"map(${IPv6.fuzzingValues.head}, 1)::Map(IPv6, Int8)"),
        Seq(CHFuzzableType.MapIPv6Int8)
      )
  case MapStringInt8
      extends CHFuzzableAbstractType(
        Seq(s"map(${String.fuzzingValues.head}, 1)::Map(String, Int8)"),
        Seq(CHFuzzableType.MapFixedStringInt8, CHFuzzableType.MapStringInt8)
      )
  case MapUUIDInt8
      extends CHFuzzableAbstractType(
        Seq(s"map(${UUID.fuzzingValues.head}, 1)::Map(UUID, Int8)"),
        Seq(CHFuzzableType.MapUUIDInt8)
      )

  // Tuple1
  case Tuple1Numbers
      extends CHFuzzableAbstractType(
        Seq(s"tuple(${Numbers.fuzzingValues.head})::Tuple(UInt8)"),
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
        Seq(s"tuple(${Date.fuzzingValues.head})::Tuple(Date)"),
        Seq(CHFuzzableType.Tuple1Date, CHFuzzableType.Tuple1Date32)
      )
  case Tuple1DateTime
      extends CHFuzzableAbstractType(
        Seq(s"tuple(${DateTime.fuzzingValues.head})::Tuple(DateTime)"),
        Seq(CHFuzzableType.Tuple1DateTime, CHFuzzableType.Tuple1DateTime64)
      )
  case Tuple1IntervalDate
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.Tuple1IntervalDay.fuzzingValues.head),
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
        Seq(CHFuzzableType.Tuple1IntervalNanosecond.fuzzingValues.head),
        Seq(
          CHFuzzableType.Tuple1IntervalNanosecond,
          CHFuzzableType.Tuple1IntervalMicrosecond,
          CHFuzzableType.Tuple1IntervalMillisecond,
          CHFuzzableType.Tuple1IntervalSecond,
          CHFuzzableType.Tuple1IntervalMinute,
          CHFuzzableType.Tuple1IntervalHour
        )
      )
  case Tuple1Point
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.Tuple1Point.fuzzingValues.head),
        Seq(CHFuzzableType.Tuple1Point)
      )
  case Tuple1Ring
      extends CHFuzzableAbstractType(Seq(CHFuzzableType.Tuple1Ring.fuzzingValues.head), Seq(CHFuzzableType.Tuple1Ring))
  case Tuple1Polygon
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.Tuple1Polygon.fuzzingValues.head),
        Seq(CHFuzzableType.Tuple1Polygon)
      )
  case Tuple1MultiPolygon
      extends CHFuzzableAbstractType(
        Seq(CHFuzzableType.Tuple1MultiPolygon.fuzzingValues.head),
        Seq(CHFuzzableType.Tuple1MultiPolygon)
      )
  case Tuple1Enum
      extends CHFuzzableAbstractType(
        Seq(s"tuple(${Enum.fuzzingValues.head})::Tuple(Enum('hello' = 1, 'world' = 2))"),
        Seq(CHFuzzableType.Tuple1Enum, CHFuzzableType.Tuple1Enum8, CHFuzzableType.Tuple1Enum16)
      )
  case Tuple1IPv4
      extends CHFuzzableAbstractType(
        Seq(s"tuple(${IPv4.fuzzingValues.head})::Tuple(IPv4)"),
        Seq(CHFuzzableType.Tuple1IPv4)
      )
  case Tuple1IPv6
      extends CHFuzzableAbstractType(
        Seq(s"tuple(${IPv6.fuzzingValues.head})::Tuple(IPv6)"),
        Seq(CHFuzzableType.Tuple1IPv6)
      )
  case Tuple1Json
      extends CHFuzzableAbstractType(
        Seq(s"tuple(${Json.fuzzingValues.head})::Tuple(JSON)"),
        Seq(CHFuzzableType.Tuple1Json)
      )
  // case Tuple1Nothing extends CHFuzzableAbstractType(CHFuzzableType.Tuple1Nothing.fuzzingValues, Seq(CHFuzzableType.Tuple1Nothing))
  case Tuple1String
      extends CHFuzzableAbstractType(
        Seq(s"tuple(${String.fuzzingValues.head})::Tuple(String)"),
        Seq(CHFuzzableType.Tuple1FixedString, CHFuzzableType.Tuple1String)
      )
  case Tuple1UUID
      extends CHFuzzableAbstractType(
        Seq(s"tuple(${UUID.fuzzingValues.head})::Tuple(UUID)"),
        Seq(CHFuzzableType.Tuple1UUID)
      )

  // Nested
  case ArrayMapStringInt8
      extends CHFuzzableAbstractType(
        Seq(s"[${MapStringInt8.fuzzingValues.head}]::Array(Map(String, Int8))"),
        Seq(CHFuzzableType.ArrayMapStringInt8)
      )
  case ArrayTuple1Numbers
      extends CHFuzzableAbstractType(
        Seq(s"[${Tuple1Numbers.fuzzingValues.head}]::Array(Tuple(UInt8))"),
        Seq(CHFuzzableType.ArrayTuple1UInt8)
      )
  case Tuple1ArrayNumbers
      extends CHFuzzableAbstractType(
        Seq(s"tuple(${ArrayNumbers.fuzzingValues.head})::Tuple(Array(UInt8))"),
        Seq(CHFuzzableType.Tuple1ArrayUInt8)
      )
  case Tuple1MapStringInt8
      extends CHFuzzableAbstractType(
        Seq(s"tuple(${MapStringInt8.fuzzingValues.head})::Tuple(Map(String, Int8))"),
        Seq(CHFuzzableType.Tuple1MapStringInt8)
      )

  // Special
  case ClickHouseType extends CHFuzzableAbstractType(Seq("Nullable(String)"), Seq(CHFuzzableType.ClickHouseType))
  case WindowFunctionMode
      extends CHFuzzableAbstractType(
        CHFuzzableType.WindowFunctionMode.fuzzingValues,
        Seq(CHFuzzableType.WindowFunctionMode)
      )
