package com.amendil.entities

enum CHAbstractType(val fuzzingValues: Seq[String], val chTypes: Seq[CHType]) {
  // Numbers
  case Numbers
      extends CHAbstractType(
        Seq("1"),
        Seq(
          CHType.Int8,
          CHType.Int16,
          CHType.Int32,
          CHType.Int64,
          CHType.Int128,
          CHType.Int256,
          CHType.UInt8,
          CHType.UInt16,
          CHType.UInt32,
          CHType.UInt64,
          CHType.UInt128,
          CHType.UInt256,
          CHType.Float32,
          CHType.Float64,
          CHType.Decimal32,
          CHType.Decimal64,
          CHType.Decimal128,
          CHType.Decimal256,
          CHType.LowCardinalityInt8,
          CHType.LowCardinalityInt16,
          CHType.LowCardinalityInt32,
          CHType.LowCardinalityInt64,
          CHType.LowCardinalityInt128,
          CHType.LowCardinalityInt256,
          CHType.LowCardinalityUInt8,
          CHType.LowCardinalityUInt16,
          CHType.LowCardinalityUInt32,
          CHType.LowCardinalityUInt64,
          CHType.LowCardinalityUInt128,
          CHType.LowCardinalityUInt256,
          CHType.LowCardinalityFloat32,
          CHType.LowCardinalityFloat64,
          CHType.LowCardinalityNullableInt8,
          CHType.LowCardinalityNullableInt16,
          CHType.LowCardinalityNullableInt32,
          CHType.LowCardinalityNullableInt64,
          CHType.LowCardinalityNullableInt128,
          CHType.LowCardinalityNullableInt256,
          CHType.LowCardinalityNullableUInt8,
          CHType.LowCardinalityNullableUInt16,
          CHType.LowCardinalityNullableUInt32,
          CHType.LowCardinalityNullableUInt64,
          CHType.LowCardinalityNullableUInt128,
          CHType.LowCardinalityNullableUInt256,
          CHType.LowCardinalityNullableFloat32,
          CHType.LowCardinalityNullableFloat64,
          CHType.NullableInt8,
          CHType.NullableInt16,
          CHType.NullableInt32,
          CHType.NullableInt64,
          CHType.NullableInt128,
          CHType.NullableInt256,
          CHType.NullableUInt8,
          CHType.NullableUInt16,
          CHType.NullableUInt32,
          CHType.NullableUInt64,
          CHType.NullableUInt128,
          CHType.NullableUInt256,
          CHType.NullableFloat32,
          CHType.NullableFloat64,
          CHType.NullableDecimal32,
          CHType.NullableDecimal64,
          CHType.NullableDecimal128,
          CHType.NullableDecimal256
        )
      )

  // Date
  case Date
      extends CHAbstractType(
        Seq("'1970-01-02'::Date"),
        Seq(
          CHType.Date,
          CHType.Date32,
          CHType.LowCardinalityDate,
          CHType.LowCardinalityDate32,
          CHType.LowCardinalityNullableDate,
          CHType.LowCardinalityNullableDate32,
          CHType.NullableDate,
          CHType.NullableDate32
        )
      )
  case DateTime
      extends CHAbstractType(
        Seq("'1970-01-02 00:00:00'::DateTime"),
        Seq(
          CHType.DateTime,
          CHType.DateTime64,
          CHType.LowCardinalityDateTime,
          CHType.LowCardinalityNullableDateTime,
          CHType.NullableDateTime,
          CHType.NullableDateTime64
        )
      )
  case IntervalDate
      extends CHAbstractType(
        Seq(CHType.IntervalDay.fuzzingValues.head),
        Seq(
          CHType.IntervalDay,
          CHType.IntervalWeek,
          CHType.IntervalMonth,
          CHType.IntervalQuarter,
          CHType.IntervalYear,
          CHType.NullableIntervalDay,
          CHType.NullableIntervalWeek,
          CHType.NullableIntervalMonth,
          CHType.NullableIntervalQuarter,
          CHType.NullableIntervalYear
        )
      )
  case IntervalDateTime
      extends CHAbstractType(
        Seq(CHType.IntervalNanosecond.fuzzingValues.head),
        Seq(
          CHType.IntervalNanosecond,
          CHType.IntervalMicrosecond,
          CHType.IntervalMillisecond,
          CHType.IntervalSecond,
          CHType.IntervalMinute,
          CHType.IntervalHour,
          CHType.NullableIntervalNanosecond,
          CHType.NullableIntervalMicrosecond,
          CHType.NullableIntervalMillisecond,
          CHType.NullableIntervalSecond,
          CHType.NullableIntervalMinute,
          CHType.NullableIntervalHour
        )
      )

  // Geo
  case Point extends CHAbstractType(Seq(CHType.Point.fuzzingValues.head), Seq(CHType.Point))
  case Ring extends CHAbstractType(Seq(CHType.Ring.fuzzingValues.head), Seq(CHType.Ring))
  case Polygon extends CHAbstractType(Seq(CHType.Polygon.fuzzingValues.head), Seq(CHType.Polygon))
  case MultiPolygon extends CHAbstractType(Seq(CHType.MultiPolygon.fuzzingValues.head), Seq(CHType.MultiPolygon))

  // Misc
  case Enum
      extends CHAbstractType(
        Seq(CHType.Enum.fuzzingValues.head),
        Seq(CHType.Enum, CHType.Enum8, CHType.Enum16, CHType.NullableEnum, CHType.NullableEnum8, CHType.NullableEnum16)
      )
  case IPv4 extends CHAbstractType(Seq(CHType.IPv4.fuzzingValues.head), Seq(CHType.IPv4, CHType.NullableIPv4))
  case IPv6 extends CHAbstractType(Seq(CHType.IPv6.fuzzingValues.head), Seq(CHType.IPv6, CHType.NullableIPv6))
  case Json extends CHAbstractType(Seq(CHType.Json.fuzzingValues.head), Seq(CHType.Json))
  // case Nothing extends CHAbstractType(CHType.NullableNothing.fuzzingValues, Seq(CHType.NullableNothing))
  case String
      extends CHAbstractType(
        Seq(CHType.StringType.fuzzingValues.head),
        Seq(
          CHType.StringType,
          CHType.FixedString,
          CHType.LowCardinalityFixedString,
          CHType.LowCardinalityString,
          CHType.LowCardinalityNullableFixedString,
          CHType.LowCardinalityNullableString,
          CHType.NullableFixedString,
          CHType.NullableString
        )
      )
  case UUID extends CHAbstractType(Seq(CHType.UUID.fuzzingValues.head), Seq(CHType.UUID, CHType.NullableUUID))

  // Array
  case ArrayNumbers
      extends CHAbstractType(
        Seq(s"array(${Numbers.fuzzingValues.head})"),
        Seq(
          CHType.ArrayInt8,
          CHType.ArrayInt16,
          CHType.ArrayInt32,
          CHType.ArrayInt64,
          CHType.ArrayInt128,
          CHType.ArrayInt256,
          CHType.ArrayUInt8,
          CHType.ArrayUInt16,
          CHType.ArrayUInt32,
          CHType.ArrayUInt64,
          CHType.ArrayUInt128,
          CHType.ArrayUInt256,
          CHType.ArrayFloat32,
          CHType.ArrayFloat64,
          CHType.ArrayDecimal32,
          CHType.ArrayDecimal64,
          CHType.ArrayDecimal128,
          CHType.ArrayDecimal256
        )
      )

  case ArrayDate
      extends CHAbstractType(
        Seq(s"[${Date.fuzzingValues.head}]::Array(Date)"),
        Seq(CHType.ArrayDate, CHType.ArrayDate32)
      )
  case ArrayDateTime
      extends CHAbstractType(
        Seq(s"[${DateTime.fuzzingValues.head}]::Array(DateTime)"),
        Seq(CHType.ArrayDateTime, CHType.ArrayDateTime64)
      )
  case ArrayIntervalDate
      extends CHAbstractType(
        Seq(CHType.ArrayIntervalDay.fuzzingValues.head),
        Seq(
          CHType.ArrayIntervalDay,
          CHType.ArrayIntervalWeek,
          CHType.ArrayIntervalMonth,
          CHType.ArrayIntervalQuarter,
          CHType.ArrayIntervalYear
        )
      )
  case ArrayIntervalDateTime
      extends CHAbstractType(
        Seq(CHType.ArrayIntervalNanosecond.fuzzingValues.head),
        Seq(
          CHType.ArrayIntervalNanosecond,
          CHType.ArrayIntervalMicrosecond,
          CHType.ArrayIntervalMillisecond,
          CHType.ArrayIntervalSecond,
          CHType.ArrayIntervalMinute,
          CHType.ArrayIntervalHour
        )
      )
  case ArrayPoint extends CHAbstractType(Seq(CHType.ArrayPoint.fuzzingValues.head), Seq(CHType.ArrayPoint))
  case ArrayRing extends CHAbstractType(Seq(CHType.ArrayRing.fuzzingValues.head), Seq(CHType.ArrayRing))
  case ArrayPolygon extends CHAbstractType(Seq(CHType.ArrayPolygon.fuzzingValues.head), Seq(CHType.ArrayPolygon))
  case ArrayMultiPolygon
      extends CHAbstractType(Seq(CHType.ArrayMultiPolygon.fuzzingValues.head), Seq(CHType.ArrayMultiPolygon))
  case ArrayEnum
      extends CHAbstractType(
        Seq(s"[${Enum.fuzzingValues.head}]"),
        Seq(CHType.ArrayEnum, CHType.ArrayEnum8, CHType.ArrayEnum16)
      )
  case ArrayIPv4 extends CHAbstractType(Seq(s"[${IPv4.fuzzingValues.head}]::Array(IPv4)"), Seq(CHType.ArrayIPv4))
  case ArrayIPv6 extends CHAbstractType(Seq(s"[${IPv6.fuzzingValues.head}]::Array(IPv6)"), Seq(CHType.ArrayIPv6))
  case ArrayJson extends CHAbstractType(Seq(s"[${Json.fuzzingValues.head}]::Array(JSON)"), Seq(CHType.ArrayJson))
  // case ArrayNothing extends CHAbstractType(CHType.ArrayNothing.fuzzingValues, Seq(CHType.ArrayNothing))
  case ArrayString
      extends CHAbstractType(
        Seq(s"[${String.fuzzingValues.head}]::Array(String)"),
        Seq(CHType.ArrayFixedString, CHType.ArrayString)
      )
  case ArrayUUID extends CHAbstractType(Seq(s"[${UUID.fuzzingValues.head}]::Array(UUID)"), Seq(CHType.ArrayUUID))

  // Map
  case MapNumbersInt8
      extends CHAbstractType(
        Seq(s"map(${Numbers.fuzzingValues.head}, 1)"),
        Seq(
          CHType.MapInt8Int8,
          CHType.MapInt16Int8,
          CHType.MapInt32Int8,
          CHType.MapInt64Int8,
          CHType.MapInt128Int8,
          CHType.MapInt256Int8,
          CHType.MapUInt8Int8,
          CHType.MapUInt16Int8,
          CHType.MapUInt32Int8,
          CHType.MapUInt64Int8,
          CHType.MapUInt128Int8,
          CHType.MapUInt256Int8
        )
      )

  case MapDateInt8
      extends CHAbstractType(
        Seq(CHType.MapDateInt8.fuzzingValues.head),
        Seq(CHType.MapDateInt8, CHType.MapDate32Int8)
      )
  case MapDateTimeInt8
      extends CHAbstractType(
        Seq(CHType.MapDateTimeInt8.fuzzingValues.head),
        Seq(CHType.MapDateTimeInt8)
      )
  case MapIntervalDateInt8
      extends CHAbstractType(
        Seq(CHType.MapIntervalDayInt8.fuzzingValues.head),
        Seq(
          CHType.MapIntervalDayInt8,
          CHType.MapIntervalWeekInt8,
          CHType.MapIntervalMonthInt8,
          CHType.MapIntervalQuarterInt8,
          CHType.MapIntervalYearInt8
        )
      )
  case MapIntervalDateTimeInt8
      extends CHAbstractType(
        Seq(CHType.MapIntervalNanosecondInt8.fuzzingValues.head),
        Seq(
          CHType.MapIntervalNanosecondInt8,
          CHType.MapIntervalMicrosecondInt8,
          CHType.MapIntervalMillisecondInt8,
          CHType.MapIntervalSecondInt8,
          CHType.MapIntervalMinuteInt8,
          CHType.MapIntervalHourInt8
        )
      )
  case MapEnumInt8
      extends CHAbstractType(
        Seq(s"map(${Enum.fuzzingValues.head}, 1)"),
        Seq(CHType.MapEnumInt8, CHType.MapEnum8Int8, CHType.MapEnum16Int8)
      )
  case MapIPv4Int8
      extends CHAbstractType(Seq(s"map(${IPv4.fuzzingValues.head}, 1)::Map(IPv4, Int8)"), Seq(CHType.MapIPv4Int8))
  case MapIPv6Int8
      extends CHAbstractType(Seq(s"map(${IPv6.fuzzingValues.head}, 1)::Map(IPv6, Int8)"), Seq(CHType.MapIPv6Int8))
  case MapStringInt8
      extends CHAbstractType(
        Seq(s"map(${String.fuzzingValues.head}, 1)::Map(String, Int8)"),
        Seq(CHType.MapFixedStringInt8, CHType.MapStringInt8)
      )
  case MapUUIDInt8
      extends CHAbstractType(Seq(s"map(${UUID.fuzzingValues.head}, 1)::Map(UUID, Int8)"), Seq(CHType.MapUUIDInt8))

  // Tuple1
  case Tuple1Numbers
      extends CHAbstractType(
        Seq(s"tuple(${Numbers.fuzzingValues.head})::Tuple(UInt8)"),
        Seq(
          CHType.Tuple1Int8,
          CHType.Tuple1Int16,
          CHType.Tuple1Int32,
          CHType.Tuple1Int64,
          CHType.Tuple1Int128,
          CHType.Tuple1Int256,
          CHType.Tuple1UInt8,
          CHType.Tuple1UInt16,
          CHType.Tuple1UInt32,
          CHType.Tuple1UInt64,
          CHType.Tuple1UInt128,
          CHType.Tuple1UInt256,
          CHType.Tuple1Float32,
          CHType.Tuple1Float64,
          CHType.Tuple1Decimal32,
          CHType.Tuple1Decimal64,
          CHType.Tuple1Decimal128,
          CHType.Tuple1Decimal256
        )
      )
  case Tuple1Date
      extends CHAbstractType(
        Seq(s"tuple(${Date.fuzzingValues.head})::Tuple(Date)"),
        Seq(CHType.Tuple1Date, CHType.Tuple1Date32)
      )
  case Tuple1DateTime
      extends CHAbstractType(
        Seq(s"tuple(${DateTime.fuzzingValues.head})::Tuple(DateTime)"),
        Seq(CHType.Tuple1DateTime, CHType.Tuple1DateTime64)
      )
  case Tuple1IntervalDate
      extends CHAbstractType(
        Seq(CHType.Tuple1IntervalDay.fuzzingValues.head),
        Seq(
          CHType.Tuple1IntervalDay,
          CHType.Tuple1IntervalWeek,
          CHType.Tuple1IntervalMonth,
          CHType.Tuple1IntervalQuarter,
          CHType.Tuple1IntervalYear
        )
      )
  case Tuple1IntervalDateTime
      extends CHAbstractType(
        Seq(CHType.Tuple1IntervalNanosecond.fuzzingValues.head),
        Seq(
          CHType.Tuple1IntervalNanosecond,
          CHType.Tuple1IntervalMicrosecond,
          CHType.Tuple1IntervalMillisecond,
          CHType.Tuple1IntervalSecond,
          CHType.Tuple1IntervalMinute,
          CHType.Tuple1IntervalHour
        )
      )
  case Tuple1Point extends CHAbstractType(Seq(CHType.Tuple1Point.fuzzingValues.head), Seq(CHType.Tuple1Point))
  case Tuple1Ring extends CHAbstractType(Seq(CHType.Tuple1Ring.fuzzingValues.head), Seq(CHType.Tuple1Ring))
  case Tuple1Polygon extends CHAbstractType(Seq(CHType.Tuple1Polygon.fuzzingValues.head), Seq(CHType.Tuple1Polygon))
  case Tuple1MultiPolygon
      extends CHAbstractType(Seq(CHType.Tuple1MultiPolygon.fuzzingValues.head), Seq(CHType.Tuple1MultiPolygon))
  case Tuple1Enum
      extends CHAbstractType(
        Seq(s"tuple(${Enum.fuzzingValues.head})::Tuple(Enum('hello' = 1, 'world' = 2))"),
        Seq(CHType.Tuple1Enum, CHType.Tuple1Enum8, CHType.Tuple1Enum16)
      )
  case Tuple1IPv4 extends CHAbstractType(Seq(s"tuple(${IPv4.fuzzingValues.head})::Tuple(IPv4)"), Seq(CHType.Tuple1IPv4))
  case Tuple1IPv6 extends CHAbstractType(Seq(s"tuple(${IPv6.fuzzingValues.head})::Tuple(IPv6)"), Seq(CHType.Tuple1IPv6))
  case Tuple1Json extends CHAbstractType(Seq(s"tuple(${Json.fuzzingValues.head})::Tuple(JSON)"), Seq(CHType.Tuple1Json))
  // case Tuple1Nothing extends CHAbstractType(CHType.Tuple1Nothing.fuzzingValues, Seq(CHType.Tuple1Nothing))
  case Tuple1String
      extends CHAbstractType(
        Seq(s"tuple(${String.fuzzingValues.head})::Tuple(String)"),
        Seq(CHType.Tuple1FixedString, CHType.Tuple1String)
      )
  case Tuple1UUID extends CHAbstractType(Seq(s"tuple(${UUID.fuzzingValues.head})::Tuple(UUID)"), Seq(CHType.Tuple1UUID))

  // Special
  case ArrayMapStringInt8
      extends CHAbstractType(
        Seq(s"[${MapStringInt8.fuzzingValues.head}]::Array(Map(String, Int8))"),
        Seq(CHType.ArrayMapStringInt8)
      )
  case ArrayTuple1Numbers
      extends CHAbstractType(
        Seq(s"[${Tuple1Numbers.fuzzingValues.head}]::Array(Tuple(UInt8))"),
        Seq(CHType.ArrayTuple1UInt8)
      )
  case Tuple1ArrayNumbers
      extends CHAbstractType(
        Seq(s"tuple(${ArrayNumbers.fuzzingValues.head})::Tuple(Array(UInt8))"),
        Seq(CHType.Tuple1ArrayUInt8)
      )
  case Tuple1MapStringInt8
      extends CHAbstractType(
        Seq(s"tuple(${MapStringInt8.fuzzingValues.head})::Tuple(Map(String, Int8))"),
        Seq(CHType.Tuple1MapStringInt8)
      )
}
