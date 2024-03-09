package com.amendil.entities

import com.typesafe.scalalogging.StrictLogging

trait CHInputType { def name: String }
enum CHAggregatedType(val name: String) extends CHInputType:
  case Any extends CHAggregatedType("Any")

enum CHType(
    val name: String,
    val fuzzingValues: Seq[String],
    val aliases: Seq[String] = Nil
) extends CHInputType:

  // Numbers
  case Int8
      extends CHType(
        "Int8",
        Seq("-128::Int8", "127::Int8", "0::Int8", "1::Int8")
      )
  case Int16
      extends CHType(
        "Int16",
        Seq("-32768::Int16", "32767::Int16", "0::Int16", "1::Int16")
      )
  case Int32
      extends CHType(
        "Int32",
        Seq("-2147483648::Int32", "2147483647::Int32", "0::Int32", "1::Int32")
      )
  case Int64
      extends CHType(
        "Int64",
        Seq("-9223372036854775808::Int64", "9223372036854775807::Int64", "0::Int64", "1::Int64")
      )
  case Int128
      extends CHType(
        "Int128",
        Seq(
          "-170141183460469231731687303715884105728::Int128",
          "170141183460469231731687303715884105727::Int128",
          "0::Int128",
          "1::Int128"
        )
      )
  case Int256
      extends CHType(
        "Int256",
        Seq(
          "-57896044618658097711785492504343953926634992332820282019728792003956564819968::Int256",
          "57896044618658097711785492504343953926634992332820282019728792003956564819967::Int256",
          "0::Int256",
          "1::Int256"
        )
      )
  case UInt8
      extends CHType(
        "UInt8",
        Seq("0::UInt8", "1::UInt8", "255::UInt8")
      )
  case UInt16
      extends CHType(
        "UInt16",
        Seq("0::UInt16", "1::UInt16", "65535::UInt16")
      )
  case UInt32
      extends CHType(
        "UInt32",
        Seq("0::UInt32", "1::UInt32", "4294967295::UInt32")
      )
  case UInt64
      extends CHType(
        "UInt64",
        Seq("0::UInt64", "1::UInt64", "18446744073709551615::UInt64")
      )
  case UInt128
      extends CHType(
        "UInt128",
        Seq("0::UInt128", "1::UInt128", "340282366920938463463374607431768211455::UInt128")
      )
  case UInt256
      extends CHType(
        "UInt256",
        Seq(
          "0::UInt256",
          "1::UInt256",
          "115792089237316195423570985008687907853269984665640564039457584007913129639935::UInt256"
        )
      )

  case Float32
      extends CHType(
        "Float32",
        Seq("-inf::Float32", "nan::Float32", "0.5::Float32", "0::Float32")
      )
  case Float64
      extends CHType(
        "Float64",
        Seq("-inf::Float64", "nan::Float64", "0.5::Float64", "0::Float64")
      )
  case Decimal32
      extends CHType(
        "Decimal32",
        Seq(
          "-999999999::Decimal32(0)",
          "999999999::Decimal32(0)",
          "-0.999999999::Decimal32(9)",
          "0.999999999::Decimal32(9)"
        ),
        aliases = Seq("Decimal(9, 0)", "Decimal(9, 9)")
      )
  case Decimal64
      extends CHType(
        "Decimal64",
        Seq(
          "-999999999999999999::Decimal64(0)",
          "999999999999999999::Decimal64(0)",
          "-0.999999999999999999::Decimal64(18)",
          "0.999999999999999999::Decimal64(18)",
          "-999999999999999999::Decimal",
          "-9999999999::Decimal",
          "9999999999::Decimal",
          "999999999999999999::Decimal"
        ),
        aliases = Seq("Decimal(18, 0)", "Decimal(18, 18)", "Decimal(10, 0)")
      )
  case Decimal128
      extends CHType(
        "Decimal128",
        Seq(
          "-999999999999999999999999999999999999::Decimal128(0)",
          "999999999999999999999999999999999999::Decimal128(0)",
          "-0.99999999999999999999999999999999999999::Decimal128(38)",
          "0.99999999999999999999999999999999999999::Decimal128(38)"
        ),
        aliases = Seq("Decimal(38, 0)", "Decimal(38, 9)", "Decimal(38, 18)", "Decimal(38, 38)")
      )
  case Decimal256
      extends CHType(
        "Decimal256",
        Seq(
          "-999999999999999999999999999999999999999999999999999999999999999999999999::Decimal256(0)",
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
        ),
        aliases = Seq("Decimal(39, 38)", "Decimal(40, 0)", "Decimal(76, 0)", "Decimal(76, 38)", "Decimal(76, 76)")
      )

  // Date
  case Date
      extends CHType(
        "Date",
        Seq("'1970-01-01'::Date", "'2149-06-06'::Date")
      )
  case Date32
      extends CHType(
        "Date32",
        Seq("'1900-01-01'::Date32", "'2299-12-31'::Date32")
      )
  case DateTime
      extends CHType(
        "DateTime",
        Seq(
          "'1970-01-01 00:00:00'::DateTime('Asia/Istanbul')",
          "'2106-02-07 06:28:15'::DateTime('Asia/Istanbul')",
          "'1970-01-01 00:00:00'::DateTime",
          "'2106-02-07 06:28:15'::DateTime"
        ),
        aliases = Seq("DateTime('Asia/Istanbul')")
      )
  case DateTime64
      extends CHType(
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
        ),
        aliases = Seq(
          "DateTime64(0, 'Asia/Istanbul')",
          "DateTime64(9, 'Asia/Istanbul')",
          "DateTime64(8, 'Asia/Istanbul')",
          "DateTime64(0)",
          "DateTime64(1)",
          "DateTime64(8)",
          "DateTime64(9)"
        )
      )
  case IntervalNanosecond
      extends CHType(
        "IntervalNanosecond",
        Seq("INTERVAL 1 Nanosecond::IntervalNanosecond")
      )
  case IntervalMicrosecond
      extends CHType(
        "IntervalMicrosecond",
        Seq("INTERVAL 1 Microsecond::IntervalMicrosecond")
      )
  case IntervalMillisecond
      extends CHType(
        "IntervalMillisecond",
        Seq("INTERVAL 1 Millisecond::IntervalMillisecond")
      )
  case IntervalSecond
      extends CHType(
        "IntervalSecond",
        Seq("INTERVAL 1 Second::IntervalSecond")
      )
  case IntervalMinute
      extends CHType(
        "IntervalMinute",
        Seq("INTERVAL 1 Minute::IntervalMinute")
      )
  case IntervalHour
      extends CHType(
        "IntervalHour",
        Seq("INTERVAL 1 Hour::IntervalHour")
      )
  case IntervalDay
      extends CHType(
        "IntervalDay",
        Seq("INTERVAL 1 Day::IntervalDay")
      )
  case IntervalWeek
      extends CHType(
        "IntervalWeek",
        Seq("INTERVAL 1 Week::IntervalWeek")
      )
  case IntervalMonth
      extends CHType(
        "IntervalMonth",
        Seq("INTERVAL 1 Month::IntervalMonth")
      )
  case IntervalQuarter
      extends CHType(
        "IntervalQuarter",
        Seq("INTERVAL 1 Quarter::IntervalQuarter")
      )
  case IntervalYear
      extends CHType(
        "IntervalYear",
        Seq("INTERVAL 1 Year::IntervalYear")
      )

  // Geo
  case Point extends CHType("Point", Seq("(0, 0)::Point"))
  case Ring extends CHType("Ring", Seq("[(0, 0), (10, 0), (10, 10), (0, 10)]::Ring"))
  case Polygon
      extends CHType(
        "Polygon",
        Seq("[[(20, 20), (50, 20), (50, 50), (20, 50)], [(30, 30), (50, 50), (50, 30)]]::Polygon")
      )
  case MultiPolygon
      extends CHType(
        "MultiPolygon",
        Seq(
          "[[[(0,0),(10,0),(10,10),(0,10)]],[[(20,20),(50,20),(50,50),(20,50)],[(30,30),(50,50),(50,30)]]]::MultiPolygon"
        )
      )

  // Misc
  case Enum
      extends CHType(
        "Enum",
        Seq(
          "'hello'::Enum('hello' = 1, 'world' = 2)",
          "'hello'::Enum('hello', 'world')"
        )
      )
  case Enum8
      extends CHType(
        "Enum8",
        Seq(
          "'hello'::Enum8('hello' = -128, 'world' = 2)",
          "'hello'::Enum8('hello' = 127, 'world' = 2)",
          "'hello'::Enum8('hello', 'world')"
        ),
        aliases = Seq(
          "Enum8('hello' = -128, 'world' = 2)",
          "Enum8('world' = 2, 'hello' = 127)",
          "Enum8('hello' = 1, 'world' = 2)"
        )
      )
  case Enum16
      extends CHType(
        "Enum16",
        Seq(
          "'hello'::Enum16('hello' = -32768, 'world' = 2)",
          "'hello'::Enum16('hello' = 32767, 'world' = 2)",
          "'hello'::Enum16('hello', 'world')"
        ),
        aliases = Seq(
          "Enum16('hello' = -32768, 'world' = 2)",
          "Enum16('world' = 2, 'hello' = 32767)",
          "Enum16('hello' = 1, 'world' = 2)"
        )
      )
  case FixedString
      extends CHType(
        "FixedString",
        Seq("'azertyuiop'::FixedString(10)", "''::FixedString(1)"),
        aliases = Seq("FixedString(1)", "FixedString(10)", "FixedString(255)", "FixedString(65535)")
      )
  case IPv4 extends CHType("IPv4", Seq("'116.106.34.242'::IPv4"))
  case IPv6
      extends CHType(
        "IPv6",
        Seq(
          "'2001:44c8:129:2632:33:0:252:2'::IPv6",
          "'2a02:e980:1e::1'::IPv6",
          "'116.106.34.242'::IPv6"
        )
      )
  case Json
      extends CHType(
        "JSON",
        Seq("""'{"a": 1, "b": { "c": "foo", "d": [1, 2, 3] }, "c": null}'::JSON""")
      )
  case StringType
      extends CHType(
        "String",
        Seq("'foo'::String", "''::String")
      )
  case UUID
      extends CHType(
        "UUID",
        Seq("'00000000-0000-0000-0000-000000000000'::UUID", "'61f0c404-5cb3-11e7-907b-a6006ad3dba0'::UUID")
      )

  // LowCardinality
  case LowCardinalityInt8
      extends CHType(
        "LowCardinality(Int8)",
        CHType.lowCardinalityFuzzingValues(Int8.fuzzingValues)
      )
  case LowCardinalityInt16
      extends CHType(
        "LowCardinality(Int16)",
        CHType.lowCardinalityFuzzingValues(Int16.fuzzingValues)
      )
  case LowCardinalityInt32
      extends CHType(
        "LowCardinality(Int32)",
        CHType.lowCardinalityFuzzingValues(Int32.fuzzingValues)
      )
  case LowCardinalityInt64
      extends CHType(
        "LowCardinality(Int64)",
        CHType.lowCardinalityFuzzingValues(Int64.fuzzingValues)
      )
  case LowCardinalityInt128
      extends CHType(
        "LowCardinality(Int128)",
        CHType.lowCardinalityFuzzingValues(Int128.fuzzingValues)
      )
  case LowCardinalityInt256
      extends CHType(
        "LowCardinality(Int256)",
        CHType.lowCardinalityFuzzingValues(Int256.fuzzingValues)
      )
  case LowCardinalityUInt8
      extends CHType(
        "LowCardinality(UInt8)",
        CHType.lowCardinalityFuzzingValues(UInt8.fuzzingValues)
      )
  case LowCardinalityUInt16
      extends CHType(
        "LowCardinality(UInt16)",
        CHType.lowCardinalityFuzzingValues(UInt16.fuzzingValues)
      )
  case LowCardinalityUInt32
      extends CHType(
        "LowCardinality(UInt32)",
        CHType.lowCardinalityFuzzingValues(UInt32.fuzzingValues)
      )
  case LowCardinalityUInt64
      extends CHType(
        "LowCardinality(UInt64)",
        CHType.lowCardinalityFuzzingValues(UInt64.fuzzingValues)
      )
  case LowCardinalityUInt128
      extends CHType(
        "LowCardinality(UInt128)",
        CHType.lowCardinalityFuzzingValues(UInt128.fuzzingValues)
      )
  case LowCardinalityUInt256
      extends CHType(
        "LowCardinality(UInt256)",
        CHType.lowCardinalityFuzzingValues(UInt256.fuzzingValues)
      )

  case LowCardinalityFloat32
      extends CHType(
        "LowCardinality(Float32)",
        CHType.lowCardinalityFuzzingValues(Float32.fuzzingValues)
      )
  case LowCardinalityFloat64
      extends CHType(
        "LowCardinality(Float64)",
        CHType.lowCardinalityFuzzingValues(Float64.fuzzingValues)
      )

  case LowCardinalityDate
      extends CHType(
        "LowCardinality(Date)",
        CHType.lowCardinalityFuzzingValues(Date.fuzzingValues)
      )
  case LowCardinalityDate32
      extends CHType(
        "LowCardinality(Date32)",
        CHType.lowCardinalityFuzzingValues(Date32.fuzzingValues)
      )
  case LowCardinalityDateTime
      extends CHType(
        "LowCardinality(DateTime)",
        CHType.lowCardinalityFuzzingValues(DateTime.fuzzingValues),
        aliases = Seq("LowCardinality(DateTime('Asia/Istanbul'))")
      )
  case LowCardinalityFixedString
      extends CHType(
        "LowCardinality(FixedString)",
        CHType.lowCardinalityFuzzingValues(FixedString.fuzzingValues),
        aliases = Seq(
          "LowCardinality(FixedString(1))",
          "LowCardinality(FixedString(10))",
          "LowCardinality(FixedString(255))",
          "LowCardinality(FixedString(65535))"
        )
      )
  case LowCardinalityString
      extends CHType(
        "LowCardinality(String)",
        CHType.lowCardinalityFuzzingValues(StringType.fuzzingValues)
      )

  // LowCardinality(Nullable)
  case LowCardinalityNullableInt8
      extends CHType(
        "LowCardinality(Nullable(Int8))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(Int8.fuzzingValues))
      )
  case LowCardinalityNullableInt16
      extends CHType(
        "LowCardinality(Nullable(Int16))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(Int16.fuzzingValues))
      )
  case LowCardinalityNullableInt32
      extends CHType(
        "LowCardinality(Nullable(Int32))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(Int32.fuzzingValues))
      )
  case LowCardinalityNullableInt64
      extends CHType(
        "LowCardinality(Nullable(Int64))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(Int64.fuzzingValues))
      )
  case LowCardinalityNullableInt128
      extends CHType(
        "LowCardinality(Nullable(Int128))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(Int128.fuzzingValues))
      )
  case LowCardinalityNullableInt256
      extends CHType(
        "LowCardinality(Nullable(Int256))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(Int256.fuzzingValues))
      )
  case LowCardinalityNullableUInt8
      extends CHType(
        "LowCardinality(Nullable(UInt8))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(UInt8.fuzzingValues))
      )
  case LowCardinalityNullableUInt16
      extends CHType(
        "LowCardinality(Nullable(UInt16))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(UInt16.fuzzingValues))
      )
  case LowCardinalityNullableUInt32
      extends CHType(
        "LowCardinality(Nullable(UInt32))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(UInt32.fuzzingValues))
      )
  case LowCardinalityNullableUInt64
      extends CHType(
        "LowCardinality(Nullable(UInt64))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(UInt64.fuzzingValues))
      )
  case LowCardinalityNullableUInt128
      extends CHType(
        "LowCardinality(Nullable(UInt128))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(UInt128.fuzzingValues))
      )
  case LowCardinalityNullableUInt256
      extends CHType(
        "LowCardinality(Nullable(UInt256))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(UInt256.fuzzingValues))
      )

  case LowCardinalityNullableFloat32
      extends CHType(
        "LowCardinality(Nullable(Float32))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(Float32.fuzzingValues))
      )
  case LowCardinalityNullableFloat64
      extends CHType(
        "LowCardinality(Nullable(Float64))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(Float64.fuzzingValues))
      )

  case LowCardinalityNullableDate
      extends CHType(
        "LowCardinality(Nullable(Date))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(Date.fuzzingValues))
      )
  case LowCardinalityNullableDate32
      extends CHType(
        "LowCardinality(Nullable(Date32))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(Date32.fuzzingValues))
      )
  case LowCardinalityNullableDateTime
      extends CHType(
        "LowCardinality(Nullable(DateTime))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(DateTime.fuzzingValues))
      )
  case LowCardinalityNullableFixedString
      extends CHType(
        "LowCardinality(Nullable(FixedString))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(FixedString.fuzzingValues))
      )
  case LowCardinalityNullableString
      extends CHType(
        "LowCardinality(Nullable(String))",
        CHType.lowCardinalityFuzzingValues(CHType.nullableFuzzingValues(StringType.fuzzingValues))
      )

  // Nullable
  case NullableInt8
      extends CHType(
        "Nullable(Int8)",
        CHType.nullableFuzzingValues(Int8.fuzzingValues)
      )
  case NullableInt16
      extends CHType(
        "Nullable(Int16)",
        CHType.nullableFuzzingValues(Int16.fuzzingValues)
      )
  case NullableInt32
      extends CHType(
        "Nullable(Int32)",
        CHType.nullableFuzzingValues(Int32.fuzzingValues)
      )
  case NullableInt64
      extends CHType(
        "Nullable(Int64)",
        CHType.nullableFuzzingValues(Int64.fuzzingValues)
      )
  case NullableInt128
      extends CHType(
        "Nullable(Int128)",
        CHType.nullableFuzzingValues(Int128.fuzzingValues)
      )
  case NullableInt256
      extends CHType(
        "Nullable(Int256)",
        CHType.nullableFuzzingValues(Int256.fuzzingValues)
      )
  case NullableUInt8
      extends CHType(
        "Nullable(UInt8)",
        CHType.nullableFuzzingValues(UInt8.fuzzingValues)
      )
  case NullableUInt16
      extends CHType(
        "Nullable(UInt16)",
        CHType.nullableFuzzingValues(UInt16.fuzzingValues)
      )
  case NullableUInt32
      extends CHType(
        "Nullable(UInt32)",
        CHType.nullableFuzzingValues(UInt32.fuzzingValues)
      )
  case NullableUInt64
      extends CHType(
        "Nullable(UInt64)",
        CHType.nullableFuzzingValues(UInt64.fuzzingValues)
      )
  case NullableUInt128
      extends CHType(
        "Nullable(UInt128)",
        CHType.nullableFuzzingValues(UInt128.fuzzingValues)
      )
  case NullableUInt256
      extends CHType(
        "Nullable(UInt256)",
        CHType.nullableFuzzingValues(UInt256.fuzzingValues)
      )

  case NullableFloat32
      extends CHType(
        "Nullable(Float32)",
        CHType.nullableFuzzingValues(Float32.fuzzingValues)
      )
  case NullableFloat64
      extends CHType(
        "Nullable(Float64)",
        CHType.nullableFuzzingValues(Float64.fuzzingValues)
      )
  case NullableDecimal32
      extends CHType(
        "Nullable(Decimal32)",
        CHType.nullableFuzzingValues(Decimal32.fuzzingValues),
        aliases = Seq("Nullable(Decimal(9, 0))", "Nullable(Decimal(9, 9))")
      )
  case NullableDecimal64
      extends CHType(
        "Nullable(Decimal64)",
        CHType.nullableFuzzingValues(Decimal64.fuzzingValues),
        aliases = Seq("Nullable(Decimal(18, 0))", "Nullable(Decimal(18, 18))", "Nullable(Decimal(10, 0))")
      )
  case NullableDecimal128
      extends CHType(
        "Nullable(Decimal128)",
        CHType.nullableFuzzingValues(Decimal128.fuzzingValues),
        aliases = Seq(
          "Nullable(Decimal(38, 0))",
          "Nullable(Decimal(38, 9))",
          "Nullable(Decimal(38, 18))",
          "Nullable(Decimal(38, 38))"
        )
      )
  case NullableDecimal256
      extends CHType(
        "Nullable(Decimal256)",
        CHType.nullableFuzzingValues(Decimal256.fuzzingValues),
        aliases = Seq(
          "Nullable(Decimal(39, 38))",
          "Nullable(Decimal(40, 0))",
          "Nullable(Decimal(76, 0))",
          "Nullable(Decimal(76, 38))",
          "Nullable(Decimal(76, 76))"
        )
      )

  // Date
  case NullableDate
      extends CHType(
        "Nullable(Date)",
        CHType.nullableFuzzingValues(Date.fuzzingValues)
      )
  case NullableDate32
      extends CHType(
        "Nullable(Date32)",
        CHType.nullableFuzzingValues(Date32.fuzzingValues)
      )
  case NullableDateTime
      extends CHType(
        "Nullable(DateTime)",
        CHType.nullableFuzzingValues(DateTime.fuzzingValues),
        aliases = Seq("Nullable(DateTime('Asia/Istanbul'))")
      )
  case NullableDateTime64
      extends CHType(
        "Nullable(DateTime64)",
        CHType.nullableFuzzingValues(DateTime64.fuzzingValues),
        aliases = Seq(
          "Nullable(DateTime64(0, 'Asia/Istanbul'))",
          "Nullable(DateTime64(9, 'Asia/Istanbul'))",
          "Nullable(DateTime64(8, 'Asia/Istanbul'))",
          "Nullable(DateTime64(0))",
          "Nullable(DateTime64(1))",
          "Nullable(DateTime64(8))",
          "Nullable(DateTime64(9))"
        )
      )
  case NullableIntervalNanosecond
      extends CHType(
        "Nullable(IntervalNanosecond)",
        CHType.nullableFuzzingValues(IntervalNanosecond.fuzzingValues)
      )
  case NullableIntervalMicrosecond
      extends CHType(
        "Nullable(IntervalMicrosecond)",
        CHType.nullableFuzzingValues(IntervalMicrosecond.fuzzingValues)
      )
  case NullableIntervalMillisecond
      extends CHType(
        "Nullable(IntervalMillisecond)",
        CHType.nullableFuzzingValues(IntervalMillisecond.fuzzingValues)
      )
  case NullableIntervalSecond
      extends CHType(
        "Nullable(IntervalSecond)",
        CHType.nullableFuzzingValues(IntervalSecond.fuzzingValues)
      )
  case NullableIntervalMinute
      extends CHType(
        "Nullable(IntervalMinute)",
        CHType.nullableFuzzingValues(IntervalMinute.fuzzingValues)
      )
  case NullableIntervalHour
      extends CHType(
        "Nullable(IntervalHour)",
        CHType.nullableFuzzingValues(IntervalHour.fuzzingValues)
      )
  case NullableIntervalDay
      extends CHType(
        "Nullable(IntervalDay)",
        CHType.nullableFuzzingValues(IntervalDay.fuzzingValues)
      )
  case NullableIntervalWeek
      extends CHType(
        "Nullable(IntervalWeek)",
        CHType.nullableFuzzingValues(IntervalWeek.fuzzingValues)
      )
  case NullableIntervalMonth
      extends CHType(
        "Nullable(IntervalMonth)",
        CHType.nullableFuzzingValues(IntervalMonth.fuzzingValues)
      )
  case NullableIntervalQuarter
      extends CHType(
        "Nullable(IntervalQuarter)",
        CHType.nullableFuzzingValues(IntervalQuarter.fuzzingValues)
      )
  case NullableIntervalYear
      extends CHType(
        "Nullable(IntervalYear)",
        CHType.nullableFuzzingValues(IntervalYear.fuzzingValues)
      )
  case NullableEnum
      extends CHType(
        "Nullable(Enum)",
        CHType.nullableFuzzingValues(Enum.fuzzingValues)
      )
  case NullableEnum8
      extends CHType(
        "Nullable(Enum8)",
        CHType.nullableFuzzingValues(Enum8.fuzzingValues),
        aliases = Seq(
          "Nullable(Enum8('hello' = -128, 'world' = 2))",
          "Nullable(Enum8('world' = 2, 'hello' = 127))",
          "Nullable(Enum8('hello' = 1, 'world' = 2))"
        )
      )
  case NullableEnum16
      extends CHType(
        "Nullable(Enum16)",
        CHType.nullableFuzzingValues(Enum16.fuzzingValues),
        aliases = Seq(
          "Nullable(Enum16('hello' = -32768, 'world' = 2))",
          "Nullable(Enum16('world' = 2, 'hello' = 32767))",
          "Nullable(Enum16('hello' = 1, 'world' = 2))"
        )
      )
  case NullableFixedString
      extends CHType(
        "Nullable(FixedString)",
        CHType.nullableFuzzingValues(FixedString.fuzzingValues),
        aliases = Seq(
          "Nullable(FixedString(1))",
          "Nullable(FixedString(10))",
          "Nullable(FixedString(255))",
          "Nullable(FixedString(65535))"
        )
      )
  case NullableIPv4
      extends CHType(
        "Nullable(IPv4)",
        CHType.nullableFuzzingValues(IPv4.fuzzingValues)
      )
  case NullableIPv6
      extends CHType(
        "Nullable(IPv6)",
        CHType.nullableFuzzingValues(IPv6.fuzzingValues)
      )
  // case NullableNothing extends CHType("Nullable(Nothing)", Seq("null::Nullable(Nothing)"))
  case NullableString
      extends CHType(
        "Nullable(String)",
        CHType.nullableFuzzingValues(StringType.fuzzingValues)
      )
  case NullableUUID
      extends CHType(
        "Nullable(UUID)",
        CHType.nullableFuzzingValues(UUID.fuzzingValues)
      )

  // Array
  case ArrayInt8
      extends CHType(
        "Array(Int8)",
        Seq(
          s"[${Int8.fuzzingValues.mkString(", ")}]::Array(Int8)"
        )
      )
  case ArrayInt16
      extends CHType(
        "Array(Int16)",
        Seq(
          s"[${Int16.fuzzingValues.mkString(", ")}]::Array(Int16)"
        )
      )
  case ArrayInt32
      extends CHType(
        "Array(Int32)",
        Seq(
          s"[${Int32.fuzzingValues.mkString(", ")}]::Array(Int32)"
        )
      )
  case ArrayInt64
      extends CHType(
        "Array(Int64)",
        Seq(
          s"[${Int64.fuzzingValues.mkString(", ")}]::Array(Int64)"
        )
      )
  case ArrayInt128
      extends CHType(
        "Array(Int128)",
        Seq(
          s"[${Int128.fuzzingValues.mkString(", ")}]::Array(Int128)"
        )
      )
  case ArrayInt256
      extends CHType(
        "Array(Int256)",
        Seq(
          s"[${Int256.fuzzingValues.mkString(", ")}]::Array(Int256)"
        )
      )
  case ArrayUInt8
      extends CHType(
        "Array(UInt8)",
        Seq(
          s"[${UInt8.fuzzingValues.mkString(", ")}]::Array(UInt8)"
        )
      )
  case ArrayUInt16
      extends CHType(
        "Array(UInt16)",
        Seq(
          s"[${UInt16.fuzzingValues.mkString(", ")}]::Array(UInt16)"
        )
      )
  case ArrayUInt32
      extends CHType(
        "Array(UInt32)",
        Seq(
          s"[${UInt32.fuzzingValues.mkString(", ")}]::Array(UInt32)"
        )
      )
  case ArrayUInt64
      extends CHType(
        "Array(UInt64)",
        Seq(
          s"[${UInt64.fuzzingValues.mkString(", ")}]::Array(UInt64)"
        )
      )
  case ArrayUInt128
      extends CHType(
        "Array(UInt128)",
        Seq(
          s"[${UInt128.fuzzingValues.mkString(", ")}]::Array(UInt128)"
        )
      )
  case ArrayUInt256
      extends CHType(
        "Array(UInt256)",
        Seq(
          s"[${UInt256.fuzzingValues.mkString(", ")}]::Array(UInt256)"
        )
      )
  case ArrayFloat32
      extends CHType(
        "Array(Float32)",
        Seq(
          s"[${Float32.fuzzingValues.mkString(", ")}]::Array(Float32)"
        )
      )
  case ArrayFloat64
      extends CHType(
        "Array(Float64)",
        Seq(
          s"[${Float64.fuzzingValues.mkString(", ")}]::Array(Float64)"
        )
      )
  case ArrayDecimal32
      extends CHType(
        "Array(Decimal32)",
        Seq(
          s"[-999999999::Decimal32(0)]::Array(Decimal32(0))"
        ),
        aliases = Seq("Array(Decimal(9, 0))", "Array(Decimal(9, 9))")
      )
  case ArrayDecimal64
      extends CHType(
        "Array(Decimal64)",
        Seq(
          s"[-999999999999999999::Decimal64(0)]::Array(Decimal64(0))"
        ),
        aliases = Seq("Array(Decimal(18, 0))", "Array(Decimal(18, 18))", "Array(Decimal(10, 0))")
      )
  case ArrayDecimal128
      extends CHType(
        "Array(Decimal128)",
        Seq(
          s"[-999999999999999999999999999999999999::Decimal128(0)]::Array(Decimal128(0))"
        ),
        aliases =
          Seq("Array(Decimal(38, 0))", "Array(Decimal(38, 38))", "Array(Decimal(38, 18))", "Array(Decimal(38, 9))")
      )
  case ArrayDecimal256
      extends CHType(
        "Array(Decimal256)",
        Seq(
          s"[-999999999999999999999999999999999999999999999999999999999999999999999999::Decimal256(0)]::Array(Decimal256(0))"
        ),
        aliases = Seq(
          "Array(Decimal(76, 0))",
          "Array(Decimal(76, 76))",
          "Array(Decimal(39, 38))",
          "Array(Decimal(40, 0))",
          "Array(Decimal(76, 38))"
        )
      )
  case ArrayDate
      extends CHType(
        "Array(Date)",
        Seq(
          s"[${Date.fuzzingValues.mkString(", ")}]::Array(Date)"
        )
      )
  case ArrayDate32
      extends CHType(
        "Array(Date32)",
        Seq(
          s"[${Date32.fuzzingValues.mkString(", ")}]::Array(Date32)"
        )
      )
  case ArrayDateTime
      extends CHType(
        "Array(DateTime)",
        Seq(
          s"['1970-01-01 00:00:00'::DateTime('Asia/Istanbul')]::Array(DateTime('Asia/Istanbul'))"
        ),
        aliases = Seq("Array(DateTime('Asia/Istanbul'))")
      )
  case ArrayDateTime64
      extends CHType(
        "Array(DateTime64)",
        Seq(
          s"['1900-01-01 00:00:00'::DateTime64(0, 'Asia/Istanbul')]::Array(DateTime64(0, 'Asia/Istanbul'))"
        ),
        aliases = Seq(
          "Array(DateTime64(0, 'Asia/Istanbul'))",
          "Array(DateTime64(9, 'Asia/Istanbul'))",
          "Array(DateTime64(8, 'Asia/Istanbul'))",
          "Array(DateTime64(0))",
          "Array(DateTime64(9))",
          "Array(DateTime64(8))"
        )
      )
  case ArrayIntervalNanosecond
      extends CHType("Array(IntervalNanosecond)", Seq("[INTERVAL 1 Nanosecond]::Array(IntervalNanosecond)"))
  case ArrayIntervalMicrosecond
      extends CHType(
        "Array(IntervalMicrosecond)",
        Seq("[INTERVAL 1 Microsecond]::Array(IntervalMicrosecond)")
      )
  case ArrayIntervalMillisecond
      extends CHType(
        "Array(IntervalMillisecond)",
        Seq("[INTERVAL 1 Millisecond]::Array(IntervalMillisecond)")
      )
  case ArrayIntervalSecond extends CHType("Array(IntervalSecond)", Seq("[INTERVAL 1 Second]::Array(IntervalSecond)"))
  case ArrayIntervalMinute extends CHType("Array(IntervalMinute)", Seq("[INTERVAL 1 Minute]::Array(IntervalMinute)"))
  case ArrayIntervalHour extends CHType("Array(IntervalHour)", Seq("[INTERVAL 1 Hour]::Array(IntervalHour)"))
  case ArrayIntervalDay extends CHType("Array(IntervalDay)", Seq("[INTERVAL 1 Day]::Array(IntervalDay)"))
  case ArrayIntervalWeek extends CHType("Array(IntervalWeek)", Seq("[INTERVAL 1 Week]::Array(IntervalWeek)"))
  case ArrayIntervalMonth extends CHType("Array(IntervalMonth)", Seq("[INTERVAL 1 Month]::Array(IntervalMonth)"))
  case ArrayIntervalQuarter
      extends CHType("Array(IntervalQuarter)", Seq("[INTERVAL 1 Quarter]::Array(IntervalQuarter)"))
  case ArrayIntervalYear extends CHType("Array(IntervalYear)", Seq("[INTERVAL 1 Year]::Array(IntervalYear)"))
  case ArrayPoint extends CHType("Array(Point)", Seq("[(0, 0)]::Array(Point)"))
  case ArrayRing extends CHType("Array(Ring)", Seq("[[(0, 0), (10, 0), (10, 10), (0, 10)]]::Array(Ring)"))
  case ArrayPolygon
      extends CHType(
        "Array(Polygon)",
        Seq("[[[(20, 20), (50, 20), (50, 50), (20, 50)], [(30, 30), (50, 50), (50, 30)]]]::Array(Polygon)")
      )
  case ArrayMultiPolygon
      extends CHType(
        "Array(MultiPolygon)",
        Seq(
          "[[[[(0,0),(10,0),(10,10),(0,10)]],[[(20,20),(50,20),(50,50),(20,50)],[(30,30),(50,50),(50,30)]]]]::Array(MultiPolygon)"
        )
      )
  case ArrayEnum
      extends CHType(
        "Array(Enum)",
        Seq(
          s"['hello'::Enum('hello' = 1, 'world' = 2)]::Array(Enum('hello' = 1, 'world' = 2))"
        )
      )
  case ArrayEnum8
      extends CHType(
        "Array(Enum8)",
        Seq(
          s"['hello'::Enum8('hello' = -128, 'world' = 2)]::Array(Enum8('hello' = -128, 'world' = 2))"
        ),
        aliases = Seq(
          "Array(Enum8('hello' = -128, 'world' = 2))",
          "Array(Enum8('world' = 2, 'hello' = 127))",
          "Array(Enum8('hello' = 1, 'world' = 2))"
        )
      )
  case ArrayEnum16
      extends CHType(
        "Array(Enum16)",
        Seq(
          s"['hello'::Enum16('hello' = -32768, 'world' = 2)]::Array(Enum16('hello' = -32768, 'world' = 2))"
        ),
        aliases = Seq(
          "Array(Enum16('hello' = -32768, 'world' = 2))",
          "Array(Enum16('world' = 2, 'hello' = 32767))",
          "Array(Enum16('hello' = 1, 'world' = 2))"
        )
      )
  case ArrayFixedString
      extends CHType(
        "Array(FixedString)",
        Seq(
          s"[${FixedString.fuzzingValues.mkString(", ")}]::Array(FixedString(10))"
        ),
        aliases = Seq("Array(FixedString(1))", "Array(FixedString(10))")
      )
  case ArrayIPv4 extends CHType("Array(IPv4)", Seq(s"[${IPv4.fuzzingValues.mkString(", ")}]::Array(IPv4)"))
  case ArrayIPv6 extends CHType("Array(IPv6)", Seq(s"[${IPv6.fuzzingValues.mkString(", ")}]::Array(IPv6)"))
  case ArrayJson
      extends CHType(
        "Array(JSON)",
        Seq(
          s"[${Json.fuzzingValues.mkString(", ")}]::Array(JSON)"
        )
      )
  case ArrayString
      extends CHType(
        "Array(String)",
        Seq(
          s"[${StringType.fuzzingValues.mkString(", ")}]::Array(String)"
        )
      )
  case ArrayUUID
      extends CHType(
        "Array(UUID)",
        Seq(s"[${UUID.fuzzingValues.mkString(", ")}]::Array(UUID)")
      )

  // Map
  case MapInt8Int8
      extends CHType(
        "Map(Int8, Int8)",
        Int8.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(Int8, Int8)" }
      )
  case MapInt16Int8
      extends CHType(
        "Map(Int16, Int8)",
        Int16.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(Int16, Int8)" }
      )
  case MapInt32Int8
      extends CHType(
        "Map(Int32, Int8)",
        Int32.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(Int32, Int8)" }
      )
  case MapInt64Int8
      extends CHType(
        "Map(Int64, Int8)",
        Int64.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(Int64, Int8)" }
      )
  case MapInt128Int8
      extends CHType(
        "Map(Int128, Int8)",
        Int128.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(Int128, Int8)" }
      )
  case MapInt256Int8
      extends CHType(
        "Map(Int256, Int8)",
        Int256.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(Int256, Int8)" }
      )
  case MapUInt8Int8
      extends CHType(
        "Map(UInt8, Int8)",
        UInt8.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(UInt8, Int8)" }
      )
  case MapUInt16Int8
      extends CHType(
        "Map(UInt16, Int8)",
        UInt16.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(UInt16, Int8)" }
      )
  case MapUInt32Int8
      extends CHType(
        "Map(UInt32, Int8)",
        UInt32.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(UInt32, Int8)" }
      )
  case MapUInt64Int8
      extends CHType(
        "Map(UInt64, Int8)",
        UInt64.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(UInt64, Int8)" }
      )
  case MapUInt128Int8
      extends CHType(
        "Map(UInt128, Int8)",
        UInt128.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(UInt128, Int8)" }
      )
  case MapUInt256Int8
      extends CHType(
        "Map(UInt256, Int8)",
        UInt256.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(UInt256, Int8)" }
      )
  case MapDateInt8
      extends CHType(
        "Map(Date, Int8)",
        Date.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(Date, Int8)" }
      )
  case MapDate32Int8
      extends CHType(
        "Map(Date32, Int8)",
        Date32.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(Date32, Int8)" }
      )
  case MapDateTimeInt8
      extends CHType(
        "Map(DateTime, Int8)",
        DateTime.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(DateTime, Int8)" }
      )
  case MapIntervalNanosecondInt8
      extends CHType(
        "Map(IntervalNanosecond, Int8)",
        DateTime.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(DateTime, Int8)" }
      )
  case MapIntervalMicrosecondInt8
      extends CHType(
        "Map(IntervalMicrosecond, Int8)",
        DateTime.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(DateTime, Int8)" }
      )
  case MapIntervalMillisecondInt8
      extends CHType(
        "Map(IntervalMillisecond, Int8)",
        IntervalMillisecond.fuzzingValues.map { fuzzingValue =>
          s"map($fuzzingValue, 1)::Map(IntervalMillisecond, Int8)"
        }
      )
  case MapIntervalSecondInt8
      extends CHType(
        "Map(IntervalSecond, Int8)",
        IntervalSecond.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IntervalSecond, Int8)" }
      )
  case MapIntervalMinuteInt8
      extends CHType(
        "Map(IntervalMinute, Int8)",
        IntervalMinute.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IntervalMinute, Int8)" }
      )
  case MapIntervalHourInt8
      extends CHType(
        "Map(IntervalHour, Int8)",
        IntervalHour.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IntervalHour, Int8)" }
      )
  case MapIntervalDayInt8
      extends CHType(
        "Map(IntervalDay, Int8)",
        IntervalDay.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IntervalDay, Int8)" }
      )
  case MapIntervalWeekInt8
      extends CHType(
        "Map(IntervalWeek, Int8)",
        IntervalWeek.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IntervalWeek, Int8)" }
      )
  case MapIntervalMonthInt8
      extends CHType(
        "Map(IntervalMonth, Int8)",
        IntervalMonth.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IntervalMonth, Int8)" }
      )
  case MapIntervalQuarterInt8
      extends CHType(
        "Map(IntervalQuarter, Int8)",
        IntervalQuarter.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IntervalQuarter, Int8)" }
      )
  case MapIntervalYearInt8
      extends CHType(
        "Map(IntervalYear, Int8)",
        IntervalYear.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IntervalYear, Int8)" }
      )
  case MapEnumInt8
      extends CHType(
        "Map(Enum, Int8)",
        Seq(
          s"map('hello'::Enum8('hello' = -128, 'world' = 2), 1)::Map(Enum8('hello' = -128, 'world' = 2), Int8)"
        )
      )
  case MapEnum8Int8
      extends CHType(
        "Map(Enum8, Int8)",
        Seq(
          s"map('hello'::Enum8('hello' = -128, 'world' = 2), 1)::Map(Enum8('hello' = -128, 'world' = 2), Int8)"
        )
      )
  case MapEnum16Int8
      extends CHType(
        "Map(Enum16, Int8)",
        Seq(
          s"map('hello'::Enum16('hello' = -32768, 'world' = 2), 1)::Map(Enum16('hello' = -32768, 'world' = 2), Int8)"
        )
      )
  case MapFixedStringInt8
      extends CHType(
        "Map(FixedString, Int8)",
        FixedString.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(FixedString(10), Int8)" },
        aliases = Seq("Map(FixedString(10), Int8)")
      )
  case MapIPv4Int8
      extends CHType(
        "Map(IPv4, Int8)",
        IPv4.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IPv4, Int8)" }
      )
  case MapIPv6Int8
      extends CHType(
        "Map(IPv6, Int8)",
        IPv6.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IPv6, Int8)" }
      )
  case MapStringInt8
      extends CHType(
        "Map(String, Int8)",
        StringType.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(String, Int8)" }
      )
  case MapUUIDInt8
      extends CHType(
        "Map(UUID, Int8)",
        UUID.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(UUID, Int8)" }
      )

  // Tuple1
  case Tuple1Int8
      extends CHType(
        "Tuple(Int8)",
        Seq(s"tuple(${Int8.fuzzingValues.head})::Tuple(Int8)")
      )
  case Tuple1Int16
      extends CHType(
        "Tuple(Int16)",
        Seq(s"tuple(${Int16.fuzzingValues.head})::Tuple(Int16)")
      )
  case Tuple1Int32
      extends CHType(
        "Tuple(Int32)",
        Seq(s"tuple(${Int32.fuzzingValues.head})::Tuple(Int32)")
      )
  case Tuple1Int64
      extends CHType(
        "Tuple(Int64)",
        Seq(s"tuple(${Int64.fuzzingValues.head})::Tuple(Int64)")
      )
  case Tuple1Int128
      extends CHType(
        "Tuple(Int128)",
        Seq(s"tuple(${Int128.fuzzingValues.head})::Tuple(Int128)")
      )
  case Tuple1Int256
      extends CHType(
        "Tuple(Int256)",
        Seq(s"tuple(${Int256.fuzzingValues.head})::Tuple(Int256)")
      )
  case Tuple1UInt8
      extends CHType(
        "Tuple(UInt8)",
        Seq(s"tuple(${UInt8.fuzzingValues.head})::Tuple(UInt8)")
      )
  case Tuple1UInt16
      extends CHType(
        "Tuple(UInt16)",
        Seq(s"tuple(${UInt16.fuzzingValues.head})::Tuple(UInt16)")
      )
  case Tuple1UInt32
      extends CHType(
        "Tuple(UInt32)",
        Seq(s"tuple(${UInt32.fuzzingValues.head})::Tuple(UInt32)")
      )
  case Tuple1UInt64
      extends CHType(
        "Tuple(UInt64)",
        Seq(s"tuple(${UInt64.fuzzingValues.head})::Tuple(UInt64)")
      )
  case Tuple1UInt128
      extends CHType(
        "Tuple(UInt128)",
        Seq(s"tuple(${UInt128.fuzzingValues.head})::Tuple(UInt128)")
      )
  case Tuple1UInt256
      extends CHType(
        "Tuple(UInt256)",
        Seq(s"tuple(${UInt256.fuzzingValues.head})::Tuple(UInt256)")
      )
  case Tuple1Float32
      extends CHType(
        "Tuple(Float32)",
        Seq(s"tuple(${Float32.fuzzingValues.head})::Tuple(Float32)")
      )
  case Tuple1Float64
      extends CHType(
        "Tuple(Float64)",
        Seq(s"tuple(${Float64.fuzzingValues.head})::Tuple(Float64)")
      )
  case Tuple1Decimal32
      extends CHType(
        "Tuple(Decimal32)",
        Seq(s"tuple(${Decimal32.fuzzingValues.head})::Tuple(Decimal32(0))"),
        aliases = Seq("Tuple(Decimal(9, 0), UInt64)", "Tuple(Decimal(9, 9), UInt64)")
      )
  case Tuple1Decimal64
      extends CHType(
        "Tuple(Decimal64)",
        Seq(s"tuple(${Decimal64.fuzzingValues.head})::Tuple(Decimal64(0))"),
        aliases =
          Seq("Tuple(Decimal(18, 0), UInt64)", "Tuple(Decimal(18, 18), UInt64)", "Tuple(Decimal(10, 0), UInt64)")
      )
  case Tuple1Decimal128
      extends CHType(
        "Tuple(Decimal128)",
        Seq(s"tuple(${Decimal128.fuzzingValues.head})::Tuple(Decimal128(0))"),
        aliases = Seq(
          "Tuple(Decimal(38, 0), UInt64)",
          "Tuple(Decimal(38, 9), UInt64)",
          "Tuple(Decimal(38, 18), UInt64)",
          "Tuple(Decimal(38, 38), UInt64)"
        )
      )
  case Tuple1Decimal256
      extends CHType(
        "Tuple(Decimal256)",
        Seq(s"tuple(${Decimal256.fuzzingValues.head})::Tuple(Decimal256(0))"),
        aliases = Seq(
          "Tuple(Decimal(39, 38), UInt64)",
          "Tuple(Decimal(40, 0), UInt64)",
          "Tuple(Decimal(76, 0), UInt64)",
          "Tuple(Decimal(76, 38), UInt64)",
          "Tuple(Decimal(76, 76), UInt64)"
        )
      )
  case Tuple1Date
      extends CHType(
        "Tuple(Date)",
        Seq(s"tuple(${Date.fuzzingValues.head})::Tuple(Date)")
      )
  case Tuple1Date32
      extends CHType(
        "Tuple(Date32)",
        Seq(s"tuple(${Date32.fuzzingValues.head})::Tuple(Date32)")
      )
  case Tuple1DateTime
      extends CHType(
        "Tuple(DateTime)",
        Seq(s"tuple(${DateTime.fuzzingValues.head})::Tuple(DateTime)")
      )
  case Tuple1DateTime64
      extends CHType(
        "Tuple(DateTime64)",
        Seq(s"tuple(${DateTime64.fuzzingValues.head})::Tuple(DateTime64)")
      )
  case Tuple1IntervalNanosecond
      extends CHType("Tuple(IntervalNanosecond)", Seq("tuple(INTERVAL 1 Nanosecond)::Tuple(IntervalNanosecond)"))
  case Tuple1IntervalMicrosecond
      extends CHType("Tuple(IntervalMicrosecond)", Seq("tuple(INTERVAL 1 Microsecond)::Tuple(IntervalMicrosecond)"))
  case Tuple1IntervalMillisecond
      extends CHType("Tuple(IntervalMillisecond)", Seq("tuple(INTERVAL 1 Millisecond)::Tuple(IntervalMillisecond)"))
  case Tuple1IntervalSecond
      extends CHType("Tuple(IntervalSecond)", Seq("tuple(INTERVAL 1 Second)::Tuple(IntervalSecond)"))
  case Tuple1IntervalMinute
      extends CHType("Tuple(IntervalMinute)", Seq("tuple(INTERVAL 1 Minute)::Tuple(IntervalMinute)"))
  case Tuple1IntervalHour extends CHType("Tuple(IntervalHour)", Seq("tuple(INTERVAL 1 Hour)::Tuple(IntervalHour)"))
  case Tuple1IntervalDay extends CHType("Tuple(IntervalDay)", Seq("tuple(INTERVAL 1 Day)::Tuple(IntervalDay)"))
  case Tuple1IntervalWeek extends CHType("Tuple(IntervalWeek)", Seq("tuple(INTERVAL 1 Week)::Tuple(IntervalWeek)"))
  case Tuple1IntervalMonth extends CHType("Tuple(IntervalMonth)", Seq("tuple(INTERVAL 1 Month)::Tuple(IntervalMonth)"))
  case Tuple1IntervalQuarter
      extends CHType("Tuple(IntervalQuarter)", Seq("tuple(INTERVAL 1 Quarter)::Tuple(IntervalQuarter)"))
  case Tuple1IntervalYear extends CHType("Tuple(IntervalYear)", Seq("tuple(INTERVAL 1 Year)::Tuple(IntervalYear)"))
  case Tuple1Point extends CHType("Tuple(Point)", Seq("tuple((0, 0))::Tuple(Point)"))
  case Tuple1Ring extends CHType("Tuple(Ring)", Seq("tuple([(0, 0), (10, 0), (10, 10), (0, 10)])::Tuple(Ring)"))
  case Tuple1Polygon
      extends CHType(
        "Tuple(Polygon)",
        Seq("tuple([[(20, 20), (50, 20), (50, 50), (20, 50)], [(30, 30), (50, 50), (50, 30)]])::Tuple(Polygon)")
      )
  case Tuple1MultiPolygon
      extends CHType(
        "Tuple(MultiPolygon)",
        Seq(
          "tuple([[[(0,0),(10,0),(10,10),(0,10)]],[[(20,20),(50,20),(50,50),(20,50)],[(30,30),(50,50),(50,30)]]])::Tuple(MultiPolygon)"
        )
      )
  case Tuple1Enum
      extends CHType(
        "Tuple(Enum)",
        Seq(s"tuple('hello'::Enum('hello' = 1, 'world' = 2))::Tuple(Enum('hello' = 1, 'world' = 2))")
      )
  case Tuple1Enum8
      extends CHType(
        "Tuple(Enum8)",
        Seq(s"tuple('hello'::Enum8('hello' = -128, 'world' = 2))::Tuple(Enum8('hello' = -128, 'world' = 2))")
      )
  case Tuple1Enum16
      extends CHType(
        "Tuple(Enum16)",
        Seq(s"tuple('hello'::Enum16('hello' = -32768, 'world' = 2))::Tuple(Enum16('hello' = -32768, 'world' = 2))")
      )
  case Tuple1FixedString
      extends CHType(
        "Tuple(FixedString)",
        Seq(s"tuple('azertyuiop'::FixedString(10))::Tuple(FixedString(10))")
      )
  case Tuple1IPv4 extends CHType("Tuple(IPv4)", Seq(s"tuple(${IPv4.fuzzingValues.head})::Tuple(IPv4)"))
  case Tuple1IPv6 extends CHType("Tuple(IPv6)", Seq(s"tuple(${IPv6.fuzzingValues.head})::Tuple(IPv6)"))
  case Tuple1Json
      extends CHType(
        "Tuple(JSON)",
        Seq(s"tuple(${Json.fuzzingValues.head})::Tuple(JSON)")
      )
  // case Tuple1Nothing extends CHType("Tuple(Nullable(Nothing))", Seq("tuple(null)::Tuple(Nullable(Nothing))"))
  case Tuple1String
      extends CHType(
        "Tuple(String)",
        Seq(s"tuple(${StringType.fuzzingValues.head})::Tuple(String)")
      )
  case Tuple1UUID
      extends CHType(
        "Tuple(UUID)",
        Seq(s"tuple(${UUID.fuzzingValues.head})::Tuple(UUID)")
      )

  // Nested
  case ArrayMapStringInt8
      extends CHType(
        "Array(Map(String, Int8))",
        Seq(s"[${MapStringInt8.fuzzingValues.head}]::Array(Map(String, Int8))")
      )
  case ArrayTuple1UInt8
      extends CHType(
        "Array(Tuple)",
        Seq(s"[${Tuple1UInt8.fuzzingValues.head}]::Array(Tuple(UInt8))")
      )
  case Tuple1ArrayUInt8
      extends CHType(
        "Tuple(Array(UInt8))",
        Seq(s"tuple(${ArrayUInt8.fuzzingValues.head})::Tuple(Array(Nullable(UInt8)))")
      )
  case Tuple1MapStringInt8
      extends CHType(
        "Tuple(Map(String, Int8))",
        Seq(s"tuple(${MapStringInt8.fuzzingValues.head})::Tuple(Map(String, Int8))")
      )

  // Special
  case ClickHouseType
      extends CHType(
        "ClickHouseType",
        // XXX Should be a conf?! => Built using `SELECT name FROM system.data_type_families WHERE empty(alias_to) ORDER BY name FORMAT TSV`
        // XXX How to handle parameterized types?
        Seq(
          "AggregateFunction",
          "Array",
          "Bool",
          "Date",
          "Date32",
          "DateTime",
          "DateTime32",
          "DateTime64",
          "Decimal",
          "Decimal128",
          "Decimal256",
          "Decimal32",
          "Decimal64",
          "Enum",
          "Enum16",
          "Enum8",
          "FixedString",
          "Float32",
          "Float64",
          "IPv4",
          "IPv6",
          "Int128",
          "Int16",
          "Int256",
          "Int32",
          "Int64",
          "Int8",
          "IntervalDay",
          "IntervalHour",
          "IntervalMicrosecond",
          "IntervalMillisecond",
          "IntervalMinute",
          "IntervalMonth",
          "IntervalNanosecond",
          "IntervalQuarter",
          "IntervalSecond",
          "IntervalWeek",
          "IntervalYear",
          "JSON",
          "LowCardinality",
          "Map",
          "MultiPolygon",
          "Nested",
          "Nothing",
          "Nullable",
          "Object",
          "Point",
          "Polygon",
          "Ring",
          "SimpleAggregateFunction",
          "String",
          "Tuple",
          "UInt128",
          "UInt16",
          "UInt256",
          "UInt32",
          "UInt64",
          "UInt8",
          "UUID",
          "Variant"
        )
      )
  case WindowFunctionMode
      extends CHType(
        "WindowFunctionMode",
        Seq("strict_deduplication", "strict_increase", "strict_order")
      )

object CHType extends StrictLogging:

  def getByName(name: String): CHType =
    findByName(name).getOrElse:
      val errMsg = s"Unable to determine CHType for $name"
      logger.debug(errMsg)
      throw new IllegalArgumentException(errMsg)

  def findByName(name: String): Option[CHType] =
    CHType.values.find(t => t.name.equals(name) || t.aliases.contains(name))

  def merge(type1: String, type2: String): String =
    val exceptionIfUnknown = new IllegalArgumentException(s"Unable to determine higher type for $type1 and $type2")
    if type1 == type2 then type1 // Expects both type to be identical, should be the most obvious use case
    else
      val chType1 = CHType.getByName(type1)
      val chType2 = CHType.getByName(type2)

      val mergedType =
        if chType1 == chType2 then chType1
        else if chType1 == Int8 then
          chType2 match
            case UInt8                                   => Int16
            case UInt16                                  => Int32
            case UInt32                                  => Int64
            case UInt64                                  => Int128
            case UInt128 | UInt256                       => Int256
            case Int16 | Int32 | Int64 | Int128 | Int256 => chType2
            case _                                       => throw exceptionIfUnknown
        else if chType2 == Int8 then
          chType1 match
            case UInt8                                   => Int16
            case UInt16                                  => Int32
            case UInt32                                  => Int64
            case UInt64                                  => Int128
            case UInt128 | UInt256                       => Int256
            case Int16 | Int32 | Int64 | Int128 | Int256 => chType1
            case _                                       => throw exceptionIfUnknown
        else if chType1 == Int16 then
          chType2 match
            case UInt8                           => Int16
            case UInt16                          => Int32
            case UInt32                          => Int64
            case UInt64                          => Int128
            case UInt128 | UInt256               => Int256
            case Int32 | Int64 | Int128 | Int256 => chType2
            case _                               => throw exceptionIfUnknown
        else if chType2 == Int16 then
          chType1 match
            case UInt8                           => Int16
            case UInt16                          => Int32
            case UInt32                          => Int64
            case UInt64                          => Int128
            case UInt128 | UInt256               => Int256
            case Int32 | Int64 | Int128 | Int256 => chType1
            case _                               => throw exceptionIfUnknown
        else if chType1 == Int32 then
          chType2 match
            case UInt8 | UInt16          => Int32
            case UInt32                  => Int64
            case UInt64                  => Int128
            case UInt128 | UInt256       => Int256
            case Int64 | Int128 | Int256 => chType2
            case _                       => throw exceptionIfUnknown
        else if chType2 == Int32 then
          chType1 match
            case UInt8 | UInt16          => Int32
            case UInt32                  => Int64
            case UInt64                  => Int128
            case UInt128 | UInt256       => Int256
            case Int64 | Int128 | Int256 => chType1
            case _                       => throw exceptionIfUnknown
        else if chType1 == Int64 then
          chType2 match
            case UInt8 | UInt16 | UInt32 => Int64
            case UInt64                  => Int128
            case UInt128 | UInt256       => Int256
            case Int128 | Int256         => chType2
            case _                       => throw exceptionIfUnknown
        else if chType2 == Int64 then
          chType1 match
            case UInt8 | UInt16 | UInt32 => Int64
            case UInt64                  => Int128
            case UInt128 | UInt256       => Int256
            case Int128 | Int256         => chType1
            case _                       => throw exceptionIfUnknown
        else if chType1 == Int128 then
          chType2 match
            case UInt8 | UInt16 | UInt32 | UInt64 => Int128
            case UInt128 | UInt256                => Int256
            case Int256                           => Int256
            case _                                => throw exceptionIfUnknown
        else if chType2 == Int128 then
          chType1 match
            case UInt8 | UInt16 | UInt32 | UInt64 => Int128
            case UInt128 | UInt256                => Int256
            case Int256                           => Int256
            case _                                => throw exceptionIfUnknown
        else if chType1 == Int256 then Int256
        else if chType2 == Int256 then Int256
        // From now on, neither type1 nor type2 can be a signed integer
        else if chType1 == UInt8 then
          chType2 match
            case UInt16 | UInt32 | UInt64 | UInt128 | UInt256 => chType2
            case _                                            => throw exceptionIfUnknown
        else if chType2 == UInt8 then
          chType1 match
            case UInt16 | UInt32 | UInt64 | UInt128 | UInt256 => chType1
            case _                                            => throw exceptionIfUnknown
        else if chType1 == UInt16 then
          chType2 match
            case UInt32 | UInt64 | UInt128 | UInt256 => chType2
            case _                                   => throw exceptionIfUnknown
        else if chType2 == UInt16 then
          chType1 match
            case UInt32 | UInt64 | UInt128 | UInt256 => chType1
            case _                                   => throw exceptionIfUnknown
        else if chType1 == UInt32 then
          chType2 match
            case UInt64 | UInt128 | UInt256 => chType2
            case _                          => throw exceptionIfUnknown
        else if chType2 == UInt32 then
          chType1 match
            case UInt64 | UInt128 | UInt256 => chType1
            case _                          => throw exceptionIfUnknown
        else if chType1 == UInt64 then
          chType2 match
            case UInt128 | UInt256 => chType2
            case _                 => throw exceptionIfUnknown
        else if chType2 == UInt64 then
          chType1 match
            case UInt128 | UInt256 => chType1
            case _                 => throw exceptionIfUnknown
        else if chType1 == UInt128 then
          chType2 match
            case UInt256 => chType2
            case _       => throw exceptionIfUnknown
        else if chType2 == UInt128 then
          chType1 match
            case UInt256 => chType1
            case _       => throw exceptionIfUnknown
        // From now on, neither type1 nor type2 can be an unsigned integer
        else if chType1 == Float32 then
          chType2 match
            case Float64 => Float64
            case _       => throw exceptionIfUnknown
        else if chType2 == Float32 then
          chType1 match
            case Float64 => Float64
            case _       => throw exceptionIfUnknown
        // From now on, neither type1 nor type2 can be a float number
        else throw exceptionIfUnknown

      mergedType.name

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
