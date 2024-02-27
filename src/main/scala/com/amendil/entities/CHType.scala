package com.amendil.entities

import com.typesafe.scalalogging.StrictLogging

enum CHType(
    val name: String,
    val baseFuzzingValues: Seq[String],
    lowCardinality: Boolean = false,
    nullable: Boolean = false
) {

  def fuzzingValues = {
    // Build a sample fuzzingValue and fetch its type
    val firstFuzzingValue = baseFuzzingValues.head
    val typeIndex = firstFuzzingValue.lastIndexOf("::")
    val fuzzingValueWithoutType = firstFuzzingValue.substring(0, typeIndex)
    val fuzzingType = firstFuzzingValue.substring(typeIndex + "::".size)

    // Handle lowCardinality
    val lowCardinalityValues: Seq[String] =
      if (lowCardinality) {
        Seq(s"$fuzzingValueWithoutType::LowCardinality($fuzzingType)")
      } else Nil

    // Handle nullable
    val nullableFuzzingValues: Seq[String] =
      if (nullable) {
        Seq(
          s"$fuzzingValueWithoutType::Nullable($fuzzingType)",
          s"null::Nullable($fuzzingType)"
        )
      } else Nil

    baseFuzzingValues ++ lowCardinalityValues ++ nullableFuzzingValues
  }

  // Numbers
  case Int8
      extends CHType(
        "Int8",
        Seq("-128::Int8", "127::Int8", "0::Int8", "1::Int8"),
        lowCardinality = true,
        nullable = true
      )
  case Int16
      extends CHType(
        "Int16",
        Seq("-32768::Int16", "32767::Int16", "0::Int16", "1::Int16"),
        lowCardinality = true,
        nullable = true
      )
  case Int32
      extends CHType(
        "Int32",
        Seq("-2147483648::Int32", "2147483647::Int32", "0::Int32", "1::Int32"),
        lowCardinality = true,
        nullable = true
      )
  case Int64
      extends CHType(
        "Int64",
        Seq("-9223372036854775808::Int64", "9223372036854775807::Int64", "0::Int64", "1::Int64"),
        lowCardinality = true,
        nullable = true
      )
  case Int128
      extends CHType(
        "Int128",
        Seq(
          "-170141183460469231731687303715884105728::Int128",
          "170141183460469231731687303715884105727::Int128",
          "0::Int128",
          "1::Int128"
        ),
        lowCardinality = true,
        nullable = true
      )
  case Int256
      extends CHType(
        "Int256",
        Seq(
          "-57896044618658097711785492504343953926634992332820282019728792003956564819968::Int256",
          "57896044618658097711785492504343953926634992332820282019728792003956564819967::Int256",
          "0::Int256",
          "1::Int256"
        ),
        lowCardinality = true,
        nullable = true
      )
  case UInt8
      extends CHType(
        "UInt8",
        Seq("0::UInt8", "1::UInt8", "255::UInt8"),
        lowCardinality = true,
        nullable = true
      )
  case UInt16
      extends CHType(
        "UInt16",
        Seq("0::UInt16", "1::UInt16", "65535::UInt16"),
        lowCardinality = true,
        nullable = true
      )
  case UInt32
      extends CHType(
        "UInt32",
        Seq("0::UInt32", "1::UInt32", "4294967295::UInt32"),
        lowCardinality = true,
        nullable = true
      )
  case UInt64
      extends CHType(
        "UInt64",
        Seq("0::UInt64", "1::UInt64", "18446744073709551615::UInt64"),
        lowCardinality = true,
        nullable = true
      )
  case UInt128
      extends CHType(
        "UInt128",
        Seq("0::UInt128", "1::UInt128", "340282366920938463463374607431768211455::UInt128"),
        lowCardinality = true,
        nullable = true
      )
  case UInt256
      extends CHType(
        "UInt256",
        Seq(
          "0::UInt256",
          "1::UInt256",
          "115792089237316195423570985008687907853269984665640564039457584007913129639935::UInt256"
        ),
        lowCardinality = true,
        nullable = true
      )

  case Float32
      extends CHType(
        "Float32",
        Seq("-inf::Float32", "nan::Float32", "0.5::Float32", "0::Float32"),
        lowCardinality = true,
        nullable = true
      )
  case Float64
      extends CHType(
        "Float64",
        Seq("-inf::Float64", "nan::Float64", "0.5::Float64", "0::Float64"),
        lowCardinality = true,
        nullable = true
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
        nullable = true
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
        nullable = true
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
        nullable = true
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
        nullable = true
      )

  // Date
  case Date
      extends CHType(
        "Date",
        Seq("'1970-01-01'::Date", "'2149-06-06'::Date"),
        lowCardinality = true,
        nullable = true
      )
  case Date32
      extends CHType(
        "Date32",
        Seq("'1900-01-01'::Date32", "'2299-12-31'::Date32"),
        lowCardinality = true,
        nullable = true
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
        lowCardinality = true,
        nullable = true
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
        nullable = true
      )
  case IntervalNanosecond
      extends CHType(
        "IntervalNanosecond",
        Seq("INTERVAL 1 Nanosecond::IntervalNanosecond"),
        nullable = true
      )
  case IntervalMicrosecond
      extends CHType(
        "IntervalMicrosecond",
        Seq("INTERVAL 1 Microsecond::IntervalMicrosecond"),
        nullable = true
      )
  case IntervalMillisecond
      extends CHType(
        "IntervalMillisecond",
        Seq("INTERVAL 1 Millisecond::IntervalMillisecond"),
        nullable = true
      )
  case IntervalSecond
      extends CHType(
        "IntervalSecond",
        Seq("INTERVAL 1 Second::IntervalSecond"),
        nullable = true
      )
  case IntervalMinute
      extends CHType(
        "IntervalMinute",
        Seq("INTERVAL 1 Minute::IntervalMinute"),
        nullable = true
      )
  case IntervalHour
      extends CHType(
        "IntervalHour",
        Seq("INTERVAL 1 Hour::IntervalHour"),
        nullable = true
      )
  case IntervalDay
      extends CHType(
        "IntervalDay",
        Seq("INTERVAL 1 Day::IntervalDay"),
        nullable = true
      )
  case IntervalWeek
      extends CHType(
        "IntervalWeek",
        Seq("INTERVAL 1 Week::IntervalWeek"),
        nullable = true
      )
  case IntervalMonth
      extends CHType(
        "IntervalMonth",
        Seq("INTERVAL 1 Month::IntervalMonth"),
        nullable = true
      )
  case IntervalQuarter
      extends CHType(
        "IntervalQuarter",
        Seq("INTERVAL 1 Quarter::IntervalQuarter"),
        nullable = true
      )
  case IntervalYear
      extends CHType(
        "IntervalYear",
        Seq("INTERVAL 1 Year::IntervalYear"),
        nullable = true
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
        ),
        nullable = true
      )
  case Enum8
      extends CHType(
        "Enum8",
        Seq(
          "'hello'::Enum8('hello' = -128, 'world' = 2)",
          "'hello'::Enum8('hello' = 127, 'world' = 2)",
          "'hello'::Enum8('hello', 'world')"
        ),
        nullable = true
      )
  case Enum16
      extends CHType(
        "Enum16",
        Seq(
          "'hello'::Enum16('hello' = -32768, 'world' = 2)",
          "'hello'::Enum16('hello' = 32767, 'world' = 2)",
          "'hello'::Enum16('hello', 'world')"
        ),
        nullable = true
      )
  case FixedString
      extends CHType(
        "FixedString",
        Seq("'azertyuiop'::FixedString(10)", "''::FixedString(1)"),
        lowCardinality = true,
        nullable = true
      )
  case IPv4 extends CHType("IPv4", Seq("'116.106.34.242'::IPv4"), nullable = true)
  case IPv6
      extends CHType(
        "IPv6",
        Seq(
          "'2001:44c8:129:2632:33:0:252:2'::IPv6",
          "'2a02:e980:1e::1'::IPv6",
          "'116.106.34.242'::IPv6"
        ),
        nullable = true
      )
  case Json
      extends CHType(
        "JSON",
        Seq("""'{"a": 1, "b": { "c": "foo", "d": [1, 2, 3] }, "c": null}'::JSON""")
      )
  // case Nothing extends CHType("Nothing", Seq("null::Nullable(Nothing)"))
  case StringType
      extends CHType(
        "String",
        Seq("'foo'::String", "''::String"),
        lowCardinality = true,
        nullable = true
      )
  case UUID
      extends CHType(
        "UUID",
        Seq("'00000000-0000-0000-0000-000000000000'::UUID", "'61f0c404-5cb3-11e7-907b-a6006ad3dba0'::UUID"),
        nullable = true
      )

  // Array
  case ArrayInt8
      extends CHType(
        "Array(Int8)",
        Seq(
          s"[${Int8.fuzzingValues.mkString(", ")}]::Array(Nullable(Int8))"
        )
      )
  case ArrayInt16
      extends CHType(
        "Array(Int16)",
        Seq(
          s"[${Int16.fuzzingValues.mkString(", ")}]::Array(Nullable(Int16))"
        )
      )
  case ArrayInt32
      extends CHType(
        "Array(Int32)",
        Seq(
          s"[${Int32.fuzzingValues.mkString(", ")}]::Array(Nullable(Int32))"
        )
      )
  case ArrayInt64
      extends CHType(
        "Array(Int64)",
        Seq(
          s"[${Int64.fuzzingValues.mkString(", ")}]::Array(Nullable(Int64))"
        )
      )
  case ArrayInt128
      extends CHType(
        "Array(Int128)",
        Seq(
          s"[${Int128.fuzzingValues.mkString(", ")}]::Array(Nullable(Int128))"
        )
      )
  case ArrayInt256
      extends CHType(
        "Array(Int256)",
        Seq(
          s"[${Int256.fuzzingValues.mkString(", ")}]::Array(Nullable(Int256))"
        )
      )
  case ArrayUInt8
      extends CHType(
        "Array(UInt8)",
        Seq(
          s"[${UInt8.fuzzingValues.mkString(", ")}]::Array(Nullable(UInt8))"
        )
      )
  case ArrayUInt16
      extends CHType(
        "Array(UInt16)",
        Seq(
          s"[${UInt16.fuzzingValues.mkString(", ")}]::Array(Nullable(UInt16))"
        )
      )
  case ArrayUInt32
      extends CHType(
        "Array(UInt32)",
        Seq(
          s"[${UInt32.fuzzingValues.mkString(", ")}]::Array(Nullable(UInt32))"
        )
      )
  case ArrayUInt64
      extends CHType(
        "Array(UInt64)",
        Seq(
          s"[${UInt64.fuzzingValues.mkString(", ")}]::Array(Nullable(UInt64))"
        )
      )
  case ArrayUInt128
      extends CHType(
        "Array(UInt128)",
        Seq(
          s"[${UInt128.fuzzingValues.mkString(", ")}]::Array(Nullable(UInt128))"
        )
      )
  case ArrayUInt256
      extends CHType(
        "Array(UInt256)",
        Seq(
          s"[${UInt256.fuzzingValues.mkString(", ")}]::Array(Nullable(UInt256))"
        )
      )
  case ArrayFloat32
      extends CHType(
        "Array(Float32)",
        Seq(
          s"[${Float32.fuzzingValues.mkString(", ")}]::Array(Nullable(Float32))"
        )
      )
  case ArrayFloat64
      extends CHType(
        "Array(Float64)",
        Seq(
          s"[${Float64.fuzzingValues.mkString(", ")}]::Array(Nullable(Float64))"
        )
      )
  case ArrayDecimal32
      extends CHType(
        "Array(Decimal32)",
        Seq(
          s"[-999999999::Decimal32(0)]::Array(Decimal32(0))"
        )
      )
  case ArrayDecimal64
      extends CHType(
        "Array(Decimal64)",
        Seq(
          s"[-999999999999999999::Decimal64(0)]::Array(Decimal64(0))"
        )
      )
  case ArrayDecimal128
      extends CHType(
        "Array(Decimal128)",
        Seq(
          s"[-999999999999999999999999999999999999::Decimal128(0)]::Array(Decimal128(0))"
        )
      )
  case ArrayDecimal256
      extends CHType(
        "Array(Decimal256)",
        Seq(
          s"[-999999999999999999999999999999999999999999999999999999999999999999999999::Decimal256(0)]::Array(Decimal256(0))"
        )
      )
  case ArrayDate
      extends CHType(
        "Array(Date)",
        Seq(
          s"[${Date.fuzzingValues.mkString(", ")}]::Array(Nullable(Date))"
        )
      )
  case ArrayDate32
      extends CHType(
        "Array(Date32)",
        Seq(
          s"[${Date32.fuzzingValues.mkString(", ")}]::Array(Nullable(Date32))"
        )
      )
  case ArrayDateTime
      extends CHType(
        "Array(DateTime)",
        Seq(
          s"['1970-01-01 00:00:00'::DateTime('Asia/Istanbul')]::Array(DateTime('Asia/Istanbul'))"
        )
      )
  case ArrayDateTime64
      extends CHType(
        "Array(DateTime64)",
        Seq(
          s"['1900-01-01 00:00:00'::DateTime64(0, 'Asia/Istanbul')]::Array(DateTime64(0, 'Asia/Istanbul'))"
        )
      )
  case ArrayIntervalNanosecond
      extends CHType("Array(IntervalNanosecond)", Seq("[INTERVAL 1 Nanosecond]::Array(Nullable(IntervalNanosecond))"))
  case ArrayIntervalMicrosecond
      extends CHType(
        "Array(IntervalMicrosecond)",
        Seq("[INTERVAL 1 Microsecond]::Array(Nullable(IntervalMicrosecond))")
      )
  case ArrayIntervalMillisecond
      extends CHType(
        "Array(IntervalMillisecond)",
        Seq("[INTERVAL 1 Millisecond]::Array(Nullable(IntervalMillisecond))")
      )
  case ArrayIntervalSecond
      extends CHType("Array(IntervalSecond)", Seq("[INTERVAL 1 Second]::Array(Nullable(IntervalSecond))"))
  case ArrayIntervalMinute
      extends CHType("Array(IntervalMinute)", Seq("[INTERVAL 1 Minute]::Array(Nullable(IntervalMinute))"))
  case ArrayIntervalHour extends CHType("Array(IntervalHour)", Seq("[INTERVAL 1 Hour]::Array(Nullable(IntervalHour))"))
  case ArrayIntervalDay extends CHType("Array(IntervalDay)", Seq("[INTERVAL 1 Day]::Array(Nullable(IntervalDay))"))
  case ArrayIntervalWeek extends CHType("Array(IntervalWeek)", Seq("[INTERVAL 1 Week]::Array(Nullable(IntervalWeek))"))
  case ArrayIntervalMonth
      extends CHType("Array(IntervalMonth)", Seq("[INTERVAL 1 Month]::Array(Nullable(IntervalMonth))"))
  case ArrayIntervalQuarter
      extends CHType("Array(IntervalQuarter)", Seq("[INTERVAL 1 Quarter]::Array(Nullable(IntervalQuarter))"))
  case ArrayIntervalYear extends CHType("Array(IntervalYear)", Seq("[INTERVAL 1 Year]::Array(Nullable(IntervalYear))"))
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
        )
      )
  case ArrayEnum16
      extends CHType(
        "Array(Enum16)",
        Seq(
          s"['hello'::Enum16('hello' = -32768, 'world' = 2)]::Array(Enum16('hello' = -32768, 'world' = 2))"
        )
      )
  case ArrayFixedString
      extends CHType(
        "Array(FixedString)",
        Seq(
          s"[${FixedString.fuzzingValues.mkString(", ")}]::Array(Nullable(FixedString(10)))"
        )
      )
  case ArrayIPv4 extends CHType("Array(IPv4)", Seq(s"[${IPv4.fuzzingValues.mkString(", ")}]::Array(Nullable(IPv4))"))
  case ArrayIPv6 extends CHType("Array(IPv6)", Seq(s"[${IPv6.fuzzingValues.mkString(", ")}]::Array(Nullable(IPv6))"))
  case ArrayJson
      extends CHType(
        "Array(JSON)",
        Seq(
          s"[${Json.fuzzingValues.mkString(", ")}]::Array(JSON)"
        )
      )
  // case ArrayNothing
  //     extends CHType("Array(Nothing)", Seq("array()::Array(Nothing)", "array(null)::Array(Nullable(Nothing))"))
  case ArrayString
      extends CHType(
        "Array(String)",
        Seq(
          s"[${StringType.fuzzingValues.mkString(", ")}]::Array(Nullable(String))"
        )
      )
  case ArrayUUID
      extends CHType(
        "Array(UUID)",
        Seq(s"[${UUID.fuzzingValues.mkString(", ")}]::Array(Nullable(UUID))")
      )

  // Map
  case MapInt8Int8
      extends CHType(
        "Map(Int8, Int8)",
        Int8.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(Int8, Int8)" }
      )
  case MapInt16Int8
      extends CHType(
        "Map(Int16, Int8)",
        Int16.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(Int16, Int8)" }
      )
  case MapInt32Int8
      extends CHType(
        "Map(Int32, Int8)",
        Int32.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(Int32, Int8)" }
      )
  case MapInt64Int8
      extends CHType(
        "Map(Int64, Int8)",
        Int64.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(Int64, Int8)" }
      )
  case MapInt128Int8
      extends CHType(
        "Map(Int128, Int8)",
        Int128.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(Int128, Int8)" }
      )
  case MapInt256Int8
      extends CHType(
        "Map(Int256, Int8)",
        Int256.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(Int256, Int8)" }
      )
  case MapUInt8Int8
      extends CHType(
        "Map(UInt8, Int8)",
        UInt8.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(UInt8, Int8)" }
      )
  case MapUInt16Int8
      extends CHType(
        "Map(UInt16, Int8)",
        UInt16.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(UInt16, Int8)" }
      )
  case MapUInt32Int8
      extends CHType(
        "Map(UInt32, Int8)",
        UInt32.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(UInt32, Int8)" }
      )
  case MapUInt64Int8
      extends CHType(
        "Map(UInt64, Int8)",
        UInt64.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(UInt64, Int8)" }
      )
  case MapUInt128Int8
      extends CHType(
        "Map(UInt128, Int8)",
        UInt128.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(UInt128, Int8)" }
      )
  case MapUInt256Int8
      extends CHType(
        "Map(UInt256, Int8)",
        UInt256.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(UInt256, Int8)" }
      )
  case MapDateInt8
      extends CHType(
        "Map(Date, Int8)",
        Date.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(Date, Int8)" }
      )
  case MapDate32Int8
      extends CHType(
        "Map(Date32, Int8)",
        Date32.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(Date32, Int8)" }
      )
  case MapDateTimeInt8
      extends CHType(
        "Map(DateTime, Int8)",
        DateTime.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(DateTime, Int8)" }
      )
  case MapIntervalNanosecondInt8
      extends CHType(
        "Map(IntervalNanosecond, Int8)",
        DateTime.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(DateTime, Int8)" }
      )
  case MapIntervalMicrosecondInt8
      extends CHType(
        "Map(IntervalMicrosecond, Int8)",
        DateTime.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(DateTime, Int8)" }
      )
  case MapIntervalMillisecondInt8
      extends CHType(
        "Map(IntervalMillisecond, Int8)",
        IntervalMillisecond.baseFuzzingValues.map { fuzzingValue =>
          s"map($fuzzingValue, 1)::Map(IntervalMillisecond, Int8)"
        }
      )
  case MapIntervalSecondInt8
      extends CHType(
        "Map(IntervalSecond, Int8)",
        IntervalSecond.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IntervalSecond, Int8)" }
      )
  case MapIntervalMinuteInt8
      extends CHType(
        "Map(IntervalMinute, Int8)",
        IntervalMinute.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IntervalMinute, Int8)" }
      )
  case MapIntervalHourInt8
      extends CHType(
        "Map(IntervalHour, Int8)",
        IntervalHour.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IntervalHour, Int8)" }
      )
  case MapIntervalDayInt8
      extends CHType(
        "Map(IntervalDay, Int8)",
        IntervalDay.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IntervalDay, Int8)" }
      )
  case MapIntervalWeekInt8
      extends CHType(
        "Map(IntervalWeek, Int8)",
        IntervalWeek.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IntervalWeek, Int8)" }
      )
  case MapIntervalMonthInt8
      extends CHType(
        "Map(IntervalMonth, Int8)",
        IntervalMonth.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IntervalMonth, Int8)" }
      )
  case MapIntervalQuarterInt8
      extends CHType(
        "Map(IntervalQuarter, Int8)",
        IntervalQuarter.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IntervalQuarter, Int8)" }
      )
  case MapIntervalYearInt8
      extends CHType(
        "Map(IntervalYear, Int8)",
        IntervalYear.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IntervalYear, Int8)" }
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
        FixedString.baseFuzzingValues.map { fuzzingValue =>
          s"map($fuzzingValue, 1)::Map(FixedString(10), Int8)"
        } :+ "map('azertyuiop'::LowCardinality(FixedString(10)), 1)::Map(LowCardinality(FixedString(10)), Int8)"
      )
  case MapIPv4Int8
      extends CHType(
        "Map(IPv4, Int8)",
        IPv4.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IPv4, Int8)" }
      )
  case MapIPv6Int8
      extends CHType(
        "Map(IPv6, Int8)",
        IPv6.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(IPv6, Int8)" }
      )
  case MapStringInt8
      extends CHType(
        "Map(String, Int8)",
        StringType.baseFuzzingValues.map { fuzzingValue =>
          s"map($fuzzingValue, 1)::Map(String, Int8)"
        } :+ "map('foo'::LowCardinality(String), 1)::Map(LowCardinality(String), Int8)"
      )
  case MapUUIDInt8
      extends CHType(
        "Map(UUID, Int8)",
        UUID.baseFuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)::Map(UUID, Int8)" }
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
        Seq(s"tuple(${Decimal32.fuzzingValues.head})::Tuple(Decimal32(0))")
      )
  case Tuple1Decimal64
      extends CHType(
        "Tuple(Decimal64)",
        Seq(s"tuple(${Decimal64.fuzzingValues.head})::Tuple(Decimal64(0))")
      )
  case Tuple1Decimal128
      extends CHType(
        "Tuple(Decimal128)",
        Seq(s"tuple(${Decimal128.fuzzingValues.head})::Tuple(Decimal128(0))")
      )
  case Tuple1Decimal256
      extends CHType(
        "Tuple(Decimal256)",
        Seq(s"tuple(${Decimal256.fuzzingValues.head})::Tuple(Decimal256(0))")
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

  // Special
  case ArrayMapStringInt8
      extends CHType(
        "Array(Map(String, Int8))",
        Seq(s"[${MapStringInt8.fuzzingValues.head}]::Array(Map(String, Int8))")
      )
  case ArrayTuple1UInt8
      extends CHType(
        "Array(Tuple(UInt8))",
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
}

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
          CHType.Decimal256
        )
      )

  // Date
  case Date extends CHAbstractType(Seq("'1970-01-02'::Date"), Seq(CHType.Date, CHType.Date32))
  case DateTime extends CHAbstractType(Seq("'1970-01-02 00:00:00'::DateTime"), Seq(CHType.DateTime, CHType.DateTime64))
  case IntervalDate
      extends CHAbstractType(
        Seq(CHType.IntervalDay.fuzzingValues.head),
        Seq(CHType.IntervalDay, CHType.IntervalWeek, CHType.IntervalMonth, CHType.IntervalQuarter, CHType.IntervalYear)
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
          CHType.IntervalHour
        )
      )

  // Geo
  case Point extends CHAbstractType(Seq(CHType.Point.fuzzingValues.head), Seq(CHType.Point))
  case Ring extends CHAbstractType(Seq(CHType.Ring.fuzzingValues.head), Seq(CHType.Ring))
  case Polygon extends CHAbstractType(Seq(CHType.Polygon.fuzzingValues.head), Seq(CHType.Polygon))
  case MultiPolygon extends CHAbstractType(Seq(CHType.MultiPolygon.fuzzingValues.head), Seq(CHType.MultiPolygon))

  // Misc
  case Enum extends CHAbstractType(Seq(CHType.Enum.fuzzingValues.head), Seq(CHType.Enum, CHType.Enum8, CHType.Enum16))
  case IPv4 extends CHAbstractType(Seq(CHType.IPv4.fuzzingValues.head), Seq(CHType.IPv4))
  case IPv6 extends CHAbstractType(Seq(CHType.IPv6.fuzzingValues.head), Seq(CHType.IPv6))
  case Json extends CHAbstractType(Seq(CHType.Json.fuzzingValues.head), Seq(CHType.Json))
  // case Nothing extends CHAbstractType(CHType.Nothing.fuzzingValues, Seq(CHType.Nothing))
  case String
      extends CHAbstractType(Seq(CHType.StringType.fuzzingValues.head), Seq(CHType.StringType, CHType.FixedString))
  case UUID extends CHAbstractType(Seq(CHType.UUID.fuzzingValues.head), Seq(CHType.UUID))

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

object CHType extends StrictLogging {

  def getByName(name: String): CHType =
    findByName(name).getOrElse {
      val errMsg = s"Unable to determine CHType for $name"
      logger.error(errMsg)
      throw new IllegalArgumentException(errMsg)
    }

  def findByName(name: String): Option[CHType] =
    CHType.values.find(_.name.equals(name))

  def merge(type1: CHType, type2: CHType): CHType =
    val exceptionIfUnknown = new IllegalArgumentException(s"Unable to determine higher type for $type1 and $type2")
    if (type1 == type2) type1 // Expects both type to be identical, should be the most obvious use case
    else if (type1 == Int8) {
      type2 match
        case UInt8                                   => Int16
        case UInt16                                  => Int32
        case UInt32                                  => Int64
        case UInt64                                  => Int128
        case UInt128 | UInt256                       => Int256
        case Int16 | Int32 | Int64 | Int128 | Int256 => type2
        case _                                       => throw exceptionIfUnknown
    } else if (type2 == Int8) {
      type1 match
        case UInt8                                   => Int16
        case UInt16                                  => Int32
        case UInt32                                  => Int64
        case UInt64                                  => Int128
        case UInt128 | UInt256                       => Int256
        case Int16 | Int32 | Int64 | Int128 | Int256 => type1
        case _                                       => throw exceptionIfUnknown
    } else if (type1 == Int16) {
      type2 match
        case UInt8                           => Int16
        case UInt16                          => Int32
        case UInt32                          => Int64
        case UInt64                          => Int128
        case UInt128 | UInt256               => Int256
        case Int32 | Int64 | Int128 | Int256 => type2
        case _                               => throw exceptionIfUnknown
    } else if (type2 == Int16) {
      type1 match
        case UInt8                           => Int16
        case UInt16                          => Int32
        case UInt32                          => Int64
        case UInt64                          => Int128
        case UInt128 | UInt256               => Int256
        case Int32 | Int64 | Int128 | Int256 => type1
        case _                               => throw exceptionIfUnknown
    } else if (type1 == Int32) {
      type2 match
        case UInt8 | UInt16          => Int32
        case UInt32                  => Int64
        case UInt64                  => Int128
        case UInt128 | UInt256       => Int256
        case Int64 | Int128 | Int256 => type2
        case _                       => throw exceptionIfUnknown
    } else if (type2 == Int32) {
      type1 match
        case UInt8 | UInt16          => Int32
        case UInt32                  => Int64
        case UInt64                  => Int128
        case UInt128 | UInt256       => Int256
        case Int64 | Int128 | Int256 => type1
        case _                       => throw exceptionIfUnknown
    } else if (type1 == Int64) {
      type2 match
        case UInt8 | UInt16 | UInt32 => Int64
        case UInt64                  => Int128
        case UInt128 | UInt256       => Int256
        case Int128 | Int256         => type2
        case _                       => throw exceptionIfUnknown
    } else if (type2 == Int64) {
      type1 match
        case UInt8 | UInt16 | UInt32 => Int64
        case UInt64                  => Int128
        case UInt128 | UInt256       => Int256
        case Int128 | Int256         => type1
        case _                       => throw exceptionIfUnknown
    } else if (type1 == Int128) {
      type2 match
        case UInt8 | UInt16 | UInt32 | UInt64 => Int128
        case UInt128 | UInt256                => Int256
        case Int256                           => Int256
        case _                                => throw exceptionIfUnknown
    } else if (type2 == Int128) {
      type1 match
        case UInt8 | UInt16 | UInt32 | UInt64 => Int128
        case UInt128 | UInt256                => Int256
        case Int256                           => Int256
        case _                                => throw exceptionIfUnknown
    } else if (type1 == Int256) Int256
    else if (type2 == Int256) Int256
    // From now on, neither type1 nor type2 can be a signed integer
    else if (type1 == UInt8) {
      type2 match
        case UInt16 | UInt32 | UInt64 | UInt128 | UInt256 => type2
        case _                                            => throw exceptionIfUnknown
    } else if (type2 == UInt8) {
      type1 match
        case UInt16 | UInt32 | UInt64 | UInt128 | UInt256 => type1
        case _                                            => throw exceptionIfUnknown
    } else if (type1 == UInt16) {
      type2 match
        case UInt32 | UInt64 | UInt128 | UInt256 => type2
        case _                                   => throw exceptionIfUnknown
    } else if (type2 == UInt16) {
      type1 match
        case UInt32 | UInt64 | UInt128 | UInt256 => type1
        case _                                   => throw exceptionIfUnknown
    } else if (type1 == UInt32) {
      type2 match
        case UInt64 | UInt128 | UInt256 => type2
        case _                          => throw exceptionIfUnknown
    } else if (type2 == UInt32) {
      type1 match
        case UInt64 | UInt128 | UInt256 => type1
        case _                          => throw exceptionIfUnknown
    } else if (type1 == UInt64) {
      type2 match
        case UInt128 | UInt256 => type2
        case _                 => throw exceptionIfUnknown
    } else if (type2 == UInt64) {
      type1 match
        case UInt128 | UInt256 => type1
        case _                 => throw exceptionIfUnknown
    } else if (type1 == UInt128) {
      type2 match
        case UInt256 => type2
        case _       => throw exceptionIfUnknown
    } else if (type2 == UInt128) {
      type1 match
        case UInt256 => type1
        case _       => throw exceptionIfUnknown
    }
    // From now on, neither type1 nor type2 can be an unsigned integer
    else if (type1 == Float32) {
      type2 match
        case Float64 => Float64
        case _       => throw exceptionIfUnknown
    } else if (type2 == Float32) {
      type1 match
        case Float64 => Float64
        case _       => throw exceptionIfUnknown
    }
    // From now on, neither type1 nor type2 can be a float number
    else throw new IllegalArgumentException(s"Unable to determine higher type for $type1 and $type2")
}
