package com.amendil.common.entities.`type`

import com.amendil.common.Settings
import com.amendil.common.entities.*

enum CHFuzzableType(
    val name: String,
    val fuzzingValues: Seq[String],
    val aliasOpt: Option[String] = None
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

  // Misc
  case Enum
      extends CHFuzzableType(
        "Enum",
        Seq(
          "'hello'::Enum('hello' = 1, 'world' = 2)",
          "'hello'::Enum('hello' = 32767, 'world' = 2)",
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
          "'a'::String",
          "'2020-20-20'::String",
          "'2020-20-20 20:20:20'::String"
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
  case ArrayEnum
      extends CHFuzzableType(
        "Array(Enum)",
        Seq(
          s"['hello'::Enum('hello' = 1, 'world' = 2)]::Array(Enum('hello' = 1, 'world' = 2))",
          s"['hello'::Enum('hello' = -32768, 'world' = 2)]::Array(Enum('hello' = -32768, 'world' = 2))"
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
        "Map(Bool, UnknownType)",
        BooleanType.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapInt8Int
      extends CHFuzzableType(
        "Map(Int8, UnknownType)",
        Int8.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapInt16Int
      extends CHFuzzableType(
        "Map(Int16, UnknownType)",
        Int16.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapInt32Int
      extends CHFuzzableType(
        "Map(Int32, UnknownType)",
        Int32.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapInt64Int
      extends CHFuzzableType(
        "Map(Int64, UnknownType)",
        Int64.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapInt128Int
      extends CHFuzzableType(
        "Map(Int128, UnknownType)",
        Int128.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapInt256Int
      extends CHFuzzableType(
        "Map(Int256, UnknownType)",
        Int256.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapUInt8Int
      extends CHFuzzableType(
        "Map(UInt8, UnknownType)",
        UInt8.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapUInt16Int
      extends CHFuzzableType(
        "Map(UInt16, UnknownType)",
        UInt16.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapUInt32Int
      extends CHFuzzableType(
        "Map(UInt32, UnknownType)",
        UInt32.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapUInt64Int
      extends CHFuzzableType(
        "Map(UInt64, UnknownType)",
        UInt64.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapUInt128Int
      extends CHFuzzableType(
        "Map(UInt128, UnknownType)",
        UInt128.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapUInt256Int
      extends CHFuzzableType(
        "Map(UInt256, UnknownType)",
        UInt256.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapDateInt
      extends CHFuzzableType(
        "Map(Date, UnknownType)",
        Date.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapDate32Int
      extends CHFuzzableType(
        "Map(Date32, UnknownType)",
        Date32.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapDateTimeInt
      extends CHFuzzableType(
        "Map(DateTime, UnknownType)",
        DateTime.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalNanosecondInt
      extends CHFuzzableType(
        "Map(IntervalNanosecond, UnknownType)",
        IntervalNanosecond.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalMicrosecondInt
      extends CHFuzzableType(
        "Map(IntervalMicrosecond, UnknownType)",
        IntervalMicrosecond.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalMillisecondInt
      extends CHFuzzableType(
        "Map(IntervalMillisecond, UnknownType)",
        IntervalMillisecond.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalSecondInt
      extends CHFuzzableType(
        "Map(IntervalSecond, UnknownType)",
        IntervalSecond.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalMinuteInt
      extends CHFuzzableType(
        "Map(IntervalMinute, UnknownType)",
        IntervalMinute.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalHourInt
      extends CHFuzzableType(
        "Map(IntervalHour, UnknownType)",
        IntervalHour.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalDayInt
      extends CHFuzzableType(
        "Map(IntervalDay, UnknownType)",
        IntervalDay.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalWeekInt
      extends CHFuzzableType(
        "Map(IntervalWeek, UnknownType)",
        IntervalWeek.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalMonthInt
      extends CHFuzzableType(
        "Map(IntervalMonth, UnknownType)",
        IntervalMonth.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalQuarterInt
      extends CHFuzzableType(
        "Map(IntervalQuarter, UnknownType)",
        IntervalQuarter.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIntervalYearInt
      extends CHFuzzableType(
        "Map(IntervalYear, UnknownType)",
        IntervalYear.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapEnumInt
      extends CHFuzzableType(
        "Map(Enum, UnknownType)",
        Seq(
          s"map('hello'::Enum('hello' = -128, 'world' = 2), 1)",
          s"map('hello'::Enum('hello' = -32768, 'world' = 2), 1)"
        )
      )
  case MapEnum8Int
      extends CHFuzzableType(
        "Map(Enum8, UnknownType)",
        Seq(s"map('hello'::Enum8('hello' = -128, 'world' = 2), 1)")
      )
  case MapEnum16Int
      extends CHFuzzableType(
        "Map(Enum16, UnknownType)",
        Seq(
          s"map('hello'::Enum16('hello' = -32768, 'world' = 2), 1)"
        )
      )
  case MapFixedStringInt
      extends CHFuzzableType(
        "Map(FixedString, UnknownType)",
        FixedString.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIPv4Int
      extends CHFuzzableType(
        "Map(IPv4, UnknownType)",
        IPv4.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapIPv6Int
      extends CHFuzzableType(
        "Map(IPv6, UnknownType)",
        IPv6.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapStringInt
      extends CHFuzzableType(
        "Map(String, UnknownType)",
        StringType.fuzzingValues.map { fuzzingValue => s"map($fuzzingValue, 1)" }
      )
  case MapUUIDInt
      extends CHFuzzableType(
        "Map(UUID, UnknownType)",
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
  case Tuple1Enum
      extends CHFuzzableType(
        "Tuple(Enum)",
        Seq(
          s"tuple('hello'::Enum('hello' = 1, 'world' = 2))::Tuple(Enum('hello' = 1, 'world' = 2))",
          s"tuple('hello'::Enum('hello' = 1, 'world' = 2))::Tuple(a Enum('hello' = 1, 'world' = 2))",
          s"tuple('hello'::Enum('hello' = -32768, 'world' = 2))::Tuple(a Enum('hello' = -32768, 'world' = 2))"
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
  case ArrayArrayString
      extends CHFuzzableType(
        "Array(Array(String))",
        Seq(s"[[${StringType.fuzzingValues.head}]]::Array(Array(String))")
      )
  case ArrayMapStringInt
      extends CHFuzzableType(
        "Array(Map(String, UnknownType))",
        Seq(s"[map(${StringType.fuzzingValues.head}, 1)::Map(String, Int8)]::Array(Map(String, Int8))")
      )
  case ArrayTuple1UInt8
      extends CHFuzzableType(
        "Array(Tuple(UInt8))",
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
        "Tuple(Map(String, UnknownType))",
        Seq(
          s"tuple(map(${StringType.fuzzingValues.head}, 1)::Map(String, Int8))::Tuple(Map(String, Int8))",
          s"tuple(map(${StringType.fuzzingValues.head}, 1)::Map(String, Int8))::Tuple(a Map(String, Int8))"
        )
      )

  // Special
  // Special - Geo
  case Point extends CHFuzzableType("Point", Seq("(0, 0)::Point"), aliasOpt = Some("Tuple(Float64, Float64)"))
  case Ring
      extends CHFuzzableType(
        "Ring",
        Seq("[(0, 0), (10, 0), (10, 10), (0, 10)]::Ring"),
        aliasOpt = Some("Array(Tuple(Float64, Float64))")
      )
  case Polygon
      extends CHFuzzableType(
        "Polygon",
        Seq("[[(20, 20), (50, 20), (50, 50), (20, 50)], [(30, 30), (50, 50), (50, 30)]]::Polygon"),
        aliasOpt = Some("Array(Array(Tuple(Float64, Float64)))")
      )
  case MultiPolygon
      extends CHFuzzableType(
        "MultiPolygon",
        Seq(
          "[[[(0,0),(10,0),(10,10),(0,10)]],[[(20,20),(50,20),(50,50),(20,50)],[(30,30),(50,50),(50,30)]]]::MultiPolygon"
        ),
        aliasOpt = Some("Array(Array(Array(Tuple(Float64, Float64))))")
      )

  case Tuple1Point
      extends CHFuzzableType(
        "Tuple(Point)",
        Seq("tuple((0, 0))::Tuple(Point)", "tuple((0, 0))::Tuple(a Point)"),
        aliasOpt = Some("Tuple(Tuple(Float64, Float64))")
      )
  case Tuple1Ring
      extends CHFuzzableType(
        "Tuple(Ring)",
        Seq(
          "tuple([(0, 0), (10, 0), (10, 10), (0, 10)])::Tuple(Ring)",
          "tuple([(0, 0), (10, 0), (10, 10), (0, 10)])::Tuple(a Ring)"
        ),
        aliasOpt = Some("Tuple(Array(Tuple(Float64, Float64)))")
      )
  case Tuple1Polygon
      extends CHFuzzableType(
        "Tuple(Polygon)",
        Seq(
          "tuple([[(20, 20), (50, 20), (50, 50), (20, 50)], [(30, 30), (50, 50), (50, 30)]])::Tuple(Polygon)",
          "tuple([[(20, 20), (50, 20), (50, 50), (20, 50)], [(30, 30), (50, 50), (50, 30)]])::Tuple(a Polygon)"
        ),
        aliasOpt = Some("Tuple(Array(Array(Tuple(Float64, Float64))))")
      )
  case Tuple1MultiPolygon
      extends CHFuzzableType(
        "Tuple(MultiPolygon)",
        Seq(
          "tuple([[[(0,0),(10,0),(10,10),(0,10)]],[[(20,20),(50,20),(50,50),(20,50)],[(30,30),(50,50),(50,30)]]])::Tuple(MultiPolygon)",
          "tuple([[[(0,0),(10,0),(10,10),(0,10)]],[[(20,20),(50,20),(50,50),(20,50)],[(30,30),(50,50),(50,30)]]])::Tuple(a MultiPolygon)"
        ),
        aliasOpt = Some("Tuple(Array(Array(Array(Tuple(Float64, Float64)))))")
      )

  // Special - Misc
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
  case TopKOption
      extends CHFuzzableType(
        "TopKOption",
        Seq("'counts'")
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
