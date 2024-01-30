package com.amendil

enum CHType(val name: String, val fuzzingValues: Seq[String]) {

  // Numbers
  case Int8
      extends CHType(
        "Int8",
        Seq("-128::Int8", "127::Int8", "-128::LowCardinality(Int8)", "-128::Nullable(Int8)", "null::Nullable(Int8)")
      )
  case Int16
      extends CHType(
        "Int16",
        Seq(
          "-32768::Int16",
          "32767::Int16",
          "-32768::LowCardinality(Int16)",
          "-32768::Nullable(Int16)",
          "null::Nullable(Int16)"
        )
      )
  case Int32
      extends CHType(
        "Int32",
        Seq(
          "-2147483648::Int32",
          "2147483647::Int32",
          "-2147483648::LowCardinality(Int32)",
          "-2147483648::Nullable(Int32)",
          "null::Nullable(Int32)"
        )
      )
  case Int64
      extends CHType(
        "Int64",
        Seq(
          "-9223372036854775808::Int64",
          "9223372036854775807::Int64",
          "-9223372036854775808::LowCardinality(Int64)",
          "-9223372036854775808::Nullable(Int64)",
          "null::Nullable(Int64)"
        )
      )
  case Int128
      extends CHType(
        "Int128",
        Seq(
          "-170141183460469231731687303715884105728::Int128",
          "170141183460469231731687303715884105727::Int128",
          "-170141183460469231731687303715884105728::LowCardinality(Int128)",
          "-170141183460469231731687303715884105728::Nullable(Int128)",
          "null::Nullable(Int128)"
        )
      )
  case Int256
      extends CHType(
        "Int256",
        Seq(
          "-57896044618658097711785492504343953926634992332820282019728792003956564819968::Int256",
          "57896044618658097711785492504343953926634992332820282019728792003956564819967::Int256",
          "-57896044618658097711785492504343953926634992332820282019728792003956564819968::LowCardinality(Int256)",
          "-57896044618658097711785492504343953926634992332820282019728792003956564819968::Nullable(Int256)",
          "null::Nullable(Int256)"
        )
      )
  case UInt8
      extends CHType(
        "UInt8",
        Seq(
          "0::UInt8",
          "1::UInt8",
          "255::UInt8",
          "0::LowCardinality(UInt8)",
          "0::Nullable(UInt8)",
          "null::Nullable(UInt8)"
        )
      )
  case UInt16
      extends CHType(
        "UInt16",
        Seq("0::UInt16", "65535::UInt16", "0::LowCardinality(UInt16)", "0::Nullable(UInt16)", "null::Nullable(UInt16)")
      )
  case UInt32
      extends CHType(
        "UInt32",
        Seq(
          "0::UInt32",
          "4294967295::UInt32",
          "0::LowCardinality(UInt32)",
          "0::Nullable(UInt32)",
          "null::Nullable(UInt32)"
        )
      )
  case UInt64
      extends CHType(
        "UInt64",
        Seq(
          "0::UInt64",
          "18446744073709551615::UInt64",
          "0::LowCardinality(UInt64)",
          "0::Nullable(UInt64)",
          "null::Nullable(UInt64)"
        )
      )
  case UInt128
      extends CHType(
        "UInt128",
        Seq(
          "0::UInt128",
          "340282366920938463463374607431768211455::UInt128",
          "0::LowCardinality(UInt128)",
          "0::Nullable(UInt128)",
          "null::Nullable(UInt128)"
        )
      )
  case UInt256
      extends CHType(
        "UInt256",
        Seq(
          "0::UInt256",
          "115792089237316195423570985008687907853269984665640564039457584007913129639935::UInt256",
          "0::LowCardinality(UInt256)",
          "0::Nullable(UInt256)",
          "null::Nullable(UInt256)"
        )
      )

  case Float32
      extends CHType(
        "Float32",
        Seq(
          "-inf::Float32",
          "nan::Float32",
          "0.5::Float32",
          "-inf::LowCardinality(Float32)",
          "0.5::Nullable(Float32)",
          "null::Nullable(Float32)"
        )
      )
  case Float64
      extends CHType(
        "Float64",
        Seq(
          "-inf::Float64",
          "nan::Float64",
          "0.5::Float64",
          "-inf::LowCardinality(Float64)",
          "0.5::Nullable(Float64)",
          "null::Nullable(Float64)"
        )
      )
  case Decimal32
      extends CHType(
        "Decimal32",
        Seq(
          "-999999999::Decimal32(0)",
          "999999999::Decimal32(0)",
          "-0.999999999::Decimal32(9)",
          "0.999999999::Decimal32(9)",
          "-999999999::Nullable(Decimal32(0))",
          "null::Nullable(Decimal32(0))"
        )
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
          "999999999999999999::Decimal",
          "-999999999999999999::Nullable(Decimal64(0))",
          "null::Nullable(Decimal64(0))"
        )
      )
  case Decimal128
      extends CHType(
        "Decimal128",
        Seq(
          "-999999999999999999999999999999999999::Decimal128(0)",
          "999999999999999999999999999999999999::Decimal128(0)",
          "-0.99999999999999999999999999999999999999::Decimal128(38)",
          "0.99999999999999999999999999999999999999::Decimal128(38)",
          "-999999999999999999999999999999999999::Nullable(Decimal128(0))",
          "null::Nullable(Decimal128(0))"
        )
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
          "9999999999999999999999999999999999999999999999999999999999999999999999999999::Decimal(40)",
          "-999999999999999999999999999999999999999999999999999999999999999999999999::Nullable(Decimal256(0))",
          "null::Nullable(Decimal256(0))"
        )
      )

  // Date
  case Date
      extends CHType(
        "Date",
        Seq(
          "'1970-01-01'::Date",
          "'2149-06-06'::Date",
          "'1970-01-01'::LowCardinality(Date)",
          "'1970-01-01'::Nullable(Date)",
          "null::Nullable(Date)"
        )
      )
  case Date32
      extends CHType(
        "Date32",
        Seq(
          "'1900-01-01'::Date32",
          "'2299-12-31'::Date32",
          "'1900-01-01'::LowCardinality(Date32)",
          "'1900-01-01'::Nullable(Date32)",
          "null::Nullable(Date32)"
        )
      )
  case DateTime
      extends CHType(
        "DateTime",
        Seq(
          "'1970-01-01 00:00:00'::DateTime('Asia/Istanbul')",
          "'2106-02-07 06:28:15'::DateTime('Asia/Istanbul')",
          "'1970-01-01 00:00:00'::DateTime",
          "'2106-02-07 06:28:15'::DateTime",
          "'1970-01-01 00:00:00'::LowCardinality(DateTime)",
          "'1970-01-01 00:00:00'::Nullable(DateTime('Asia/Istanbul'))",
          "null::Nullable(DateTime('Asia/Istanbul'))"
        )
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
          "'2262-04-11 23:47:16.854775807'::DateTime64(9)",
          "'1900-01-01 00:00:00'::Nullable(DateTime64(0, 'Asia/Istanbul'))",
          "null::Nullable(DateTime64(0, 'Asia/Istanbul'))"
        )
      )

  // Misc
  case Enum
      extends CHType(
        "Enum",
        Seq(
          "'hello'::Enum('hello' = 1, 'world' = 2)",
          "'hello'::Enum('hello', 'world')",
          "'hello'::Nullable(Enum('hello' = 1, 'world' = 2))",
          "null::Nullable(Enum('hello' = 1, 'world' = 2))"
        )
      )
  case Enum8
      extends CHType(
        "Enum8",
        Seq(
          "'hello'::Enum8('hello' = -128, 'world' = 2)",
          "'hello'::Enum8('hello' = 127, 'world' = 2)",
          "'hello'::Enum8('hello', 'world')",
          "'hello'::Nullable(Enum8('hello' = -128, 'world' = 2))",
          "null::Nullable(Enum8('hello' = -128, 'world' = 2))"
        )
      )
  case Enum16
      extends CHType(
        "Enum16",
        Seq(
          "'hello'::Enum16('hello' = -32768, 'world' = 2)",
          "'hello'::Enum16('hello' = 32767, 'world' = 2)",
          "'hello'::Enum16('hello', 'world')",
          "'hello'::Nullable(Enum16('hello' = -32768, 'world' = 2))",
          "null::Nullable(Enum16('hello' = -32768, 'world' = 2))"
        )
      )
  case FixedString
      extends CHType(
        "FixedString",
        Seq(
          "'azertyuiop'::FixedString(10)",
          "''::FixedString(1)",
          "'foo'::LowCardinality(FixedString(3))",
          "'azertyuiop'::Nullable(FixedString(10))",
          "null::Nullable(FixedString(10))"
        )
      )
  case Json
      extends CHType(
        "JSON",
        Seq(
          """'{"a": 1, "b": { "c": "foo", "d": [1, 2, 3] }, "c": null}'::JSON"""
        )
      )
  case StringType
      extends CHType(
        "String",
        Seq(
          "'foo'::String",
          "''::String",
          "'foo'::LowCardinality(String)",
          "'foo'::Nullable(String)",
          "null::Nullable(String)"
        )
      )
  case UUID
      extends CHType(
        "UUID",
        Seq(
          "'00000000-0000-0000-0000-000000000000'::UUID",
          "'61f0c404-5cb3-11e7-907b-a6006ad3dba0'::UUID",
          "'00000000-0000-0000-0000-000000000000'::Nullable(UUID)",
          "null::Nullable(UUID)"
        )
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
          s"[${StringType.fuzzingValues.mkString(", ")}]::Array(Nullable(String))"
        )
      )
  case ArrayUUID
      extends CHType(
        "Array(UUID)",
        Seq(
          s"[${UUID.fuzzingValues.mkString(", ")}]::Array(Nullable(UUID))"
        )
      )

  // Tuple1
  case Tuple1Int8
      extends CHType(
        "Tuple(Int8)",
        Seq(
          s"tuple(${Int8.fuzzingValues.head})::Tuple(Int8)"
        )
      )
  case Tuple1Int16
      extends CHType(
        "Tuple(Int16)",
        Seq(
          s"tuple(${Int16.fuzzingValues.head})::Tuple(Int16)"
        )
      )
  case Tuple1Int32
      extends CHType(
        "Tuple(Int32)",
        Seq(
          s"tuple(${Int32.fuzzingValues.head})::Tuple(Int32)"
        )
      )
  case Tuple1Int64
      extends CHType(
        "Tuple(Int64)",
        Seq(
          s"tuple(${Int64.fuzzingValues.head})::Tuple(Int64)"
        )
      )
  case Tuple1Int128
      extends CHType(
        "Tuple(Int128)",
        Seq(
          s"tuple(${Int128.fuzzingValues.head})::Tuple(Int128)"
        )
      )
  case Tuple1Int256
      extends CHType(
        "Tuple(Int256)",
        Seq(
          s"tuple(${Int256.fuzzingValues.head})::Tuple(Int256)"
        )
      )
  case Tuple1UInt8
      extends CHType(
        "Tuple(UInt8)",
        Seq(
          s"tuple(${UInt8.fuzzingValues.head})::Tuple(UInt8)"
        )
      )
  case Tuple1UInt16
      extends CHType(
        "Tuple(UInt16)",
        Seq(
          s"tuple(${UInt16.fuzzingValues.head})::Tuple(UInt16)"
        )
      )
  case Tuple1UInt32
      extends CHType(
        "Tuple(UInt32)",
        Seq(
          s"tuple(${UInt32.fuzzingValues.head})::Tuple(UInt32)"
        )
      )
  case Tuple1UInt64
      extends CHType(
        "Tuple(UInt64)",
        Seq(
          s"tuple(${UInt64.fuzzingValues.head})::Tuple(UInt64)"
        )
      )
  case Tuple1UInt128
      extends CHType(
        "Tuple(UInt128)",
        Seq(
          s"tuple(${UInt128.fuzzingValues.head})::Tuple(UInt128)"
        )
      )
  case Tuple1UInt256
      extends CHType(
        "Tuple(UInt256)",
        Seq(
          s"tuple(${UInt256.fuzzingValues.head})::Tuple(UInt256)"
        )
      )
  case Tuple1Float32
      extends CHType(
        "Tuple(Float32)",
        Seq(
          s"tuple(${Float32.fuzzingValues.head})::Tuple(Float32)"
        )
      )
  case Tuple1Float64
      extends CHType(
        "Tuple(Float64)",
        Seq(
          s"tuple(${Float64.fuzzingValues.head})::Tuple(Float64)"
        )
      )
  case Tuple1Decimal32
      extends CHType(
        "Tuple(Decimal32)",
        Seq(
          s"tuple(${Decimal32.fuzzingValues.head})::Tuple(Decimal32(0))"
        )
      )
  case Tuple1Decimal64
      extends CHType(
        "Tuple(Decimal64)",
        Seq(
          s"tuple(${Decimal64.fuzzingValues.head})::Tuple(Decimal64(0))"
        )
      )
  case Tuple1Decimal128
      extends CHType(
        "Tuple(Decimal128)",
        Seq(
          s"tuple(${Decimal128.fuzzingValues.head})::Tuple(Decimal128(0))"
        )
      )
  case Tuple1Decimal256
      extends CHType(
        "Tuple(Decimal256)",
        Seq(
          s"tuple(${Decimal256.fuzzingValues.head})::Tuple(Decimal256(0))"
        )
      )
  case Tuple1Date
      extends CHType(
        "Tuple(Date)",
        Seq(
          s"tuple(${Date.fuzzingValues.head})::Tuple(Date)"
        )
      )
  case Tuple1Date32
      extends CHType(
        "Tuple(Date32)",
        Seq(
          s"tuple(${Date32.fuzzingValues.head})::Tuple(Date32)"
        )
      )
  case Tuple1DateTime
      extends CHType(
        "Tuple(DateTime)",
        Seq(
          s"tuple(${DateTime.fuzzingValues.head})::Tuple(DateTime)"
        )
      )
  case Tuple1DateTime64
      extends CHType(
        "Tuple(DateTime64)",
        Seq(
          s"tuple(${DateTime64.fuzzingValues.head})::Tuple(DateTime64)"
        )
      )
  case Tuple1Enum
      extends CHType(
        "Tuple(Enum)",
        Seq(
          s"tuple('hello'::Enum('hello' = 1, 'world' = 2))::Tuple(Enum('hello' = 1, 'world' = 2))"
        )
      )
  case Tuple1Enum8
      extends CHType(
        "Tuple(Enum8)",
        Seq(
          s"tuple('hello'::Enum8('hello' = -128, 'world' = 2))::Tuple(Enum8('hello' = -128, 'world' = 2))"
        )
      )
  case Tuple1Enum16
      extends CHType(
        "Tuple(Enum16)",
        Seq(
          s"tuple('hello'::Enum16('hello' = -32768, 'world' = 2))::Tuple(Enum16('hello' = -32768, 'world' = 2))"
        )
      )
  case Tuple1FixedString
      extends CHType(
        "Tuple(FixedString)",
        Seq(
          s"tuple('azertyuiop'::FixedString(10))::Tuple(FixedString(10))"
        )
      )
  case Tuple1Json
      extends CHType(
        "Tuple(JSON)",
        Seq(
          s"tuple(${Json.fuzzingValues.head})::Tuple(JSON)"
        )
      )
  case Tuple1String
      extends CHType(
        "Tuple(String)",
        Seq(
          s"tuple(${StringType.fuzzingValues.head})::Tuple(String)"
        )
      )
  case Tuple1UUID
      extends CHType(
        "Tuple(UUID)",
        Seq(
          s"tuple(${UUID.fuzzingValues.head})::Tuple(UUID)"
        )
      )

  // Special
  case Tuple1ArrayUInt8
      extends CHType(
        "Tuple(Array(UInt8))",
        Seq(
          s"tuple(${ArrayUInt8.fuzzingValues.mkString(", ")})::Tuple(Array(Nullable(UInt8)))"
        )
      )
  case ArrayTuple1UInt8
      extends CHType(
        "Array(Tuple(UInt8))",
        Seq(
          s"[${Tuple1UInt8.fuzzingValues.head}]::Array(Tuple(UInt8))"
        )
      )
}

enum CHAbstractType(val fuzzingValues: Seq[String], val chTypes: Seq[CHType]) {
  case Date extends CHAbstractType(Seq("'1970-01-02'::Date"), Seq(CHType.Date, CHType.Date32))
  case DateTime extends CHAbstractType(Seq("'1970-01-02 00:00:00'::DateTime"), Seq(CHType.DateTime, CHType.DateTime64))
  case Enum extends CHAbstractType(Seq(CHType.Enum.fuzzingValues.head), Seq(CHType.Enum, CHType.Enum8, CHType.Enum16))
  case Json extends CHAbstractType(Seq(CHType.Json.fuzzingValues.head), Seq(CHType.Json))
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

  case String
      extends CHAbstractType(Seq(CHType.StringType.fuzzingValues.head), Seq(CHType.StringType, CHType.FixedString))
  case UUID extends CHAbstractType(Seq(CHType.UUID.fuzzingValues.head), Seq(CHType.UUID))

  // Array
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
  case ArrayEnum
      extends CHAbstractType(
        Seq(s"[${Enum.fuzzingValues.head}]"),
        Seq(CHType.ArrayEnum, CHType.ArrayEnum8, CHType.ArrayEnum16)
      )
  case ArrayJson extends CHAbstractType(Seq(s"[${Json.fuzzingValues.head}]::Array(JSON)"), Seq(CHType.ArrayJson))
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
  case ArrayString
      extends CHAbstractType(
        Seq(s"[${String.fuzzingValues.head}]::Array(String)"),
        Seq(CHType.ArrayFixedString, CHType.ArrayString)
      )
  case ArrayUUID extends CHAbstractType(Seq(s"[${UUID.fuzzingValues.head}]::Array(UUID)"), Seq(CHType.ArrayUUID))

  // Tuple1
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
  case Tuple1Enum
      extends CHAbstractType(
        Seq(s"tuple(${Enum.fuzzingValues.head})::Tuple(Enum('hello' = 1, 'world' = 2))"),
        Seq(CHType.Tuple1Enum, CHType.Tuple1Enum8, CHType.Tuple1Enum16)
      )
  case Tuple1Json extends CHAbstractType(Seq(s"tuple(${Json.fuzzingValues.head})::Tuple(JSON)"), Seq(CHType.Tuple1Json))
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
  case Tuple1String
      extends CHAbstractType(
        Seq(s"tuple(${String.fuzzingValues.head})::Tuple(String)"),
        Seq(CHType.Tuple1FixedString, CHType.Tuple1String)
      )
  case Tuple1UUID extends CHAbstractType(Seq(s"tuple(${UUID.fuzzingValues.head})::Tuple(UUID)"), Seq(CHType.Tuple1UUID))

  // Special
  case Tuple1ArrayNumbers
      extends CHAbstractType(
        Seq(s"tuple(${ArrayNumbers.fuzzingValues.head})::Tuple(Array(UInt8))"),
        Seq(CHType.Tuple1ArrayUInt8)
      )
  case ArrayTuple1Numbers
      extends CHAbstractType(
        Seq(s"[${Tuple1Numbers.fuzzingValues.head}]::Array(Tuple(UInt8))"),
        Seq(CHType.ArrayTuple1UInt8)
      )
}
