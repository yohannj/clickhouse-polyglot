package com.amendil

enum CHType(val name: String, val fuzzingValues: Seq[String]) {

  // Numbers
  case Int8 extends CHType("Int8", Seq("-128::Int8", "127::Int8", "-128::LowCardinality(Int8)"))
  case Int16 extends CHType("Int16", Seq("-32768::Int16", "32767::Int16", "-32768::LowCardinality(Int16)"))
  case Int32
      extends CHType("Int32", Seq("-2147483648::Int32", "2147483647::Int32", "-2147483648::LowCardinality(Int32)"))
  case Int64
      extends CHType(
        "Int64",
        Seq("-9223372036854775808::Int64", "9223372036854775807::Int64", "-9223372036854775808::LowCardinality(Int64)")
      )
  case Int128
      extends CHType(
        "Int128",
        Seq(
          "-170141183460469231731687303715884105728::Int128",
          "170141183460469231731687303715884105727::Int128",
          "-170141183460469231731687303715884105728::LowCardinality(Int128)"
        )
      )
  case Int256
      extends CHType(
        "Int256",
        Seq(
          "-57896044618658097711785492504343953926634992332820282019728792003956564819968::Int256",
          "57896044618658097711785492504343953926634992332820282019728792003956564819967::Int256",
          "-57896044618658097711785492504343953926634992332820282019728792003956564819968::LowCardinality(Int256)"
        )
      )
  case UInt8 extends CHType("UInt8", Seq("0::UInt8", "1::UInt8", "255::UInt8", "0::LowCardinality(UInt8)"))
  case UInt16 extends CHType("UInt16", Seq("0::UInt16", "65535::UInt16", "0::LowCardinality(UInt16)"))
  case UInt32 extends CHType("UInt32", Seq("0::UInt32", "4294967295::UInt32", "0::LowCardinality(UInt32)"))
  case UInt64 extends CHType("UInt64", Seq("0::UInt64", "18446744073709551615::UInt64", "0::LowCardinality(UInt64)"))
  case UInt128
      extends CHType(
        "UInt128",
        Seq("0::UInt128", "340282366920938463463374607431768211455::UInt128", "0::LowCardinality(UInt128)")
      )
  case UInt256
      extends CHType(
        "UInt256",
        Seq(
          "0::UInt256",
          "115792089237316195423570985008687907853269984665640564039457584007913129639935::UInt256",
          "0::LowCardinality(UInt256)"
        )
      )

  case Float32
      extends CHType("Float32", Seq("-inf::Float32", "nan::Float32", "0.5::Float32", "-inf::LowCardinality(Float32)"))
  case Float64
      extends CHType("Float64", Seq("-inf::Float64", "nan::Float64", "0.5::Float64", "-inf::LowCardinality(Float64)"))
  case Decimal32
      extends CHType(
        "Decimal32",
        Seq(
          "-999999999::Decimal32(0)",
          "999999999::Decimal32(0)",
          "-0.999999999::Decimal32(9)",
          "0.999999999::Decimal32(9)"
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
          "999999999999999999::Decimal"
        )
      )
  case Decimal128
      extends CHType(
        "Decimal128",
        Seq(
          "-999999999999999999999999999999999999::Decimal128(0)",
          "999999999999999999999999999999999999::Decimal128(0)",
          "-0.99999999999999999999999999999999999999::Decimal128(38)",
          "0.99999999999999999999999999999999999999::Decimal128(38)"
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
          "9999999999999999999999999999999999999999999999999999999999999999999999999999::Decimal(40)"
        )
      )

  // Date
  case Date
      extends CHType("Date", Seq("'1970-01-01'::Date", "'2149-06-06'::Date", "'1970-01-01'::LowCardinality(Date)"))
  case Date32
      extends CHType(
        "Date32",
        Seq("'1900-01-01'::Date32", "'2299-12-31'::Date32", "'1900-01-01'::LowCardinality(Date32)")
      )
  case DateTime
      extends CHType(
        "DateTime",
        Seq(
          "'1970-01-01 00:00:00'::DateTime('Asia/Istanbul')",
          "'2106-02-07 06:28:15'::DateTime('Asia/Istanbul')",
          "'1970-01-01 00:00:00'::DateTime",
          "'2106-02-07 06:28:15'::DateTime",
          "'1970-01-01 00:00:00'::LowCardinality(DateTime)"
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
          "'2262-04-11 23:47:16.854775807'::LowCardinality(DateTime64(9))"
        )
      )

  // Misc
  case Enum extends CHType("Enum", Seq("'hello'::Enum('hello' = 1, 'world' = 2)", "'hello'::Enum('hello', 'world')"))
  case Enum8
      extends CHType(
        "Enum8",
        Seq(
          "'hello'::Enum8('hello' = -128, 'world' = 2)",
          "'hello'::Enum8('hello' = 127, 'world' = 2)",
          "'hello'::Enum8('hello', 'world')"
        )
      )
  case Enum16
      extends CHType(
        "Enum16",
        Seq(
          "'hello'::Enum16('hello' = -32768, 'world' = 2)",
          "'hello'::Enum16('hello' = 32767, 'world' = 2)",
          "'hello'::Enum16('hello', 'world')"
        )
      )
  case FixedString
      extends CHType(
        "FixedString",
        Seq("'azertyuiop'::FixedString(10)", "''::FixedString(1)", "'foo'::LowCardinality(FixedString(3))")
      )
  case Json extends CHType("JSON", Seq("""'{"a": 1, "b": { "c": "foo", "d": [1, 2, 3] }, "c": null}'::JSON"""))
  case StringType extends CHType("String", Seq("'foo'::String", "''::String", "'foo'::LowCardinality(String)"))
  case UUID
      extends CHType(
        "UUID",
        Seq("'00000000-0000-0000-0000-000000000000'::UUID", "'61f0c404-5cb3-11e7-907b-a6006ad3dba0'::UUID")
      )

  // Array
  case ArrayInt8 extends CHType("Array(Int8)", Seq(s"[${Int8.fuzzingValues.mkString(", ")}]"))
  case ArrayInt16 extends CHType("Array(Int16)", Seq(s"[${Int16.fuzzingValues.mkString(", ")}]"))
  case ArrayInt32 extends CHType("Array(Int32)", Seq(s"[${Int32.fuzzingValues.mkString(", ")}]"))
  case ArrayInt64 extends CHType("Array(Int64)", Seq(s"[${Int64.fuzzingValues.mkString(", ")}]"))
  case ArrayInt128 extends CHType("Array(Int128)", Seq(s"[${Int128.fuzzingValues.mkString(", ")}]"))
  case ArrayInt256 extends CHType("Array(Int256)", Seq(s"[${Int256.fuzzingValues.mkString(", ")}]"))
  case ArrayUInt8 extends CHType("Array(UInt8)", Seq(s"[${UInt8.fuzzingValues.mkString(", ")}]"))
  case ArrayUInt16 extends CHType("Array(UInt16)", Seq(s"[${UInt16.fuzzingValues.mkString(", ")}]"))
  case ArrayUInt32 extends CHType("Array(UInt32)", Seq(s"[${UInt32.fuzzingValues.mkString(", ")}]"))
  case ArrayUInt64 extends CHType("Array(UInt64)", Seq(s"[${UInt64.fuzzingValues.mkString(", ")}]"))
  case ArrayUInt128 extends CHType("Array(UInt128)", Seq(s"[${UInt128.fuzzingValues.mkString(", ")}]"))
  case ArrayUInt256 extends CHType("Array(UInt256)", Seq(s"[${UInt256.fuzzingValues.mkString(", ")}]"))
  case ArrayFloat32 extends CHType("Array(Float32)", Seq(s"[${Float32.fuzzingValues.mkString(", ")}]"))
  case ArrayFloat64 extends CHType("Array(Float64)", Seq(s"[${Float64.fuzzingValues.mkString(", ")}]"))
  case ArrayDecimal32 extends CHType("Array(Decimal32)", Seq(s"[${Decimal32.fuzzingValues.mkString(", ")}]"))
  case ArrayDecimal64 extends CHType("Array(Decimal64)", Seq(s"[${Decimal64.fuzzingValues.mkString(", ")}]"))
  case ArrayDecimal128 extends CHType("Array(Decimal128)", Seq(s"[${Decimal128.fuzzingValues.mkString(", ")}]"))
  case ArrayDecimal256 extends CHType("Array(Decimal256)", Seq(s"[${Decimal256.fuzzingValues.mkString(", ")}]"))
  case ArrayDate extends CHType("Array(Date)", Seq(s"[${Date.fuzzingValues.mkString(", ")}]"))
  case ArrayDate32 extends CHType("Array(Date32)", Seq(s"[${Date32.fuzzingValues.mkString(", ")}]"))
  case ArrayDateTime extends CHType("Array(DateTime)", Seq(s"[${DateTime.fuzzingValues.mkString(", ")}]"))
  case ArrayDateTime64 extends CHType("Array(DateTime64)", Seq(s"[${DateTime64.fuzzingValues.mkString(", ")}]"))
  case ArrayEnum extends CHType("Array(Enum)", Seq(s"[${Enum.fuzzingValues.mkString(", ")}]"))
  case ArrayEnum8 extends CHType("Array(Enum8)", Seq(s"[${Enum8.fuzzingValues.mkString(", ")}]"))
  case ArrayEnum16 extends CHType("Array(Enum16)", Seq(s"[${Enum16.fuzzingValues.mkString(", ")}]"))
  case ArrayFixedString extends CHType("Array(FixedString)", Seq(s"[${FixedString.fuzzingValues.mkString(", ")}]"))
  case ArrayJson extends CHType("Array(JSON)", Seq(s"[${Json.fuzzingValues.mkString(", ")}]"))
  case ArrayString extends CHType("Array(String)", Seq(s"[${StringType.fuzzingValues.mkString(", ")}]"))
  case ArrayUUID extends CHType("Array(UUID)", Seq(s"[${UUID.fuzzingValues.mkString(", ")}]"))
}

enum CHAbstractType(val fuzzingValue: Any, val chTypes: Seq[CHType]) {
  case Date extends CHAbstractType("'1970-01-02'::Date", Seq(CHType.Date, CHType.Date32))
  case DateTime extends CHAbstractType("'1970-01-02 00:00:00'::DateTime", Seq(CHType.DateTime, CHType.DateTime64))
  case Enum extends CHAbstractType(CHType.Enum.fuzzingValues.head, Seq(CHType.Enum, CHType.Enum8, CHType.Enum16))
  case Json extends CHAbstractType(CHType.Json.fuzzingValues.head, Seq(CHType.Json))
  case Numbers
      extends CHAbstractType(
        1,
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

  case String extends CHAbstractType(CHType.StringType.fuzzingValues.head, Seq(CHType.StringType, CHType.FixedString))
  case UUID extends CHAbstractType(CHType.UUID.fuzzingValues.head, Seq(CHType.UUID))

  case ArrayDate extends CHAbstractType(s"[${Date.fuzzingValue}]", Seq(CHType.ArrayDate, CHType.ArrayDate32))
  case ArrayDateTime
      extends CHAbstractType(s"[${DateTime.fuzzingValue}]", Seq(CHType.ArrayDateTime, CHType.ArrayDateTime64))
  case ArrayEnum
      extends CHAbstractType(s"[${Enum.fuzzingValue}]", Seq(CHType.ArrayEnum, CHType.ArrayEnum8, CHType.ArrayEnum16))
  case ArrayJson extends CHAbstractType(s"[${Json.fuzzingValue}]", Seq(CHType.ArrayJson))
  case ArrayNumbers
      extends CHAbstractType(
        s"[${Numbers.fuzzingValue}]",
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
  case ArrayString extends CHAbstractType(s"[${String.fuzzingValue}]", Seq(CHType.ArrayFixedString, CHType.ArrayString))
  case ArrayUUID extends CHAbstractType(s"[${UUID.fuzzingValue}]", Seq(CHType.ArrayUUID))
}
