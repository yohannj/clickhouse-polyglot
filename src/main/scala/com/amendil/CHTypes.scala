package com.amendil

enum CHType(val name: String, val fuzzingValues: Seq[String]) {

  // Numbers
  case Int8 extends CHType("Int8", Seq("-128::Int8", "127::Int8"))
  case Int16 extends CHType("Int16", Seq("-32768::Int16", "32767::Int16"))
  case Int32 extends CHType("Int32", Seq("-2147483648::Int32", "2147483647::Int32"))
  case Int64 extends CHType("Int64", Seq("-9223372036854775808::Int64", "9223372036854775807::Int64"))
  case Int128
      extends CHType(
        "Int128",
        Seq("-170141183460469231731687303715884105728::Int128", "170141183460469231731687303715884105727::Int128")
      )
  case Int256
      extends CHType(
        "Int256",
        Seq(
          "-57896044618658097711785492504343953926634992332820282019728792003956564819968::Int256",
          "57896044618658097711785492504343953926634992332820282019728792003956564819967::Int256"
        )
      )
  case UInt8 extends CHType("UInt8", Seq("0::UInt8", "1::UInt8", "255::UInt8"))
  case UInt16 extends CHType("UInt16", Seq("0::UInt16", "65535::UInt16"))
  case UInt32 extends CHType("UInt32", Seq("0::UInt32", "4294967295::UInt32"))
  case UInt64 extends CHType("UInt64", Seq("0::UInt64", "18446744073709551615::UInt64"))
  case UInt128 extends CHType("UInt128", Seq("0::UInt128", "340282366920938463463374607431768211455::UInt128"))
  case UInt256
      extends CHType(
        "UInt256",
        Seq("0::UInt256", "115792089237316195423570985008687907853269984665640564039457584007913129639935::UInt256")
      )

  case Float32 extends CHType("Float32", Seq("-inf::Float32", "nan::Float32", "0.5::Float32"))
  case Float64 extends CHType("Float64", Seq("-inf::Float64", "nan::Float64", "0.5::Float64"))
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

  // Others
  case FixedString extends CHType("FixedString", Seq("'azertyuiop'::FixedString(10)"))
  case StringType extends CHType("String", Seq("'foo'::String"))
  case UUID
      extends CHType(
        "UUID",
        Seq("'00000000-0000-0000-0000-000000000000'::UUID", "'61f0c404-5cb3-11e7-907b-a6006ad3dba0'::UUID")
      )
}

enum CHAbstractTypes(val fuzzingValue: Any, val chTypes: Seq[CHType]) {
  case Numbers
      extends CHAbstractTypes(
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

  case String extends CHAbstractTypes(CHType.StringType.fuzzingValues.head, Seq(CHType.StringType, CHType.FixedString))
  case UUID extends CHAbstractTypes(CHType.UUID.fuzzingValues.head, Seq(CHType.UUID))
}
