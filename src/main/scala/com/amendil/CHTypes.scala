package com.amendil

enum CHType(val name: String, val fuzzingValues: Seq[String]) {

  // Numbers
  case Int8 extends CHType("Int8", Seq("-128", "127"))
  case Int16 extends CHType("Int16", Seq("-32768", "32767"))
  case Int32 extends CHType("Int32", Seq("-2147483648", "2147483647"))
  case Int64 extends CHType("Int64", Seq("-9223372036854775808", "9223372036854775807"))
  case Int128
      extends CHType(
        "Int128",
        Seq("-170141183460469231731687303715884105728", "170141183460469231731687303715884105727")
      )
  case Int256
      extends CHType(
        "Int256",
        Seq(
          "-57896044618658097711785492504343953926634992332820282019728792003956564819968",
          "57896044618658097711785492504343953926634992332820282019728792003956564819967"
        )
      )
  case UInt8 extends CHType("UInt8", Seq("0", "1", "255"))
  case UInt16 extends CHType("UInt16", Seq("0", "65535"))
  case UInt32 extends CHType("UInt32", Seq("0", "4294967295"))
  case UInt64 extends CHType("UInt64", Seq("0", "18446744073709551615"))
  case UInt128 extends CHType("UInt128", Seq("0", "340282366920938463463374607431768211455"))
  case UInt256
      extends CHType(
        "UInt256",
        Seq("0", "115792089237316195423570985008687907853269984665640564039457584007913129639935")
      )

  case Float32 extends CHType("Float32", Seq("-inf", "nan", "0.5"))
  case Float64 extends CHType("Float64", Seq("-inf", "nan", "0.5"))

  // Others
  case StringType extends CHType("String", Seq("'foo'"))
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
          CHType.Float64
        )
      )

  case String extends CHAbstractTypes(CHType.StringType.fuzzingValues.head, Seq(CHType.StringType))
}
