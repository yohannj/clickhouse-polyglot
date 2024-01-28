package com.amendil

enum CHTypes[T](val name: String, val fuzzingValues: Seq[T]) {
  case Int8 extends CHTypes[Short]("Int8", Seq(-128, 127))
  case Int16 extends CHTypes[Short]("Int16", Seq(-32768, 32767))
  case Int32 extends CHTypes[Int]("Int32", Seq(-2147483648, 2147483647))
  case Int64 extends CHTypes[Long]("Int64", Seq(-9223372036854775808L, 9223372036854775807L))
  case Int128
      extends CHTypes[BigInt](
        "Int128",
        Seq(BigInt("-170141183460469231731687303715884105728"), BigInt("170141183460469231731687303715884105727"))
      )
  case Int256
      extends CHTypes[BigInt](
        "Int256",
        Seq(
          BigInt("-57896044618658097711785492504343953926634992332820282019728792003956564819968"),
          BigInt("57896044618658097711785492504343953926634992332820282019728792003956564819967")
        )
      )
  case UInt8 extends CHTypes[Short]("UInt8", Seq(0, 1, 255))
  case UInt16 extends CHTypes[Int]("UInt16", Seq(0, 65535))
  case UInt32 extends CHTypes[Long]("UInt32", Seq(0, 4294967295L))
  case UInt64 extends CHTypes[BigInt]("UInt64", Seq(0, BigInt("18446744073709551615")))
  case UInt128 extends CHTypes[BigInt]("UInt128", Seq(0, BigInt("340282366920938463463374607431768211455")))
  case UInt256
      extends CHTypes[BigInt](
        "UInt256",
        Seq(0, BigInt("115792089237316195423570985008687907853269984665640564039457584007913129639935"))
      )
}
