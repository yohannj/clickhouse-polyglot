package com.amendil.signature.entities

import com.amendil.common.entities.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CHFunctionIOSpec extends AnyFreeSpec with Matchers:

  "CHFunctionIO" - {
    "should aggregate Function1 signatures" - {
      "having the same output type" - {
        val outputType = CHSpecialType.GenericType("foo")

        "when they accept all numbers" in {
          val actual = CHFunctionIO.aggregate(
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
              CHFuzzableType.Decimal256
            ).map(CHFunctionIO.Function1(_, outputType))
          )
          val expected = Seq(CHFunctionIO.Function1(CHAggregatedType.Number, outputType))

          actual shouldBe expected
        }

        // "when they accept all numbers" in {
        //   CHFunctionIO.aggregate(Seq(
        //     ???
        //   ))
        //   val actual = CHFuzzableAbstractType.values.flatMap(_.chFuzzableTypes)
        //   val expected = CHFuzzableType.values

        //   expected.toSet.diff(actual.toSet) shouldBe Set.empty
        // }
      }
    }
  }
