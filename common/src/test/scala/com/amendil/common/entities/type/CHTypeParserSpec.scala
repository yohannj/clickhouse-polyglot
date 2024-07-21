package com.amendil.common.entities.`type`

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CHTypeSpec extends AnyFreeSpec with Matchers:

  "CHType" - {
    "should normalize" - {
      "Tuple(UInt64, UInt64)" in {
        val actual = CHType.normalize(CHFuzzableType.Tuple2UInt64UInt64)
        val expected = CHSpecialType.Tuple(Seq(CHFuzzableType.UInt64, CHFuzzableType.UInt64))

        actual shouldBe expected
      }
    }
  }
