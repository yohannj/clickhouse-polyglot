package com.amendil.common.entities

import com.amendil.common.entities._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CHFuzzableTypeSpec extends AnyFreeSpec with Matchers {

  "CHType" - {
    "should parse" - {
      "DateTime" in {
        val actual = CHType.getByName("DateTime")
        val expected = CHFuzzableType.DateTime

        actual shouldBe expected
      }

      "DateTime('Asia/Istanbul')" in {
        val actual = CHType.getByName("DateTime('Asia/Istanbul')")
        val expected = CHFuzzableType.DateTime

        actual shouldBe expected
      }

      "Tuple(d_statistic Float64)" in {
        val actual = CHType.getByName("Tuple(d_statistic Float64)")
        val expected = CHSpecialType.Tuple(Seq(CHFuzzableType.Float64))

        actual shouldBe expected
      }

      "Tuple(p_value Float64)" in {
        val actual = CHType.getByName("Tuple(p_value Float64)")
        val expected = CHSpecialType.Tuple(Seq(CHFuzzableType.Float64))

        actual shouldBe expected
      }

      "Tuple(d_statistic Float64,p_value Float64)" in {
        val actual = CHType.getByName("Tuple(d_statistic Float64,p_value Float64)")
        val expected = CHSpecialType.Tuple(Seq(CHFuzzableType.Float64, CHFuzzableType.Float64))

        actual shouldBe expected
      }

      "Map(DateTime, Date)" in {
        val actual = CHType.getByName("Map(DateTime, Date)")
        val expected = CHSpecialType.Map(CHFuzzableType.DateTime, CHFuzzableType.Date)

        actual shouldBe expected
      }
    }
  }
}
