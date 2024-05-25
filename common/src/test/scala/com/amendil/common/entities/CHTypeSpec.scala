package com.amendil.common.entities

import com.amendil.common.entities.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CHFuzzableTypeSpec extends AnyFreeSpec with Matchers:

  "CHType" - {
    "should parse" - {
      "Array(Tuple(bs_9_a String))" in {
        val actual = CHType.getByName("Array(Tuple(bs_9_a String))")
        val expected = CHSpecialType.Array(CHSpecialType.Tuple(Seq(CHFuzzableType.StringType)))

        actual shouldBe expected
      }

      "Array(Tuple(IntervalMillisecond, IntervalQuarter))" in {
        val actual = CHType.getByName("Array(Tuple(IntervalMillisecond, IntervalQuarter))")
        val expected = CHSpecialType.Array(
          CHSpecialType.Tuple(Seq(CHFuzzableType.IntervalMillisecond, CHFuzzableType.IntervalQuarter))
        )

        actual shouldBe expected
      }

      "AggregateFunction(groupBitmap, Int8)" in {
        val actual = CHType.getByName("AggregateFunction(groupBitmap, Int8)")
        val expected = CHFuzzableType.BitmapInt8

        actual shouldBe expected
      }

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

      "DateTime64(1)" in {
        val actual = CHType.getByName("DateTime64(1)")
        val expected = CHFuzzableType.DateTime64

        actual shouldBe expected
      }

      "DateTime64(1, 'Asia/Istanbul')" in {
        val actual = CHType.getByName("DateTime64(1, 'Asia/Istanbul')")
        val expected = CHFuzzableType.DateTime64

        actual shouldBe expected
      }

      "Decimal(76, 0)" in {
        val actual = CHType.getByName("Decimal(76, 0)")
        val expected = CHFuzzableType.Decimal256

        actual shouldBe expected
      }

      "Decimal(76, 38)" in {
        val actual = CHType.getByName("Decimal(76, 38)")
        val expected = CHFuzzableType.Decimal256

        actual shouldBe expected
      }

      "Decimal(76, 9)" in {
        val actual = CHType.getByName("Decimal(76, 9)")
        val expected = CHFuzzableType.Decimal256

        actual shouldBe expected
      }

      "Enum8('hello' = -128, 'world' = 2)" in {
        val actual = CHType.getByName("Enum8('hello' = -128, 'world' = 2)")
        val expected = CHFuzzableType.Enum8

        actual shouldBe expected
      }

      "FixedString(1)" in {
        val actual = CHType.getByName("FixedString(1)")
        val expected = CHFuzzableType.FixedString

        actual shouldBe expected
      }

      "FixedString(16)" in {
        val actual = CHType.getByName("FixedString(16)")
        val expected = CHFuzzableType.FixedString

        actual shouldBe expected
      }

      "Nullable(Nothing)" in {
        val actual = CHType.getByName("Nullable(Nothing)")
        val expected = CHSpecialType.Nullable(CHSpecialType.Nothing)

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
