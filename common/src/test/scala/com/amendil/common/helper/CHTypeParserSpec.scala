package com.amendil.common.helper

import com.amendil.common.entities.*
import com.amendil.common.entities.`type`.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CHTypeParserSpec extends AnyFreeSpec with Matchers:

  "CHTypeParser" - {
    "should normalize" - {
      "Tuple(UInt64, UInt64)" in {
        val actual = CHType.normalize(CHFuzzableType.Tuple2UInt64UInt64)
        val expected = CHSpecialType.Tuple(Seq(CHFuzzableType.UInt64, CHFuzzableType.UInt64))

        actual shouldBe expected
      }
    }

    "should parse" - {
      "Array(Tuple(bs_9_a String))" in {
        val actual = CHTypeParser.getByName("Array(Tuple(bs_9_a String))")
        val expected = CHSpecialType.Array(CHSpecialType.Tuple(Seq(CHFuzzableType.StringType)))

        actual shouldBe expected
      }

      "Array(Tuple(IntervalMillisecond, IntervalQuarter))" in {
        val actual = CHTypeParser.getByName("Array(Tuple(IntervalMillisecond, IntervalQuarter))")
        val expected = CHSpecialType.Array(
          CHSpecialType.Tuple(Seq(CHFuzzableType.IntervalMillisecond, CHFuzzableType.IntervalQuarter))
        )

        actual shouldBe expected
      }

      "AggregateFunction(groupBitmap, Int8)" in {
        val actual = CHTypeParser.getByName("AggregateFunction(groupBitmap, Int8)")
        val expected = CHFuzzableType.BitmapInt8

        actual shouldBe expected
      }

      "DateTime" in {
        val actual = CHTypeParser.getByName("DateTime")
        val expected = CHFuzzableType.DateTime

        actual shouldBe expected
      }

      "DateTime('Asia/Istanbul')" in {
        val actual = CHTypeParser.getByName("DateTime('Asia/Istanbul')")
        val expected = CHFuzzableType.DateTime

        actual shouldBe expected
      }

      "DateTime64(1)" in {
        val actual = CHTypeParser.getByName("DateTime64(1)")
        val expected = CHFuzzableType.DateTime64

        actual shouldBe expected
      }

      "DateTime64(1, 'Asia/Istanbul')" in {
        val actual = CHTypeParser.getByName("DateTime64(1, 'Asia/Istanbul')")
        val expected = CHFuzzableType.DateTime64

        actual shouldBe expected
      }

      "Decimal(9, 0)" in {
        val actual = CHTypeParser.getByName("Decimal(9, 0)")
        val expected = CHFuzzableType.Decimal32

        actual shouldBe expected
      }

      "Decimal(9, 9)" in {
        val actual = CHTypeParser.getByName("Decimal(9, 9)")
        val expected = CHFuzzableType.Decimal32

        actual shouldBe expected
      }

      "Decimal(18, 0)" in {
        val actual = CHTypeParser.getByName("Decimal(18, 0)")
        val expected = CHFuzzableType.Decimal64

        actual shouldBe expected
      }

      "Decimal(18, 9)" in {
        val actual = CHTypeParser.getByName("Decimal(18, 9)")
        val expected = CHFuzzableType.Decimal64

        actual shouldBe expected
      }

      "Decimal(38, 0)" in {
        val actual = CHTypeParser.getByName("Decimal(38, 0)")
        val expected = CHFuzzableType.Decimal128

        actual shouldBe expected
      }

      "Decimal(38, 9)" in {
        val actual = CHTypeParser.getByName("Decimal(38, 9)")
        val expected = CHFuzzableType.Decimal128

        actual shouldBe expected
      }

      "Decimal(76, 0)" in {
        val actual = CHTypeParser.getByName("Decimal(76, 0)")
        val expected = CHFuzzableType.Decimal256

        actual shouldBe expected
      }

      "Decimal(76, 38)" in {
        val actual = CHTypeParser.getByName("Decimal(76, 38)")
        val expected = CHFuzzableType.Decimal256

        actual shouldBe expected
      }

      "Decimal(76, 9)" in {
        val actual = CHTypeParser.getByName("Decimal(76, 9)")
        val expected = CHFuzzableType.Decimal256

        actual shouldBe expected
      }

      "Enum8('hello' = -128, 'world' = 2)" in {
        val actual = CHTypeParser.getByName("Enum8('hello' = -128, 'world' = 2)")
        val expected = CHFuzzableType.Enum8

        actual shouldBe expected
      }

      "FixedString(1)" in {
        val actual = CHTypeParser.getByName("FixedString(1)")
        val expected = CHFuzzableType.FixedString

        actual shouldBe expected
      }

      "FixedString(16)" in {
        val actual = CHTypeParser.getByName("FixedString(16)")
        val expected = CHFuzzableType.FixedString

        actual shouldBe expected
      }

      "Nullable(Nothing)" in {
        val actual = CHTypeParser.getByName("Nullable(Nothing)")
        val expected = CHSpecialType.Nullable(CHSpecialType.Nothing)

        actual shouldBe expected
      }

      "Tuple()" in {
        val actual = CHTypeParser.getByName("Tuple()")
        val expected = CHSpecialType.Tuple(Nil)

        actual shouldBe expected
      }

      "Tuple(d_statistic Float64)" in {
        val actual = CHTypeParser.getByName("Tuple(d_statistic Float64)")
        val expected = CHSpecialType.Tuple(Seq(CHFuzzableType.Float64))

        actual shouldBe expected
      }

      "Tuple(p_value Float64)" in {
        val actual = CHTypeParser.getByName("Tuple(p_value Float64)")
        val expected = CHSpecialType.Tuple(Seq(CHFuzzableType.Float64))

        actual shouldBe expected
      }

      "Tuple(d_statistic Float64,p_value Float64)" in {
        val actual = CHTypeParser.getByName("Tuple(d_statistic Float64,p_value Float64)")
        val expected = CHSpecialType.Tuple(Seq(CHFuzzableType.Float64, CHFuzzableType.Float64))

        actual shouldBe expected
      }

      "Map(DateTime, Date)" in {
        val actual = CHTypeParser.getByName("Map(DateTime, Date)")
        val expected = CHSpecialType.Map(CHFuzzableType.DateTime, CHFuzzableType.Date)

        actual shouldBe expected
      }
    }
  }
