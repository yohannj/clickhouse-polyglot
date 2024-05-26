package com.amendil.common.entities

import com.amendil.common.entities.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CHFuzzableTypeSpec extends AnyFreeSpec with Matchers:

  "CHType" - {
    "should merge" - {
      "input types" - {
        "to DecimalLike" in {
          val actual = CHType.mergeInputTypes(CHFuzzableType.values.filter(_.name.startsWith("Decimal")).toSet)
          val expected = Set(CHAggregatedType.DecimalLike)

          actual shouldBe expected
        }
        "to Float" in {
          val actual = CHType.mergeInputTypes(Set(CHFuzzableType.Float32, CHFuzzableType.Float64))
          val expected = Set(CHAggregatedType.Float)

          actual shouldBe expected
        }
        "to Int" in {
          val intTypes = CHFuzzableType.values.filter(_.name.startsWith("Int")).filterNot(_.name.startsWith("Interval"))
          val actual1 = CHType.mergeInputTypes(intTypes.toSet)
          val actual2 = CHType.mergeInputTypes(intTypes.toSet + CHFuzzableType.BooleanType)
          val expected = Set(CHAggregatedType.Int)

          actual1 shouldBe expected
          actual2 shouldBe expected
        }
        "to IntMax64Bits" in {
          val actual1 = CHType.mergeInputTypes(
            Set(CHFuzzableType.Int8, CHFuzzableType.Int16, CHFuzzableType.Int32, CHFuzzableType.Int64)
          )
          val actual2 = CHType.mergeInputTypes(
            Set(
              CHFuzzableType.BooleanType,
              CHFuzzableType.Int8,
              CHFuzzableType.Int16,
              CHFuzzableType.Int32,
              CHFuzzableType.Int64
            )
          )
          val expected = Set(CHAggregatedType.IntMax64Bits)

          actual1 shouldBe expected
          actual2 shouldBe expected
        }
        "to NonDecimal" in {
          val actual1 = CHType.mergeInputTypes(Set(CHAggregatedType.Int, CHAggregatedType.UInt, CHAggregatedType.Float))
          val actual2 = CHType.mergeInputTypes(
            Set(
              CHAggregatedType.NonDecimalNorFloatMax64Bits,
              CHFuzzableType.Int128,
              CHFuzzableType.Int256,
              CHFuzzableType.UInt128,
              CHFuzzableType.UInt256,
              CHAggregatedType.Float
            )
          )
          val actual3 = CHType.mergeInputTypes(
            Set(
              CHAggregatedType.NonDecimalMax64Bits,
              CHFuzzableType.Int128,
              CHFuzzableType.Int256,
              CHFuzzableType.UInt128,
              CHFuzzableType.UInt256
            )
          )
          val expected = Set(CHAggregatedType.NonDecimal)

          actual1 shouldBe expected
          actual2 shouldBe expected
          actual3 shouldBe expected
        }
        "to NonDecimalMax64Bits" in {
          val actual1 = CHType.mergeInputTypes(
            Set(CHAggregatedType.IntMax64Bits, CHAggregatedType.UIntMax64Bits, CHAggregatedType.Float)
          )
          val actual2 =
            CHType.mergeInputTypes(Set(CHAggregatedType.NonDecimalNorFloatMax64Bits, CHAggregatedType.Float))
          val expected = Set(CHAggregatedType.NonDecimalMax64Bits)

          actual1 shouldBe expected
          actual2 shouldBe expected
        }
        "to NonDecimalNorFloatMax64Bits" in {
          val actual = CHType.mergeInputTypes(Set(CHAggregatedType.IntMax64Bits, CHAggregatedType.UIntMax64Bits))
          val expected = Set(CHAggregatedType.NonDecimalNorFloatMax64Bits)

          actual shouldBe expected
        }
        "to Number" in {
          val actual = CHType.mergeInputTypes(Set(CHAggregatedType.NonDecimal, CHAggregatedType.DecimalLike))
          val expected = Set(CHAggregatedType.Number)

          actual shouldBe expected
        }
        "to UInt" in {
          val actual1 = CHType.mergeInputTypes(CHFuzzableType.values.filter(_.name.startsWith("UInt")).toSet)
          val actual2 =
            CHType.mergeInputTypes(Set(CHAggregatedType.UIntMax64Bits, CHFuzzableType.UInt128, CHFuzzableType.UInt256))
          val expected = Set(CHAggregatedType.UInt)

          actual1 shouldBe expected
          actual2 shouldBe expected
        }
        "to UIntMax64Bits" in {
          val actual = CHType.mergeInputTypes(
            Set(CHFuzzableType.UInt8, CHFuzzableType.UInt16, CHFuzzableType.UInt32, CHFuzzableType.UInt64)
          )
          val expected = Set(CHAggregatedType.UIntMax64Bits)

          actual shouldBe expected
        }
      }
    }

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
