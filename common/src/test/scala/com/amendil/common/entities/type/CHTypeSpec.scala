package com.amendil.common.entities.`type`

import com.amendil.common.entities.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.nowarn
import scala.util.Try

class CHFuzzableTypeSpec extends AnyFreeSpec with Matchers:

  "CHType" - {
    "should merge" - {
      "input types" - {
        "to Any" in {
          val actual = CHType.mergeInputTypes(CHFuzzableType.values.toSet - CHFuzzableType.Variant)
          val expected = Set(CHAggregatedType.Any)

          actual shouldBe expected
        }
        "to AnyNonNullableNonLowCardinality" in {
          val actual = CHType.mergeInputTypes(
            CHFuzzableType.values
              .filterNot(_.name.contains("Nullable"))
              .filterNot(_.name.contains("LowCardinality"))
              .toSet - CHFuzzableType.Variant
          )
          val expected = Set(CHAggregatedType.AnyNonNullableNonLowCardinality)

          actual shouldBe expected
        }
        "of type array" - {
          "to Array(Any)" in {
            val actual = CHType.mergeInputTypes(CHFuzzableType.values.filter(_.name.startsWith("Array(")).toSet)
            val expected = Set(CHSpecialType.Array(CHAggregatedType.Any))

            actual shouldBe expected
          }

          "to Array(DecimalLike)" in {
            val actual = CHType.mergeInputTypes(CHFuzzableType.values.filter(_.name.startsWith("Array(Decimal")).toSet)
            val expected = Set(CHSpecialType.Array(CHAggregatedType.DecimalLike))

            actual shouldBe expected
          }
          "to Array(Float)" in {
            val actual = CHType.mergeInputTypes(Set(CHFuzzableType.ArrayFloat32, CHFuzzableType.ArrayFloat64))
            val expected = Set(CHSpecialType.Array(CHAggregatedType.Float))

            actual shouldBe expected
          }
          "to Array(Int)" in {
            val intTypes = CHFuzzableType.values
              .filter(_.name.startsWith("Array(Int"))
              .filterNot(_.name.startsWith("Array(Interval"))
            val actual1 = CHType.mergeInputTypes(intTypes.toSet)
            val actual2 = CHType.mergeInputTypes(intTypes.toSet + CHFuzzableType.ArrayBoolean)
            val expected = Set(CHSpecialType.Array(CHAggregatedType.Int))

            actual1 shouldBe expected
            actual2 shouldBe expected
          }
          "to Array(IntMax64Bits)" in {
            val actual1 = CHType.mergeInputTypes(
              Set(
                CHFuzzableType.ArrayInt8,
                CHFuzzableType.ArrayInt16,
                CHFuzzableType.ArrayInt32,
                CHFuzzableType.ArrayInt64
              )
            )
            val actual2 = CHType.mergeInputTypes(
              Set(
                CHFuzzableType.ArrayBoolean,
                CHFuzzableType.ArrayInt8,
                CHFuzzableType.ArrayInt16,
                CHFuzzableType.ArrayInt32,
                CHFuzzableType.ArrayInt64
              )
            )
            val expected = Set(CHSpecialType.Array(CHAggregatedType.IntMax64Bits))

            actual1 shouldBe expected
            actual2 shouldBe expected
          }
          "to Array(NonDecimal)" in {
            val actual1 = CHType.mergeInputTypes(
              Set(
                CHSpecialType.Array(CHAggregatedType.Int),
                CHSpecialType.Array(CHAggregatedType.UInt),
                CHSpecialType.Array(CHAggregatedType.Float)
              )
            )
            val actual2 = CHType.mergeInputTypes(
              Set(
                CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax64Bits),
                CHFuzzableType.ArrayInt128,
                CHFuzzableType.ArrayInt256,
                CHFuzzableType.ArrayUInt128,
                CHFuzzableType.ArrayUInt256,
                CHSpecialType.Array(CHAggregatedType.Float)
              )
            )
            val actual3 = CHType.mergeInputTypes(
              Set(
                CHSpecialType.Array(CHAggregatedType.NonDecimalMax64Bits),
                CHFuzzableType.ArrayInt128,
                CHFuzzableType.ArrayInt256,
                CHFuzzableType.ArrayUInt128,
                CHFuzzableType.ArrayUInt256
              )
            )
            val expected = Set(CHSpecialType.Array(CHAggregatedType.NonDecimal))

            actual1 shouldBe expected
            actual2 shouldBe expected
            actual3 shouldBe expected
          }
          "to Array(NonDecimalMax64Bits)" in {
            val actual1 = CHType.mergeInputTypes(
              Set(
                CHSpecialType.Array(CHAggregatedType.IntMax64Bits),
                CHSpecialType.Array(CHAggregatedType.UIntMax64Bits),
                CHSpecialType.Array(CHAggregatedType.Float)
              )
            )
            val actual2 = CHType.mergeInputTypes(
              Set(
                CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax64Bits),
                CHSpecialType.Array(CHAggregatedType.Float)
              )
            )
            val expected = Set(CHSpecialType.Array(CHAggregatedType.NonDecimalMax64Bits))

            actual1 shouldBe expected
            actual2 shouldBe expected
          }
          "to Array(NonDecimalNorFloatMax64Bits)" in {
            val actual = CHType.mergeInputTypes(
              Set(
                CHSpecialType.Array(CHAggregatedType.IntMax64Bits),
                CHSpecialType.Array(CHAggregatedType.UIntMax64Bits)
              )
            )
            val expected = Set(CHSpecialType.Array(CHAggregatedType.NonDecimalNorFloatMax64Bits))

            actual shouldBe expected
          }
          "to Array(Number)" in {
            val actual = CHType.mergeInputTypes(
              Set(CHSpecialType.Array(CHAggregatedType.NonDecimal), CHSpecialType.Array(CHAggregatedType.DecimalLike))
            )
            val expected = Set(CHSpecialType.Array(CHAggregatedType.Number))

            actual shouldBe expected
          }
          "to Array(UInt)" in {
            val actual1 = CHType.mergeInputTypes(CHFuzzableType.values.filter(_.name.startsWith("Array(UInt")).toSet)
            val actual2 =
              CHType.mergeInputTypes(
                Set(
                  CHSpecialType.Array(CHAggregatedType.UIntMax64Bits),
                  CHFuzzableType.ArrayUInt128,
                  CHFuzzableType.ArrayUInt256
                )
              )
            val expected = Set(CHSpecialType.Array(CHAggregatedType.UInt))

            actual1 shouldBe expected
            actual2 shouldBe expected
          }
          "to Array(UIntMax64Bits)" in {
            val actual = CHType.mergeInputTypes(
              Set(
                CHFuzzableType.ArrayUInt8,
                CHFuzzableType.ArrayUInt16,
                CHFuzzableType.ArrayUInt32,
                CHFuzzableType.ArrayUInt64
              )
            )
            val expected = Set(CHSpecialType.Array(CHAggregatedType.UIntMax64Bits))

            actual shouldBe expected
          }
        }

        "of type Map" - {
          "to Map(MapKey, Int)" in {
            val actual = CHType.mergeInputTypes(CHFuzzableType.values.filter(_.name.startsWith("Map(")).toSet)
            val expected = Set(CHSpecialType.Map(CHAggregatedType.MapKey, CHAggregatedType.Int))

            actual shouldBe expected
          }

          "to Map(Int, Int)" in {
            val intTypes =
              CHFuzzableType.values.filter(_.name.startsWith("Map(Int")).filterNot(_.name.startsWith("Map(Interval"))
            val actual1 = CHType.mergeInputTypes(intTypes.toSet)
            val actual2 = CHType.mergeInputTypes(intTypes.toSet + CHFuzzableType.MapBooleanInt)
            val expected = Set(CHSpecialType.Map(CHAggregatedType.Int, CHAggregatedType.Int))

            actual1 shouldBe expected
            actual2 shouldBe expected
          }
          "to Map(IntMax64Bits, Int)" in {
            val actual1 = CHType.mergeInputTypes(
              Set(
                CHFuzzableType.MapInt8Int,
                CHFuzzableType.MapInt16Int,
                CHFuzzableType.MapInt32Int,
                CHFuzzableType.MapInt64Int
              )
            )
            val actual2 = CHType.mergeInputTypes(
              Set(
                CHFuzzableType.MapBooleanInt,
                CHFuzzableType.MapInt8Int,
                CHFuzzableType.MapInt16Int,
                CHFuzzableType.MapInt32Int,
                CHFuzzableType.MapInt64Int
              )
            )
            val expected = Set(CHSpecialType.Map(CHAggregatedType.IntMax64Bits, CHAggregatedType.Int))

            actual1 shouldBe expected
            actual2 shouldBe expected
          }
          "to Map(NonDecimalNorFloat, Int)" in {
            val actual1 = CHType.mergeInputTypes(
              Set(
                CHSpecialType.Map(CHAggregatedType.Int, CHAggregatedType.Int),
                CHSpecialType.Map(CHAggregatedType.UInt, CHAggregatedType.Int)
              )
            )
            val actual2 = CHType.mergeInputTypes(
              Set(
                CHSpecialType.Map(CHAggregatedType.NonDecimalNorFloatMax64Bits, CHAggregatedType.Int),
                CHFuzzableType.MapInt128Int,
                CHFuzzableType.MapInt256Int,
                CHFuzzableType.MapUInt128Int,
                CHFuzzableType.MapUInt256Int
              )
            )
            val expected = Set(CHSpecialType.Map(CHAggregatedType.NonDecimalNorFloat, CHAggregatedType.Int))

            actual1 shouldBe expected
            actual2 shouldBe expected
          }
          "to Map(NonDecimalNorFloatMax64Bits, Int)" in {
            val actual = CHType.mergeInputTypes(
              Set(
                CHSpecialType.Map(CHAggregatedType.IntMax64Bits, CHAggregatedType.Int),
                CHSpecialType.Map(CHAggregatedType.UIntMax64Bits, CHAggregatedType.Int)
              )
            )
            val expected = Set(CHSpecialType.Map(CHAggregatedType.NonDecimalNorFloatMax64Bits, CHAggregatedType.Int))

            actual shouldBe expected
          }
          "to Map(UInt, Int)" in {
            val actual1 = CHType.mergeInputTypes(CHFuzzableType.values.filter(_.name.startsWith("Map(UInt")).toSet)
            val actual2 =
              CHType.mergeInputTypes(
                Set(
                  CHSpecialType.Map(CHAggregatedType.UIntMax64Bits, CHAggregatedType.Int),
                  CHFuzzableType.MapUInt128Int,
                  CHFuzzableType.MapUInt256Int
                )
              )
            val expected = Set(CHSpecialType.Map(CHAggregatedType.UInt, CHAggregatedType.Int))

            actual1 shouldBe expected
            actual2 shouldBe expected
          }
          "to Map(UIntMax64Bits, Int)" in {
            val actual = CHType.mergeInputTypes(
              Set(
                CHFuzzableType.MapUInt8Int,
                CHFuzzableType.MapUInt16Int,
                CHFuzzableType.MapUInt32Int,
                CHFuzzableType.MapUInt64Int
              )
            )
            val expected = Set(CHSpecialType.Map(CHAggregatedType.UIntMax64Bits, CHAggregatedType.Int))

            actual shouldBe expected
          }
        }

        "of type number" - {
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
            val intTypes =
              CHFuzzableType.values.filter(_.name.startsWith("Int")).filterNot(_.name.startsWith("Interval"))
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
            val actual1 =
              CHType.mergeInputTypes(Set(CHAggregatedType.Int, CHAggregatedType.UInt, CHAggregatedType.Float))
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
              CHType.mergeInputTypes(
                Set(CHAggregatedType.UIntMax64Bits, CHFuzzableType.UInt128, CHFuzzableType.UInt256)
              )
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

        "of type Tuple" - {
          val tuplesTypes =
            Try(
              CHFuzzableType.values
                .filter(_.name.startsWith("Tuple("))
                .map(t => (t, CHType.getByName(t.name).asInstanceOf[CHSpecialType.Tuple]))
            ).recover(err =>
              err.printStackTrace()
              throw err
            ).get

          "to Tuple1(Any)" in {
            val actual = CHType.mergeInputTypes(tuplesTypes.filter(_._2.innerTypes.size == 1).map(_._1).toSet)
            val expected = Set(CHSpecialType.Tuple(Seq(CHAggregatedType.Any)))

            actual shouldBe expected
          }

          "to Tuple1(DecimalLike)" in {
            val actual = CHType.mergeInputTypes(CHFuzzableType.values.filter(_.name.startsWith("Tuple(Decimal")).toSet)
            val expected = Set(CHSpecialType.Tuple(Seq(CHAggregatedType.DecimalLike)))

            actual shouldBe expected
          }
          "to Tuple1(Float)" in {
            val actual = CHType.mergeInputTypes(Set(CHFuzzableType.Tuple1Float32, CHFuzzableType.Tuple1Float64))
            val expected = Set(CHSpecialType.Tuple(Seq(CHAggregatedType.Float)))

            actual shouldBe expected
          }
          "to Tuple1(Int)" in {
            val intTypes = CHFuzzableType.values
              .filter(_.name.startsWith("Tuple(Int"))
              .filterNot(_.name.startsWith("Tuple(Interval"))
            val actual1 = CHType.mergeInputTypes(intTypes.toSet)
            val actual2 = CHType.mergeInputTypes(intTypes.toSet + CHFuzzableType.Tuple1Boolean)
            val expected = Set(CHSpecialType.Tuple(Seq(CHAggregatedType.Int)))

            actual1 shouldBe expected
            actual2 shouldBe expected
          }
          "to Tuple1(IntMax64Bits)" in {
            val actual1 = CHType.mergeInputTypes(
              Set(
                CHFuzzableType.Tuple1Int8,
                CHFuzzableType.Tuple1Int16,
                CHFuzzableType.Tuple1Int32,
                CHFuzzableType.Tuple1Int64
              )
            )
            val actual2 = CHType.mergeInputTypes(
              Set(
                CHFuzzableType.Tuple1Boolean,
                CHFuzzableType.Tuple1Int8,
                CHFuzzableType.Tuple1Int16,
                CHFuzzableType.Tuple1Int32,
                CHFuzzableType.Tuple1Int64
              )
            )
            val expected = Set(CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax64Bits)))

            actual1 shouldBe expected
            actual2 shouldBe expected
          }
          "to Tuple1(NonDecimal)" in {
            val actual1 = CHType.mergeInputTypes(
              Set(
                CHSpecialType.Tuple(Seq(CHAggregatedType.Int)),
                CHSpecialType.Tuple(Seq(CHAggregatedType.UInt)),
                CHSpecialType.Tuple(Seq(CHAggregatedType.Float))
              )
            )
            val actual2 = CHType.mergeInputTypes(
              Set(
                CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax64Bits)),
                CHFuzzableType.Tuple1Int128,
                CHFuzzableType.Tuple1Int256,
                CHFuzzableType.Tuple1UInt128,
                CHFuzzableType.Tuple1UInt256,
                CHSpecialType.Tuple(Seq(CHAggregatedType.Float))
              )
            )
            val actual3 = CHType.mergeInputTypes(
              Set(
                CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalMax64Bits)),
                CHFuzzableType.Tuple1Int128,
                CHFuzzableType.Tuple1Int256,
                CHFuzzableType.Tuple1UInt128,
                CHFuzzableType.Tuple1UInt256
              )
            )
            val expected = Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimal)))

            actual1 shouldBe expected
            actual2 shouldBe expected
            actual3 shouldBe expected
          }
          "to Tuple1(NonDecimalMax64Bits)" in {
            val actual1 = CHType.mergeInputTypes(
              Set(
                CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax64Bits)),
                CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax64Bits)),
                CHSpecialType.Tuple(Seq(CHAggregatedType.Float))
              )
            )
            val actual2 = CHType.mergeInputTypes(
              Set(
                CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax64Bits)),
                CHSpecialType.Tuple(Seq(CHAggregatedType.Float))
              )
            )
            val expected = Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalMax64Bits)))

            actual1 shouldBe expected
            actual2 shouldBe expected
          }
          "to Tuple1(NonDecimalNorFloatMax64Bits)" in {
            val actual = CHType.mergeInputTypes(
              Set(
                CHSpecialType.Tuple(Seq(CHAggregatedType.IntMax64Bits)),
                CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax64Bits))
              )
            )
            val expected = Set(CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimalNorFloatMax64Bits)))

            actual shouldBe expected
          }
          "to Tuple1(Number)" in {
            val actual = CHType.mergeInputTypes(
              Set(
                CHSpecialType.Tuple(Seq(CHAggregatedType.NonDecimal)),
                CHSpecialType.Tuple(Seq(CHAggregatedType.DecimalLike))
              )
            )
            val expected = Set(CHSpecialType.Tuple(Seq(CHAggregatedType.Number)))

            actual shouldBe expected
          }
          "to Tuple1(UInt)" in {
            val actual1 = CHType.mergeInputTypes(CHFuzzableType.values.filter(_.name.startsWith("Tuple(UInt")).toSet)
            val actual2 =
              CHType.mergeInputTypes(
                Set(
                  CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax64Bits)),
                  CHFuzzableType.Tuple1UInt128,
                  CHFuzzableType.Tuple1UInt256
                )
              )
            val expected = Set(CHSpecialType.Tuple(Seq(CHAggregatedType.UInt)))

            actual1 shouldBe expected
            actual2 shouldBe expected
          }
          "to Tuple1(UIntMax64Bits)" in {
            val actual = CHType.mergeInputTypes(
              Set(
                CHFuzzableType.Tuple1UInt8,
                CHFuzzableType.Tuple1UInt16,
                CHFuzzableType.Tuple1UInt32,
                CHFuzzableType.Tuple1UInt64
              )
            )
            val expected = Set(CHSpecialType.Tuple(Seq(CHAggregatedType.UIntMax64Bits)))

            actual shouldBe expected
          }

          "to Tuple2(Any, Any)" in {
            val actual = CHType.mergeInputTypes(tuplesTypes.filter(_._2.innerTypes.size == 2).map(_._1).toSet)

            @nowarn // No Tuple2 implemented yet
            val expected = Set(CHSpecialType.Tuple(Seq(CHAggregatedType.Any, CHAggregatedType.Any)))

            actual shouldBe Set.empty // expected
          }

          "to Tuple3(Any, Any, Any)" in {
            val actual = CHType.mergeInputTypes(tuplesTypes.filter(_._2.innerTypes.size == 3).map(_._1).toSet)

            @nowarn // No Tuple3 implemented yet
            val expected =
              Set(CHSpecialType.Tuple(Seq(CHAggregatedType.Any, CHAggregatedType.Any, CHAggregatedType.Any)))

            actual shouldBe Set.empty // expected
          }
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
