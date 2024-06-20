package com.amendil.signature.entities

import com.amendil.common.entities.*
import com.amendil.common.entities.`type`.*
import com.amendil.common.http.CHClient
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

class CHFunctionIOSpec extends AnyFreeSpec with Matchers:

  "CHFunctionIO" - {
    given ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))
    given client: CHClient = new CHClient:
      override def execute(query: String): Future[CHResponse] = ???
      override def executeNoResult(query: String): Future[Unit] = ???
      override def executeNoResultNoSettings(query: String): Future[Unit] = ???

    "should not aggregate signatures" - {
      "based on their generic types" - {
        "when they are Scalars" in {
          val innerTypes = Seq(CHAggregatedType.Float, CHAggregatedType.NonDecimalNorFloat)
          val genericTypes = innerTypes.map(t => CHSpecialType.GenericType("T1", t))
          val nonAggregatedFunctions = genericTypes.map(t => CHFunctionIO.Function2(t, t, CHFuzzableType.UInt8))

          val actual = CHFunctionIO.aggregate(nonAggregatedFunctions)
          val expected = nonAggregatedFunctions

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "when they are Arrays" in {
          val innerTypes = Seq(CHAggregatedType.Float, CHAggregatedType.NonDecimalNorFloat)
          val genericTypes = innerTypes.map(t => CHSpecialType.GenericType("T1", t))
          val nonAggregatedFunctions = genericTypes.map(t =>
            CHFunctionIO.Function2(CHSpecialType.Array(t), CHSpecialType.Array(t), CHFuzzableType.UInt8)
          )

          val actual = CHFunctionIO.aggregate(nonAggregatedFunctions)
          val expected = nonAggregatedFunctions

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "when they are in different kind of types" in {
          val innerTypes = Seq(CHAggregatedType.Float, CHAggregatedType.NonDecimalNorFloat)
          val genericTypes = innerTypes.map(t => CHSpecialType.GenericType("T1", t))
          val nonAggregatedFunctions =
            genericTypes.map(t => CHFunctionIO.Function2(CHSpecialType.Array(t), t, CHFuzzableType.UInt8))

          val actual = CHFunctionIO.aggregate(nonAggregatedFunctions)
          val expected = nonAggregatedFunctions

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }
      }
    }

    "should aggregate signatures" - {
      val arrayTypes: Seq[CHSpecialType.Array] =
        CHFuzzableType.values
          .filter(_.name.startsWith("Array("))
          .map(t => CHType.normalize(t).asInstanceOf[CHSpecialType.Array])
          .toSeq

      val bitmapTypes: Seq[CHSpecialType.Bitmap] =
        CHFuzzableType.values
          .filter(_.name.startsWith("Bitmap("))
          .map(t => CHType.normalize(t).asInstanceOf[CHSpecialType.Bitmap])
          .toSeq

      val dateTypes: Seq[CHFuzzableType] =
        CHFuzzableType.values
          .filter(_.name.startsWith("Date"))
          .filterNot(_.name.toLowerCase().contains("unit"))
          .toSeq

      val floatTypes: Seq[CHFuzzableType] =
        CHType.allNumberTypes.collect { case t: CHFuzzableType if t.name.startsWith("Float") => t }.toSeq

      val intervalTypes: Seq[CHFuzzableType] =
        CHFuzzableType.values
          .filter(_.name.startsWith("Interval"))
          .toSeq

      val mapTypes: Seq[CHSpecialType.Map] =
        CHFuzzableType.values
          .filter(_.name.startsWith("Map("))
          .map(t => CHType.normalize(t).asInstanceOf[CHSpecialType.Map])
          .toSeq

      val numberTypes: Seq[CHFuzzableType] =
        CHType.allNumberTypes.collect { case t: CHFuzzableType => t }.toSeq

      val stringTypes: Seq[CHFuzzableType] =
        Seq(CHFuzzableType.FixedString, CHFuzzableType.StringType)

      "having a unique output type" - {
        val outputType = CHAggregatedType.Any

        "when a argument accept all numbers" in {
          val actual1 = CHFunctionIO.aggregate(
            numberTypes.map(CHFunctionIO.Function2(_, CHFuzzableType.StringType, outputType))
          )
          val expected1 = Seq(CHFunctionIO.Function2(CHAggregatedType.Number, CHFuzzableType.StringType, outputType))
          actual1.map(_.asString()) shouldBe expected1.map(_.asString())

          val actual2 = CHFunctionIO.aggregate(
            numberTypes.map(CHFunctionIO.Function2(CHFuzzableType.StringType, _, outputType))
          )
          val expected2 = Seq(CHFunctionIO.Function2(CHFuzzableType.StringType, CHAggregatedType.Number, outputType))
          actual2.map(_.asString()) shouldBe expected2.map(_.asString())
        }

        "when a parameter accept all numbers" in {
          val actual1 = CHFunctionIO.aggregate(
            numberTypes.map(
              CHFunctionIO.Parametric2Function1(_, CHFuzzableType.StringType, CHFuzzableType.StringType, outputType)
            )
          )
          val expected1 = Seq(
            CHFunctionIO.Parametric2Function1(
              CHAggregatedType.Number,
              CHFuzzableType.StringType,
              CHFuzzableType.StringType,
              outputType
            )
          )
          actual1.map(_.asString()) shouldBe expected1.map(_.asString())

          val actual2 = CHFunctionIO.aggregate(
            numberTypes.map(
              CHFunctionIO.Parametric2Function1(CHFuzzableType.StringType, _, CHFuzzableType.StringType, outputType)
            )
          )
          val expected2 = Seq(
            CHFunctionIO.Parametric2Function1(
              CHFuzzableType.StringType,
              CHAggregatedType.Number,
              CHFuzzableType.StringType,
              outputType
            )
          )
          actual2.map(_.asString()) shouldBe expected2.map(_.asString())
        }

        "when multiple columns accept aggregatable types" in {
          val actual = CHFunctionIO.aggregate(
            for
              number <- numberTypes
              str <- stringTypes
              date <- dateTypes
              interval <- intervalTypes
            yield CHFunctionIO.Parametric2Function2(number, str, date, interval, outputType)
          )

          val expected = Seq(
            CHFunctionIO.Parametric2Function2(
              CHAggregatedType.Number,
              CHAggregatedType.StringLike,
              CHAggregatedType.DateLikeOrDateTimeLike,
              CHAggregatedType.Interval,
              outputType
            )
          )

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "when multiple columns accept the exact same type" in {
          val actual = CHFunctionIO.aggregate(
            for
              number <- numberTypes
              arrayType <- arrayTypes
            yield CHFunctionIO.Parametric2Function3(number, arrayType, number, arrayType, number, outputType)
          )

          val expectedGenericType1 = CHSpecialType.GenericType("T1", CHAggregatedType.Number)
          val expectedGenericType2 = CHSpecialType.Array(CHSpecialType.GenericType("T2", CHAggregatedType.Any))
          val expected = Seq(
            CHFunctionIO.Parametric2Function3(
              expectedGenericType1,
              expectedGenericType2,
              expectedGenericType1,
              expectedGenericType2,
              expectedGenericType1,
              outputType
            )
          )

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }
      }

      "having a non unique output" - {
        "scalar input column == output column" in {
          val actual1 = CHFunctionIO.aggregate(
            numberTypes.map(t => CHFunctionIO.Function2(t, CHFuzzableType.StringType, t))
          )

          val expectedGenericType = CHSpecialType.GenericType("T1", CHAggregatedType.Number)
          val expected1 =
            Seq(CHFunctionIO.Function2(expectedGenericType, CHFuzzableType.StringType, expectedGenericType))

          actual1.map(_.asString()) shouldBe expected1.map(_.asString())

          // Only in this test case: we check the aggregation works on another column
          // Let's keep the other tests simple
          val actual2 = CHFunctionIO.aggregate(
            numberTypes.map(t => CHFunctionIO.Function2(CHFuzzableType.StringType, t, t))
          )

          val expected2 =
            Seq(CHFunctionIO.Function2(CHFuzzableType.StringType, expectedGenericType, expectedGenericType))

          actual2.map(_.asString()) shouldBe expected2.map(_.asString())
        }

        "Array(input column) == Array(output type)" in {
          val actual = CHFunctionIO.aggregate(
            arrayTypes.map(t => CHFunctionIO.Function1(t, t))
          )

          val expectedGenericType = CHSpecialType.GenericType("T1", CHAggregatedType.Any)
          val expectedAggregatedType = CHSpecialType.Array(expectedGenericType)
          val expected = Seq(CHFunctionIO.Function1(expectedAggregatedType, expectedAggregatedType))

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Map(input column, UnknownType) == Map(input column, Int8)" in {
          val actual = CHFunctionIO.aggregate(
            mapTypes.map(t =>
              CHFunctionIO.Function2(
                t,
                CHFuzzableType.StringType,
                CHSpecialType.Map(CHType.normalize(t).asInstanceOf[CHSpecialType.Map].keyType, CHFuzzableType.Int8)
              )
            )
          )

          val expectedGenericType = CHSpecialType.GenericType("T1", CHAggregatedType.MapKey)
          val expected = Seq(
            CHFunctionIO.Function2(
              CHSpecialType.Map(expectedGenericType, CHSpecialType.UnknownType),
              CHFuzzableType.StringType,
              CHSpecialType.Map(expectedGenericType, CHFuzzableType.Int8)
            )
          )

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Array(input column) == output type" in {
          val actual = CHFunctionIO.aggregate(
            arrayTypes.map(t => CHFunctionIO.Function1(t, t.innerType))
          )

          val expectedGenericType = CHSpecialType.GenericType("T1", CHAggregatedType.Any)
          val expectedAggregatedType = CHSpecialType.Array(expectedGenericType)
          val expected = Seq(CHFunctionIO.Function1(expectedAggregatedType, expectedGenericType))

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "input column == Array(output type)" in {
          val actual = CHFunctionIO.aggregate(
            arrayTypes.map(t => CHFunctionIO.Function1(t.innerType, t))
          )

          val expectedGenericType = CHSpecialType.GenericType("T1", CHAggregatedType.Any)
          val expectedAggregatedType = CHSpecialType.Array(expectedGenericType)
          val expected = Seq(CHFunctionIO.Function1(expectedGenericType, expectedAggregatedType))

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Array(input column) == Array(Array(output type))" in {
          val actual = CHFunctionIO.aggregate(
            arrayTypes.map(t => CHFunctionIO.Function1(t, CHSpecialType.Array(t)))
          )

          val expectedGenericType = CHSpecialType.GenericType("T1", CHAggregatedType.Any)
          val expected = Seq(
            CHFunctionIO.Function1(
              CHSpecialType.Array(expectedGenericType),
              CHSpecialType.Array(CHSpecialType.Array(expectedGenericType))
            )
          )

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Bitmap(input column) == output type" in {
          val actual = CHFunctionIO.aggregate(
            bitmapTypes.map(t => CHFunctionIO.Function1(t, t.innerType))
          )

          val expectedGenericType = CHSpecialType.GenericType("T1", CHAggregatedType.NonDecimalNorFloatMax64Bits)
          val expectedAggregatedType = CHSpecialType.Bitmap(expectedGenericType)
          val expected = Seq(CHFunctionIO.Function1(expectedAggregatedType, expectedGenericType))

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Map(input column, UnknownType) == output type" in {
          val actual = CHFunctionIO.aggregate(
            mapTypes.map(t => CHFunctionIO.Function1(t, t.keyType))
          )

          val expectedGenericType = CHSpecialType.GenericType("T1", CHAggregatedType.MapKey)
          val expectedAggregatedType: CHSpecialType.Map =
            CHSpecialType.Map(expectedGenericType, CHSpecialType.UnknownType)
          val expected = Seq(CHFunctionIO.Function1(expectedAggregatedType, expectedGenericType))

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "input column == Map(output type, UInt32)" in {
          val actual = CHFunctionIO.aggregate(
            mapTypes.map(t => CHFunctionIO.Function1(t.keyType, CHSpecialType.Map(t.keyType, CHFuzzableType.UInt32)))
          )

          val expectedGenericType = CHSpecialType.GenericType("T1", CHAggregatedType.MapKey)
          val expectedAggregatedType: CHSpecialType.Map = CHSpecialType.Map(expectedGenericType, CHFuzzableType.UInt32)
          val expected = Seq(CHFunctionIO.Function1(expectedGenericType, expectedAggregatedType))

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "input column == Array(Tuple(output type, UInt8, UInt32))" in {
          val arrayTupleTypes = numberTypes.map(n =>
            CHSpecialType.Array(CHSpecialType.Tuple(Seq(n, CHFuzzableType.UInt8, CHFuzzableType.UInt32)))
          )
          val actual = CHFunctionIO.aggregate(
            arrayTupleTypes.map(t =>
              CHFunctionIO.Function1(t.innerType.asInstanceOf[CHSpecialType.Tuple].innerTypes.head, t)
            )
          )

          val expectedGenericType = CHSpecialType.GenericType("T1", CHAggregatedType.Number)
          val expectedAggregatedType = CHSpecialType.Array(
            CHSpecialType.Tuple(Seq(expectedGenericType, CHFuzzableType.UInt8, CHFuzzableType.UInt32))
          )
          val expected = Seq(CHFunctionIO.Function1(expectedGenericType, expectedAggregatedType))

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Array(input column) == Map(output column, UInt32)" in {
          val actual = CHFunctionIO.aggregate(
            numberTypes.map(t =>
              CHFunctionIO.Function1(
                CHSpecialType.Array(t),
                CHSpecialType.Map(t, CHFuzzableType.UInt32)
              )
            )
          )

          val expectedGenericType = CHSpecialType.GenericType("T1", CHAggregatedType.Number)
          val expected = Seq(
            CHFunctionIO.Function1(
              CHSpecialType.Array(expectedGenericType),
              CHSpecialType.Map(expectedGenericType, CHFuzzableType.UInt32)
            )
          )

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Array(input column) == Array(Tuple(output column))" in {
          val actual = CHFunctionIO.aggregate(
            numberTypes.map(t =>
              CHFunctionIO.Function1(
                CHSpecialType.Array(t),
                CHSpecialType.Array(CHSpecialType.Tuple(Seq(t)))
              )
            )
          )

          val expectedGenericType = CHSpecialType.GenericType("T1", CHAggregatedType.Number)
          val expected = Seq(
            CHFunctionIO.Function1(
              CHSpecialType.Array(expectedGenericType),
              CHSpecialType.Array(CHSpecialType.Tuple(Seq(expectedGenericType)))
            )
          )

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Array(input column1), Array(input column2) == Map(output column1, output column2) with numbers on second arg" in {
          val actual = CHFunctionIO.aggregate(
            for
              numberType1 <- numberTypes
              numberType2 <- numberTypes
            yield CHFunctionIO.Function2(
              CHSpecialType.Array(numberType1),
              CHSpecialType.Array(numberType2),
              CHSpecialType.Map(numberType1, numberType2)
            )
          )

          val expectedGenericType1 = CHSpecialType.GenericType("T1", CHAggregatedType.Number)
          val expectedGenericType2 = CHSpecialType.GenericType("T2", CHAggregatedType.Number)
          val expected = Seq(
            CHFunctionIO.Function2(
              CHSpecialType.Array(expectedGenericType1),
              CHSpecialType.Array(expectedGenericType2),
              CHSpecialType.Map(expectedGenericType1, expectedGenericType2)
            )
          )

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Array(input column1), Array(input column2) == Map(output column1, output column2) with maps on second arg" in {
          val actual = CHFunctionIO.aggregate(
            for
              numberType <- numberTypes
              mapKeyType <- mapTypes.map(_.keyType)
            yield CHFunctionIO.Function2(
              CHSpecialType.Array(numberType),
              CHSpecialType.Map(mapKeyType, CHSpecialType.UnknownType),
              CHSpecialType.Map(numberType, CHSpecialType.Tuple(Seq(mapKeyType, CHFuzzableType.UInt32)))
            )
          )

          val expectedGenericType1 = CHSpecialType.GenericType("T1", CHAggregatedType.Number)
          val expectedGenericType2 = CHSpecialType.GenericType("T2", CHAggregatedType.MapKey)
          val expected = Seq(
            CHFunctionIO.Function2(
              CHSpecialType.Array(expectedGenericType1),
              CHSpecialType.Map(expectedGenericType2, CHSpecialType.UnknownType),
              CHSpecialType.Map(
                expectedGenericType1,
                CHSpecialType.Tuple(Seq(expectedGenericType2, CHSpecialType.UnknownType))
              )
            )
          )

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Array(input column1), Array(input column2) == Map(output column1, output column2) with numbers and maps on second arg" in {
          val actual = CHFunctionIO.aggregate(
            (for
              numberType1 <- numberTypes
              numberType2 <- numberTypes
            yield CHFunctionIO.Function2(
              CHSpecialType.Array(numberType1),
              CHSpecialType.Array(numberType2),
              CHSpecialType.Map(numberType1, numberType2)
            ))
              ++
                (for
                  numberType <- numberTypes
                  mapKeyType <- mapTypes.map(_.keyType)
                yield CHFunctionIO.Function2(
                  CHSpecialType.Array(numberType),
                  CHSpecialType.Map(mapKeyType, CHSpecialType.UnknownType),
                  CHSpecialType.Map(numberType, CHSpecialType.Tuple(Seq(mapKeyType, CHFuzzableType.UInt32)))
                ))
          )

          val expectedGenericType1 = CHSpecialType.GenericType("T1", CHAggregatedType.Number)
          val expectedGenericType2 = CHSpecialType.GenericType("T2", CHAggregatedType.Number)
          val expectedGenericType3 = CHSpecialType.GenericType("T2", CHAggregatedType.MapKey)
          val expected = Seq(
            CHFunctionIO.Function2(
              CHSpecialType.Array(expectedGenericType1),
              CHSpecialType.Array(expectedGenericType2),
              CHSpecialType.Map(expectedGenericType1, expectedGenericType2)
            ),
            CHFunctionIO.Function2(
              CHSpecialType.Array(expectedGenericType1),
              CHSpecialType.Map(expectedGenericType3, CHSpecialType.UnknownType),
              CHSpecialType.Map(
                expectedGenericType1,
                CHSpecialType.Tuple(Seq(expectedGenericType3, CHSpecialType.UnknownType))
              )
            )
          )

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Array(input column1) == Tuple(Array(output column1))" in {
          val actual = CHFunctionIO.aggregate(
            for floatType1 <- floatTypes
            yield CHFunctionIO.Function1(
              CHSpecialType.Array(floatType1),
              CHSpecialType.Tuple(Seq(CHSpecialType.Array(floatType1)))
            )
          )

          val expectedGenericType1 = CHSpecialType.GenericType("T1", CHAggregatedType.Float)
          val expected = Seq(
            CHFunctionIO.Function1(
              CHSpecialType.Array(expectedGenericType1),
              CHSpecialType.Tuple(Seq(CHSpecialType.Array(expectedGenericType1)))
            )
          )

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Array(input column1), Array(input column2) == Tuple(Array(output column1), Array(Nullableoutput column2))" in {
          val actual = CHFunctionIO.aggregate(
            for
              floatType1 <- floatTypes
              floatType2 <- floatTypes
            yield CHFunctionIO.Function2(
              CHSpecialType.Array(floatType1),
              CHSpecialType.Array(floatType2),
              CHSpecialType.Tuple(Seq(CHSpecialType.Array(floatType1), CHSpecialType.Array(floatType2)))
            )
          )

          val expectedGenericType1 = CHSpecialType.GenericType("T1", CHAggregatedType.Float)
          val expectedGenericType2 = CHSpecialType.GenericType("T2", CHAggregatedType.Float)
          val expected = Seq(
            CHFunctionIO.Function2(
              CHSpecialType.Array(expectedGenericType1),
              CHSpecialType.Array(expectedGenericType2),
              CHSpecialType.Tuple(
                Seq(CHSpecialType.Array(expectedGenericType1), CHSpecialType.Array(expectedGenericType2))
              )
            )
          )

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Array(input column1), Array(input column2), Array(input column3) == Tuple(Array(output column1), Array(Nullableoutput column2), Array(output column3))" in {
          val actual = CHFunctionIO.aggregate(
            for
              floatType1 <- floatTypes
              floatType2 <- floatTypes
              floatType3 <- floatTypes
            yield CHFunctionIO.Function3(
              CHSpecialType.Array(floatType1),
              CHSpecialType.Array(floatType2),
              CHSpecialType.Array(floatType3),
              CHSpecialType.Tuple(
                Seq(CHSpecialType.Array(floatType1), CHSpecialType.Array(floatType2), CHSpecialType.Array(floatType3))
              )
            )
          )

          val expectedGenericType1 = CHSpecialType.GenericType("T1", CHAggregatedType.Float)
          val expectedGenericType2 = CHSpecialType.GenericType("T2", CHAggregatedType.Float)
          val expectedGenericType3 = CHSpecialType.GenericType("T3", CHAggregatedType.Float)
          val expected = Seq(
            CHFunctionIO.Function3(
              CHSpecialType.Array(expectedGenericType1),
              CHSpecialType.Array(expectedGenericType2),
              CHSpecialType.Array(expectedGenericType3),
              CHSpecialType.Tuple(
                Seq(
                  CHSpecialType.Array(expectedGenericType1),
                  CHSpecialType.Array(expectedGenericType2),
                  CHSpecialType.Array(expectedGenericType3)
                )
              )
            )
          )

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Array(input column1) == Tuple(Array(Nullable(output column1)))" in {
          val actual = CHFunctionIO.aggregate(
            for floatType1 <- floatTypes
            yield CHFunctionIO.Function1(
              CHSpecialType.Array(floatType1),
              CHSpecialType.Tuple(Seq(CHSpecialType.Array(CHSpecialType.Nullable(floatType1))))
            )
          )

          val expectedGenericType1 = CHSpecialType.GenericType("T1", CHAggregatedType.Float)
          val expected = Seq(
            CHFunctionIO.Function1(
              CHSpecialType.Array(expectedGenericType1),
              CHSpecialType.Tuple(Seq(CHSpecialType.Array(CHSpecialType.Nullable(expectedGenericType1))))
            )
          )

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Array(input column1), Array(input column2) == Tuple(Array(Nullable(output column1)), Array(Nullable(output column2)))" in {
          val actual = CHFunctionIO.aggregate(
            for
              floatType1 <- floatTypes
              floatType2 <- floatTypes
            yield CHFunctionIO.Function2(
              CHSpecialType.Array(floatType1),
              CHSpecialType.Array(floatType2),
              CHSpecialType.Tuple(
                Seq(
                  CHSpecialType.Array(CHSpecialType.Nullable(floatType1)),
                  CHSpecialType.Array(CHSpecialType.Nullable(floatType2))
                )
              )
            )
          )

          val expectedGenericType1 = CHSpecialType.GenericType("T1", CHAggregatedType.Float)
          val expectedGenericType2 = CHSpecialType.GenericType("T2", CHAggregatedType.Float)
          val expected = Seq(
            CHFunctionIO.Function2(
              CHSpecialType.Array(expectedGenericType1),
              CHSpecialType.Array(expectedGenericType2),
              CHSpecialType.Tuple(
                Seq(
                  CHSpecialType.Array(CHSpecialType.Nullable(expectedGenericType1)),
                  CHSpecialType.Array(CHSpecialType.Nullable(expectedGenericType2))
                )
              )
            )
          )

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Array(input column1), Array(input column2), Array(input column3) == Tuple(Array(Nullable(output column1)), Array(Nullable(output column2)), Array(Nullable(output column3)))" in {
          val actual = CHFunctionIO.aggregate(
            for
              floatType1 <- floatTypes
              floatType2 <- floatTypes
              floatType3 <- floatTypes
            yield CHFunctionIO.Function3(
              CHSpecialType.Array(floatType1),
              CHSpecialType.Array(floatType2),
              CHSpecialType.Array(floatType3),
              CHSpecialType.Tuple(
                Seq(
                  CHSpecialType.Array(CHSpecialType.Nullable(floatType1)),
                  CHSpecialType.Array(CHSpecialType.Nullable(floatType2)),
                  CHSpecialType.Array(CHSpecialType.Nullable(floatType3))
                )
              )
            )
          )

          val expectedGenericType1 = CHSpecialType.GenericType("T1", CHAggregatedType.Float)
          val expectedGenericType2 = CHSpecialType.GenericType("T2", CHAggregatedType.Float)
          val expectedGenericType3 = CHSpecialType.GenericType("T3", CHAggregatedType.Float)
          val expected = Seq(
            CHFunctionIO.Function3(
              CHSpecialType.Array(expectedGenericType1),
              CHSpecialType.Array(expectedGenericType2),
              CHSpecialType.Array(expectedGenericType3),
              CHSpecialType.Tuple(
                Seq(
                  CHSpecialType.Array(CHSpecialType.Nullable(expectedGenericType1)),
                  CHSpecialType.Array(CHSpecialType.Nullable(expectedGenericType2)),
                  CHSpecialType.Array(CHSpecialType.Nullable(expectedGenericType3))
                )
              )
            )
          )

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }

        "Variant(Array(input column), Map(input column1, input column2)) == Variant(output column, Tuple(output column1, output column2))" in {
          val actual = CHFunctionIO.aggregate(
            (for numberType <- numberTypes
            yield CHFunctionIO.Function1(CHSpecialType.Array(numberType), numberType))
              ++
                (for mapKeyType <- mapTypes.map(_.keyType)
                yield CHFunctionIO.Function1(
                  CHSpecialType.Map(mapKeyType, CHSpecialType.UnknownType),
                  CHSpecialType.Tuple(Seq(mapKeyType, CHFuzzableType.UInt32))
                ))
          )

          val expectedGenericType1 = CHSpecialType.GenericType("T1", CHAggregatedType.Number)
          val expectedGenericType2 = CHSpecialType.GenericType("T1", CHAggregatedType.MapKey)
          val expected = Seq(
            CHFunctionIO.Function1(
              CHSpecialType.Array(expectedGenericType1),
              expectedGenericType1
            ),
            CHFunctionIO.Function1(
              CHSpecialType.Map(expectedGenericType2, CHSpecialType.UnknownType),
              CHSpecialType.Tuple(Seq(expectedGenericType2, CHSpecialType.UnknownType))
            )
          )

          actual.map(_.asString()) shouldBe expected.map(_.asString())
        }
      }

      "having a String and a Timezone in the same column but for different signatures" in {
        val actual = CHFunctionIO.aggregate(
          Seq(
            CHFunctionIO.Function2(CHFuzzableType.Date, CHFuzzableType.StringType, CHFuzzableType.StringType),
            CHFunctionIO.Function2(CHFuzzableType.Date32, CHFuzzableType.StringType, CHFuzzableType.StringType),
            CHFunctionIO.Function2(CHFuzzableType.DateTime, CHFuzzableType.TimeZone, CHFuzzableType.StringType),
            CHFunctionIO.Function2(CHFuzzableType.DateTime64, CHFuzzableType.TimeZone, CHFuzzableType.StringType)
          )
        )

        val expected = Seq(
          CHFunctionIO.Function2(CHAggregatedType.DateTimeLike, CHFuzzableType.TimeZone, CHFuzzableType.StringType)
        )

        actual.map(_.asString()) shouldBe expected.map(_.asString())
      }
    }
  }
