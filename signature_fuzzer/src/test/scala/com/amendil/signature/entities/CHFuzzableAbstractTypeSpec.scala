package com.amendil.signature.entities

import com.amendil.common.entities.`type`.CHFuzzableType
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CHFuzzableAbstractTypeSpec extends AnyFreeSpec with Matchers:

  "CHFuzzableAbstractType" - {
    val typesForHardcodedFunctions = Set(
      CHFuzzableType.EncryptionMode,
      CHFuzzableType.PValueComputationMethod,
      CHFuzzableType.TestAlternative,
      CHFuzzableType.Usevar,
      CHFuzzableType.Variant
    )

    val typesForAggregations = Set(
      CHFuzzableType.ArrayEnum,
      CHFuzzableType.Enum,
      CHFuzzableType.MapEnumInt,
      CHFuzzableType.NullableEnum,
      CHFuzzableType.Tuple1Enum
    )

    "should list all CHFuzzableType" in {
      val actual = CHFuzzableAbstractType.values.flatMap(_.chFuzzableTypes).toSet
      val expected = CHFuzzableType.values
        .filterNot(typesForHardcodedFunctions.contains)
        .filterNot(typesForAggregations.contains)
        .toSet

      expected diff actual shouldBe Set.empty
      actual diff expected shouldBe Set.empty
    }

    "should reference a CHFuzzableType only once" in {
      val referencedCHFuzzableTypes = CHFuzzableAbstractType.values.flatMap(_.chFuzzableTypes)

      val duplicates = referencedCHFuzzableTypes diff referencedCHFuzzableTypes.distinct
      duplicates shouldBe Array.empty[CHFuzzableType]
    }
  }
