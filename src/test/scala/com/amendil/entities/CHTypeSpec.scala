package com.amendil.entities

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CHFuzzableTypeSpec extends AnyFreeSpec with Matchers {

  "CHFuzzableAbstractType" - {
    "should list all CHFuzzableType" in {
      val actual = CHFuzzableAbstractType.values.flatMap(_.CHFuzzableTypes)
      val expected = CHFuzzableType.values

      expected.toSet.diff(actual.toSet) shouldBe Set.empty
    }

    "should reference a CHFuzzableType only once" in {
      val referencedCHFuzzableTypes = CHFuzzableAbstractType.values.flatMap(_.CHFuzzableTypes)

      val duplicates = referencedCHFuzzableTypes diff referencedCHFuzzableTypes.distinct
      duplicates shouldBe Array.empty[CHFuzzableType]
    }
  }
}
