package com.amendil

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CHTypeSpec extends AnyFreeSpec with Matchers {

  "CHAbstractType" - {
    "should list all CHType" in {
      val actual = CHAbstractType.values.flatMap(_.chTypes)
      val expected = CHType.values

      expected.toSet.diff(actual.toSet) shouldBe Set.empty
    }

    "should reference a CHType only once" in {
      val referencedCHTypes = CHAbstractType.values.flatMap(_.chTypes)

      val duplicates = referencedCHTypes diff referencedCHTypes.distinct
      duplicates shouldBe Array.empty[CHType]
    }
  }
}
