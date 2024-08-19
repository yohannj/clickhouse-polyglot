package com.amendil.signature.fuzz

import _root_.com.amendil.signature.entities.CHFuzzableAbstractType
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class FuzzerSpec extends AnyFreeSpec with Matchers:

  "Fuzzer" - {
    "should deduplicate abstract input combinations" - {
      "with 0 common types" in {
        val combination1 = Seq(CHFuzzableAbstractType.String)
        val combination2 = Seq(CHFuzzableAbstractType.Number)

        val actual = Fuzzer.deduplicateAbstractInputCombinations(Seq(combination1, combination2))
        val expected = Seq(combination1, combination2)

        expected diff actual shouldBe Seq.empty
      }

      "with some common types with combinations containing a single column" in {
        val combination1 = Seq(CHFuzzableAbstractType.String)
        val combination2 = Seq(CHFuzzableAbstractType.Number)
        val combination3 = Seq(CHFuzzableAbstractType.SpecialString)

        val actual = Fuzzer.deduplicateAbstractInputCombinations(Seq(combination1, combination2, combination3))
        val expected = Seq(combination1, combination2)

        expected diff actual shouldBe Seq.empty
      }

      "with some common types with combinations containing multiple column" in {
        val combination1 = Seq(CHFuzzableAbstractType.String, CHFuzzableAbstractType.String)
        val combination2 = Seq(CHFuzzableAbstractType.String, CHFuzzableAbstractType.Number)
        val combination3 = Seq(CHFuzzableAbstractType.String, CHFuzzableAbstractType.SpecialString)

        val actual = Fuzzer.deduplicateAbstractInputCombinations(Seq(combination1, combination2, combination3))
        val expected = Seq(combination1, combination2)

        expected diff actual shouldBe Seq.empty
      }
    }
  }
