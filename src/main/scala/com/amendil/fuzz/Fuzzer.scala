package com.amendil.fuzz

import com.amendil.ConcurrencyUtils.executeChain
import com.amendil.entities._
import com.amendil.http.CHClient

import scala.concurrent.{ExecutionContext, Future}

object Fuzzer:
  def fuzz(functionName: String)(implicit client: CHClient, ec: ExecutionContext): Future[CHFunctionFuzzResult] =
    val fuzzingFunctionsWithCost: Seq[((CHFunctionFuzzResult) => Future[CHFunctionFuzzResult], Long)] =
      FuzzerSpecialFunctions.fuzzingFunctionWithCost ++
        FuzzerNonParametricFunctions.fuzzingFunctionWithCost ++
        FuzzerParametricFunctions.fuzzingFunctionWithCost

    val sortedFuzzingFunctions = fuzzingFunctionsWithCost.sortBy(_._2).map(_._1)

    executeChain(
      CHFunctionFuzzResult(name = functionName),
      sortedFuzzingFunctions
    )

  /**
    * @param chAbstractTypeList List of CHAbstractType that will be used to generate the combinations
    * @return All combinations of CHAbstractType of argCount elements.
    */
  private[fuzz] def generateCHAbstractTypeCombinations(
      argCount: Int,
      chAbstractTypeList: Seq[CHAbstractType] = CHAbstractType.values.toSeq
  ): Seq[Seq[CHAbstractType]] =
    generateCHAbstractTypeCombinations(argCount = argCount, currentArgs = Nil, chAbstractTypeList = chAbstractTypeList)

  private def generateCHAbstractTypeCombinations(
      argCount: Int,
      currentArgs: Seq[CHAbstractType],
      chAbstractTypeList: Seq[CHAbstractType]
  ): Seq[Seq[CHAbstractType]] =
    if argCount > 0 then
      chAbstractTypeList.toSeq.map { abstractType =>
        generateCHAbstractTypeCombinations(argCount - 1, currentArgs :+ abstractType, chAbstractTypeList)
      }.flatten
    else Seq(currentArgs)

  private[fuzz] def generateCHTypeCombinations(
      abstractTypes: Seq[CHAbstractType],
      currentArgs: Seq[CHType] = Seq.empty
  ): Seq[Seq[CHType]] =
    abstractTypes match
      case Seq(head, tail @ _*) =>
        head.chTypes
          .map(chType => generateCHTypeCombinations(tail, currentArgs :+ chType))
          .flatten
      case Seq() => Seq(currentArgs)

  private[fuzz] def buildFuzzingValuesArgs(argumentsValues: Seq[Seq[String]]): Seq[String] =
    argumentsValues match
      case Seq()   => throw IllegalArgumentException("Tried to fuzz an argument without any value")
      case Seq(el) => el
      case Seq(head, tail @ _*) =>
        val subChoices = buildFuzzingValuesArgs(tail)
        head.flatMap(el => subChoices.map(subChoice => s"$el, $subChoice"))
